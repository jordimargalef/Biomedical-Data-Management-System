# ============================================================
# 06_quality_flagging.R
# Biomedical Data Quality Flagging System
# PostgreSQL + R
# ============================================================
#
# PURPOSE:
# This script contains database infrastructure and functions
# for persisting validation flags and rejected patient
# submissions.
#
# IMPORTANT:
# - This script DOES NOT validate patient records.
# - This script DOES NOT insert patient records into patients.
# - This script DOES NOT run tests automatically.
# - This script preserves existing data.
# - This script is designed to work with:
#   04_metadata_setup.R
#   05_validation_engine.R
#   07_insert_patient_pipeline.R
#
# MAIN RESPONSIBILITIES:
# - Create public.quality_flags if missing
# - Create public.rejected_patient_submissions if missing
# - Insert one quality flag
# - Insert multiple quality flags
# - Insert one rejected patient submission
# - Retrieve flags by patient_uuid
# - Retrieve flags by submission_id
# - Count flags by severity
# - Count flags by issue_type
# - Count flags by variable
# - Count flags by detected_by_user
# - Generate quality summaries
# - Create human-readable admin views for pgAdmin
#
# ============================================================

# ------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------

# DBI provides the standard interface that R uses to communicate with databases.
# It is used here for connecting, executing SQL statements, querying tables,
# handling transactions, and disconnecting from PostgreSQL.
library(DBI)

# RPostgres is the PostgreSQL driver used by DBI.
# It allows this script to connect specifically to the PostgreSQL database.
library(RPostgres)

# tidyverse provides data manipulation tools such as tibble(), bind_rows(),
# pipes, filtering, and other utilities used throughout this script.
library(tidyverse)

# stringr provides safe string manipulation functions.
# In this script it is used, for example, to trim empty strings and process text values.
library(stringr)

# lubridate provides date and time utilities.
# It is loaded for consistency with the rest of the biomedical data system.
library(lubridate)

# uuid provides UUIDgenerate(), which is used to create unique identifiers
# for rejected submissions and quality flags.
library(uuid)

# glue allows readable string interpolation.
# It is used to build dynamic messages and error descriptions.
library(glue)

# janitor provides data-cleaning utilities.
# It is loaded as part of the general project package set.
library(janitor)

# ------------------------------------------------------------
# Database Connection Configuration
# ------------------------------------------------------------

# This function creates a connection to the PostgreSQL database.
# It centralizes the connection configuration so other functions do not need
# to repeat the same dbConnect() code.
db_connection <- function() {

  # dbConnect() opens the database connection.
  # The username and password are read from environment variables instead of
  # being hardcoded directly in the script.
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "biomedical_db",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  # Return the active connection object so it can be used by other functions.
  return(conn)
}

# ------------------------------------------------------------
# Safe SQL Execution Helper
# ------------------------------------------------------------

# This helper executes SQL statements that modify the database structure or data.
# It is mainly used for CREATE TABLE, ALTER TABLE, CREATE INDEX, CREATE VIEW,
# and UPDATE statements.
#
# Parameters:
# - conn: active database connection
# - sql_query: SQL statement to execute
# - description: human-readable explanation used in success/error messages
execute_sql_safe <- function(conn,
                             sql_query,
                             description = "SQL execution") {

  # tryCatch() is used so that SQL errors are reported clearly.
  tryCatch({

    # dbExecute() sends the SQL command to PostgreSQL.
    dbExecute(conn, sql_query)

    # If execution succeeds, print a success message.
    message(glue("SUCCESS: {description}"))

  }, error = function(e) {

    # If something fails, print the specific step that failed and the error message.
    message(glue("ERROR during {description}: {e$message}"))

    # Re-throw the error so the calling function can roll back or stop safely.
    stop(e)
  })
}

# ------------------------------------------------------------
# Safe Query Helper
# ------------------------------------------------------------

# This helper executes SELECT queries safely and returns their result.
# It is used when the script needs to retrieve information from the database.
#
# Parameters:
# - conn: active database connection
# - sql_query: SELECT query to run
# - description: explanation used if the query fails
db_get_query_safe <- function(conn,
                              sql_query,
                              description = "Database query") {

  tryCatch({

    # dbGetQuery() runs a SQL query and returns the result as a data frame.
    result <- dbGetQuery(conn, sql_query)

    return(result)

  }, error = function(e) {

    # If the query fails, show a clear message explaining which query failed.
    message(glue("ERROR during {description}: {e$message}"))

    # Stop execution and pass the error upward.
    stop(e)
  })
}

# ------------------------------------------------------------
# Utility: Current User
# ------------------------------------------------------------

# This function detects the operating-system user currently running the script.
# The result is stored in quality flags and rejected submissions for traceability.
get_current_system_user <- function() {

  # Sys.info()[["user"]] returns the current system username.
  detected_by_user <- Sys.info()[["user"]]

  # If the username cannot be detected, use a safe default.
  if (is.null(detected_by_user) ||
      is.na(detected_by_user) ||
      detected_by_user == "") {

    detected_by_user <- "unknown_user"
  }

  # Always return the value as a character string.
  return(as.character(detected_by_user))
}

# ------------------------------------------------------------
# Utility: Missing Value Check
# ------------------------------------------------------------

# This function defines what the system considers a missing value.
# It handles different missing-value forms that may appear in R:
# - NULL
# - length 0
# - NA
# - empty strings such as ""
#
# This is useful because patient data and validation flags may come from
# different sources, such as Shiny forms, CSV files, or manually created lists.
is_missing_value <- function(x) {

  if (is.null(x)) {
    return(TRUE)
  }

  if (length(x) == 0) {
    return(TRUE)
  }

  if (all(is.na(x))) {
    return(TRUE)
  }

  if (is.character(x) && length(x) == 1 && str_trim(x) == "") {
    return(TRUE)
  }

  return(FALSE)
}

# ------------------------------------------------------------
# Utility: UUID or NULL
# ------------------------------------------------------------

# This function prepares UUID-like values for insertion into the database.
# If the value is missing or empty, it returns NA_character_.
# Otherwise, it trims the value and returns it as text.
#
# This is used for nullable UUID fields such as patient_uuid, submission_id,
# and rule_id.
uuid_or_null <- function(x) {

  if (is_missing_value(x)) {
    return(NA_character_)
  }

  value <- as.character(x[[1]])
  value <- str_trim(value)

  if (value == "") {
    return(NA_character_)
  }

  return(value)
}

# ------------------------------------------------------------
# Utility: SQL-safe JSON Serialization
# ------------------------------------------------------------
# This lightweight JSON serializer avoids introducing additional
# packages beyond the requested project package set.
#
# It supports the data structures used in this project:
# - named lists
# - one-row data frames
# - atomic character/numeric/logical/date values
# - NA/null values
# ------------------------------------------------------------

# This function escapes special characters inside strings so they can be safely
# represented inside JSON text.
#
# For example:
# - backslashes are escaped
# - quotes are escaped
# - new lines, carriage returns, and tabs are converted to JSON-compatible sequences
json_escape_string <- function(x) {

  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  x <- gsub("\r", "\\\\r", x)
  x <- gsub("\t", "\\\\t", x)

  return(x)
}

# This function converts a single R value into a JSON-compatible value.
#
# It handles:
# - NULL and empty values as JSON null
# - vectors as JSON arrays
# - NA as JSON null
# - Date/POSIX date-time values as JSON strings
# - logical values as true/false
# - numeric values as numbers
# - character values as quoted and escaped strings
to_json_value <- function(x) {

  # NULL becomes JSON null.
  if (is.null(x)) {
    return("null")
  }

  # Empty vectors become JSON null.
  if (length(x) == 0) {
    return("null")
  }

  # If the value is an atomic vector with multiple elements,
  # convert each element and combine them into a JSON array.
  if (length(x) > 1 && !is.list(x)) {

    values <- vapply(x, to_json_value, character(1))

    return(glue("[{paste(values, collapse = ',')}]"))
  }

  # NA values become JSON null.
  if (all(is.na(x))) {
    return("null")
  }

  # Date values are converted into quoted ISO-style date strings.
  if (inherits(x, "Date")) {
    return(glue("\"{as.character(x[[1]])}\""))
  }

  # POSIX date-time values are formatted as UTC timestamp strings.
  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(glue("\"{format(x[[1]], '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')}\""))
  }

  # Logical values become JSON true or false.
  if (is.logical(x)) {
    return(ifelse(isTRUE(x[[1]]), "true", "false"))
  }

  # Numeric and integer values are kept as JSON numbers.
  if (is.numeric(x) || is.integer(x)) {

    if (is.na(x[[1]])) {
      return("null")
    }

    return(as.character(x[[1]]))
  }

  # Any remaining value is treated as text and safely escaped.
  return(glue("\"{json_escape_string(x[[1]])}\""))
}

# This function converts an entire patient record or payload into JSON text.
#
# It supports:
# - one-row data frames
# - named lists, which become JSON objects
# - unnamed lists, which become JSON arrays
# - single atomic values
#
# The result is inserted into PostgreSQL as JSONB for rejected submissions.
record_to_json <- function(record) {

  # If a data frame is passed, only the first row is serialized.
  # This matches the project logic where one rejected submission corresponds
  # to one attempted patient record.
  if (is.data.frame(record)) {

    if (nrow(record) == 0) {
      return("{}")
    }

    record <- as.list(record[1, ])
  }

  # If the object is not a list, serialize it as one JSON value.
  if (!is.list(record)) {
    return(to_json_value(record))
  }

  # If the list has no valid names, serialize it as a JSON array.
  if (is.null(names(record)) || any(names(record) == "")) {

    values <- vapply(record, to_json_value, character(1))

    return(glue("[{paste(values, collapse = ',')}]"))
  }

  # For named lists, serialize each name-value pair as a JSON object field.
  fields <- character()

  for (name in names(record)) {

    key <- json_escape_string(name)
    value <- to_json_value(record[[name]])

    fields <- c(fields, glue("\"{key}\":{value}"))
  }

  # Combine all fields into a JSON object.
  json_text <- glue("{{{paste(fields, collapse = ',')}}}")

  return(as.character(json_text))
}

# ------------------------------------------------------------
# Create rejected_patient_submissions Table
# ------------------------------------------------------------

# This function creates the table used to store rejected patient submissions.
#
# A rejected submission is an attempted patient insertion that failed validation,
# usually because the validation engine found at least one CRITICAL issue.
#
# Instead of losing the attempted data, the system stores:
# - a submission UUID
# - the attempted patient_id
# - the submitted payload as JSONB
# - the severity
# - the rejection reason
# - the user who submitted it
# - the timestamp
create_rejected_patient_submissions_table <- function(conn) {

  rejected_sql <- "

  CREATE TABLE IF NOT EXISTS public.rejected_patient_submissions (

      submission_id UUID PRIMARY KEY,

      patient_id_attempted TEXT,

      submitted_payload JSONB,

      overall_severity TEXT NOT NULL,

      rejection_reason TEXT NOT NULL,

      submitted_by_user TEXT NOT NULL,

      created_at TIMESTAMP WITH TIME ZONE
          DEFAULT CURRENT_TIMESTAMP

  );

  "

  execute_sql_safe(
    conn,
    rejected_sql,
    "Creating rejected_patient_submissions table"
  )
}

# ------------------------------------------------------------
# Create quality_flags Table
# ------------------------------------------------------------
# quality_flags can be linked either to:
# - an inserted patient through patient_uuid
# - a rejected submission through submission_id
#
# Both are nullable to allow flexible staging, but standard
# pipeline usage should populate exactly one of them.
# ------------------------------------------------------------

# This function creates the quality_flags table.
#
# This table stores all validation flags produced by the validation engine.
# A flag can be linked to:
# - patient_uuid: if the patient was inserted but has warnings
# - submission_id: if the patient submission was rejected
#
# The table also stores the rule metadata and the issue description,
# making it possible to audit data-quality problems later.
create_quality_flags_table <- function(conn) {

  quality_flags_sql <- "

  CREATE TABLE IF NOT EXISTS public.quality_flags (

      flag_id UUID PRIMARY KEY,

      patient_uuid UUID NULL
          REFERENCES public.patients(patient_uuid)
          ON DELETE CASCADE,

      submission_id UUID NULL
          REFERENCES public.rejected_patient_submissions(submission_id)
          ON DELETE CASCADE,

      rule_id UUID NULL
          REFERENCES public.validation_rules(rule_id)
          ON DELETE SET NULL,

      rule_name TEXT NULL,

      variable_name TEXT NOT NULL,

      issue_type TEXT NOT NULL,

      severity TEXT NOT NULL,

      issue_description TEXT NOT NULL,

      detected_by_user TEXT NOT NULL,

      created_at TIMESTAMP WITH TIME ZONE
          DEFAULT CURRENT_TIMESTAMP

  );

  "

  execute_sql_safe(
    conn,
    quality_flags_sql,
    "Creating quality_flags table"
  )
}

# ------------------------------------------------------------
# Upgrade Existing quality_flags Table If Needed
# ------------------------------------------------------------
# This protects compatibility with older versions of the project.
# It does not delete or overwrite existing flags.
# ------------------------------------------------------------

# This function updates the quality_flags table structure if the project
# is being run on an older database version.
#
# ADD COLUMN IF NOT EXISTS is used so that existing data is preserved
# and the function can safely be run multiple times.
upgrade_quality_flags_table_if_needed <- function(conn) {

  # These ALTER TABLE statements add any missing columns required by the current version.
  alter_sql <- c(

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS patient_uuid UUID NULL;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS submission_id UUID NULL;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS rule_id UUID NULL;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS rule_name TEXT NULL;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS variable_name TEXT;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS issue_type TEXT;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS severity TEXT;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS issue_description TEXT;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS detected_by_user TEXT;",

    "ALTER TABLE public.quality_flags
     ADD COLUMN IF NOT EXISTS created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP;"
  )

  # Execute each ALTER TABLE statement safely.
  for (sql in alter_sql) {

    execute_sql_safe(
      conn,
      sql,
      "Upgrading quality_flags table structure"
    )
  }

  # ----------------------------------------------------------
  # If an older column named detected_by exists, copy values
  # into detected_by_user where possible.
  # ----------------------------------------------------------

  # This query checks whether a legacy column called detected_by exists.
  # Older versions of the project may have used that name instead of detected_by_user.
  detected_by_exists <- db_get_query_safe(
    conn,
    "
    SELECT COUNT(*) AS n
    FROM information_schema.columns
    WHERE table_schema = 'public'
      AND table_name = 'quality_flags'
      AND column_name = 'detected_by';
    ",
    "Checking for legacy detected_by column"
  )

  # If the legacy column exists, copy its values into the new detected_by_user column
  # only where detected_by_user is still NULL.
  if (detected_by_exists$n[[1]] > 0) {

    execute_sql_safe(
      conn,
      "
      UPDATE public.quality_flags
      SET detected_by_user = detected_by
      WHERE detected_by_user IS NULL
        AND detected_by IS NOT NULL;
      ",
      "Migrating detected_by to detected_by_user"
    )
  }

  # Fill any remaining missing detected_by_user values with a safe default.
  execute_sql_safe(
    conn,
    "
    UPDATE public.quality_flags
    SET detected_by_user = 'unknown_user'
    WHERE detected_by_user IS NULL;
    ",
    "Backfilling missing detected_by_user values"
  )
}

# ------------------------------------------------------------
# Create Useful Indexes
# ------------------------------------------------------------

# This function creates indexes that make common quality-flag queries faster.
#
# Indexes are useful for columns frequently used in WHERE, JOIN, ORDER BY,
# or GROUP BY operations.
create_quality_indexes <- function(conn) {

  index_sql <- c(

    # --------------------------------------------------------
    # quality_flags indexes
    # --------------------------------------------------------

    # Speeds up retrieval of flags linked to a specific inserted patient.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_patient_uuid
     ON public.quality_flags(patient_uuid);",

    # Speeds up retrieval of flags linked to a specific rejected submission.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_submission_id
     ON public.quality_flags(submission_id);",

    # Speeds up filtering or grouping flags by severity.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_severity
     ON public.quality_flags(severity);",

    # Speeds up summaries grouped by issue type.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_issue_type
     ON public.quality_flags(issue_type);",

    # Speeds up summaries and filters by affected variable.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_variable_name
     ON public.quality_flags(variable_name);",

    # Speeds up lookup and aggregation by validation rule name.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_rule_name
     ON public.quality_flags(rule_name);",

    # Speeds up retrieval of recent flags.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_created_at
     ON public.quality_flags(created_at);",

    # Speeds up summaries by the user who detected or generated the flag.
    "CREATE INDEX IF NOT EXISTS idx_quality_flags_detected_by_user
     ON public.quality_flags(detected_by_user);",

    # --------------------------------------------------------
    # rejected_patient_submissions indexes
    # --------------------------------------------------------

    # Speeds up searching rejected submissions by attempted patient ID.
    "CREATE INDEX IF NOT EXISTS idx_rejected_patient_id_attempted
     ON public.rejected_patient_submissions(patient_id_attempted);",

    # Speeds up retrieving recent rejected submissions.
    "CREATE INDEX IF NOT EXISTS idx_rejected_created_at
     ON public.rejected_patient_submissions(created_at);",

    # Speeds up filtering rejected submissions by submitting user.
    "CREATE INDEX IF NOT EXISTS idx_rejected_submitted_by_user
     ON public.rejected_patient_submissions(submitted_by_user);"
  )

  # Execute every CREATE INDEX statement.
  for (sql in index_sql) {

    execute_sql_safe(
      conn,
      sql,
      "Creating quality/rejected indexes"
    )
  }
}

# ------------------------------------------------------------
# Create Human-Readable Admin Views for pgAdmin
# ------------------------------------------------------------

# This function creates views intended for easier inspection in pgAdmin.
#
# Views do not store new data themselves.
# They are saved SQL queries that present existing tables in a more readable way.
create_admin_views <- function(conn) {

  # ----------------------------------------------------------
  # 1. Patients admin view
  # ----------------------------------------------------------

  # This view shows the main patient fields in a simple ordered format.
  # It is useful for quickly inspecting inserted patients from pgAdmin.
  v_patients_admin_sql <- "

  CREATE OR REPLACE VIEW public.v_patients_admin AS

  SELECT

      patient_id,
      patient_uuid,
      date_of_birth,
      age,
      sex,
      weight_kg,
      height_cm,
      blood_type,
      diagnosis_code,
      dosage_mg,
      smoker,
      doctor_name,
      created_at

  FROM public.patients

  ORDER BY patient_id;

  "

  execute_sql_safe(
    conn,
    v_patients_admin_sql,
    "Creating v_patients_admin"
  )

  # ----------------------------------------------------------
  # 2. Quality flags admin view
  # ----------------------------------------------------------

  # This view joins quality flags with patients when possible.
  # If a flag belongs to an inserted patient, the patient_id appears.
  # If a flag belongs to a rejected submission, patient_id may be NULL
  # but submission_id remains visible.
  v_quality_flags_admin_sql <- "

  CREATE OR REPLACE VIEW public.v_quality_flags_admin AS

  SELECT

      p.patient_id,
      qf.patient_uuid,
      qf.submission_id,
      qf.rule_name,
      qf.variable_name,
      qf.issue_type,
      qf.severity,
      qf.issue_description,
      qf.detected_by_user,
      qf.created_at

  FROM public.quality_flags qf

  LEFT JOIN public.patients p
      ON qf.patient_uuid = p.patient_uuid

  ORDER BY
      p.patient_id NULLS LAST,
      qf.submission_id NULLS LAST,
      qf.created_at DESC;

  "

  execute_sql_safe(
    conn,
    v_quality_flags_admin_sql,
    "Creating v_quality_flags_admin"
  )

  # ----------------------------------------------------------
  # 3. Patient quality overview admin view
  # ----------------------------------------------------------

  # This view summarizes how many quality flags each inserted patient has.
  # It counts total flags, CRITICAL flags, and WARNING flags.
  #
  # This is useful for quickly identifying patients with potential data-quality issues.
  v_patient_quality_overview_admin_sql <- "

  CREATE OR REPLACE VIEW public.v_patient_quality_overview_admin AS

  SELECT

      p.patient_id,
      p.patient_uuid,
      p.age,
      p.sex,
      p.diagnosis_code,
      p.doctor_name,
      p.created_at,

      COUNT(qf.flag_id) AS total_flags,

      COUNT(qf.flag_id)
          FILTER (WHERE qf.severity = 'CRITICAL') AS critical_flags,

      COUNT(qf.flag_id)
          FILTER (WHERE qf.severity = 'WARNING') AS warning_flags

  FROM public.patients p

  LEFT JOIN public.quality_flags qf
      ON p.patient_uuid = qf.patient_uuid

  GROUP BY
      p.patient_id,
      p.patient_uuid,
      p.age,
      p.sex,
      p.diagnosis_code,
      p.doctor_name,
      p.created_at

  ORDER BY p.patient_id;

  "

  execute_sql_safe(
    conn,
    v_patient_quality_overview_admin_sql,
    "Creating v_patient_quality_overview_admin"
  )

  # ----------------------------------------------------------
  # 4. Rejected submissions admin view
  # ----------------------------------------------------------

  # This view provides a simplified list of rejected patient submissions.
  # It hides the full JSON payload to make the view easier to read,
  # while still showing the attempted patient ID, reason, user, and timestamp.
  v_rejected_submissions_admin_sql <- "

  CREATE OR REPLACE VIEW public.v_rejected_submissions_admin AS

  SELECT

      patient_id_attempted,
      submission_id,
      overall_severity,
      rejection_reason,
      submitted_by_user,
      created_at

  FROM public.rejected_patient_submissions

  ORDER BY created_at DESC;

  "

  execute_sql_safe(
    conn,
    v_rejected_submissions_admin_sql,
    "Creating v_rejected_submissions_admin"
  )
}

# ------------------------------------------------------------
# Initialize Quality Flagging Infrastructure
# ------------------------------------------------------------
# This function is safe to run multiple times.
# It creates or upgrades tables, creates indexes, and creates
# admin views.
# ------------------------------------------------------------

# This is the main setup function for the quality flagging system.
#
# It:
# 1. Connects to the database.
# 2. Starts a transaction.
# 3. Creates the rejected submissions table if needed.
# 4. Creates the quality flags table if needed.
# 5. Upgrades the quality flags table if it comes from an older version.
# 6. Creates useful indexes.
# 7. Creates admin views.
# 8. Commits the transaction if everything succeeds.
initialize_quality_flagging_system <- function() {

  # Initialize conn as NULL so the error/finally blocks can check whether
  # a connection was actually opened.
  conn <- NULL

  tryCatch({

    # Open database connection.
    conn <- db_connection()

    # Start transaction so all infrastructure changes are applied together.
    dbBegin(conn)

    # Create table for rejected submissions.
    create_rejected_patient_submissions_table(conn)

    # Create table for quality flags.
    create_quality_flags_table(conn)

    # Add missing columns for compatibility with older project versions.
    upgrade_quality_flags_table_if_needed(conn)

    # Create indexes for faster queries.
    create_quality_indexes(conn)

    # Create pgAdmin-friendly views.
    create_admin_views(conn)

    # Commit all changes if every step succeeded.
    dbCommit(conn)

    message("==========================================")
    message("QUALITY FLAGGING SYSTEM INITIALIZED")
    message("==========================================")

    return(TRUE)

  }, error = function(e) {

    # If anything fails, roll back the transaction to avoid partial setup.
    if (!is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    message(glue("FATAL ERROR initializing quality system: {e$message}"))

    return(FALSE)

  }, finally = {

    # Always close the database connection if it was opened.
    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in initialize_quality_flagging_system().")
      })
    }
  })
}

# ------------------------------------------------------------
# Insert One Rejected Patient Submission
# ------------------------------------------------------------
# Used by 07_insert_patient_pipeline.R when validation severity
# is CRITICAL.
# ------------------------------------------------------------

# This function inserts one rejected patient submission into the database.
#
# It is used when validation fails critically, meaning the patient record
# should not be inserted into public.patients.
#
# Instead, the attempted record is stored in rejected_patient_submissions
# so that administrators can review what was submitted and why it failed.
insert_rejected_patient_submission <- function(patient_id_attempted,
                                               submitted_payload,
                                               overall_severity = "CRITICAL",
                                               rejection_reason,
                                               submitted_by_user = get_current_system_user(),
                                               conn = NULL) {

  # Tracks whether this function opened its own connection.
  local_connection <- FALSE

  tryCatch({

    # If no connection is supplied, open one and start a transaction.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
      dbBegin(conn)
    }

    # Generate a unique identifier for this rejected submission.
    submission_id <- UUIDgenerate()

    # Convert the submitted patient payload into JSON text.
    payload_json <- record_to_json(submitted_payload)

    # Insert the rejected submission into PostgreSQL.
    # Parameterized SQL is used here to avoid unsafe string interpolation.
    dbExecute(
      conn,
      "
      INSERT INTO public.rejected_patient_submissions (

          submission_id,
          patient_id_attempted,
          submitted_payload,
          overall_severity,
          rejection_reason,
          submitted_by_user

      ) VALUES (

          $1::uuid,
          $2,
          $3::jsonb,
          $4,
          $5,
          $6

      );
      ",
      params = list(
        as.character(submission_id),
        as.character(patient_id_attempted),
        as.character(payload_json),
        as.character(overall_severity),
        as.character(rejection_reason),
        as.character(submitted_by_user)
      )
    )

    # If this function opened the transaction, commit it here.
    if (local_connection) {
      dbCommit(conn)
    }

    # Return the submission_id so related quality flags can reference it.
    return(as.character(submission_id))

  }, error = function(e) {

    # If an error occurs and this function opened the transaction, roll it back.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    stop(glue("Failed to insert rejected patient submission: {e$message}"))

  }, finally = {

    # Close the connection only if this function opened it.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in insert_rejected_patient_submission().")
      })
    }
  })
}

# ------------------------------------------------------------
# Normalize One Flag Row
# ------------------------------------------------------------

# This function standardizes one quality flag before insertion.
#
# The validation engine may return flags as rows of a data frame/tibble.
# This function converts one flag into a consistent tibble with the exact columns
# expected by the quality_flags table.
#
# It also attaches either:
# - patient_uuid, if the patient was inserted
# - submission_id, if the submission was rejected
normalize_quality_flag <- function(flag,
                                   patient_uuid = NA_character_,
                                   submission_id = NA_character_) {

  # If the flag is a data frame, it must contain exactly one row.
  if (is.data.frame(flag)) {

    if (nrow(flag) != 1) {
      stop("normalize_quality_flag() expects one flag row.")
    }

    # Convert the single row into a list for easier field access.
    flag <- as.list(flag[1, ])
  }

  # The normalized flag must be based on a named list or one-row data frame.
  if (!is.list(flag)) {
    stop("Flag must be a named list or one-row data.frame.")
  }

  # Use detected_by_user from the flag if available.
  detected_by_user <- flag[["detected_by_user"]]

  # If missing, automatically detect the current system user.
  if (is_missing_value(detected_by_user)) {
    detected_by_user <- get_current_system_user()
  }

  # Return a standardized one-row tibble ready for insertion into quality_flags.
  tibble(
    flag_id = as.character(UUIDgenerate()),
    patient_uuid = uuid_or_null(patient_uuid),
    submission_id = uuid_or_null(submission_id),
    rule_id = uuid_or_null(flag[["rule_id"]]),
    rule_name = ifelse(
      is_missing_value(flag[["rule_name"]]),
      NA_character_,
      as.character(flag[["rule_name"]])
    ),
    variable_name = as.character(flag[["variable_name"]]),
    issue_type = as.character(flag[["issue_type"]]),
    severity = as.character(flag[["severity"]]),
    issue_description = as.character(flag[["issue_description"]]),
    detected_by_user = as.character(detected_by_user)
  )
}

# ------------------------------------------------------------
# Insert One Quality Flag
# ------------------------------------------------------------

# This function inserts one quality flag into public.quality_flags.
#
# A quality flag must be linked to either:
# - patient_uuid: for an inserted patient
# - submission_id: for a rejected submission
#
# This ensures that every flag can be traced back to the related patient record
# or the rejected attempt.
insert_quality_flag <- function(flag,
                                patient_uuid = NA_character_,
                                submission_id = NA_character_,
                                conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # If no connection is provided, create one and start a transaction.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
      dbBegin(conn)
    }

    # Standardize the incoming flag into the expected database format.
    normalized_flag <- normalize_quality_flag(
      flag = flag,
      patient_uuid = patient_uuid,
      submission_id = submission_id
    )

    # Prevent orphan flags.
    # Every flag should be linked to either an inserted patient or a rejected submission.
    if (is.na(normalized_flag$patient_uuid[[1]]) &&
        is.na(normalized_flag$submission_id[[1]])) {

      stop("A quality flag must be linked to either patient_uuid or submission_id.")
    }

    # Insert the normalized flag into the database.
    # NULLIF($2, '')::uuid allows empty strings to become SQL NULL before UUID casting.
    dbExecute(
      conn,
      "
      INSERT INTO public.quality_flags (

          flag_id,
          patient_uuid,
          submission_id,
          rule_id,
          rule_name,
          variable_name,
          issue_type,
          severity,
          issue_description,
          detected_by_user

      ) VALUES (

          $1::uuid,
          NULLIF($2, '')::uuid,
          NULLIF($3, '')::uuid,
          NULLIF($4, '')::uuid,
          $5,
          $6,
          $7,
          $8,
          $9,
          $10

      );
      ",
      params = list(
        normalized_flag$flag_id[[1]],
        ifelse(is.na(normalized_flag$patient_uuid[[1]]), "", normalized_flag$patient_uuid[[1]]),
        ifelse(is.na(normalized_flag$submission_id[[1]]), "", normalized_flag$submission_id[[1]]),
        ifelse(is.na(normalized_flag$rule_id[[1]]), "", normalized_flag$rule_id[[1]]),
        normalized_flag$rule_name[[1]],
        normalized_flag$variable_name[[1]],
        normalized_flag$issue_type[[1]],
        normalized_flag$severity[[1]],
        normalized_flag$issue_description[[1]],
        normalized_flag$detected_by_user[[1]]
      )
    )

    # Commit only if this function created the transaction.
    if (local_connection) {
      dbCommit(conn)
    }

    # Return the normalized flag so the caller can see exactly what was inserted.
    return(normalized_flag)

  }, error = function(e) {

    # Roll back if this function opened the transaction.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    stop(glue("Failed to insert quality flag: {e$message}"))

  }, finally = {

    # Close connection only if it was opened locally.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in insert_quality_flag().")
      })
    }
  })
}

# ------------------------------------------------------------
# Insert Multiple Quality Flags
# ------------------------------------------------------------

# This function inserts several quality flags.
#
# It loops through each row of the flags data frame and calls insert_quality_flag().
# Using one function for single-flag insertion and one for multiple-flag insertion
# keeps the logic reusable and consistent.
insert_quality_flags <- function(flags,
                                 patient_uuid = NA_character_,
                                 submission_id = NA_character_,
                                 conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # If no flags are provided, return an empty tibble with the expected structure.
    # This avoids errors in downstream code that expects a data frame.
    if (is.null(flags) || nrow(flags) == 0) {

      return(tibble(
        flag_id = character(),
        patient_uuid = character(),
        submission_id = character(),
        rule_id = character(),
        rule_name = character(),
        variable_name = character(),
        issue_type = character(),
        severity = character(),
        issue_description = character(),
        detected_by_user = character()
      ))
    }

    # If no connection was provided, open one and start a transaction.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
      dbBegin(conn)
    }

    # This tibble will collect the normalized flags inserted into the database.
    inserted_flags <- tibble()

    # Insert each flag one by one.
    for (i in seq_len(nrow(flags))) {

      inserted_flag <- insert_quality_flag(
        flag = flags[i, ],
        patient_uuid = patient_uuid,
        submission_id = submission_id,
        conn = conn
      )

      inserted_flags <- bind_rows(
        inserted_flags,
        inserted_flag
      )
    }

    # Commit the transaction if this function opened it.
    if (local_connection) {
      dbCommit(conn)
    }

    return(inserted_flags)

  }, error = function(e) {

    # Roll back the transaction if something fails during insertion.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    stop(glue("Failed to insert multiple quality flags: {e$message}"))

  }, finally = {

    # Close connection only if opened locally.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in insert_quality_flags().")
      })
    }
  })
}

# ------------------------------------------------------------
# Get Flags for One Inserted Patient
# ------------------------------------------------------------

# This function retrieves all quality flags linked to one inserted patient.
# It uses patient_uuid as the database-level identifier.
get_flags_by_patient_uuid <- function(patient_uuid,
                                      conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open a connection if none was provided.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Query quality_flags and join patients to also show the human-readable patient_id.
    result <- dbGetQuery(
      conn,
      "
      SELECT

          qf.flag_id,
          p.patient_id,
          qf.patient_uuid,
          qf.submission_id,
          qf.rule_id,
          qf.rule_name,
          qf.variable_name,
          qf.issue_type,
          qf.severity,
          qf.issue_description,
          qf.detected_by_user,
          qf.created_at

      FROM public.quality_flags qf

      LEFT JOIN public.patients p
          ON qf.patient_uuid = p.patient_uuid

      WHERE qf.patient_uuid = $1::uuid

      ORDER BY qf.created_at DESC;
      ",
      params = list(as.character(patient_uuid))
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to retrieve flags by patient_uuid: {e$message}"))

  }, finally = {

    # Close connection only if this function opened it.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in get_flags_by_patient_uuid().")
      })
    }
  })
}

# ------------------------------------------------------------
# Get Flags for One Rejected Submission
# ------------------------------------------------------------

# This function retrieves all quality flags linked to a rejected submission.
# It uses submission_id to identify the rejected payload.
get_flags_by_submission_id <- function(submission_id,
                                       conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open a connection if none was provided.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Query quality_flags and join rejected_patient_submissions to show
    # the attempted patient_id associated with the rejected submission.
    result <- dbGetQuery(
      conn,
      "
      SELECT

          qf.flag_id,
          rps.patient_id_attempted,
          qf.patient_uuid,
          qf.submission_id,
          qf.rule_id,
          qf.rule_name,
          qf.variable_name,
          qf.issue_type,
          qf.severity,
          qf.issue_description,
          qf.detected_by_user,
          qf.created_at

      FROM public.quality_flags qf

      LEFT JOIN public.rejected_patient_submissions rps
          ON qf.submission_id = rps.submission_id

      WHERE qf.submission_id = $1::uuid

      ORDER BY qf.created_at DESC;
      ",
      params = list(as.character(submission_id))
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to retrieve flags by submission_id: {e$message}"))

  }, finally = {

    # Close connection only if opened locally.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in get_flags_by_submission_id().")
      })
    }
  })
}

# ------------------------------------------------------------
# Count Flags by Severity
# ------------------------------------------------------------

# This function counts how many quality flags exist for each severity level.
# It is useful for high-level monitoring of data quality.
count_flags_by_severity <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Count flags grouped by severity.
    # The CASE statement orders CRITICAL first, then WARNING, then INFO.
    result <- db_get_query_safe(
      conn,
      "
      SELECT

          severity,
          COUNT(*) AS n_flags

      FROM public.quality_flags

      GROUP BY severity

      ORDER BY
          CASE severity
              WHEN 'CRITICAL' THEN 1
              WHEN 'WARNING' THEN 2
              WHEN 'INFO' THEN 3
              ELSE 4
          END;
      ",
      "Counting flags by severity"
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to count flags by severity: {e$message}"))

  }, finally = {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in count_flags_by_severity().")
      })
    }
  })
}

# ------------------------------------------------------------
# Count Flags by Issue Type
# ------------------------------------------------------------

# This function summarizes flags by issue_type.
# Examples of issue types include format_failure, range_failure,
# implausible_value, or controlled_vocabulary_failure.
count_flags_by_issue_type <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Group flags by issue_type and order by most frequent.
    result <- db_get_query_safe(
      conn,
      "
      SELECT

          issue_type,
          COUNT(*) AS n_flags

      FROM public.quality_flags

      GROUP BY issue_type

      ORDER BY n_flags DESC, issue_type;
      ",
      "Counting flags by issue_type"
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to count flags by issue_type: {e$message}"))

  }, finally = {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in count_flags_by_issue_type().")
      })
    }
  })
}

# ------------------------------------------------------------
# Count Flags by Variable
# ------------------------------------------------------------

# This function counts flags by the affected variable.
# It helps identify which patient fields most often produce data-quality issues.
count_flags_by_variable <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Group flags by variable_name and order by most frequent.
    result <- db_get_query_safe(
      conn,
      "
      SELECT

          variable_name,
          COUNT(*) AS n_flags

      FROM public.quality_flags

      GROUP BY variable_name

      ORDER BY n_flags DESC, variable_name;
      ",
      "Counting flags by variable_name"
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to count flags by variable: {e$message}"))

  }, finally = {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in count_flags_by_variable().")
      })
    }
  })
}

# ------------------------------------------------------------
# Count Flags by User
# ------------------------------------------------------------

# This function counts how many flags were detected/generated by each system user.
# It is useful for auditability and for understanding who submitted or processed data.
count_flags_by_user <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Group flags by detected_by_user.
    result <- db_get_query_safe(
      conn,
      "
      SELECT

          detected_by_user,
          COUNT(*) AS n_flags

      FROM public.quality_flags

      GROUP BY detected_by_user

      ORDER BY n_flags DESC, detected_by_user;
      ",
      "Counting flags by detected_by_user"
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to count flags by user: {e$message}"))

  }, finally = {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in count_flags_by_user().")
      })
    }
  })
}

# ------------------------------------------------------------
# Count Flags by Rule
# ------------------------------------------------------------

# This function counts flags by validation rule.
# It helps identify which validation rules are triggered most often.
count_flags_by_rule <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Group flags by rule_name and order by frequency.
    result <- db_get_query_safe(
      conn,
      "
      SELECT

          rule_name,
          COUNT(*) AS n_flags

      FROM public.quality_flags

      GROUP BY rule_name

      ORDER BY n_flags DESC, rule_name;
      ",
      "Counting flags by rule_name"
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to count flags by rule: {e$message}"))

  }, finally = {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in count_flags_by_rule().")
      })
    }
  })
}

# ------------------------------------------------------------
# Generate Quality Summary
# ------------------------------------------------------------

# This function generates a complete data-quality summary.
#
# It combines:
# - total number of patients
# - total number of quality flags
# - total number of rejected submissions
# - counts by severity
# - counts by issue type
# - counts by variable
# - counts by user
# - counts by rule
#
# The output is a list, so each part can be printed, inspected,
# or used in a Shiny dashboard.
generate_quality_summary <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Count all inserted patients.
    total_patients <- db_get_query_safe(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM public.patients;
      ",
      "Counting total patients"
    )$n[[1]]

    # Count all stored quality flags.
    total_flags <- db_get_query_safe(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM public.quality_flags;
      ",
      "Counting total quality flags"
    )$n[[1]]

    # Count all rejected patient submissions.
    total_rejected <- db_get_query_safe(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM public.rejected_patient_submissions;
      ",
      "Counting rejected submissions"
    )$n[[1]]

    # Generate detailed grouped summaries.
    severity_counts <- count_flags_by_severity(conn)
    issue_type_counts <- count_flags_by_issue_type(conn)
    variable_counts <- count_flags_by_variable(conn)
    user_counts <- count_flags_by_user(conn)
    rule_counts <- count_flags_by_rule(conn)

    # Combine all summary elements into one structured object.
    summary <- list(
      generated_at = Sys.time(),
      total_patients = total_patients,
      total_quality_flags = total_flags,
      total_rejected_submissions = total_rejected,
      flags_by_severity = severity_counts,
      flags_by_issue_type = issue_type_counts,
      flags_by_variable = variable_counts,
      flags_by_user = user_counts,
      flags_by_rule = rule_counts
    )

    return(summary)

  }, error = function(e) {

    stop(glue("Failed to generate quality summary: {e$message}"))

  }, finally = {

    # Close local connection if this function opened it.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in generate_quality_summary().")
      })
    }
  })
}

# ------------------------------------------------------------
# Get Recent Quality Flags
# ------------------------------------------------------------

# This function retrieves the most recent quality flags from the admin view.
# The limit parameter controls how many rows are returned.
get_recent_quality_flags <- function(limit = 50,
                                     conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Query the human-readable admin view and return the latest flags first.
    result <- dbGetQuery(
      conn,
      "
      SELECT *

      FROM public.v_quality_flags_admin

      ORDER BY created_at DESC

      LIMIT $1;
      ",
      params = list(as.integer(limit))
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to retrieve recent quality flags: {e$message}"))

  }, finally = {

    # Close local connection if needed.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in get_recent_quality_flags().")
      })
    }
  })
}

# ------------------------------------------------------------
# Get Recent Rejected Submissions
# ------------------------------------------------------------

# This function retrieves the most recent rejected submissions from the admin view.
# It is useful for reviewing failed patient insertion attempts.
get_recent_rejected_submissions <- function(limit = 50,
                                            conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    # Open connection if needed.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # Query the admin view and return the most recent rejected submissions first.
    result <- dbGetQuery(
      conn,
      "
      SELECT *

      FROM public.v_rejected_submissions_admin

      ORDER BY created_at DESC

      LIMIT $1;
      ",
      params = list(as.integer(limit))
    )

    return(result)

  }, error = function(e) {

    stop(glue("Failed to retrieve recent rejected submissions: {e$message}"))

  }, finally = {

    # Close local connection if needed.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in get_recent_rejected_submissions().")
      })
    }
  })
}

# ------------------------------------------------------------
# Refresh Admin Views
# ------------------------------------------------------------
# Convenience wrapper for rebuilding views without touching
# stored data.
# ------------------------------------------------------------

# This function rebuilds the admin views without modifying the stored patient,
# rejected submission, or quality flag data.
#
# It is useful if the view definitions are updated and need to be reapplied.
refresh_admin_views <- function() {

  conn <- NULL

  tryCatch({

    # Open database connection.
    conn <- db_connection()

    # Recreate or replace the admin views.
    create_admin_views(conn)

    message("Admin views refreshed successfully.")

    return(TRUE)

  }, error = function(e) {

    # If refreshing the views fails, report the error and return FALSE.
    message(glue("Failed to refresh admin views: {e$message}"))

    return(FALSE)

  }, finally = {

    # Always close the connection if it was opened.
    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in refresh_admin_views().")
      })
    }
  })
}