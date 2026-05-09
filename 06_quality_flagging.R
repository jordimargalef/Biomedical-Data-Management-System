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

library(DBI)
library(RPostgres)
library(tidyverse)
library(stringr)
library(lubridate)
library(uuid)
library(glue)
library(janitor)

# ------------------------------------------------------------
# Database Connection Configuration
# ------------------------------------------------------------

db_connection <- function() {

  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "biomedical_db",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  return(conn)
}

# ------------------------------------------------------------
# Safe SQL Execution Helper
# ------------------------------------------------------------

execute_sql_safe <- function(conn,
                             sql_query,
                             description = "SQL execution") {

  tryCatch({

    dbExecute(conn, sql_query)

    message(glue("SUCCESS: {description}"))

  }, error = function(e) {

    message(glue("ERROR during {description}: {e$message}"))

    stop(e)
  })
}

# ------------------------------------------------------------
# Safe Query Helper
# ------------------------------------------------------------

db_get_query_safe <- function(conn,
                              sql_query,
                              description = "Database query") {

  tryCatch({

    result <- dbGetQuery(conn, sql_query)

    return(result)

  }, error = function(e) {

    message(glue("ERROR during {description}: {e$message}"))

    stop(e)
  })
}

# ------------------------------------------------------------
# Utility: Current User
# ------------------------------------------------------------

get_current_system_user <- function() {

  detected_by_user <- Sys.info()[["user"]]

  if (is.null(detected_by_user) ||
      is.na(detected_by_user) ||
      detected_by_user == "") {

    detected_by_user <- "unknown_user"
  }

  return(as.character(detected_by_user))
}

# ------------------------------------------------------------
# Utility: Missing Value Check
# ------------------------------------------------------------

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

json_escape_string <- function(x) {

  x <- as.character(x)
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\n", "\\\\n", x)
  x <- gsub("\r", "\\\\r", x)
  x <- gsub("\t", "\\\\t", x)

  return(x)
}

to_json_value <- function(x) {

  if (is.null(x)) {
    return("null")
  }

  if (length(x) == 0) {
    return("null")
  }

  if (length(x) > 1 && !is.list(x)) {

    values <- vapply(x, to_json_value, character(1))

    return(glue("[{paste(values, collapse = ',')}]"))
  }

  if (all(is.na(x))) {
    return("null")
  }

  if (inherits(x, "Date")) {
    return(glue("\"{as.character(x[[1]])}\""))
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(glue("\"{format(x[[1]], '%Y-%m-%dT%H:%M:%SZ', tz = 'UTC')}\""))
  }

  if (is.logical(x)) {
    return(ifelse(isTRUE(x[[1]]), "true", "false"))
  }

  if (is.numeric(x) || is.integer(x)) {

    if (is.na(x[[1]])) {
      return("null")
    }

    return(as.character(x[[1]]))
  }

  return(glue("\"{json_escape_string(x[[1]])}\""))
}

record_to_json <- function(record) {

  if (is.data.frame(record)) {

    if (nrow(record) == 0) {
      return("{}")
    }

    record <- as.list(record[1, ])
  }

  if (!is.list(record)) {
    return(to_json_value(record))
  }

  if (is.null(names(record)) || any(names(record) == "")) {

    values <- vapply(record, to_json_value, character(1))

    return(glue("[{paste(values, collapse = ',')}]"))
  }

  fields <- character()

  for (name in names(record)) {

    key <- json_escape_string(name)
    value <- to_json_value(record[[name]])

    fields <- c(fields, glue("\"{key}\":{value}"))
  }

  json_text <- glue("{{{paste(fields, collapse = ',')}}}")

  return(as.character(json_text))
}

# ------------------------------------------------------------
# Create rejected_patient_submissions Table
# ------------------------------------------------------------

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

upgrade_quality_flags_table_if_needed <- function(conn) {

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

create_quality_indexes <- function(conn) {

  index_sql <- c(

    # --------------------------------------------------------
    # quality_flags indexes
    # --------------------------------------------------------

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_patient_uuid
     ON public.quality_flags(patient_uuid);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_submission_id
     ON public.quality_flags(submission_id);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_severity
     ON public.quality_flags(severity);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_issue_type
     ON public.quality_flags(issue_type);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_variable_name
     ON public.quality_flags(variable_name);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_rule_name
     ON public.quality_flags(rule_name);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_created_at
     ON public.quality_flags(created_at);",

    "CREATE INDEX IF NOT EXISTS idx_quality_flags_detected_by_user
     ON public.quality_flags(detected_by_user);",

    # --------------------------------------------------------
    # rejected_patient_submissions indexes
    # --------------------------------------------------------

    "CREATE INDEX IF NOT EXISTS idx_rejected_patient_id_attempted
     ON public.rejected_patient_submissions(patient_id_attempted);",

    "CREATE INDEX IF NOT EXISTS idx_rejected_created_at
     ON public.rejected_patient_submissions(created_at);",

    "CREATE INDEX IF NOT EXISTS idx_rejected_submitted_by_user
     ON public.rejected_patient_submissions(submitted_by_user);"
  )

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

create_admin_views <- function(conn) {

  # ----------------------------------------------------------
  # 1. Patients admin view
  # ----------------------------------------------------------

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

initialize_quality_flagging_system <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    dbBegin(conn)

    create_rejected_patient_submissions_table(conn)

    create_quality_flags_table(conn)

    upgrade_quality_flags_table_if_needed(conn)

    create_quality_indexes(conn)

    create_admin_views(conn)

    dbCommit(conn)

    message("==========================================")
    message("QUALITY FLAGGING SYSTEM INITIALIZED")
    message("==========================================")

    return(TRUE)

  }, error = function(e) {

    if (!is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    message(glue("FATAL ERROR initializing quality system: {e$message}"))

    return(FALSE)

  }, finally = {

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

insert_rejected_patient_submission <- function(patient_id_attempted,
                                               submitted_payload,
                                               overall_severity = "CRITICAL",
                                               rejection_reason,
                                               submitted_by_user = get_current_system_user(),
                                               conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
      dbBegin(conn)
    }

    submission_id <- UUIDgenerate()

    payload_json <- record_to_json(submitted_payload)

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

    if (local_connection) {
      dbCommit(conn)
    }

    return(as.character(submission_id))

  }, error = function(e) {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    stop(glue("Failed to insert rejected patient submission: {e$message}"))

  }, finally = {

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

normalize_quality_flag <- function(flag,
                                   patient_uuid = NA_character_,
                                   submission_id = NA_character_) {

  if (is.data.frame(flag)) {

    if (nrow(flag) != 1) {
      stop("normalize_quality_flag() expects one flag row.")
    }

    flag <- as.list(flag[1, ])
  }

  if (!is.list(flag)) {
    stop("Flag must be a named list or one-row data.frame.")
  }

  detected_by_user <- flag[["detected_by_user"]]

  if (is_missing_value(detected_by_user)) {
    detected_by_user <- get_current_system_user()
  }

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

insert_quality_flag <- function(flag,
                                patient_uuid = NA_character_,
                                submission_id = NA_character_,
                                conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
      dbBegin(conn)
    }

    normalized_flag <- normalize_quality_flag(
      flag = flag,
      patient_uuid = patient_uuid,
      submission_id = submission_id
    )

    if (is.na(normalized_flag$patient_uuid[[1]]) &&
        is.na(normalized_flag$submission_id[[1]])) {

      stop("A quality flag must be linked to either patient_uuid or submission_id.")
    }

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

    if (local_connection) {
      dbCommit(conn)
    }

    return(normalized_flag)

  }, error = function(e) {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    stop(glue("Failed to insert quality flag: {e$message}"))

  }, finally = {

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

insert_quality_flags <- function(flags,
                                 patient_uuid = NA_character_,
                                 submission_id = NA_character_,
                                 conn = NULL) {

  local_connection <- FALSE

  tryCatch({

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

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
      dbBegin(conn)
    }

    inserted_flags <- tibble()

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

    if (local_connection) {
      dbCommit(conn)
    }

    return(inserted_flags)

  }, error = function(e) {

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    stop(glue("Failed to insert multiple quality flags: {e$message}"))

  }, finally = {

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

get_flags_by_patient_uuid <- function(patient_uuid,
                                      conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

get_flags_by_submission_id <- function(submission_id,
                                       conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

count_flags_by_severity <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

count_flags_by_issue_type <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

count_flags_by_variable <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

count_flags_by_user <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

count_flags_by_rule <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

generate_quality_summary <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    total_patients <- db_get_query_safe(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM public.patients;
      ",
      "Counting total patients"
    )$n[[1]]

    total_flags <- db_get_query_safe(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM public.quality_flags;
      ",
      "Counting total quality flags"
    )$n[[1]]

    total_rejected <- db_get_query_safe(
      conn,
      "
      SELECT COUNT(*) AS n
      FROM public.rejected_patient_submissions;
      ",
      "Counting rejected submissions"
    )$n[[1]]

    severity_counts <- count_flags_by_severity(conn)
    issue_type_counts <- count_flags_by_issue_type(conn)
    variable_counts <- count_flags_by_variable(conn)
    user_counts <- count_flags_by_user(conn)
    rule_counts <- count_flags_by_rule(conn)

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

get_recent_quality_flags <- function(limit = 50,
                                     conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

get_recent_rejected_submissions <- function(limit = 50,
                                            conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

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

refresh_admin_views <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    create_admin_views(conn)

    message("Admin views refreshed successfully.")

    return(TRUE)

  }, error = function(e) {

    message(glue("Failed to refresh admin views: {e$message}"))

    return(FALSE)

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in refresh_admin_views().")
      })
    }
  })
}