# ============================================================
# 07_insert_patient_pipeline.R
# Secure Patient Insertion Pipeline
# PostgreSQL + R
# ============================================================
#
# PURPOSE:
# This script contains the secure patient insertion pipeline.
#
# IMPORTANT:
# - This script DOES NOT run tests automatically.
# - This script DOES NOT recreate metadata.
# - This script DOES NOT autocorrect invalid clinical data.
# - This script validates, classifies severity, inserts valid
#   or warning records into public.patients, and stores rejected
#   critical submissions in public.rejected_patient_submissions.
#
# DEPENDENCIES:
# - 04_metadata_setup.R must have been run before.
# - 05_validation_engine.R must be sourced before using this file.
# - 06_quality_flagging.R must be sourced before using this file.
# - initialize_quality_flagging_system() should be executed once
#   before inserting records, or called from the test script.
#
# MAIN ENTRY POINT:
# insert_patient_secure(record)
#
# RETURN STRUCTURE:
# list(
#   success = TRUE/FALSE,
#   inserted = TRUE/FALSE,
#   patient_uuid = ...,
#   submission_id = ...,
#   severity = ...,
#   validation_summary = ...,
#   inserted_flags = ...,
#   transaction_status = ...
# )
#
# ============================================================

# ------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------

# DBI provides the generic database interface used to connect to,
# query, modify, and disconnect from relational databases in R.
library(DBI)

# RPostgres is the PostgreSQL driver used through DBI.
# It allows this script to communicate specifically with the PostgreSQL database.
library(RPostgres)

# tidyverse provides data manipulation tools such as tibble(), bind_rows(),
# mutate(), pipes, and other utilities used throughout the pipeline.
library(tidyverse)

# stringr provides string manipulation functions.
# In this script, it is mainly useful for trimming text and checking empty values.
library(stringr)

# lubridate provides tools for dates and times.
# It is loaded for consistency with the rest of the biomedical data-management system.
library(lubridate)

# uuid provides UUIDgenerate(), used to generate unique patient UUIDs.
library(uuid)

# glue allows readable string interpolation.
# It is used to build dynamic error messages and transaction-status messages.
library(glue)

# janitor provides data-cleaning utilities.
# It is loaded as part of the project package environment.
library(janitor)

# ------------------------------------------------------------
# Database Connection Configuration
# ------------------------------------------------------------

# This function creates a connection to the PostgreSQL database.
# It keeps all connection details in one place so the rest of the script
# can reuse the same connection logic.
db_connection <- function() {

  # dbConnect() opens a PostgreSQL connection using the RPostgres driver.
  # The database name, host, and port are explicitly defined.
  # The username and password are read from environment variables,
  # which avoids storing credentials directly in the script.
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "biomedical_db",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  # Return the connection object so other functions can use it.
  return(conn)
}

# ------------------------------------------------------------
# Safe SQL Execution Helper
# ------------------------------------------------------------

# This helper safely executes SQL statements that modify the database
# or database structure.
#
# Parameters:
# - conn: active database connection
# - sql_query: SQL statement to execute
# - description: human-readable text used in success/error messages
execute_sql_safe <- function(conn,
                             sql_query,
                             description = "SQL execution") {

  # tryCatch() allows the function to report database errors clearly.
  tryCatch({

    # dbExecute() runs SQL commands such as INSERT, UPDATE, CREATE, or DELETE.
    dbExecute(conn, sql_query)

    # If no error occurs, print a success message.
    message(glue("SUCCESS: {description}"))

  }, error = function(e) {

    # If an error occurs, include the description so the failed step is easy to identify.
    message(glue("ERROR during {description}: {e$message}"))

    # Re-throw the error so the calling function can roll back or stop safely.
    stop(e)
  })
}

# ------------------------------------------------------------
# Safe Query Helper
# ------------------------------------------------------------

# This helper safely runs SQL SELECT queries.
#
# It returns a data frame containing the query result.
# It is useful for database reads where a result is expected.
db_get_query_safe <- function(conn,
                              sql_query,
                              description = "Database query") {

  tryCatch({

    # dbGetQuery() executes a SELECT query and returns the result as an R data frame.
    result <- dbGetQuery(conn, sql_query)

    return(result)

  }, error = function(e) {

    # If the query fails, print a readable message with the failed query description.
    message(glue("ERROR during {description}: {e$message}"))

    # Re-throw the error.
    stop(e)
  })
}

# ------------------------------------------------------------
# Runtime Dependency Check
# ------------------------------------------------------------
# This pipeline depends on functions from:
# - 05_validation_engine.R
# - 06_quality_flagging.R
#
# The check is explicit so that failures are clear in Positron,
# RStudio or command-line execution.
# ------------------------------------------------------------

# This function checks that the required functions from the previous scripts
# have already been loaded into the R session.
#
# This is important because this script does not redefine the validation engine
# or the quality-flag insertion logic. It expects them to be sourced beforehand.
check_pipeline_dependencies <- function() {

  # These are the external functions required for the secure insertion pipeline to work.
  required_functions <- c(
    "validate_patient_record",
    "load_governance_context",
    "insert_rejected_patient_submission",
    "insert_quality_flags"
  )

  # For each required function, check whether it exists in the current R environment.
  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]

  # If any required function is missing, stop execution with a clear instruction.
  if (length(missing_functions) > 0) {

    stop(glue(
      "Missing required function(s): {paste(missing_functions, collapse = ', ')}. ",
      "Please run source('05_validation_engine.R') and source('06_quality_flagging.R') before using insert_patient_secure()."
    ))
  }

  return(TRUE)
}

# ------------------------------------------------------------
# Utility: Current User
# ------------------------------------------------------------

# This function detects the operating-system user running the insertion pipeline.
# The user is stored in flags and rejected submissions for traceability.
get_current_system_user_pipeline <- function() {

  current_user <- Sys.info()[["user"]]

  # If the system user cannot be detected, use a safe fallback value.
  if (is.null(current_user) ||
      is.na(current_user) ||
      current_user == "") {

    current_user <- "unknown_user"
  }

  return(as.character(current_user))
}

# ------------------------------------------------------------
# Utility: Missing Value Check
# ------------------------------------------------------------

# This function defines what this pipeline considers a missing value.
# It handles NULL, empty vectors, NA values, and empty strings.
#
# It is named with the "_pipeline" suffix to avoid conflicts with similar helpers
# defined in the validation or quality-flagging scripts.
is_missing_value_pipeline <- function(x) {

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
# Utility: Empty Inserted Flags Structure
# ------------------------------------------------------------

# This function returns an empty tibble with the same columns as inserted quality flags.
#
# It is used when a patient is inserted without warnings, so no flags are stored,
# but the pipeline still returns a consistent output structure.
empty_inserted_flags <- function() {

  tibble(
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
  )
}

# ------------------------------------------------------------
# Utility: Empty Validation Summary
# ------------------------------------------------------------

# This function creates a default validation-summary structure.
#
# It is used mainly when the pipeline itself fails unexpectedly.
# In that case, the returned object still follows the expected format.
empty_validation_summary <- function() {

  list(
    valid = FALSE,
    overall_severity = "CRITICAL",
    issues = character(),
    warnings = character(),
    flags = tibble(),
    bmi = NA_real_,
    cleaned_record = list()
  )
}

# ------------------------------------------------------------
# Convert Record to Named List
# ------------------------------------------------------------

# This function standardizes the input record before validation.
#
# The pipeline accepts:
# - a named list
# - a one-row data frame
#
# Internally, it works with named lists, so one-row data frames are converted.
record_to_named_list <- function(record) {

  # If the input is a data frame, it must contain exactly one patient row.
  if (is.data.frame(record)) {

    if (nrow(record) != 1) {
      stop("record_to_named_list() expects a named list or a one-row data.frame.")
    }

    # Convert the first and only row into a named list.
    record <- as.list(record[1, ])
  }

  # The input must be a list after conversion.
  if (!is.list(record)) {
    stop("Input record must be a named list or a one-row data.frame.")
  }

  # The list must have valid names because field names are used
  # to access patient variables such as patient_id, age, sex, etc.
  if (is.null(names(record)) || any(names(record) == "")) {
    stop("Input record must be a named list with valid field names.")
  }

  return(record)
}

# ------------------------------------------------------------
# Duplicate Patient ID Check
# ------------------------------------------------------------
# Duplicate patient_id is treated as CRITICAL.
# This is performed inside the same transaction used by the
# insertion workflow.
# ------------------------------------------------------------

# This function checks whether a patient_id already exists in public.patients.
#
# This validation is done in the insertion pipeline, not only in the validation engine,
# because it depends on the current live content of the patients table.
patient_id_exists <- function(conn,
                              patient_id) {

  # If patient_id is missing, there is nothing to check for duplication.
  if (is_missing_value_pipeline(patient_id)) {
    return(FALSE)
  }

  # Count how many records already use this patient_id.
  result <- dbGetQuery(
    conn,
    "
    SELECT COUNT(*) AS n

    FROM public.patients

    WHERE patient_id = $1;
    ",
    params = list(as.character(patient_id))
  )

  # If the count is greater than zero, the patient_id already exists.
  return(result$n[[1]] > 0)
}

# ------------------------------------------------------------
# Add Duplicate Patient ID Flag
# ------------------------------------------------------------
# The validation engine checks patient_id format. The insertion
# pipeline checks database uniqueness because that requires the
# current database state.
# ------------------------------------------------------------

# This function adds a CRITICAL duplicate patient_id flag to an existing
# validation result.
#
# It is called after validate_patient_record(), because duplicate checking
# requires querying public.patients during the insertion transaction.
add_duplicate_patient_id_flag <- function(validation_result,
                                          governance,
                                          patient_id) {

  # This block does not change runtime behavior.
  # It is left as an explicit no-op placeholder in the current code.
  if (!patient_id_exists_placeholder <- FALSE) {
    # No-op placeholder avoided intentionally.
    # This block keeps the function body explicit without
    # changing runtime behavior.
  }

  # Initialize duplicate_rule as NULL in case the helper function or rule is unavailable.
  duplicate_rule <- NULL

  # If get_rule_by_name() exists, retrieve the official duplicate_patient_id rule
  # from the governance validation rules.
  if (exists("get_rule_by_name", mode = "function")) {

    duplicate_rule <- get_rule_by_name(
      governance,
      "duplicate_patient_id"
    )
  }

  # Detect the current user for auditability.
  detected_by_user <- get_current_system_user_pipeline()

  # If the duplicate rule exists, build the flag using the official rule metadata.
  if (!is.null(duplicate_rule) && nrow(duplicate_rule) > 0) {

    duplicate_flag <- tibble(
      rule_id = as.character(duplicate_rule$rule_id[[1]]),
      rule_name = as.character(duplicate_rule$rule_name[[1]]),
      variable_name = as.character(duplicate_rule$variable_name[[1]]),
      issue_type = as.character(duplicate_rule$issue_type[[1]]),
      severity = as.character(duplicate_rule$severity[[1]]),
      issue_description = glue(
        "Duplicate patient_id detected: '{patient_id}'. This patient_id already exists in public.patients."
      ),
      detected_by_user = detected_by_user
    )

  } else {

    # If the official rule cannot be found, create a fallback duplicate flag.
    # This ensures the critical issue is still reported.
    duplicate_flag <- tibble(
      rule_id = NA_character_,
      rule_name = "duplicate_patient_id",
      variable_name = "patient_id",
      issue_type = "duplicate_identifier",
      severity = "CRITICAL",
      issue_description = glue(
        "Duplicate patient_id detected: '{patient_id}'. This patient_id already exists in public.patients."
      ),
      detected_by_user = detected_by_user
    )
  }

  # Add the duplicate flag to the validation result flags.
  validation_result$flags <- bind_rows(
    validation_result$flags,
    duplicate_flag
  )

  # Add the duplicate issue description to the critical issues list.
  validation_result$issues <- c(
    validation_result$issues,
    duplicate_flag$issue_description
  )

  # Mark the validation result as invalid and critical.
  validation_result$valid <- FALSE
  validation_result$overall_severity <- "CRITICAL"

  return(validation_result)
}

# ------------------------------------------------------------
# Insert Patient Row
# ------------------------------------------------------------
# This function inserts into public.patients only.
#
# It expects a cleaned_record produced by validate_patient_record().
# It does not insert quality flags and does not commit; transaction
# control belongs to insert_patient_secure().
# ------------------------------------------------------------

# This function performs the actual INSERT into public.patients.
#
# It only inserts the patient row. It does not:
# - validate the record
# - insert quality flags
# - create rejected submissions
# - commit or roll back the transaction
#
# Those responsibilities belong to insert_patient_secure().
insert_patient_row <- function(conn,
                               cleaned_record,
                               patient_uuid) {

  tryCatch({

    # Insert the cleaned patient record into public.patients.
    # Parameterized SQL is used to avoid building unsafe SQL strings manually.
    dbExecute(
      conn,
      "
      INSERT INTO public.patients (

          patient_uuid,
          patient_id,
          date_of_birth,
          age,
          sex,
          weight_kg,
          height_cm,
          blood_type,
          diagnosis_code,
          dosage_mg,
          smoker,
          doctor_name

      ) VALUES (

          $1::uuid,
          $2,
          $3::date,
          $4::integer,
          $5,
          $6::numeric,
          $7::numeric,
          $8,
          $9,
          $10::integer,
          $11::boolean,
          $12

      );
      ",
      params = list(
        as.character(patient_uuid),
        cleaned_record$patient_id,
        as.character(cleaned_record$date_of_birth),
        cleaned_record$age,
        cleaned_record$sex,
        cleaned_record$weight_kg,
        cleaned_record$height_cm,
        cleaned_record$blood_type,
        cleaned_record$diagnosis_code,
        cleaned_record$dosage_mg,
        cleaned_record$smoker,
        cleaned_record$doctor_name
      )
    )

    return(TRUE)

  }, error = function(e) {

    # If insertion fails, stop with a clear message.
    stop(glue("Failed to insert patient row: {e$message}"))
  })
}

# ------------------------------------------------------------
# Build Rejection Reason
# ------------------------------------------------------------

# This function converts validation issues into one rejection reason string.
#
# It is used when a patient submission is rejected because of CRITICAL issues.
build_rejection_reason <- function(validation_result) {

  # If there are no explicit issues, return a generic rejection reason.
  if (is.null(validation_result$issues) ||
      length(validation_result$issues) == 0) {

    return("Patient submission rejected due to CRITICAL validation failure.")
  }

  # If there are issues, combine them into one readable text string.
  reason <- paste(validation_result$issues, collapse = " | ")

  return(reason)
}

# ------------------------------------------------------------
# Build Pipeline Response
# ------------------------------------------------------------

# This helper builds the final standardized response returned by the pipeline.
#
# Keeping this in a helper function makes all branches return the same structure:
# - successful insertions
# - rejected submissions
# - unexpected pipeline errors
build_pipeline_response <- function(success,
                                    inserted,
                                    patient_uuid,
                                    submission_id,
                                    severity,
                                    validation_summary,
                                    inserted_flags,
                                    transaction_status) {

  list(
    success = success,
    inserted = inserted,
    patient_uuid = patient_uuid,
    submission_id = submission_id,
    severity = severity,
    validation_summary = validation_summary,
    inserted_flags = inserted_flags,
    transaction_status = transaction_status
  )
}

# ------------------------------------------------------------
# Secure Patient Insertion Pipeline
# ------------------------------------------------------------
# Workflow:
#
# INPUT RECORD
#   -> validate_patient_record()
#   -> duplicate patient_id check
#   -> severity evaluation
#
# IF CRITICAL:
#   - create rejected_patient_submissions
#   - insert flags linked to submission_id
#   - do NOT insert into public.patients
#   - commit rejected submission + flags
#
# IF WARNING:
#   - insert patient into public.patients
#   - insert flags linked to patient_uuid
#   - commit patient + flags
#
# IF INFO:
#   - insert patient into public.patients
#   - no quality flags
#   - commit patient
#
# All database writes are performed inside one transaction.
# ------------------------------------------------------------

# This is the main entry point of the secure insertion pipeline.
#
# It receives one patient record, validates it, checks database-level duplicates,
# decides what to do based on severity, and commits the appropriate database changes.
insert_patient_secure <- function(record) {

  # conn is initialized as NULL so the finally block can safely check it.
  conn <- NULL

  # transaction_started tracks whether dbBegin() was successfully called.
  # This prevents trying to roll back when no transaction is active.
  transaction_started <- FALSE

  tryCatch({

    # Check that the validation and quality-flagging functions are loaded.
    check_pipeline_dependencies()

    # Convert input into a valid named list.
    input_record <- record_to_named_list(record)

    # Open database connection.
    conn <- db_connection()

    # Start a transaction so all writes for this patient are atomic.
    # This means the patient, rejected submission, and flags are committed together
    # or rolled back together if something fails.
    dbBegin(conn)

    transaction_started <- TRUE

    # Load metadata, controlled vocabularies, and validation rules.
    governance <- load_governance_context(conn)

    # Validate the patient record using the validation engine.
    validation_result <- validate_patient_record(
      record = input_record,
      governance = governance,
      conn = conn
    )

    # Extract the cleaned record produced by the validation engine.
    cleaned_record <- validation_result$cleaned_record

    # Extract patient_id because it is needed for duplicate checking
    # and for rejected-submission tracking.
    patient_id <- cleaned_record$patient_id

    # --------------------------------------------------------
    # Database-level duplicate patient_id validation.
    # This is intentionally done in the insertion pipeline
    # because it depends on the live state of public.patients.
    # --------------------------------------------------------

    # If patient_id exists and is already present in the database,
    # add a CRITICAL duplicate flag to the validation result.
    if (!is_missing_value_pipeline(patient_id) &&
        patient_id_exists(conn, patient_id)) {

      validation_result <- add_duplicate_patient_id_flag(
        validation_result = validation_result,
        governance = governance,
        patient_id = patient_id
      )
    }

    # Read the final overall severity after all validations,
    # including duplicate patient_id validation.
    severity <- validation_result$overall_severity

    # --------------------------------------------------------
    # CRITICAL branch:
    # rejected submission + flags linked to submission_id.
    # --------------------------------------------------------

    # If severity is CRITICAL, the patient is not inserted.
    # Instead, the attempted submission and its flags are stored for review.
    if (severity == "CRITICAL") {

      # Build a human-readable explanation for rejection.
      rejection_reason <- build_rejection_reason(validation_result)

      # Insert the rejected patient submission and retrieve its submission_id.
      submission_id <- insert_rejected_patient_submission(
        patient_id_attempted = patient_id,
        submitted_payload = input_record,
        overall_severity = "CRITICAL",
        rejection_reason = rejection_reason,
        submitted_by_user = get_current_system_user_pipeline(),
        conn = conn
      )

      # Insert validation flags linked to the rejected submission.
      inserted_flags <- insert_quality_flags(
        flags = validation_result$flags,
        patient_uuid = NA_character_,
        submission_id = submission_id,
        conn = conn
      )

      # Commit the rejected submission and its flags.
      dbCommit(conn)

      transaction_started <- FALSE

      # Return a structured response indicating rejection.
      return(
        build_pipeline_response(
          success = FALSE,
          inserted = FALSE,
          patient_uuid = NA_character_,
          submission_id = as.character(submission_id),
          severity = "CRITICAL",
          validation_summary = validation_result,
          inserted_flags = inserted_flags,
          transaction_status = "committed_rejection"
        )
      )
    }

    # --------------------------------------------------------
    # WARNING / INFO branches:
    # patient inserted into public.patients.
    # Existing audit triggers on public.patients remain active
    # and will capture the insertion automatically.
    # --------------------------------------------------------

    # Generate a new UUID for the patient record.
    patient_uuid <- as.character(UUIDgenerate())

    # Insert the cleaned patient data into public.patients.
    insert_patient_row(
      conn = conn,
      cleaned_record = cleaned_record,
      patient_uuid = patient_uuid
    )

    # If the record has warnings, insert the warning flags linked to patient_uuid.
    if (severity == "WARNING") {

      inserted_flags <- insert_quality_flags(
        flags = validation_result$flags,
        patient_uuid = patient_uuid,
        submission_id = NA_character_,
        conn = conn
      )

    } else {

      # If severity is INFO, there are no flags to insert.
      inserted_flags <- empty_inserted_flags()
    }

    # Commit the patient insertion and any warning flags.
    dbCommit(conn)

    transaction_started <- FALSE

    # Return a structured response indicating successful insertion.
    return(
      build_pipeline_response(
        success = TRUE,
        inserted = TRUE,
        patient_uuid = as.character(patient_uuid),
        submission_id = NA_character_,
        severity = severity,
        validation_summary = validation_result,
        inserted_flags = inserted_flags,
        transaction_status = "committed_insert"
      )
    )

  }, error = function(e) {

    # If any error happens during the transaction,
    # roll back all database writes for this patient.
    if (!is.null(conn) && transaction_started) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    # Build a validation-like summary describing the pipeline failure.
    error_validation_summary <- empty_validation_summary()

    error_validation_summary$issues <- glue(
      "Secure insertion pipeline failed: {e$message}"
    )

    # Return a structured failure response.
    return(
      build_pipeline_response(
        success = FALSE,
        inserted = FALSE,
        patient_uuid = NA_character_,
        submission_id = NA_character_,
        severity = "CRITICAL",
        validation_summary = error_validation_summary,
        inserted_flags = empty_inserted_flags(),
        transaction_status = glue("rolled_back_error: {e$message}")
      )
    )

  }, finally = {

    # Always close the database connection if it was opened.
    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in insert_patient_secure().")
      })
    }
  })
}

# ------------------------------------------------------------
# Convenience Function: Insert Many Patients
# ------------------------------------------------------------
# This function is useful for controlled batch ingestion.
# It applies insert_patient_secure() row by row.
#
# IMPORTANT:
# Each patient is handled independently by insert_patient_secure().
# Therefore, one rejected patient does not rollback all previous
# successful patients in the batch.
# ------------------------------------------------------------

# This function inserts multiple patients using the secure insertion pipeline.
#
# It supports:
# - a data frame with multiple rows
# - a single named list
# - a list of named patient records
#
# Each patient is processed independently.
insert_patients_secure_batch <- function(records) {

  # If records is a data frame, each row is treated as one patient.
  if (is.data.frame(records)) {

    # If the data frame is empty, return an empty list.
    if (nrow(records) == 0) {
      return(list())
    }

    # Preallocate a list to store one result per row.
    results <- vector("list", nrow(records))

    # Insert each row independently.
    for (i in seq_len(nrow(records))) {

      results[[i]] <- insert_patient_secure(
        as.list(records[i, ])
      )
    }

    return(results)
  }

  # If records is a single named list, treat it as one patient record.
  if (is.list(records) && !is.null(names(records))) {

    return(list(insert_patient_secure(records)))
  }

  # If records is an unnamed list, treat each element as one patient record.
  if (is.list(records)) {

    results <- vector("list", length(records))

    for (i in seq_along(records)) {

      results[[i]] <- insert_patient_secure(records[[i]])
    }

    return(results)
  }

  # If the input format is unsupported, stop with a clear error.
  stop("records must be a data.frame, a named list, or a list of named records.")
}

# ------------------------------------------------------------
# Convenience Function: Summarize Pipeline Result
# ------------------------------------------------------------

# This function converts one pipeline result into a compact tibble.
#
# It is useful for quickly viewing the outcome of one insertion without printing
# the full nested validation object.
summarize_pipeline_result <- function(result) {

  # The input must be the list returned by insert_patient_secure().
  if (!is.list(result)) {
    stop("summarize_pipeline_result() expects a pipeline result list.")
  }

  # Build a one-row summary table.
  tibble(
    success = result$success,
    inserted = result$inserted,
    patient_uuid = result$patient_uuid,
    submission_id = result$submission_id,
    severity = result$severity,
    n_issues = length(result$validation_summary$issues),
    n_warnings = length(result$validation_summary$warnings),
    n_inserted_flags = nrow(result$inserted_flags),
    transaction_status = as.character(result$transaction_status)
  )
}

# ------------------------------------------------------------
# Convenience Function: Summarize Batch Pipeline Results
# ------------------------------------------------------------

# This function summarizes a list of pipeline results.
#
# It is useful after calling insert_patients_secure_batch(),
# because it returns one row per processed patient.
summarize_batch_pipeline_results <- function(results) {

  # The input must be a list of pipeline result objects.
  if (!is.list(results)) {
    stop("summarize_batch_pipeline_results() expects a list of pipeline results.")
  }

  # Start with an empty summary table.
  summary_table <- tibble()

  # Summarize each individual result and add the batch index.
  for (i in seq_along(results)) {

    summary_table <- bind_rows(
      summary_table,
      summarize_pipeline_result(results[[i]]) %>%
        mutate(batch_index = i, .before = 1)
    )
  }

  return(summary_table)
}