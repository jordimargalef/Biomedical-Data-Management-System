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
# Runtime Dependency Check
# ------------------------------------------------------------
# This pipeline depends on functions from:
# - 05_validation_engine.R
# - 06_quality_flagging.R
#
# The check is explicit so that failures are clear in Positron,
# RStudio or command-line execution.
# ------------------------------------------------------------

check_pipeline_dependencies <- function() {

  required_functions <- c(
    "validate_patient_record",
    "load_governance_context",
    "insert_rejected_patient_submission",
    "insert_quality_flags"
  )

  missing_functions <- required_functions[
    !vapply(required_functions, exists, logical(1), mode = "function")
  ]

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

get_current_system_user_pipeline <- function() {

  current_user <- Sys.info()[["user"]]

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

record_to_named_list <- function(record) {

  if (is.data.frame(record)) {

    if (nrow(record) != 1) {
      stop("record_to_named_list() expects a named list or a one-row data.frame.")
    }

    record <- as.list(record[1, ])
  }

  if (!is.list(record)) {
    stop("Input record must be a named list or a one-row data.frame.")
  }

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

patient_id_exists <- function(conn,
                              patient_id) {

  if (is_missing_value_pipeline(patient_id)) {
    return(FALSE)
  }

  result <- dbGetQuery(
    conn,
    "
    SELECT COUNT(*) AS n

    FROM public.patients

    WHERE patient_id = $1;
    ",
    params = list(as.character(patient_id))
  )

  return(result$n[[1]] > 0)
}

# ------------------------------------------------------------
# Add Duplicate Patient ID Flag
# ------------------------------------------------------------
# The validation engine checks patient_id format. The insertion
# pipeline checks database uniqueness because that requires the
# current database state.
# ------------------------------------------------------------

add_duplicate_patient_id_flag <- function(validation_result,
                                          governance,
                                          patient_id) {

  if (!patient_id_exists_placeholder <- FALSE) {
    # No-op placeholder avoided intentionally.
    # This block keeps the function body explicit without
    # changing runtime behavior.
  }

  duplicate_rule <- NULL

  if (exists("get_rule_by_name", mode = "function")) {

    duplicate_rule <- get_rule_by_name(
      governance,
      "duplicate_patient_id"
    )
  }

  detected_by_user <- get_current_system_user_pipeline()

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

  validation_result$flags <- bind_rows(
    validation_result$flags,
    duplicate_flag
  )

  validation_result$issues <- c(
    validation_result$issues,
    duplicate_flag$issue_description
  )

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

insert_patient_row <- function(conn,
                               cleaned_record,
                               patient_uuid) {

  tryCatch({

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

    stop(glue("Failed to insert patient row: {e$message}"))
  })
}

# ------------------------------------------------------------
# Build Rejection Reason
# ------------------------------------------------------------

build_rejection_reason <- function(validation_result) {

  if (is.null(validation_result$issues) ||
      length(validation_result$issues) == 0) {

    return("Patient submission rejected due to CRITICAL validation failure.")
  }

  reason <- paste(validation_result$issues, collapse = " | ")

  return(reason)
}

# ------------------------------------------------------------
# Build Pipeline Response
# ------------------------------------------------------------

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

insert_patient_secure <- function(record) {

  conn <- NULL

  transaction_started <- FALSE

  tryCatch({

    check_pipeline_dependencies()

    input_record <- record_to_named_list(record)

    conn <- db_connection()

    dbBegin(conn)

    transaction_started <- TRUE

    governance <- load_governance_context(conn)

    validation_result <- validate_patient_record(
      record = input_record,
      governance = governance,
      conn = conn
    )

    cleaned_record <- validation_result$cleaned_record

    patient_id <- cleaned_record$patient_id

    # --------------------------------------------------------
    # Database-level duplicate patient_id validation.
    # This is intentionally done in the insertion pipeline
    # because it depends on the live state of public.patients.
    # --------------------------------------------------------

    if (!is_missing_value_pipeline(patient_id) &&
        patient_id_exists(conn, patient_id)) {

      validation_result <- add_duplicate_patient_id_flag(
        validation_result = validation_result,
        governance = governance,
        patient_id = patient_id
      )
    }

    severity <- validation_result$overall_severity

    # --------------------------------------------------------
    # CRITICAL branch:
    # rejected submission + flags linked to submission_id.
    # --------------------------------------------------------

    if (severity == "CRITICAL") {

      rejection_reason <- build_rejection_reason(validation_result)

      submission_id <- insert_rejected_patient_submission(
        patient_id_attempted = patient_id,
        submitted_payload = input_record,
        overall_severity = "CRITICAL",
        rejection_reason = rejection_reason,
        submitted_by_user = get_current_system_user_pipeline(),
        conn = conn
      )

      inserted_flags <- insert_quality_flags(
        flags = validation_result$flags,
        patient_uuid = NA_character_,
        submission_id = submission_id,
        conn = conn
      )

      dbCommit(conn)

      transaction_started <- FALSE

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

    patient_uuid <- as.character(UUIDgenerate())

    insert_patient_row(
      conn = conn,
      cleaned_record = cleaned_record,
      patient_uuid = patient_uuid
    )

    if (severity == "WARNING") {

      inserted_flags <- insert_quality_flags(
        flags = validation_result$flags,
        patient_uuid = patient_uuid,
        submission_id = NA_character_,
        conn = conn
      )

    } else {

      inserted_flags <- empty_inserted_flags()
    }

    dbCommit(conn)

    transaction_started <- FALSE

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

    if (!is.null(conn) && transaction_started) {

      tryCatch({

        dbRollback(conn)

      }, error = function(x) NULL)
    }

    error_validation_summary <- empty_validation_summary()

    error_validation_summary$issues <- glue(
      "Secure insertion pipeline failed: {e$message}"
    )

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

insert_patients_secure_batch <- function(records) {

  if (is.data.frame(records)) {

    if (nrow(records) == 0) {
      return(list())
    }

    results <- vector("list", nrow(records))

    for (i in seq_len(nrow(records))) {

      results[[i]] <- insert_patient_secure(
        as.list(records[i, ])
      )
    }

    return(results)
  }

  if (is.list(records) && !is.null(names(records))) {

    return(list(insert_patient_secure(records)))
  }

  if (is.list(records)) {

    results <- vector("list", length(records))

    for (i in seq_along(records)) {

      results[[i]] <- insert_patient_secure(records[[i]])
    }

    return(results)
  }

  stop("records must be a data.frame, a named list, or a list of named records.")
}

# ------------------------------------------------------------
# Convenience Function: Summarize Pipeline Result
# ------------------------------------------------------------

summarize_pipeline_result <- function(result) {

  if (!is.list(result)) {
    stop("summarize_pipeline_result() expects a pipeline result list.")
  }

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

summarize_batch_pipeline_results <- function(results) {

  if (!is.list(results)) {
    stop("summarize_batch_pipeline_results() expects a list of pipeline results.")
  }

  summary_table <- tibble()

  for (i in seq_along(results)) {

    summary_table <- bind_rows(
      summary_table,
      summarize_pipeline_result(results[[i]]) %>%
        mutate(batch_index = i, .before = 1)
    )
  }

  return(summary_table)
}