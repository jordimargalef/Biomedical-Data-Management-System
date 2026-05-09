# ============================================================
# 08_run_pipeline_tests.R
# Biomedical Pipeline Integration Tests
# PostgreSQL + R
# ============================================================
#
# PURPOSE:
# This is the only script that executes pipeline tests.
#
# TEST COVERAGE:
# - Valid patient insertion
# - WARNING patient insertion
# - CRITICAL patient rejection
# - Duplicate patient_id rejection
# - Query quality flags
# - Query rejected submissions
# - Query audit_log if it exists
# - Cleanup helper functions
#
# IMPORTANT:
# - This script assumes that 04_metadata_setup.R has already
#   been executed successfully.
# - This script sources:
#   05_validation_engine.R
#   06_quality_flagging.R
#   07_insert_patient_pipeline.R
# - This script uses clearly marked test IDs:
#   P-9001, P-9002, P-9003, P-9004
# - This script uses:
#   doctor_name = "Dr. Test Pipeline"
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
# Source Pipeline Scripts
# ------------------------------------------------------------

source("05_validation_engine.R")
source("06_quality_flagging.R")
source("07_insert_patient_pipeline.R")

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
# Test Constants
# ------------------------------------------------------------

TEST_PATIENT_IDS <- c("P-9001", "P-9002", "P-9003", "P-9004")

TEST_DOCTOR_NAME <- "Dr. Test Pipeline"

# ------------------------------------------------------------
# Utility: Check Table Exists
# ------------------------------------------------------------

table_exists <- function(conn,
                         table_name,
                         schema_name = "public") {

  result <- dbGetQuery(
    conn,
    "
    SELECT COUNT(*) AS n

    FROM information_schema.tables

    WHERE table_schema = $1
      AND table_name = $2;
    ",
    params = list(schema_name, table_name)
  )

  return(result$n[[1]] > 0)
}

# ------------------------------------------------------------
# Utility: Check View Exists
# ------------------------------------------------------------

view_exists <- function(conn,
                        view_name,
                        schema_name = "public") {

  result <- dbGetQuery(
    conn,
    "
    SELECT COUNT(*) AS n

    FROM information_schema.views

    WHERE table_schema = $1
      AND table_name = $2;
    ",
    params = list(schema_name, view_name)
  )

  return(result$n[[1]] > 0)
}

# ------------------------------------------------------------
# Cleanup Function: Delete Test Quality Flags
# ------------------------------------------------------------
# Removes flags linked to:
# - test patients inserted into public.patients
# - rejected submissions using test patient IDs
# ------------------------------------------------------------

delete_test_flags <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    dbBegin(conn)

    execute_sql_safe(
      conn,
      "
      DELETE FROM public.quality_flags qf

      USING public.patients p

      WHERE qf.patient_uuid = p.patient_uuid
        AND p.patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004');
      ",
      "Deleting quality flags linked to test patients"
    )

    execute_sql_safe(
      conn,
      "
      DELETE FROM public.quality_flags qf

      USING public.rejected_patient_submissions rps

      WHERE qf.submission_id = rps.submission_id
        AND rps.patient_id_attempted IN ('P-9001', 'P-9002', 'P-9003', 'P-9004');
      ",
      "Deleting quality flags linked to test rejected submissions"
    )

    dbCommit(conn)

    message("Test quality flags deleted successfully.")

    return(TRUE)

  }, error = function(e) {

    if (!is.null(conn)) {
      tryCatch(dbRollback(conn), error = function(x) NULL)
    }

    message(glue("Failed to delete test flags: {e$message}"))

    return(FALSE)

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in delete_test_flags().")
      })
    }
  })
}

# ------------------------------------------------------------
# Cleanup Function: Delete Test Rejected Submissions
# ------------------------------------------------------------

delete_test_rejected_submissions <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    dbBegin(conn)

    execute_sql_safe(
      conn,
      "
      DELETE FROM public.rejected_patient_submissions

      WHERE patient_id_attempted IN ('P-9001', 'P-9002', 'P-9003', 'P-9004');
      ",
      "Deleting test rejected patient submissions"
    )

    dbCommit(conn)

    message("Test rejected submissions deleted successfully.")

    return(TRUE)

  }, error = function(e) {

    if (!is.null(conn)) {
      tryCatch(dbRollback(conn), error = function(x) NULL)
    }

    message(glue("Failed to delete test rejected submissions: {e$message}"))

    return(FALSE)

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in delete_test_rejected_submissions().")
      })
    }
  })
}

# ------------------------------------------------------------
# Cleanup Function: Delete Test Patients
# ------------------------------------------------------------
# Deletes patients only after deleting quality flags because
# quality_flags.patient_uuid references patients.patient_uuid.
#
# Audit triggers on patients may record these deletions if your
# audit system tracks DELETE operations.
# ------------------------------------------------------------

delete_test_patients <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    dbBegin(conn)

    execute_sql_safe(
      conn,
      "
      DELETE FROM public.patients

      WHERE patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')
         OR doctor_name = 'Dr. Test Pipeline';
      ",
      "Deleting test patients"
    )

    dbCommit(conn)

    message("Test patients deleted successfully.")

    return(TRUE)

  }, error = function(e) {

    if (!is.null(conn)) {
      tryCatch(dbRollback(conn), error = function(x) NULL)
    }

    message(glue("Failed to delete test patients: {e$message}"))

    return(FALSE)

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in delete_test_patients().")
      })
    }
  })
}

# ------------------------------------------------------------
# Cleanup Wrapper
# ------------------------------------------------------------

cleanup_pipeline_test_data <- function() {

  message("==========================================")
  message("CLEANING PREVIOUS PIPELINE TEST DATA")
  message("==========================================")

  delete_test_flags()
  delete_test_rejected_submissions()
  delete_test_patients()

  message("==========================================")
  message("TEST CLEANUP FINISHED")
  message("==========================================")

  return(TRUE)
}

# ------------------------------------------------------------
# Test Records
# ------------------------------------------------------------

build_valid_test_patient <- function() {

  list(
    patient_id = "P-9001",
    date_of_birth = as.Date("1995-05-15"),
    age = 31,
    sex = "F",
    weight_kg = 65,
    height_cm = 170,
    blood_type = "A+",
    diagnosis_code = "E11.9",
    dosage_mg = 500,
    smoker = FALSE,
    doctor_name = TEST_DOCTOR_NAME
  )
}

build_warning_test_patient <- function() {

  list(
    patient_id = "P-9002",
    date_of_birth = as.Date("1990-01-01"),
    age = 36,
    sex = "M",
    weight_kg = 320,
    height_cm = 180,
    blood_type = "O+",
    diagnosis_code = "NOT_ICD_CODE",
    dosage_mg = 6000,
    smoker = TRUE,
    doctor_name = TEST_DOCTOR_NAME
  )
}

build_critical_test_patient <- function() {

  list(
    patient_id = "P-9003",
    date_of_birth = Sys.Date() + 30,
    age = 28,
    sex = "INVALID_SEX",
    weight_kg = 70,
    height_cm = 175,
    blood_type = "INVALID_BLOOD",
    diagnosis_code = "A00",
    dosage_mg = 250,
    smoker = FALSE,
    doctor_name = TEST_DOCTOR_NAME
  )
}

build_duplicate_test_patient <- function() {

  list(
    patient_id = "P-9001",
    date_of_birth = as.Date("1988-03-20"),
    age = 38,
    sex = "M",
    weight_kg = 80,
    height_cm = 178,
    blood_type = "B+",
    diagnosis_code = "I10",
    dosage_mg = 100,
    smoker = FALSE,
    doctor_name = TEST_DOCTOR_NAME
  )
}

# ------------------------------------------------------------
# Print Pipeline Result Summary
# ------------------------------------------------------------

print_pipeline_result <- function(label,
                                  result) {

  message("==========================================")
  message(label)
  message("==========================================")

  print(summarize_pipeline_result(result))

  if (length(result$validation_summary$issues) > 0) {

    message("CRITICAL ISSUES:")

    print(result$validation_summary$issues)
  }

  if (length(result$validation_summary$warnings) > 0) {

    message("WARNINGS:")

    print(result$validation_summary$warnings)
  }

  if (!is.null(result$inserted_flags) &&
      nrow(result$inserted_flags) > 0) {

    message("INSERTED FLAGS:")

    print(result$inserted_flags)
  }

  invisible(result)
}

# ------------------------------------------------------------
# Query Admin Views
# ------------------------------------------------------------

query_admin_views <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    message("==========================================")
    message("QUERY: v_patients_admin")
    message("==========================================")

    if (view_exists(conn, "v_patients_admin")) {

      patients_view <- db_get_query_safe(
        conn,
        "
        SELECT *

        FROM public.v_patients_admin

        WHERE patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')
           OR doctor_name = 'Dr. Test Pipeline'

        ORDER BY patient_id;
        ",
        "Querying v_patients_admin"
      )

      print(patients_view)

    } else {

      message("View public.v_patients_admin does not exist.")
    }

    message("==========================================")
    message("QUERY: v_quality_flags_admin")
    message("==========================================")

    if (view_exists(conn, "v_quality_flags_admin")) {

      quality_flags_view <- db_get_query_safe(
        conn,
        "
        SELECT *

        FROM public.v_quality_flags_admin

        WHERE patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')
           OR submission_id IN (
               SELECT submission_id
               FROM public.rejected_patient_submissions
               WHERE patient_id_attempted IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')
           )

        ORDER BY
            patient_id NULLS LAST,
            submission_id NULLS LAST,
            created_at DESC;
        ",
        "Querying v_quality_flags_admin"
      )

      print(quality_flags_view)

    } else {

      message("View public.v_quality_flags_admin does not exist.")
    }

    message("==========================================")
    message("QUERY: v_patient_quality_overview_admin")
    message("==========================================")

    if (view_exists(conn, "v_patient_quality_overview_admin")) {

      patient_quality_overview <- db_get_query_safe(
        conn,
        "
        SELECT *

        FROM public.v_patient_quality_overview_admin

        WHERE patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')
           OR doctor_name = 'Dr. Test Pipeline'

        ORDER BY patient_id;
        ",
        "Querying v_patient_quality_overview_admin"
      )

      print(patient_quality_overview)

    } else {

      message("View public.v_patient_quality_overview_admin does not exist.")
    }

    message("==========================================")
    message("QUERY: v_rejected_submissions_admin")
    message("==========================================")

    if (view_exists(conn, "v_rejected_submissions_admin")) {

      rejected_view <- db_get_query_safe(
        conn,
        "
        SELECT *

        FROM public.v_rejected_submissions_admin

        WHERE patient_id_attempted IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')

        ORDER BY created_at DESC;
        ",
        "Querying v_rejected_submissions_admin"
      )

      print(rejected_view)

    } else {

      message("View public.v_rejected_submissions_admin does not exist.")
    }

    return(TRUE)

  }, error = function(e) {

    message(glue("Failed to query admin views: {e$message}"))

    return(FALSE)

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in query_admin_views().")
      })
    }
  })
}

# ------------------------------------------------------------
# Query audit_log If Exists
# ------------------------------------------------------------

query_audit_log_if_exists <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    message("==========================================")
    message("QUERY: audit_log")
    message("==========================================")

    if (!table_exists(conn, "audit_log")) {

      message("Table public.audit_log does not exist. Skipping audit_log query.")

      return(FALSE)
    }

    audit_columns <- db_get_query_safe(
      conn,
      "
      SELECT column_name

      FROM information_schema.columns

      WHERE table_schema = 'public'
        AND table_name = 'audit_log'

      ORDER BY ordinal_position;
      ",
      "Reading audit_log columns"
    )$column_name

    if ("patient_id" %in% audit_columns) {

      audit_query <- "
      SELECT *

      FROM public.audit_log

      WHERE patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')

      ORDER BY 1 DESC

      LIMIT 50;
      "

    } else if ("record_id" %in% audit_columns) {

      audit_query <- "
      SELECT *

      FROM public.audit_log

      WHERE record_id IN (
          SELECT patient_uuid::text
          FROM public.patients
          WHERE patient_id IN ('P-9001', 'P-9002', 'P-9003', 'P-9004')
      )

      ORDER BY 1 DESC

      LIMIT 50;
      "

    } else if ("table_name" %in% audit_columns) {

      audit_query <- "
      SELECT *

      FROM public.audit_log

      WHERE table_name = 'patients'

      ORDER BY 1 DESC

      LIMIT 50;
      "

    } else {

      audit_query <- "
      SELECT *

      FROM public.audit_log

      ORDER BY 1 DESC

      LIMIT 50;
      "
    }

    audit_result <- db_get_query_safe(
      conn,
      audit_query,
      "Querying audit_log"
    )

    print(audit_result)

    return(TRUE)

  }, error = function(e) {

    message(glue("Failed to query audit_log: {e$message}"))

    return(FALSE)

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in query_audit_log_if_exists().")
      })
    }
  })
}

# ------------------------------------------------------------
# Query Flags Directly by Returned IDs
# ------------------------------------------------------------

query_flags_from_test_results <- function(valid_result,
                                          warning_result,
                                          critical_result,
                                          duplicate_result) {

  message("==========================================")
  message("DIRECT FLAG QUERIES")
  message("==========================================")

  if (!is.na(warning_result$patient_uuid)) {

    message("Flags linked to WARNING inserted patient:")

    print(
      get_flags_by_patient_uuid(
        warning_result$patient_uuid
      )
    )
  }

  if (!is.na(critical_result$submission_id)) {

    message("Flags linked to CRITICAL rejected submission:")

    print(
      get_flags_by_submission_id(
        critical_result$submission_id
      )
    )
  }

  if (!is.na(duplicate_result$submission_id)) {

    message("Flags linked to DUPLICATE rejected submission:")

    print(
      get_flags_by_submission_id(
        duplicate_result$submission_id
      )
    )
  }

  invisible(TRUE)
}

# ------------------------------------------------------------
# Run Full Pipeline Tests
# ------------------------------------------------------------

run_pipeline_tests <- function(clean_before = TRUE,
                               clean_after = FALSE) {

  message("==========================================")
  message("STARTING BIOMEDICAL PIPELINE TESTS")
  message("==========================================")

  # ----------------------------------------------------------
  # Initialize quality/rejected infrastructure.
  # This creates:
  # - public.quality_flags
  # - public.rejected_patient_submissions
  # - indexes
  # - admin views
  # ----------------------------------------------------------

  initialize_quality_flagging_system()

  if (isTRUE(clean_before)) {
    cleanup_pipeline_test_data()
  }

  # ----------------------------------------------------------
  # Test 1: Valid patient
  # Expected:
  # - success TRUE
  # - inserted TRUE
  # - severity INFO
  # - no quality flags
  # ----------------------------------------------------------

  valid_patient <- build_valid_test_patient()

  valid_result <- insert_patient_secure(valid_patient)

  print_pipeline_result(
    "TEST 1 - VALID PATIENT P-9001",
    valid_result
  )

  # ----------------------------------------------------------
  # Test 2: WARNING patient
  # Expected:
  # - success TRUE
  # - inserted TRUE
  # - severity WARNING
  # - quality flags linked to patient_uuid
  # ----------------------------------------------------------

  warning_patient <- build_warning_test_patient()

  warning_result <- insert_patient_secure(warning_patient)

  print_pipeline_result(
    "TEST 2 - WARNING PATIENT P-9002",
    warning_result
  )

  # ----------------------------------------------------------
  # Test 3: CRITICAL patient
  # Expected:
  # - success FALSE
  # - inserted FALSE
  # - severity CRITICAL
  # - rejected_patient_submissions row created
  # - quality flags linked to submission_id
  # ----------------------------------------------------------

  critical_patient <- build_critical_test_patient()

  critical_result <- insert_patient_secure(critical_patient)

  print_pipeline_result(
    "TEST 3 - CRITICAL PATIENT P-9003",
    critical_result
  )

  # ----------------------------------------------------------
  # Test 4: Duplicate patient_id
  # Expected:
  # - P-9001 already exists from Test 1
  # - duplicate is rejected as CRITICAL
  # - no duplicate patient inserted
  # - rejected_patient_submissions row created
  # - quality flag linked to submission_id
  # ----------------------------------------------------------

  duplicate_patient <- build_duplicate_test_patient()

  duplicate_result <- insert_patient_secure(duplicate_patient)

  print_pipeline_result(
    "TEST 4 - DUPLICATE PATIENT_ID P-9001",
    duplicate_result
  )

  # ----------------------------------------------------------
  # Direct flag retrieval tests
  # ----------------------------------------------------------

  query_flags_from_test_results(
    valid_result = valid_result,
    warning_result = warning_result,
    critical_result = critical_result,
    duplicate_result = duplicate_result
  )

  # ----------------------------------------------------------
  # Admin view queries
  # ----------------------------------------------------------

  query_admin_views()

  # ----------------------------------------------------------
  # Audit log query
  # ----------------------------------------------------------

  query_audit_log_if_exists()

  # ----------------------------------------------------------
  # Quality summary
  # ----------------------------------------------------------

  message("==========================================")
  message("QUALITY SUMMARY")
  message("==========================================")

  quality_summary <- generate_quality_summary()

  print(quality_summary)

  # ----------------------------------------------------------
  # Final compact test summary
  # ----------------------------------------------------------

  final_summary <- bind_rows(
    summarize_pipeline_result(valid_result) %>%
      mutate(test_case = "valid_patient_P-9001", .before = 1),

    summarize_pipeline_result(warning_result) %>%
      mutate(test_case = "warning_patient_P-9002", .before = 1),

    summarize_pipeline_result(critical_result) %>%
      mutate(test_case = "critical_patient_P-9003", .before = 1),

    summarize_pipeline_result(duplicate_result) %>%
      mutate(test_case = "duplicate_patient_id_P-9001", .before = 1)
  )

  message("==========================================")
  message("FINAL PIPELINE TEST SUMMARY")
  message("==========================================")

  print(final_summary)

  if (isTRUE(clean_after)) {

    message("==========================================")
    message("CLEANING TEST DATA AFTER TEST EXECUTION")
    message("==========================================")

    cleanup_pipeline_test_data()
  }

  message("==========================================")
  message("PIPELINE TESTS FINISHED")
  message("==========================================")

  return(
    list(
      valid_result = valid_result,
      warning_result = warning_result,
      critical_result = critical_result,
      duplicate_result = duplicate_result,
      final_summary = final_summary,
      quality_summary = quality_summary
    )
  )
}

# ------------------------------------------------------------
# Execute Tests
# ------------------------------------------------------------
# This is intentionally the only script that runs tests
# automatically.
# ------------------------------------------------------------

pipeline_test_results <- run_pipeline_tests(
  clean_before = TRUE,
  clean_after = FALSE
)