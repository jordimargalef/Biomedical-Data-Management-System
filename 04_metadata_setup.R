# ============================================================
# 04_metadata_setup.R
# Biomedical Data Management System
# Metadata, Controlled Vocabularies, Validation Rules
# PostgreSQL + R
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

execute_sql_safe <- function(conn, sql_query, description = "SQL execution") {

  tryCatch({

    dbExecute(conn, sql_query)

    message(glue("SUCCESS: {description}"))

  }, error = function(e) {

    message(glue("ERROR during {description}: {e$message}"))

    stop(e)
  })
}

# ------------------------------------------------------------
# Initialize Metadata Infrastructure
# ------------------------------------------------------------

initialize_metadata_system <- function() {

  conn <- NULL

  tryCatch({

    conn <- db_connection()

    dbBegin(conn)

    # ========================================================
    # VOCABULARY REGISTRY
    # ========================================================

    vocabulary_registry_sql <- "

    CREATE TABLE IF NOT EXISTS public.vocabulary_registry (

        vocabulary_name TEXT PRIMARY KEY,
        description TEXT NOT NULL,
        terminology_source TEXT NOT NULL,
        active BOOLEAN NOT NULL DEFAULT TRUE,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP

    );

    "

    execute_sql_safe(
      conn,
      vocabulary_registry_sql,
      "Creating vocabulary_registry table"
    )

    # ========================================================
    # CONTROLLED VOCABULARIES
    # ========================================================

    controlled_vocabularies_sql <- "

    CREATE TABLE IF NOT EXISTS public.controlled_vocabularies (

        vocabulary_name TEXT NOT NULL
            REFERENCES public.vocabulary_registry(vocabulary_name),

        allowed_value TEXT NOT NULL,

        display_label TEXT NOT NULL,

        terminology_source TEXT NOT NULL,

        description TEXT NOT NULL,

        active BOOLEAN NOT NULL DEFAULT TRUE,

        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP,

        PRIMARY KEY (vocabulary_name, allowed_value)

    );

    "

    execute_sql_safe(
      conn,
      controlled_vocabularies_sql,
      "Creating controlled_vocabularies table"
    )

    # ========================================================
    # METADATA TABLE
    # ========================================================

    metadata_table_sql <- "

    CREATE TABLE IF NOT EXISTS public.metadata_table (

        variable_name TEXT PRIMARY KEY,

        table_name TEXT NOT NULL,

        display_label TEXT NOT NULL,

        datatype TEXT NOT NULL,

        unit TEXT,

        min_value NUMERIC,

        max_value NUMERIC,

        allowed_vocabulary TEXT
            REFERENCES public.vocabulary_registry(vocabulary_name),

        is_required BOOLEAN NOT NULL,

        is_system_generated BOOLEAN NOT NULL,

        is_derived BOOLEAN NOT NULL,

        semantic_type TEXT,

        description TEXT NOT NULL,

        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP

    );

    "

    execute_sql_safe(
      conn,
      metadata_table_sql,
      "Creating metadata_table"
    )

    # ========================================================
    # VALIDATION RULES
    # ========================================================

    validation_rules_sql <- "

    CREATE TABLE IF NOT EXISTS public.validation_rules (

        rule_id UUID PRIMARY KEY,

        rule_name TEXT NOT NULL,

        variable_name TEXT NOT NULL
            REFERENCES public.metadata_table(variable_name),

        validation_type TEXT NOT NULL,

        validation_scope TEXT NOT NULL,

        issue_type TEXT NOT NULL,

        severity TEXT NOT NULL,

        regex_pattern TEXT,

        hard_min_value NUMERIC,

        hard_max_value NUMERIC,

        plausible_min_value NUMERIC,

        plausible_max_value NUMERIC,

        controlled_vocabulary_name TEXT
            REFERENCES public.vocabulary_registry(vocabulary_name),

        active BOOLEAN NOT NULL DEFAULT TRUE,

        description TEXT NOT NULL,

        clinical_rationale TEXT NOT NULL,

        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP

    );

    "

    execute_sql_safe(
      conn,
      validation_rules_sql,
      "Creating validation_rules table"
    )

    # ========================================================
    # INSERT VOCABULARY REGISTRY
    # ========================================================

    dbExecute(conn, "DELETE FROM public.controlled_vocabularies;")
    dbExecute(conn, "DELETE FROM public.validation_rules;")
    dbExecute(conn, "DELETE FROM public.metadata_table;")
    dbExecute(conn, "DELETE FROM public.vocabulary_registry;")

    vocabulary_registry <- tribble(
      ~vocabulary_name, ~description, ~terminology_source,
      "sex_vocab", "Biological sex vocabulary", "Internal Clinical Standard",
      "blood_type_vocab", "Blood type controlled vocabulary", "ABO/Rh Standard",
      "smoker_vocab", "Smoking status controlled vocabulary", "Internal Clinical Standard"
    )

    dbWriteTable(
      conn,
      Id(schema = "public", table = "vocabulary_registry"),
      vocabulary_registry,
      append = TRUE,
      row.names = FALSE
    )

    # ========================================================
    # INSERT CONTROLLED VOCABULARIES
    # ========================================================

    controlled_vocabularies <- tribble(
      ~vocabulary_name, ~allowed_value, ~display_label, ~terminology_source, ~description,

      "sex_vocab", "M", "Male", "Internal", "Male biological sex",
      "sex_vocab", "F", "Female", "Internal", "Female biological sex",
      "sex_vocab", "X", "Other", "Internal", "Non-binary or unspecified",

      "blood_type_vocab", "A+", "A Positive", "ABO/Rh", "Blood type A+",
      "blood_type_vocab", "A-", "A Negative", "ABO/Rh", "Blood type A-",
      "blood_type_vocab", "B+", "B Positive", "ABO/Rh", "Blood type B+",
      "blood_type_vocab", "B-", "B Negative", "ABO/Rh", "Blood type B-",
      "blood_type_vocab", "AB+", "AB Positive", "ABO/Rh", "Blood type AB+",
      "blood_type_vocab", "AB-", "AB Negative", "ABO/Rh", "Blood type AB-",
      "blood_type_vocab", "O+", "O Positive", "ABO/Rh", "Blood type O+",
      "blood_type_vocab", "O-", "O Negative", "ABO/Rh", "Blood type O-",

      "smoker_vocab", "TRUE", "Smoker", "Internal", "Patient is smoker",
      "smoker_vocab", "FALSE", "Non-Smoker", "Internal", "Patient is non-smoker"
    )

    dbWriteTable(
      conn,
      Id(schema = "public", table = "controlled_vocabularies"),
      controlled_vocabularies,
      append = TRUE,
      row.names = FALSE
    )

    # ========================================================
    # INSERT METADATA TABLE
    # ========================================================

    metadata_table <- tribble(
      ~variable_name, ~table_name, ~display_label, ~datatype, ~unit,
      ~min_value, ~max_value, ~allowed_vocabulary,
      ~is_required, ~is_system_generated, ~is_derived,
      ~semantic_type, ~description,

      "patient_uuid", "patients", "Patient UUID", "UUID", NA,
      NA, NA, NA,
      TRUE, TRUE, FALSE,
      "identifier", "System generated unique patient identifier",

      "patient_id", "patients", "Patient ID", "TEXT", NA,
      NA, NA, NA,
      TRUE, FALSE, FALSE,
      "identifier", "Human-readable patient identifier",

      "date_of_birth", "patients", "Date of Birth", "DATE", NA,
      NA, NA, NA,
      TRUE, FALSE, FALSE,
      "demographic", "Patient date of birth",

      "age", "patients", "Age", "INTEGER", "years",
      0, 120, NA,
      TRUE, FALSE, FALSE,
      "demographic", "Patient age in years",

      "sex", "patients", "Sex", "TEXT", NA,
      NA, NA, "sex_vocab",
      TRUE, FALSE, FALSE,
      "demographic", "Patient biological sex",

      "weight_kg", "patients", "Weight", "NUMERIC", "kg",
      1, 500, NA,
      FALSE, FALSE, FALSE,
      "clinical_measurement", "Patient body weight",

      "height_cm", "patients", "Height", "NUMERIC", "cm",
      30, 300, NA,
      FALSE, FALSE, FALSE,
      "clinical_measurement", "Patient body height",

      "blood_type", "patients", "Blood Type", "TEXT", NA,
      NA, NA, "blood_type_vocab",
      FALSE, FALSE, FALSE,
      "laboratory", "Patient blood type",

      "diagnosis_code", "patients", "Diagnosis Code", "TEXT", NA,
      NA, NA, NA,
      FALSE, FALSE, FALSE,
      "clinical_code", "Clinical diagnosis code",

      "dosage_mg", "patients", "Dosage", "INTEGER", "mg",
      0, 100000, NA,
      FALSE, FALSE, FALSE,
      "medication", "Medication dosage",

      "smoker", "patients", "Smoking Status", "BOOLEAN", NA,
      NA, NA, "smoker_vocab",
      FALSE, FALSE, FALSE,
      "behavioral", "Smoking status",

      "doctor_name", "patients", "Doctor Name", "TEXT", NA,
      NA, NA, NA,
      FALSE, FALSE, FALSE,
      "provider", "Responsible physician",

      "created_at", "patients", "Created Timestamp", "TIMESTAMPTZ", NA,
      NA, NA, NA,
      TRUE, TRUE, FALSE,
      "system", "System-generated creation timestamp",

      "bmi", "derived", "Body Mass Index", "NUMERIC", "kg/m2",
      0, 100, NA,
      FALSE, FALSE, TRUE,
      "derived_measurement", "Derived body mass index"
    )

    dbWriteTable(
      conn,
      Id(schema = "public", table = "metadata_table"),
      metadata_table,
      append = TRUE,
      row.names = FALSE
    )

        # ========================================================
    # INSERT VALIDATION RULES
    # ========================================================

    validation_rules <- tribble(

      ~rule_id, ~rule_name, ~variable_name, ~validation_type,
      ~validation_scope, ~issue_type, ~severity,
      ~regex_pattern, ~hard_min_value, ~hard_max_value,
      ~plausible_min_value, ~plausible_max_value,
      ~controlled_vocabulary_name,
      ~active, ~description, ~clinical_rationale,

      # =====================================================
      # REQUIRED FIELDS
      # =====================================================

      UUIDgenerate(), "required_patient_id", "patient_id",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Patient ID is required",
      "The patient identifier is mandatory for traceability and duplicate detection",

      UUIDgenerate(), "required_date_of_birth", "date_of_birth",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Date of birth is required",
      "Date of birth is required for demographic consistency and age validation",

      UUIDgenerate(), "required_age", "age",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Age is required",
      "Age is required for clinical interpretation and consistency checks",

      UUIDgenerate(), "required_sex", "sex",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Sex is required",
      "Sex is required as a core demographic variable in the clinical registry",

      # =====================================================
      # PATIENT ID
      # =====================================================

      UUIDgenerate(), "patient_id_format", "patient_id",
      "regex", "field", "format_failure", "CRITICAL",
      "^P-[0-9]{4}$", NA, NA,
      NA, NA, NA,
      TRUE,
      "Patient ID must follow P-XXXX format",
      "A standardized patient identifier format reduces entry errors and improves traceability",

      UUIDgenerate(), "duplicate_patient_id", "patient_id",
      "uniqueness", "database", "duplicate_identifier", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Patient ID must be unique",
      "Duplicate patient identifiers compromise patient-level data integrity",

      # =====================================================
      # AGE
      # =====================================================

      UUIDgenerate(), "age_integer_validation", "age",
      "datatype", "field", "datatype_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Age must be an integer",
      "The patients table stores age as INTEGER; non-integer values would violate schema consistency",

      UUIDgenerate(), "age_hard_range", "age",
      "range", "field", "range_failure", "CRITICAL",
      NA, 0, 120,
      NA, NA, NA,
      TRUE,
      "Age must be between 0 and 120 years",
      "Values outside this interval are biologically implausible for patient age",

      UUIDgenerate(), "age_plausibility_high", "age",
      "plausibility_high", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      NA, 100, NA,
      TRUE,
      "Age above 100 years is unusual",
      "Very high age values may be valid but should be reviewed for potential entry errors",

      # =====================================================
      # DATE OF BIRTH
      # =====================================================

      UUIDgenerate(), "date_of_birth_future_date", "date_of_birth",
      "temporal", "field", "future_date", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Date of birth cannot be in the future",
      "A future date of birth is logically impossible",

      UUIDgenerate(), "dob_age_consistency", "date_of_birth",
      "cross_field", "record", "cross_field_inconsistency", "WARNING",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Date of birth and age should be coherent",
      "A large discrepancy between calculated age and recorded age suggests a possible demographic data entry error",

      # =====================================================
      # SEX
      # =====================================================

      UUIDgenerate(), "sex_controlled_vocabulary", "sex",
      "controlled_vocabulary", "field", "controlled_vocabulary_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, "sex_vocab",
      TRUE,
      "Sex must belong to the approved controlled vocabulary",
      "Controlled vocabularies ensure standardized demographic coding",

      # =====================================================
      # BLOOD TYPE
      # =====================================================

      UUIDgenerate(), "blood_type_controlled_vocabulary", "blood_type",
      "controlled_vocabulary", "field", "controlled_vocabulary_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, "blood_type_vocab",
      TRUE,
      "Blood type must belong to the approved controlled vocabulary",
      "Controlled vocabularies ensure standardized laboratory-related coding",

      # =====================================================
      # SMOKER
      # =====================================================

      UUIDgenerate(), "smoker_controlled_vocabulary", "smoker",
      "controlled_vocabulary", "field", "controlled_vocabulary_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, "smoker_vocab",
      TRUE,
      "Smoking status must belong to the approved controlled vocabulary",
      "Controlled vocabularies ensure standardized behavioral risk factor coding",

      # =====================================================
      # DIAGNOSIS CODE
      # =====================================================

      UUIDgenerate(), "diagnosis_code_regex", "diagnosis_code",
      "regex", "field", "format_failure", "WARNING",
      "^[A-TV-Z][0-9][0-9A-Z](\\.[0-9A-Z]{1,4})?$",
      NA, NA,
      NA, NA, NA,
      TRUE,
      "Diagnosis code should resemble ICD format",
      "A diagnosis code that does not resemble ICD structure may indicate a coding or data entry issue",

      # =====================================================
      # WEIGHT
      # =====================================================

      UUIDgenerate(), "weight_hard_range", "weight_kg",
      "range", "field", "range_failure", "CRITICAL",
      NA, 1, 500,
      NA, NA, NA,
      TRUE,
      "Weight must be between 1 and 500 kg",
      "Values outside this interval are considered impossible or structurally invalid for human body weight",

      UUIDgenerate(), "weight_plausibility_low", "weight_kg",
      "plausibility_low", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      30, NA, NA,
      TRUE,
      "Weight below 30 kg is clinically unusual",
      "Very low body weight may be valid in specific populations but should be reviewed",

      UUIDgenerate(), "weight_plausibility_high", "weight_kg",
      "plausibility_high", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      NA, 300, NA,
      TRUE,
      "Weight above 300 kg is clinically unusual",
      "Very high body weight may be valid but should be reviewed for possible unit or entry errors",

      # =====================================================
      # HEIGHT
      # =====================================================

      UUIDgenerate(), "height_hard_range", "height_cm",
      "range", "field", "range_failure", "CRITICAL",
      NA, 30, 300,
      NA, NA, NA,
      TRUE,
      "Height must be between 30 and 300 cm",
      "Values outside this interval are considered impossible or structurally invalid for human body height",

      UUIDgenerate(), "height_plausibility_low", "height_cm",
      "plausibility_low", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      100, NA, NA,
      TRUE,
      "Height below 100 cm is clinically unusual",
      "Very low height may be valid in specific populations but should be reviewed",

      UUIDgenerate(), "height_plausibility_high", "height_cm",
      "plausibility_high", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      NA, 250, NA,
      TRUE,
      "Height above 250 cm is clinically unusual",
      "Very high height may be valid but should be reviewed for possible unit or entry errors",

      # =====================================================
      # DOSAGE
      # =====================================================

      UUIDgenerate(), "dosage_integer_validation", "dosage_mg",
      "datatype", "field", "datatype_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Dosage must be an integer",
      "The patients table stores dosage_mg as INTEGER; decimal dosages would violate schema consistency",

      UUIDgenerate(), "dosage_hard_range", "dosage_mg",
      "range", "field", "range_failure", "CRITICAL",
      NA, 0, 100000,
      NA, NA, NA,
      TRUE,
      "Dosage must be between 0 and 100000 mg",
      "Negative or extremely high dosage values are structurally invalid for this registry",

      UUIDgenerate(), "dosage_plausibility_high", "dosage_mg",
      "plausibility_high", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      NA, 5000, NA,
      TRUE,
      "Dosage above 5000 mg is clinically unusual",
      "High dosage values may be valid in some contexts but should be reviewed for unit or entry errors",

      # =====================================================
      # BMI
      # =====================================================

      UUIDgenerate(), "bmi_low_warning", "bmi",
      "derived_metric_low", "record", "implausible_value", "WARNING",
      NA, NA, NA,
      10, NA, NA,
      TRUE,
      "BMI below 10 is clinically unusual",
      "Extremely low BMI may indicate a serious clinical state or a measurement/unit error",

      UUIDgenerate(), "bmi_high_warning", "bmi",
      "derived_metric_high", "record", "implausible_value", "WARNING",
      NA, NA, NA,
      NA, 70, NA,
      TRUE,
      "BMI above 70 is clinically unusual",
      "Extremely high BMI may indicate a serious clinical state or a measurement/unit error"
    )

    dbWriteTable(
      conn,
      Id(schema = "public", table = "validation_rules"),
      validation_rules,
      append = TRUE,
      row.names = FALSE
    )

    # ========================================================
    # CREATE INDEXES
    # ========================================================

    index_sql <- c(

  # =====================================================
  # VALIDATION RULES INDEXES
  # =====================================================

  "CREATE INDEX IF NOT EXISTS idx_validation_rules_variable_name
   ON public.validation_rules(variable_name);",

  "CREATE INDEX IF NOT EXISTS idx_validation_rules_rule_name
   ON public.validation_rules(rule_name);",

  "CREATE INDEX IF NOT EXISTS idx_validation_rules_active
   ON public.validation_rules(active);",

  "CREATE INDEX IF NOT EXISTS idx_validation_rules_severity
   ON public.validation_rules(severity);",

  # =====================================================
  # PATIENTS INDEXES
  # =====================================================

  "CREATE INDEX IF NOT EXISTS idx_patients_patient_id
   ON public.patients(patient_id);",

  "CREATE INDEX IF NOT EXISTS idx_patients_created_at
   ON public.patients(created_at);",

  "CREATE INDEX IF NOT EXISTS idx_patients_diagnosis_code
   ON public.patients(diagnosis_code);",

  "CREATE INDEX IF NOT EXISTS idx_patients_doctor_name
   ON public.patients(doctor_name);"

)

    for (sql in index_sql) {
      execute_sql_safe(conn, sql, "Creating indexes")
    }

    dbCommit(conn)

    message("==========================================")
    message("METADATA SYSTEM INITIALIZED SUCCESSFULLY")
    message("==========================================")

  }, error = function(e) {

    if (!is.null(conn)) {
      tryCatch(
        dbRollback(conn),
        error = function(x) NULL
      )
    }

    message(glue("FATAL ERROR: {e$message}"))

  }, finally = {

    if (!is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning.")
      })
    }
  })
}

# ------------------------------------------------------------
# Run Metadata Initialization
# ------------------------------------------------------------

initialize_metadata_system()