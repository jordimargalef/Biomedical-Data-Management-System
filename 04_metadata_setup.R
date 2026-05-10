# ============================================================
# 04_metadata_setup.R
# Biomedical Data Management System
# Metadata, Controlled Vocabularies, Validation Rules
# PostgreSQL + R
# ============================================================

# ------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------

# DBI provides a common interface to communicate with databases from R.
# It allows us to connect, execute SQL queries, write tables, and manage transactions.
library(DBI)

# RPostgres is the PostgreSQL driver used by DBI.
# It makes it possible to connect specifically to a PostgreSQL database.
library(RPostgres)

# tidyverse is loaded mainly for data manipulation utilities.
# In this script, it is especially useful because tribble() is used to create small data frames manually.
library(tidyverse)

# stringr provides functions for working with strings.
# It is loaded here as part of the general project environment, even if this script does not directly use many string operations.
library(stringr)

# lubridate provides tools for working with dates and times.
# This is useful in biomedical data systems where variables like date_of_birth and created_at are important.
library(lubridate)

# uuid provides the UUIDgenerate() function.
# This is used to create unique identifiers for each validation rule.
library(uuid)

# glue is used to create readable dynamic text messages.
# For example, it inserts the value of variables inside strings using { }.
library(glue)

# janitor provides functions for cleaning and checking data frames.
# It is loaded as part of the general data-management environment.
library(janitor)

# ------------------------------------------------------------
# Database Connection Configuration
# ------------------------------------------------------------

# This function centralizes the database connection logic.
# Instead of writing dbConnect() many times throughout the script,
# we define it once here and reuse it whenever a connection to PostgreSQL is needed.
db_connection <- function() {

  # dbConnect() opens a connection to the PostgreSQL database.
  # The database name, host, and port are defined explicitly.
  # The username and password are obtained from environment variables,
  # which is safer than writing credentials directly in the script.
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "biomedical_db",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  # The connection object is returned so that other functions can use it
  # to execute SQL queries or write data into the database.
  return(conn)
}

# ------------------------------------------------------------
# Safe SQL Execution Helper
# ------------------------------------------------------------

# This helper function executes SQL code in a controlled way.
# It receives:
# - conn: the active database connection
# - sql_query: the SQL statement to execute
# - description: a human-readable description of what the SQL is doing
#
# The objective is to make SQL execution safer and easier to debug.
execute_sql_safe <- function(conn, sql_query, description = "SQL execution") {

  # tryCatch() is used to handle errors gracefully.
  # If the SQL runs correctly, a success message is printed.
  # If an error occurs, the error is reported and then re-thrown with stop(e).
  tryCatch({

    # dbExecute() sends the SQL command to PostgreSQL.
    # This is used for commands that modify the database structure or data,
    # such as CREATE TABLE, DELETE, or CREATE INDEX.
    dbExecute(conn, sql_query)

    # If no error occurs, this message confirms that the step was completed.
    message(glue("SUCCESS: {description}"))

  }, error = function(e) {

    # If PostgreSQL or DBI returns an error, this message helps identify
    # exactly which part of the setup failed.
    message(glue("ERROR during {description}: {e$message}"))

    # stop(e) interrupts the execution and passes the error to the outer tryCatch().
    # This is important because if one database operation fails,
    # we do not want to continue creating an inconsistent metadata system.
    stop(e)
  })
}

# ------------------------------------------------------------
# Initialize Metadata Infrastructure
# ------------------------------------------------------------

# This is the main function of the script.
# Its purpose is to create and populate the metadata infrastructure of the biomedical database.
#
# Specifically, it:
# 1. Connects to the PostgreSQL database.
# 2. Creates tables for vocabularies, metadata, and validation rules.
# 3. Deletes previous metadata records to reload a clean configuration.
# 4. Inserts controlled vocabularies and metadata definitions.
# 5. Inserts validation rules.
# 6. Creates indexes to improve query performance.
# 7. Uses a transaction so that all changes are committed only if everything succeeds.
initialize_metadata_system <- function() {

  # The connection is initialized as NULL so that it exists in the full scope
  # of the function. This allows the error and finally blocks to check whether
  # a connection was successfully opened before trying to roll back or disconnect.
  conn <- NULL

  # The whole initialization process is wrapped in tryCatch().
  # This allows the script to roll back the transaction if an error occurs,
  # preventing the database from being left in a partially updated state.
  tryCatch({

    # Open a connection to the PostgreSQL database using the helper function defined above.
    conn <- db_connection()

    # Start a database transaction.
    # All operations after this point are treated as a single unit of work.
    # If everything succeeds, dbCommit() saves the changes.
    # If something fails, dbRollback() cancels all changes made in the transaction.
    dbBegin(conn)

    # ========================================================
    # VOCABULARY REGISTRY
    # ========================================================

    # This SQL creates the vocabulary_registry table if it does not already exist.
    #
    # This table acts as a registry of all controlled vocabularies used in the system.
    # Each vocabulary has:
    # - a unique name
    # - a description
    # - a terminology source
    # - an active flag
    # - a creation timestamp
    #
    # Example vocabularies inserted later are sex_vocab, blood_type_vocab, and smoker_vocab.
    vocabulary_registry_sql <- "

    CREATE TABLE IF NOT EXISTS public.vocabulary_registry (

        vocabulary_name TEXT PRIMARY KEY,
        description TEXT NOT NULL,
        terminology_source TEXT NOT NULL,
        active BOOLEAN NOT NULL DEFAULT TRUE,
        created_at TIMESTAMP WITH TIME ZONE DEFAULT CURRENT_TIMESTAMP

    );

    "

    # Execute the SQL statement that creates the vocabulary registry table.
    execute_sql_safe(
      conn,
      vocabulary_registry_sql,
      "Creating vocabulary_registry table"
    )

    # ========================================================
    # CONTROLLED VOCABULARIES
    # ========================================================

    # This SQL creates the controlled_vocabularies table.
    #
    # This table stores the actual allowed values for each controlled vocabulary.
    # For example:
    # - sex_vocab can contain M, F, X
    # - blood_type_vocab can contain A+, A-, B+, etc.
    # - smoker_vocab can contain TRUE and FALSE
    #
    # The vocabulary_name column references vocabulary_registry,
    # meaning that each allowed value must belong to a vocabulary already registered.
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

    # Execute the SQL statement that creates the controlled vocabularies table.
    execute_sql_safe(
      conn,
      controlled_vocabularies_sql,
      "Creating controlled_vocabularies table"
    )

    # ========================================================
    # METADATA TABLE
    # ========================================================

    # This SQL creates the metadata_table.
    #
    # This table documents the meaning and expected properties of each variable
    # used in the biomedical database.
    #
    # For each variable, the table can store:
    # - the variable name
    # - which table it belongs to
    # - a display label
    # - the expected datatype
    # - the unit, if applicable
    # - minimum and maximum accepted values
    # - the controlled vocabulary, if applicable
    # - whether the field is required
    # - whether it is system-generated
    # - whether it is derived
    # - a semantic category
    # - a human-readable description
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

    # Execute the SQL statement that creates the metadata table.
    execute_sql_safe(
      conn,
      metadata_table_sql,
      "Creating metadata_table"
    )

    # ========================================================
    # VALIDATION RULES
    # ========================================================

    # This SQL creates the validation_rules table.
    #
    # This table defines the data-quality rules that will later be used to validate
    # patient records. Each rule describes what should be checked, for which variable,
    # how severe the problem is, and why the rule matters clinically.
    #
    # The table supports different validation types, such as:
    # - required fields
    # - regular expression format checks
    # - range checks
    # - plausibility checks
    # - controlled vocabulary checks
    # - cross-field consistency checks
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

    # Execute the SQL statement that creates the validation rules table.
    execute_sql_safe(
      conn,
      validation_rules_sql,
      "Creating validation_rules table"
    )

    # ========================================================
    # INSERT VOCABULARY REGISTRY
    # ========================================================

    # Before inserting the metadata configuration, the existing records are deleted.
    # This makes the setup reproducible: every time the script runs,
    # it reloads the same metadata, vocabularies, and validation rules.
    #
    # The deletion order is important because of foreign key relationships:
    # controlled_vocabularies, validation_rules, and metadata_table reference vocabulary_registry,
    # so they must be cleared before vocabulary_registry is cleared.
    dbExecute(conn, "DELETE FROM public.controlled_vocabularies;")
    dbExecute(conn, "DELETE FROM public.validation_rules;")
    dbExecute(conn, "DELETE FROM public.metadata_table;")
    dbExecute(conn, "DELETE FROM public.vocabulary_registry;")

    # This tibble defines the controlled vocabularies that the system recognizes.
    # Each row represents a vocabulary category, not an individual allowed value.
    vocabulary_registry <- tribble(
      ~vocabulary_name, ~description, ~terminology_source,
      "sex_vocab", "Biological sex vocabulary", "Internal Clinical Standard",
      "blood_type_vocab", "Blood type controlled vocabulary", "ABO/Rh Standard",
      "smoker_vocab", "Smoking status controlled vocabulary", "Internal Clinical Standard"
    )

    # Insert the vocabulary registry records into PostgreSQL.
    # append = TRUE means the rows are added to the existing table.
    # row.names = FALSE prevents R row names from being written as an extra column.
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

    # This tibble defines all allowed values for each controlled vocabulary.
    #
    # For example:
    # - sex_vocab allows M, F, and X
    # - blood_type_vocab allows all ABO/Rh combinations listed below
    # - smoker_vocab allows TRUE and FALSE as controlled textual values
    #
    # These controlled vocabularies help standardize data entry and avoid
    # inconsistent values such as "male", "Male", "M.", or other variants.
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

    # Insert the allowed vocabulary values into the controlled_vocabularies table.
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

    # This tibble defines the metadata for all variables used in the patients table
    # and for derived variables such as BMI.
    #
    # Each row describes one variable and specifies:
    # - where the variable belongs
    # - what datatype it should have
    # - whether it has units
    # - whether it has valid numeric limits
    # - whether it must follow a controlled vocabulary
    # - whether it is required
    # - whether it is generated by the system
    # - whether it is derived from other variables
    # - what semantic category it belongs to
    metadata_table <- tribble(
      ~variable_name, ~table_name, ~display_label, ~datatype, ~unit,
      ~min_value, ~max_value, ~allowed_vocabulary,
      ~is_required, ~is_system_generated, ~is_derived,
      ~semantic_type, ~description,

      # patient_uuid is a system-generated UUID used as the internal unique identifier.
      # It is required, but it should not be manually entered by users.
      "patient_uuid", "patients", "Patient UUID", "UUID", NA,
      NA, NA, NA,
      TRUE, TRUE, FALSE,
      "identifier", "System generated unique patient identifier",

      # patient_id is a human-readable identifier.
      # Unlike patient_uuid, this value is not system-generated and is intended for traceability.
      "patient_id", "patients", "Patient ID", "TEXT", NA,
      NA, NA, NA,
      TRUE, FALSE, FALSE,
      "identifier", "Human-readable patient identifier",

      # date_of_birth stores the patient's birth date.
      # It is required because it supports demographic validation and age consistency checks.
      "date_of_birth", "patients", "Date of Birth", "DATE", NA,
      NA, NA, NA,
      TRUE, FALSE, FALSE,
      "demographic", "Patient date of birth",

      # age stores the patient's age in years.
      # The metadata defines a general valid range from 0 to 120.
      "age", "patients", "Age", "INTEGER", "years",
      0, 120, NA,
      TRUE, FALSE, FALSE,
      "demographic", "Patient age in years",

      # sex is a demographic variable and must use the sex_vocab controlled vocabulary.
      # This prevents inconsistent entries and standardizes coding.
      "sex", "patients", "Sex", "TEXT", NA,
      NA, NA, "sex_vocab",
      TRUE, FALSE, FALSE,
      "demographic", "Patient biological sex",

      # weight_kg stores body weight in kilograms.
      # It is optional but has a broad possible range defined for validation.
      "weight_kg", "patients", "Weight", "NUMERIC", "kg",
      1, 500, NA,
      FALSE, FALSE, FALSE,
      "clinical_measurement", "Patient body weight",

      # height_cm stores body height in centimeters.
      # It is optional and can later be used together with weight to calculate BMI.
      "height_cm", "patients", "Height", "NUMERIC", "cm",
      30, 300, NA,
      FALSE, FALSE, FALSE,
      "clinical_measurement", "Patient body height",

      # blood_type stores the patient's blood type.
      # It uses the blood_type_vocab vocabulary to restrict the allowed values.
      "blood_type", "patients", "Blood Type", "TEXT", NA,
      NA, NA, "blood_type_vocab",
      FALSE, FALSE, FALSE,
      "laboratory", "Patient blood type",

      # diagnosis_code stores a clinical diagnosis code.
      # The validation rules later check whether it resembles an ICD-style format.
      "diagnosis_code", "patients", "Diagnosis Code", "TEXT", NA,
      NA, NA, NA,
      FALSE, FALSE, FALSE,
      "clinical_code", "Clinical diagnosis code",

      # dosage_mg stores medication dosage in milligrams.
      # It is defined as an INTEGER and has a broad maximum value for structural validation.
      "dosage_mg", "patients", "Dosage", "INTEGER", "mg",
      0, 100000, NA,
      FALSE, FALSE, FALSE,
      "medication", "Medication dosage",

      # smoker stores the smoking status.
      # Even though the datatype is BOOLEAN, it is also associated with a controlled vocabulary
      # so that the validation layer can check standardized accepted values.
      "smoker", "patients", "Smoking Status", "BOOLEAN", NA,
      NA, NA, "smoker_vocab",
      FALSE, FALSE, FALSE,
      "behavioral", "Smoking status",

      # doctor_name stores the responsible physician.
      # This can be useful for filtering records, auditing, or clinical follow-up.
      "doctor_name", "patients", "Doctor Name", "TEXT", NA,
      NA, NA, NA,
      FALSE, FALSE, FALSE,
      "provider", "Responsible physician",

      # created_at stores the timestamp when the patient record was created.
      # It is system-generated and required for traceability and audit purposes.
      "created_at", "patients", "Created Timestamp", "TIMESTAMPTZ", NA,
      NA, NA, NA,
      TRUE, TRUE, FALSE,
      "system", "System-generated creation timestamp",

      # bmi is a derived variable, not a directly stored patient input field.
      # It is included in the metadata because it can be validated as a calculated clinical metric.
      "bmi", "derived", "Body Mass Index", "NUMERIC", "kg/m2",
      0, 100, NA,
      FALSE, FALSE, TRUE,
      "derived_measurement", "Derived body mass index"
    )

    # Insert all metadata definitions into the metadata_table.
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

    # This tibble defines all validation rules used by the system.
    #
    # Each rule includes:
    # - a generated UUID as rule_id
    # - a unique rule name
    # - the variable it applies to
    # - the validation type
    # - whether it applies to one field, one record, or the whole database
    # - the type of issue it detects
    # - the severity of the problem
    # - optional regex, numeric ranges, or controlled vocabulary links
    # - a description and clinical rationale
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

      # This rule checks that patient_id is not missing.
      # It is marked as CRITICAL because the patient identifier is essential
      # for traceability and for detecting duplicated records.
      UUIDgenerate(), "required_patient_id", "patient_id",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Patient ID is required",
      "The patient identifier is mandatory for traceability and duplicate detection",

      # This rule checks that date_of_birth is present.
      # Date of birth is needed to validate age and maintain demographic consistency.
      UUIDgenerate(), "required_date_of_birth", "date_of_birth",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Date of birth is required",
      "Date of birth is required for demographic consistency and age validation",

      # This rule checks that age is present.
      # Age is considered clinically important because many interpretations depend on it.
      UUIDgenerate(), "required_age", "age",
      "required", "field", "missing_required_value", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Age is required",
      "Age is required for clinical interpretation and consistency checks",

      # This rule checks that sex is present.
      # Sex is a core demographic field in this registry.
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

      # This rule validates the format of patient_id using a regular expression.
      # The expected format is P-XXXX, where XXXX are four digits.
      UUIDgenerate(), "patient_id_format", "patient_id",
      "regex", "field", "format_failure", "CRITICAL",
      "^P-[0-9]{4}$", NA, NA,
      NA, NA, NA,
      TRUE,
      "Patient ID must follow P-XXXX format",
      "A standardized patient identifier format reduces entry errors and improves traceability",

      # This rule checks that patient_id is unique at the database level.
      # It prevents two patient records from sharing the same human-readable identifier.
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

      # This rule checks that age is stored as an integer.
      # This supports consistency with the patients table schema.
      UUIDgenerate(), "age_integer_validation", "age",
      "datatype", "field", "datatype_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Age must be an integer",
      "The patients table stores age as INTEGER; non-integer values would violate schema consistency",

      # This rule defines a hard valid range for age.
      # Values below 0 or above 120 are treated as critical range failures.
      UUIDgenerate(), "age_hard_range", "age",
      "range", "field", "range_failure", "CRITICAL",
      NA, 0, 120,
      NA, NA, NA,
      TRUE,
      "Age must be between 0 and 120 years",
      "Values outside this interval are biologically implausible for patient age",

      # This rule marks ages above 100 as warnings rather than critical errors.
      # The value might be possible, but it should be reviewed.
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

      # This rule checks that date_of_birth is not a future date.
      # A future birth date is logically impossible and therefore critical.
      UUIDgenerate(), "date_of_birth_future_date", "date_of_birth",
      "temporal", "field", "future_date", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Date of birth cannot be in the future",
      "A future date of birth is logically impossible",

      # This rule checks consistency between date_of_birth and age.
      # It is a cross-field rule because it compares two fields in the same record.
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

      # This rule checks that sex belongs to the approved sex_vocab controlled vocabulary.
      # Values outside the vocabulary are considered critical because they break standard coding.
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

      # This rule checks that blood_type belongs to the approved blood_type_vocab vocabulary.
      # This avoids non-standard blood type entries.
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

      # This rule checks that smoker belongs to the approved smoker_vocab vocabulary.
      # It standardizes behavioral risk-factor coding.
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

      # This rule checks whether diagnosis_code resembles an ICD-style code.
      # It is a warning because the code may still be valid in another coding system,
      # but unusual formats should be reviewed.
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

      # This rule defines the hard valid range for weight in kilograms.
      # Values outside 1 to 500 kg are considered structurally invalid.
      UUIDgenerate(), "weight_hard_range", "weight_kg",
      "range", "field", "range_failure", "CRITICAL",
      NA, 1, 500,
      NA, NA, NA,
      TRUE,
      "Weight must be between 1 and 500 kg",
      "Values outside this interval are considered impossible or structurally invalid for human body weight",

      # This rule flags very low body weight values.
      # It is a warning because some values may be valid in specific populations,
      # but they should still be checked.
      UUIDgenerate(), "weight_plausibility_low", "weight_kg",
      "plausibility_low", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      30, NA, NA,
      TRUE,
      "Weight below 30 kg is clinically unusual",
      "Very low body weight may be valid in specific populations but should be reviewed",

      # This rule flags very high body weight values.
      # It helps detect potential unit errors or data entry mistakes.
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

      # This rule defines the hard valid range for height in centimeters.
      # Values outside 30 to 300 cm are considered structurally invalid.
      UUIDgenerate(), "height_hard_range", "height_cm",
      "range", "field", "range_failure", "CRITICAL",
      NA, 30, 300,
      NA, NA, NA,
      TRUE,
      "Height must be between 30 and 300 cm",
      "Values outside this interval are considered impossible or structurally invalid for human body height",

      # This rule flags very low height values.
      # It is not automatically critical because some values may be valid depending on age or population.
      UUIDgenerate(), "height_plausibility_low", "height_cm",
      "plausibility_low", "field", "implausible_value", "WARNING",
      NA, NA, NA,
      100, NA, NA,
      TRUE,
      "Height below 100 cm is clinically unusual",
      "Very low height may be valid in specific populations but should be reviewed",

      # This rule flags very high height values.
      # It is useful for detecting possible unit mistakes, such as entering meters instead of centimeters.
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

      # This rule checks that dosage_mg is an integer.
      # The database schema expects dosage_mg to be stored as INTEGER.
      UUIDgenerate(), "dosage_integer_validation", "dosage_mg",
      "datatype", "field", "datatype_failure", "CRITICAL",
      NA, NA, NA,
      NA, NA, NA,
      TRUE,
      "Dosage must be an integer",
      "The patients table stores dosage_mg as INTEGER; decimal dosages would violate schema consistency",

      # This rule defines the hard valid range for dosage.
      # Negative dosages and extremely large values are treated as critical structural errors.
      UUIDgenerate(), "dosage_hard_range", "dosage_mg",
      "range", "field", "range_failure", "CRITICAL",
      NA, 0, 100000,
      NA, NA, NA,
      TRUE,
      "Dosage must be between 0 and 100000 mg",
      "Negative or extremely high dosage values are structurally invalid for this registry",

      # This rule flags unusually high dosage values.
      # It is a warning because some high values may be valid depending on the medication,
      # but they should be reviewed.
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

      # This rule flags extremely low BMI values.
      # BMI is a derived metric, so this rule applies at the record level rather than to a raw input field.
      UUIDgenerate(), "bmi_low_warning", "bmi",
      "derived_metric_low", "record", "implausible_value", "WARNING",
      NA, NA, NA,
      10, NA, NA,
      TRUE,
      "BMI below 10 is clinically unusual",
      "Extremely low BMI may indicate a serious clinical state or a measurement/unit error",

      # This rule flags extremely high BMI values.
      # Such values may represent real clinical conditions or may indicate measurement/unit problems.
      UUIDgenerate(), "bmi_high_warning", "bmi",
      "derived_metric_high", "record", "implausible_value", "WARNING",
      NA, NA, NA,
      NA, 70, NA,
      TRUE,
      "BMI above 70 is clinically unusual",
      "Extremely high BMI may indicate a serious clinical state or a measurement/unit error"
    )

    # Insert all validation rules into the validation_rules table.
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

    # This vector contains SQL statements for creating indexes.
    #
    # Indexes improve the speed of searches and filtering operations.
    # They are especially useful for columns that are frequently used in WHERE clauses,
    # joins, or filtering operations.
    #
    # CREATE INDEX IF NOT EXISTS prevents errors if the index already exists.
    index_sql <- c(

  # =====================================================
  # VALIDATION RULES INDEXES
  # =====================================================

  # Index on variable_name speeds up queries that retrieve all validation rules
  # associated with a specific metadata variable.
  "CREATE INDEX IF NOT EXISTS idx_validation_rules_variable_name
   ON public.validation_rules(variable_name);",

  # Index on rule_name speeds up direct lookup of a specific validation rule.
  "CREATE INDEX IF NOT EXISTS idx_validation_rules_rule_name
   ON public.validation_rules(rule_name);",

  # Index on active speeds up filtering between active and inactive rules.
  # This is useful if the validation engine only applies active rules.
  "CREATE INDEX IF NOT EXISTS idx_validation_rules_active
   ON public.validation_rules(active);",

  # Index on severity speeds up filtering validation rules by severity level,
  # for example CRITICAL versus WARNING.
  "CREATE INDEX IF NOT EXISTS idx_validation_rules_severity
   ON public.validation_rules(severity);",

  # =====================================================
  # PATIENTS INDEXES
  # =====================================================

  # Index on patient_id speeds up searches for a specific patient identifier
  # and supports duplicate-checking workflows.
  "CREATE INDEX IF NOT EXISTS idx_patients_patient_id
   ON public.patients(patient_id);",

  # Index on created_at speeds up chronological searches,
  # such as retrieving recently created patient records.
  "CREATE INDEX IF NOT EXISTS idx_patients_created_at
   ON public.patients(created_at);",

  # Index on diagnosis_code speeds up filtering patients by diagnosis.
  "CREATE INDEX IF NOT EXISTS idx_patients_diagnosis_code
   ON public.patients(diagnosis_code);",

  # Index on doctor_name speeds up filtering records by responsible physician.
  "CREATE INDEX IF NOT EXISTS idx_patients_doctor_name
   ON public.patients(doctor_name);"

)

    # Loop through every SQL index statement and execute it safely.
    # Using execute_sql_safe() makes the process easier to debug,
    # because each index creation reports success or failure.
    for (sql in index_sql) {
      execute_sql_safe(conn, sql, "Creating indexes")
    }

    # If all previous operations succeeded, commit the transaction.
    # This permanently saves the table creation, inserted metadata, validation rules, and indexes.
    dbCommit(conn)

    # Final success messages printed after the transaction is committed.
    message("==========================================")
    message("METADATA SYSTEM INITIALIZED SUCCESSFULLY")
    message("==========================================")

  }, error = function(e) {

    # If any error occurs inside the main tryCatch block, this error handler runs.
    #
    # If a database connection exists, the transaction is rolled back.
    # This prevents partial changes from being saved.
    if (!is.null(conn)) {
      tryCatch(
        dbRollback(conn),
        error = function(x) NULL
      )
    }

    # Print the fatal error message so the user knows why initialization failed.
    message(glue("FATAL ERROR: {e$message}"))

  }, finally = {

    # The finally block always runs, whether the script succeeds or fails.
    # Its purpose is to safely close the database connection.
    if (!is.null(conn)) {

      tryCatch({

        # Close the PostgreSQL connection to free resources.
        dbDisconnect(conn)

      }, error = function(e) {

        # If disconnecting fails, show a warning instead of crashing the script.
        message("Connection closing warning.")
      })
    }
  })
}

# ------------------------------------------------------------
# Run Metadata Initialization
# ------------------------------------------------------------

# This line actually executes the full metadata setup process.
# Without this call, the function would be defined but nothing would be created or inserted.
initialize_metadata_system()