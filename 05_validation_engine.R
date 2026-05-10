# ============================================================
# 05_validation_engine.R
# Biomedical Defensive Validation Engine
# PostgreSQL + R
# ============================================================
#
# PURPOSE:
# This script contains validation functions for new patient
# records before insertion into public.patients.
#
# IMPORTANT:
# - This script DOES NOT insert patient records.
# - This script DOES NOT modify public.patients.
# - This script DOES NOT create tests or run tests automatically.
# - This script reads metadata, validation rules and controlled
#   vocabularies created by 04_metadata_setup.R.
#
# MAIN ENTRY POINT:
# validate_patient_record(record)
#
# EXPECTED RETURN STRUCTURE:
# list(
#   valid = TRUE/FALSE,
#   overall_severity = "INFO" / "WARNING" / "CRITICAL",
#   issues = c(...),
#   warnings = c(...),
#   flags = data.frame(...),
#   bmi = ...,
#   cleaned_record = ...
# )
#
# ============================================================

# ------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------

# DBI provides the general database interface used to connect to,
# query, and disconnect from relational databases in R.
library(DBI)

# RPostgres is the PostgreSQL backend used by DBI.
# It allows this script to communicate specifically with a PostgreSQL database.
library(RPostgres)

# tidyverse provides data manipulation tools such as filter(),
# mutate(), pull(), bind_rows(), tibble(), and pipes (%>%).
library(tidyverse)

# stringr is used for safe and readable string manipulation,
# such as trimming spaces, converting to uppercase, and detecting regex patterns.
library(stringr)

# lubridate is used to handle dates and time intervals.
# In this script, it is especially useful for parsing dates and calculating age from date_of_birth.
library(lubridate)

# uuid is loaded for UUID-related functionality.
# In this specific validation script, UUIDs are mainly already loaded from the database rules.
library(uuid)

# glue allows readable string interpolation.
# It is used to create dynamic error messages and validation descriptions.
library(glue)

# janitor provides helper functions for cleaning data.
# It is loaded as part of the general data-governance environment.
library(janitor)

# ------------------------------------------------------------
# Database Connection Configuration
# ------------------------------------------------------------
# This uses the same environment-variable pattern as
# 04_metadata_setup.R.
#
# Required environment variables:
# - PGUSER
# - PGPASSWORD
#
# If you prefer local hardcoded credentials during development,
# replace Sys.getenv("PGUSER") and Sys.getenv("PGPASSWORD") with
# your PostgreSQL username and password.
# ------------------------------------------------------------

# This function creates and returns a connection to the PostgreSQL database.
# It centralizes the connection details so the rest of the script can reuse
# the same connection logic without repeating dbConnect() every time.
db_connection <- function() {

  # dbConnect() opens a connection to the biomedical_db database.
  # The username and password are read from environment variables,
  # which avoids hardcoding sensitive credentials directly in the code.
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "biomedical_db",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  # Return the active connection object so it can be used by query functions.
  return(conn)
}

# ------------------------------------------------------------
# Safe Query Helper
# ------------------------------------------------------------

# This helper safely runs SELECT queries against the database.
# It is used when the script needs to read metadata, validation rules,
# or controlled vocabularies from PostgreSQL.
#
# Parameters:
# - conn: active database connection
# - sql_query: SQL SELECT query to execute
# - description: text used to identify the query if an error occurs
db_get_query_safe <- function(conn, sql_query, description = "Database query") {

  # tryCatch() allows the function to capture and report database errors
  # instead of failing silently.
  tryCatch({

    # dbGetQuery() sends the SQL query to PostgreSQL and returns the result
    # as an R data frame.
    result <- dbGetQuery(conn, sql_query)

    # Return the query result to the function that requested it.
    return(result)

  }, error = function(e) {

    # If an error occurs, this message explains which database query failed.
    message(glue("ERROR during {description}: {e$message}"))

    # Re-throw the error so that the calling function can stop or handle it.
    stop(e)
  })
}

# ------------------------------------------------------------
# Load Governance Context
# ------------------------------------------------------------
# Reads the governance layer created in 04_metadata_setup.R:
# - metadata_table
# - vocabulary_registry
# - controlled_vocabularies
# - validation_rules
#
# Only active validation rules and active controlled vocabulary
# values are loaded.
# ------------------------------------------------------------

# This function loads all the governance information needed by the validation engine.
# The validation engine does not hardcode most validation limits directly;
# instead, it reads them from the metadata and validation tables created earlier.
#
# The governance context returned by this function is a list containing:
# - metadata_table
# - vocabulary_registry
# - controlled_vocabularies
# - validation_rules
load_governance_context <- function(conn = NULL) {

  # local_connection indicates whether this function opened its own database connection.
  # If TRUE, the function will also be responsible for closing it.
  local_connection <- FALSE

  tryCatch({

    # If no connection was provided, create a new one.
    # This makes the function flexible: it can work independently or reuse an existing connection.
    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    # dbListTables() retrieves the list of tables visible in the connected database.
    # This is used to check whether the metadata system has been initialized.
    existing_tables <- dbListTables(conn)

    # These are the required governance tables that must exist before validation can run.
    required_tables <- c(
      "metadata_table",
      "vocabulary_registry",
      "controlled_vocabularies",
      "validation_rules"
    )

    # setdiff() identifies which required tables are missing from the database.
    missing_tables <- setdiff(required_tables, existing_tables)

    # If any governance table is missing, the validation engine cannot operate safely.
    # The user is instructed to run the metadata setup script first.
    if (length(missing_tables) > 0) {

      stop(glue(
        "Missing governance tables: {paste(missing_tables, collapse = ', ')}. ",
        "Run source('04_metadata_setup.R') before using the validation engine."
      ))
    }

    # Load the metadata table.
    # This table describes each variable, including datatype, units, required status,
    # controlled vocabulary, and semantic meaning.
    metadata_table <- db_get_query_safe(
      conn,
      "
      SELECT *
      FROM public.metadata_table
      ORDER BY table_name, variable_name;
      ",
      "Loading metadata_table"
    )

    # Load only active vocabularies from the vocabulary registry.
    # Inactive vocabularies are ignored so they are not used during validation.
    vocabulary_registry <- db_get_query_safe(
      conn,
      "
      SELECT *
      FROM public.vocabulary_registry
      WHERE active = TRUE
      ORDER BY vocabulary_name;
      ",
      "Loading vocabulary_registry"
    )

    # Load active allowed values from the controlled_vocabularies table.
    # These values are later used to check fields such as sex, blood_type, and smoker.
    controlled_vocabularies <- db_get_query_safe(
      conn,
      "
      SELECT *
      FROM public.controlled_vocabularies
      WHERE active = TRUE
      ORDER BY vocabulary_name, allowed_value;
      ",
      "Loading controlled_vocabularies"
    )

    # Load active validation rules.
    # These rules define the checks that will be applied to patient records.
    validation_rules <- db_get_query_safe(
      conn,
      "
      SELECT *
      FROM public.validation_rules
      WHERE active = TRUE
      ORDER BY variable_name, validation_type, rule_name;
      ",
      "Loading validation_rules"
    )

    # Return all governance information as a single list.
    # This makes it easier to pass the governance context into validation functions.
    list(
      metadata_table = metadata_table,
      vocabulary_registry = vocabulary_registry,
      controlled_vocabularies = controlled_vocabularies,
      validation_rules = validation_rules
    )

  }, error = function(e) {

    # If anything fails while loading the governance context,
    # the function stops with a clear error message.
    stop(glue("Unable to load governance context: {e$message}"))

  }, finally = {

    # If this function created its own connection, it closes it here.
    # If the connection was provided from outside, it is not closed here.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in load_governance_context().")
      })
    }
  })
}

# ------------------------------------------------------------
# Generic Helper Functions
# ------------------------------------------------------------

# This function checks whether a value should be considered missing.
# It handles several possible missing cases:
# - NULL
# - length 0
# - NA
# - empty character strings like ""
#
# This is important because user input may come from forms, CSV files,
# Shiny inputs, or manual lists, and each source can represent missing data differently.
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

# This function converts a value to character safely.
# If the value is missing, it returns NA_character_.
# Otherwise, it takes the first value, converts it to text, trims spaces,
# and returns NA if the final string is empty.
as_character_or_na <- function(x) {

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

# This function safely converts an input value to numeric.
# If the value is missing, it returns NA_real_.
# suppressWarnings() avoids printing warnings when conversion fails,
# for example when trying to convert "abc" to a number.
as_numeric_safe <- function(x) {

  if (is_missing_value(x)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(x[[1]]))
}

# This function converts an input value to integer only if it is truly integer-like.
# For example:
# - "25" becomes 25
# - 25.0 becomes 25
# - 25.5 becomes NA_integer_
#
# This prevents decimal values from silently becoming integers.
as_integer_safe <- function(x) {

  numeric_value <- as_numeric_safe(x)

  if (is.na(numeric_value)) {
    return(NA_integer_)
  }

  # This checks whether the numeric value is close enough to its rounded version.
  # The tolerance uses machine precision to avoid floating-point comparison problems.
  if (abs(numeric_value - round(numeric_value)) > .Machine$double.eps^0.5) {
    return(NA_integer_)
  }

  as.integer(round(numeric_value))
}

# This function safely converts different types of date input into an R Date object.
# It supports:
# - values already stored as Date
# - POSIX date-time values
# - character dates parseable by as.Date()
# - ymd, dmy, and mdy formats through lubridate
#
# If no valid date can be parsed, it returns NA as a Date.
as_date_safe <- function(x) {

  if (is_missing_value(x)) {
    return(as.Date(NA))
  }

  if (inherits(x, "Date")) {
    return(x[[1]])
  }

  if (inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
    return(as.Date(x[[1]]))
  }

  value <- as.character(x[[1]])
  value <- str_trim(value)

  # First attempt: base R date parsing.
  parsed_date <- suppressWarnings(as.Date(value))

  if (!is.na(parsed_date)) {
    return(parsed_date)
  }

  # Second attempt: year-month-day format, for example "2024-05-10".
  parsed_date <- suppressWarnings(lubridate::ymd(value))

  if (!is.na(parsed_date)) {
    return(as.Date(parsed_date))
  }

  # Third attempt: day-month-year format, for example "10/05/2024".
  parsed_date <- suppressWarnings(lubridate::dmy(value))

  if (!is.na(parsed_date)) {
    return(as.Date(parsed_date))
  }

  # Fourth attempt: month-day-year format, for example "05/10/2024".
  parsed_date <- suppressWarnings(lubridate::mdy(value))

  if (!is.na(parsed_date)) {
    return(as.Date(parsed_date))
  }

  return(as.Date(NA))
}

# This function converts common boolean-like inputs into TRUE or FALSE.
# It accepts logical values directly and also supports text inputs such as:
# TRUE, T, YES, Y, 1, FALSE, F, NO, N, 0.
#
# If the value cannot be interpreted as boolean, it returns NA.
as_boolean_safe <- function(x) {

  if (is_missing_value(x)) {
    return(NA)
  }

  if (is.logical(x)) {
    return(as.logical(x[[1]]))
  }

  value <- str_to_upper(str_trim(as.character(x[[1]])))

  if (value %in% c("TRUE", "T", "YES", "Y", "1")) {
    return(TRUE)
  }

  if (value %in% c("FALSE", "F", "NO", "N", "0")) {
    return(FALSE)
  }

  return(NA)
}

# This function converts boolean-like values into the controlled vocabulary format.
# The smoker vocabulary stores values as the strings "TRUE" and "FALSE",
# so this helper standardizes different input styles before checking the vocabulary.
boolean_to_vocab_value <- function(x) {

  if (is_missing_value(x)) {
    return(NA_character_)
  }

  if (is.logical(x)) {
    return(ifelse(isTRUE(x[[1]]), "TRUE", "FALSE"))
  }

  value <- str_to_upper(str_trim(as.character(x[[1]])))

  if (value %in% c("TRUE", "T", "YES", "Y", "1")) {
    return("TRUE")
  }

  if (value %in% c("FALSE", "F", "NO", "N", "0")) {
    return("FALSE")
  }

  return(value)
}

# This function checks whether a value can be interpreted as an integer.
# It is used for datatype validation before accepting fields such as age or dosage_mg.
is_integer_like <- function(x) {

  numeric_value <- as_numeric_safe(x)

  if (is.na(numeric_value)) {
    return(FALSE)
  }

  abs(numeric_value - round(numeric_value)) <= .Machine$double.eps^0.5
}

# This function assigns a numeric rank to each severity level.
# The numeric rank makes it easier to compare severities and determine
# the overall severity of a validation result.
severity_rank <- function(severity) {

  ranks <- c(
    "INFO" = 1,
    "WARNING" = 2,
    "CRITICAL" = 3
  )

  severity <- as.character(severity)

  if (!severity %in% names(ranks)) {
    return(NA_real_)
  }

  unname(ranks[[severity]])
}

# This function returns the highest severity from a vector of severities.
# For example, if a record has both WARNING and CRITICAL flags,
# the overall severity should be CRITICAL.
max_severity <- function(severities) {

  if (length(severities) == 0) {
    return("INFO")
  }

  severities <- severities[!is.na(severities)]

  if (length(severities) == 0) {
    return("INFO")
  }

  ranks <- sapply(severities, severity_rank)

  severities[[which.max(ranks)]]
}

# This function creates an empty flags tibble with the correct column structure.
# It is used as the starting point for validation functions,
# so that all functions return the same type of object even when no issues are found.
empty_flags <- function() {

  tibble(
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
# Validation Rule Accessors
# ------------------------------------------------------------

# This function retrieves all active validation rules for one variable.
# For example, calling it with "age" would return all age-related active rules.
get_rules_by_variable <- function(governance, variable_name) {

  governance$validation_rules %>%
    filter(.data$variable_name == !!variable_name) %>%
    filter(.data$active == TRUE)
}

# This function retrieves one active validation rule by its rule_name.
# If no matching rule exists, it returns NULL.
#
# This is useful when a specific validation function needs one known rule,
# such as "patient_id_format" or "dob_age_consistency".
get_rule_by_name <- function(governance, rule_name) {

  rule <- governance$validation_rules %>%
    filter(.data$rule_name == !!rule_name) %>%
    filter(.data$active == TRUE) %>%
    slice(1)

  if (nrow(rule) == 0) {
    return(NULL)
  }

  return(rule)
}

# This function retrieves the first active validation rule that matches
# both a variable name and a validation type.
#
# It is useful when a function needs one representative rule,
# for example the "range" rule for a numeric field.
get_rule_by_variable_and_type <- function(governance,
                                          variable_name,
                                          validation_type) {

  rule <- governance$validation_rules %>%
    filter(.data$variable_name == !!variable_name) %>%
    filter(.data$validation_type == !!validation_type) %>%
    filter(.data$active == TRUE) %>%
    slice(1)

  if (nrow(rule) == 0) {
    return(NULL)
  }

  return(rule)
}

# This function retrieves all active validation rules matching
# a variable name and validation type.
#
# It is used when a variable can have multiple rules of the same general type,
# such as plausibility checks.
get_rules_by_variable_and_type <- function(governance,
                                           variable_name,
                                           validation_type) {

  governance$validation_rules %>%
    filter(.data$variable_name == !!variable_name) %>%
    filter(.data$validation_type == !!validation_type) %>%
    filter(.data$active == TRUE)
}

# This function retrieves all active allowed values for a given vocabulary.
# For example, for "blood_type_vocab", it returns A+, A-, B+, B-, etc.
get_allowed_values <- function(governance, vocabulary_name) {

  governance$controlled_vocabularies %>%
    filter(.data$vocabulary_name == !!vocabulary_name) %>%
    filter(.data$active == TRUE) %>%
    pull(.data$allowed_value)
}

# This function retrieves allowed values based on the variable name.
# It first looks up the variable in metadata_table to find its allowed_vocabulary,
# then retrieves the actual allowed values from controlled_vocabularies.
get_allowed_values_for_variable <- function(governance, variable_name) {

  metadata_row <- governance$metadata_table %>%
    filter(.data$variable_name == !!variable_name) %>%
    slice(1)

  if (nrow(metadata_row) == 0) {
    return(character())
  }

  vocabulary_name <- metadata_row$allowed_vocabulary[[1]]

  if (is.na(vocabulary_name) || is.null(vocabulary_name)) {
    return(character())
  }

  get_allowed_values(governance, vocabulary_name)
}

# ------------------------------------------------------------
# Flag Construction
# ------------------------------------------------------------

# This function creates a standardized validation flag.
#
# A flag represents one detected data-quality issue.
# It includes:
# - the rule that detected the issue
# - the affected variable
# - the issue type
# - the severity
# - a human-readable issue description
# - the user who detected/generated the flag
#
# If the rule is missing, fallback values are used so that the validation engine
# can still report the problem in a structured way.
make_validation_flag <- function(rule,
                                 issue_description,
                                 fallback_variable_name = NA_character_,
                                 fallback_issue_type = "validation_failure",
                                 fallback_severity = "CRITICAL") {

  # Sys.info()[["user"]] gets the operating-system user running the script.
  # This is stored for auditability and traceability.
  detected_by_user <- Sys.info()[["user"]]

  # If the system user cannot be detected, use a safe default value.
  if (is.null(detected_by_user) || is.na(detected_by_user) || detected_by_user == "") {
    detected_by_user <- "unknown_user"
  }

  # If no rule is available, create a flag using fallback metadata.
  # This avoids losing the validation issue just because the specific rule
  # could not be retrieved from the database.
  if (is.null(rule) || nrow(rule) == 0) {

    return(tibble(
      rule_id = NA_character_,
      rule_name = NA_character_,
      variable_name = as.character(fallback_variable_name),
      issue_type = as.character(fallback_issue_type),
      severity = as.character(fallback_severity),
      issue_description = as.character(issue_description),
      detected_by_user = as.character(detected_by_user)
    ))
  }

  # If the rule exists, use the official rule metadata from validation_rules.
  tibble(
    rule_id = as.character(rule$rule_id[[1]]),
    rule_name = as.character(rule$rule_name[[1]]),
    variable_name = as.character(rule$variable_name[[1]]),
    issue_type = as.character(rule$issue_type[[1]]),
    severity = as.character(rule$severity[[1]]),
    issue_description = as.character(issue_description),
    detected_by_user = as.character(detected_by_user)
  )
}

# ------------------------------------------------------------
# Clean Input Record
# ------------------------------------------------------------
# This function normalizes the user input before validation.
# It does not autocorrect clinical content; it only converts
# values to the expected R types when possible.
# ------------------------------------------------------------

# This function receives a raw patient record and converts each field
# into the expected R type.
#
# Important:
# This function does not decide whether the data is valid.
# It only prepares the data for validation by making formats consistent.
clean_patient_record <- function(record) {

  # Build a new list with the same clinical fields but cleaned/converted.
  # Each field uses the appropriate helper function:
  # - character fields are trimmed
  # - dates are parsed as Date
  # - numeric fields are converted safely
  # - integer fields are converted only if integer-like
  # - boolean fields are converted from common TRUE/FALSE representations
  cleaned_record <- list(
    patient_id = as_character_or_na(record[["patient_id"]]),
    date_of_birth = as_date_safe(record[["date_of_birth"]]),
    age = as_integer_safe(record[["age"]]),
    sex = as_character_or_na(record[["sex"]]),
    weight_kg = as_numeric_safe(record[["weight_kg"]]),
    height_cm = as_numeric_safe(record[["height_cm"]]),
    blood_type = as_character_or_na(record[["blood_type"]]),
    diagnosis_code = as_character_or_na(record[["diagnosis_code"]]),
    dosage_mg = as_integer_safe(record[["dosage_mg"]]),
    smoker = as_boolean_safe(record[["smoker"]]),
    doctor_name = as_character_or_na(record[["doctor_name"]])
  )

  # Standardize sex to uppercase so values like "m" become "M".
  # This supports comparison with the controlled vocabulary.
  if (!is.na(cleaned_record$sex)) {
    cleaned_record$sex <- str_to_upper(cleaned_record$sex)
  }

  # Standardize blood type to uppercase so entries like "a+" become "A+".
  if (!is.na(cleaned_record$blood_type)) {
    cleaned_record$blood_type <- str_to_upper(cleaned_record$blood_type)
  }

  # Return the normalized record.
  return(cleaned_record)
}

# ------------------------------------------------------------
# Required Field Validation
# ------------------------------------------------------------
# Required fields are read from metadata_table.
#
# System-generated fields such as patient_uuid and created_at
# are required in the database but should not be required from
# the manual data-entry payload.
# ------------------------------------------------------------

# This function checks that all required manual fields are present.
# It uses metadata_table to determine which variables are required.
validate_required_fields <- function(record, governance) {

  # Select required variables from metadata_table.
  # System-generated fields are excluded because the user should not manually provide them.
  # Derived fields are also excluded because they are calculated, not entered manually.
  required_fields <- governance$metadata_table %>%
    filter(.data$is_required == TRUE) %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  # Start with an empty flags table.
  flags <- empty_flags()

  # Check each required field one by one.
  for (field in required_fields) {

    # If the field is missing, create a CRITICAL validation flag.
    if (is_missing_value(record[[field]])) {

      # Retrieve the corresponding required-field rule by name.
      rule <- get_rule_by_name(
        governance,
        glue("required_{field}")
      )

      # Add the new flag to the flags table.
      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = rule,
          issue_description = glue("Missing required field: {field}."),
          fallback_variable_name = field,
          fallback_issue_type = "missing_required_value",
          fallback_severity = "CRITICAL"
        )
      )
    }
  }

  return(flags)
}

# ------------------------------------------------------------
# Patient ID Format Validation
# ------------------------------------------------------------

# This function validates the format of patient_id.
# The expected format is defined in the validation_rules table,
# specifically in the rule named "patient_id_format".
validate_patient_id_format <- function(record, governance) {

  flags <- empty_flags()

  patient_id <- record[["patient_id"]]

  # If patient_id is missing, this function does not flag it.
  # Missing required values are handled by validate_required_fields().
  if (is_missing_value(patient_id)) {
    return(flags)
  }

  # Retrieve the validation rule for patient_id format.
  rule <- get_rule_by_name(governance, "patient_id_format")

  if (is.null(rule)) {
    return(flags)
  }

  # Extract the regex pattern from the rule.
  regex_pattern <- rule$regex_pattern[[1]]

  if (is.na(regex_pattern) || regex_pattern == "") {
    return(flags)
  }

  # Check whether patient_id matches the expected regex.
  # If it does not, a CRITICAL format_failure flag is created.
  if (!str_detect(as.character(patient_id), regex_pattern)) {

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "Invalid patient_id format: '{patient_id}'. Expected format is P-XXXX."
        ),
        fallback_variable_name = "patient_id",
        fallback_issue_type = "format_failure",
        fallback_severity = "CRITICAL"
      )
    )
  }

  return(flags)
}

# ------------------------------------------------------------
# Integer Datatype Validation
# ------------------------------------------------------------

# This function validates that a raw input field is integer-like.
#
# It uses the raw record instead of only the cleaned record because the cleaned version
# may already contain NA if conversion failed. Using raw_record allows the issue message
# to show the original value entered by the user.
validate_integer_field <- function(raw_record,
                                   cleaned_record,
                                   governance,
                                   variable_name,
                                   rule_name) {

  flags <- empty_flags()

  raw_value <- raw_record[[variable_name]]

  # Missing values are not handled here.
  # Required fields are validated separately.
  if (is_missing_value(raw_value)) {
    return(flags)
  }

  # If the raw value is not integer-like, create a datatype failure flag.
  if (!is_integer_like(raw_value)) {

    rule <- get_rule_by_name(governance, rule_name)

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "{variable_name} must be an integer. Received: '{as.character(raw_value[[1]])}'."
        ),
        fallback_variable_name = variable_name,
        fallback_issue_type = "datatype_failure",
        fallback_severity = "CRITICAL"
      )
    )
  }

  return(flags)
}

# ------------------------------------------------------------
# Numeric Range and Plausibility Validation
# ------------------------------------------------------------
# This function evaluates:
# - validation_type = "range"             -> CRITICAL hard range
# - validation_type = "plausibility_low"  -> WARNING below plausible min
# - validation_type = "plausibility_high" -> WARNING above plausible max
#
# If a CRITICAL hard-range failure exists for the same variable,
# plausibility warnings are skipped for that variable to avoid
# redundant flags.
# ------------------------------------------------------------

# This function validates numeric variables such as age, weight_kg,
# height_cm, and dosage_mg.
#
# It checks:
# 1. Whether the value can be interpreted as numeric.
# 2. Whether it is inside the hard valid range.
# 3. Whether it is outside the plausible clinical range.
validate_numeric_field <- function(cleaned_record,
                                   governance,
                                   variable_name) {

  flags <- empty_flags()

  value <- cleaned_record[[variable_name]]

  # Missing values are handled elsewhere, so no flag is created here.
  if (is_missing_value(value)) {
    return(flags)
  }

  numeric_value <- as_numeric_safe(value)

  # If the value cannot be converted to numeric, create a CRITICAL datatype flag.
  if (is.na(numeric_value)) {

    fallback_rule <- get_rule_by_variable_and_type(
      governance,
      variable_name,
      "range"
    )

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = fallback_rule,
        issue_description = glue("{variable_name} must be numeric."),
        fallback_variable_name = variable_name,
        fallback_issue_type = "datatype_failure",
        fallback_severity = "CRITICAL"
      )
    )

    return(flags)
  }

  # ----------------------------------------------------------
  # Hard range validation
  # ----------------------------------------------------------

  # Retrieve all hard range rules for this variable.
  hard_rules <- get_rules_by_variable_and_type(
    governance,
    variable_name,
    "range"
  )

  # Evaluate each hard range rule.
  for (i in seq_len(nrow(hard_rules))) {

    rule <- hard_rules[i, ]

    hard_min <- rule$hard_min_value[[1]]
    hard_max <- rule$hard_max_value[[1]]

    # If a hard minimum exists and the value is below it,
    # create a CRITICAL range failure.
    if (!is.na(hard_min) && numeric_value < hard_min) {

      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = rule,
          issue_description = glue(
            "{variable_name} = {numeric_value} is below the hard minimum value ({hard_min})."
          ),
          fallback_variable_name = variable_name,
          fallback_issue_type = "range_failure",
          fallback_severity = "CRITICAL"
        )
      )
    }

    # If a hard maximum exists and the value is above it,
    # create a CRITICAL range failure.
    if (!is.na(hard_max) && numeric_value > hard_max) {

      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = rule,
          issue_description = glue(
            "{variable_name} = {numeric_value} is above the hard maximum value ({hard_max})."
          ),
          fallback_variable_name = variable_name,
          fallback_issue_type = "range_failure",
          fallback_severity = "CRITICAL"
        )
      )
    }
  }

  # ----------------------------------------------------------
  # Stop here if the same variable already has a CRITICAL flag.
  # This prevents redundant CRITICAL + WARNING flags.
  # ----------------------------------------------------------

  # If the value is already critically invalid, there is no need
  # to also report plausibility warnings for the same variable.
  if (any(flags$severity == "CRITICAL")) {
    return(flags)
  }

  # ----------------------------------------------------------
  # Plausibility low validation
  # ----------------------------------------------------------

  # Retrieve plausibility-low rules for this variable.
  # These rules generate WARNING flags, not CRITICAL flags.
  plausibility_low_rules <- get_rules_by_variable_and_type(
    governance,
    variable_name,
    "plausibility_low"
  )

  # Check whether the value is below the clinically plausible lower threshold.
  for (i in seq_len(nrow(plausibility_low_rules))) {

    rule <- plausibility_low_rules[i, ]

    plausible_min <- rule$plausible_min_value[[1]]

    if (!is.na(plausible_min) && numeric_value < plausible_min) {

      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = rule,
          issue_description = glue(
            "{variable_name} = {numeric_value} is below the plausible minimum value ({plausible_min})."
          ),
          fallback_variable_name = variable_name,
          fallback_issue_type = "implausible_value",
          fallback_severity = "WARNING"
        )
      )
    }
  }

  # ----------------------------------------------------------
  # Plausibility high validation
  # ----------------------------------------------------------

  # Retrieve plausibility-high rules for this variable.
  plausibility_high_rules <- get_rules_by_variable_and_type(
    governance,
    variable_name,
    "plausibility_high"
  )

  # Check whether the value is above the clinically plausible upper threshold.
  for (i in seq_len(nrow(plausibility_high_rules))) {

    rule <- plausibility_high_rules[i, ]

    plausible_max <- rule$plausible_max_value[[1]]

    if (!is.na(plausible_max) && numeric_value > plausible_max) {

      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = rule,
          issue_description = glue(
            "{variable_name} = {numeric_value} is above the plausible maximum value ({plausible_max})."
          ),
          fallback_variable_name = variable_name,
          fallback_issue_type = "implausible_value",
          fallback_severity = "WARNING"
        )
      )
    }
  }

  return(flags)
}

# ------------------------------------------------------------
# Date of Birth Validation
# ------------------------------------------------------------

# This function validates date_of_birth.
# It checks whether:
# - a non-missing input can be parsed as a valid date
# - the date is not in the future
validate_date_of_birth <- function(raw_record,
                                   cleaned_record,
                                   governance) {

  flags <- empty_flags()

  raw_dob <- raw_record[["date_of_birth"]]
  dob <- cleaned_record[["date_of_birth"]]

  # Missing DOB is handled by required-field validation.
  if (is_missing_value(raw_dob)) {
    return(flags)
  }

  # If DOB was provided but could not be parsed into a valid date,
  # create a CRITICAL flag.
  if (is.na(dob)) {

    rule <- get_rule_by_name(governance, "date_of_birth_future_date")

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "date_of_birth is not a valid date. Received: '{as.character(raw_dob[[1]])}'."
        ),
        fallback_variable_name = "date_of_birth",
        fallback_issue_type = "invalid_date",
        fallback_severity = "CRITICAL"
      )
    )

    return(flags)
  }

  # Get the current system date for future-date comparison.
  today <- Sys.Date()

  # A date of birth cannot logically be after today.
  if (dob > today) {

    rule <- get_rule_by_name(governance, "date_of_birth_future_date")

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "date_of_birth cannot be in the future. Received: {dob}."
        ),
        fallback_variable_name = "date_of_birth",
        fallback_issue_type = "future_date",
        fallback_severity = "CRITICAL"
      )
    )
  }

  return(flags)
}

# ------------------------------------------------------------
# Date of Birth and Age Consistency Validation
# ------------------------------------------------------------

# This function checks whether the recorded age is consistent with date_of_birth.
# It calculates age from DOB and compares it to the age field.
#
# A difference greater than 2 years is flagged as a WARNING.
# This tolerance avoids false positives caused by incomplete dates,
# birthdays not yet reached in the current year, or approximate data entry.
validate_dob_age_consistency <- function(cleaned_record,
                                         governance) {

  flags <- empty_flags()

  dob <- cleaned_record[["date_of_birth"]]
  age <- cleaned_record[["age"]]

  # If DOB is missing or invalid, this check cannot be performed.
  if (is_missing_value(dob) || is.na(dob)) {
    return(flags)
  }

  # If age is missing or invalid, this check cannot be performed.
  if (is_missing_value(age) || is.na(age)) {
    return(flags)
  }

  # If DOB is in the future, this function skips consistency validation
  # because the future-date rule will already flag that problem.
  if (dob > Sys.Date()) {
    return(flags)
  }

  # Calculate the patient's age based on the interval from DOB to today's date.
  calculated_age <- as.integer(floor(time_length(interval(dob, Sys.Date()), "years")))

  if (is.na(calculated_age)) {
    return(flags)
  }

  # Compare calculated age with recorded age.
  age_difference <- abs(calculated_age - age)

  # If the difference is greater than 2 years, create a warning.
  if (age_difference > 2) {

    rule <- get_rule_by_name(governance, "dob_age_consistency")

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "Recorded age ({age}) differs from age calculated from date_of_birth ({calculated_age}) by more than 2 years."
        ),
        fallback_variable_name = "date_of_birth",
        fallback_issue_type = "cross_field_inconsistency",
        fallback_severity = "WARNING"
      )
    )
  }

  return(flags)
}

# ------------------------------------------------------------
# Controlled Vocabulary Validation
# ------------------------------------------------------------
# Optional fields are only validated when present.
# Required fields are handled separately by validate_required_fields().
# ------------------------------------------------------------

# This function validates whether a field value belongs to its controlled vocabulary.
#
# It is used for variables such as:
# - sex
# - blood_type
# - smoker
#
# Optional fields are not flagged if missing; they are only validated when provided.
validate_controlled_vocabulary_field <- function(cleaned_record,
                                                 governance,
                                                 variable_name,
                                                 rule_name) {

  flags <- empty_flags()

  value <- cleaned_record[[variable_name]]

  # Missing values are handled separately if required.
  # Optional missing values do not generate controlled vocabulary errors.
  if (is_missing_value(value)) {
    return(flags)
  }

  # Retrieve the controlled vocabulary rule.
  rule <- get_rule_by_name(governance, rule_name)

  if (is.null(rule)) {
    return(flags)
  }

  # Get the vocabulary name linked to this validation rule.
  vocabulary_name <- rule$controlled_vocabulary_name[[1]]

  if (is.na(vocabulary_name) || vocabulary_name == "") {
    return(flags)
  }

  # Retrieve the allowed values for the vocabulary.
  allowed_values <- get_allowed_values(governance, vocabulary_name)

  # smoker is stored as BOOLEAN in the cleaned record,
  # but the controlled vocabulary uses the strings "TRUE" and "FALSE".
  # Therefore, smoker needs special conversion before comparison.
  if (variable_name == "smoker") {
    value_for_vocab <- boolean_to_vocab_value(value)
  } else {
    value_for_vocab <- as.character(value)
  }

  # If the value is not part of the allowed vocabulary,
  # create a CRITICAL controlled vocabulary failure.
  if (!value_for_vocab %in% allowed_values) {

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "{variable_name} = '{value_for_vocab}' is not in the allowed vocabulary: {paste(allowed_values, collapse = ', ')}."
        ),
        fallback_variable_name = variable_name,
        fallback_issue_type = "controlled_vocabulary_failure",
        fallback_severity = "CRITICAL"
      )
    )
  }

  return(flags)
}

# ------------------------------------------------------------
# Regex Field Validation
# ------------------------------------------------------------

# This function validates a field using a regular expression stored in validation_rules.
# It is currently used for diagnosis_code.
validate_regex_field <- function(cleaned_record,
                                 governance,
                                 variable_name,
                                 rule_name) {

  flags <- empty_flags()

  value <- cleaned_record[[variable_name]]

  # Missing values are ignored here because this function validates format,
  # not requiredness.
  if (is_missing_value(value)) {
    return(flags)
  }

  # Retrieve the regex validation rule by name.
  rule <- get_rule_by_name(governance, rule_name)

  if (is.null(rule)) {
    return(flags)
  }

  # Extract the regex pattern from the rule.
  regex_pattern <- rule$regex_pattern[[1]]

  if (is.na(regex_pattern) || regex_pattern == "") {
    return(flags)
  }

  # If the value does not match the expected format,
  # create a WARNING flag.
  if (!str_detect(as.character(value), regex_pattern)) {

    flags <- bind_rows(
      flags,
      make_validation_flag(
        rule = rule,
        issue_description = glue(
          "{variable_name} = '{value}' does not match the expected format."
        ),
        fallback_variable_name = variable_name,
        fallback_issue_type = "format_failure",
        fallback_severity = "WARNING"
      )
    )
  }

  return(flags)
}

# ------------------------------------------------------------
# BMI Calculation
# ------------------------------------------------------------

# This function calculates Body Mass Index from weight and height.
#
# Formula:
# BMI = weight_kg / height_m^2
#
# The function returns NA if weight or height is missing,
# if conversion to numeric fails, or if height is not positive.
calculate_bmi <- function(weight_kg, height_cm) {

  if (is_missing_value(weight_kg) || is_missing_value(height_cm)) {
    return(NA_real_)
  }

  weight <- as_numeric_safe(weight_kg)
  height <- as_numeric_safe(height_cm)

  if (is.na(weight) || is.na(height)) {
    return(NA_real_)
  }

  if (height <= 0) {
    return(NA_real_)
  }

  # Convert height from centimeters to meters before applying the BMI formula.
  height_m <- height / 100

  # Calculate BMI.
  bmi <- weight / (height_m^2)

  # Round BMI to two decimal places for readability.
  round(bmi, 2)
}

# ------------------------------------------------------------
# BMI Validation
# ------------------------------------------------------------
# BMI is derived and is not physically stored in public.patients.
# It generates WARNING flags only.
# ------------------------------------------------------------

# This function validates the calculated BMI against plausibility thresholds.
# Since BMI is derived from weight and height, it is not a direct input field.
#
# It creates warnings for extremely low or high BMI values.
validate_bmi <- function(bmi,
                         governance) {

  flags <- empty_flags()

  # If BMI cannot be calculated, no BMI validation is performed.
  if (is_missing_value(bmi) || is.na(bmi)) {
    return(flags)
  }

  # Retrieve the low-BMI warning rule.
  low_rule <- get_rule_by_name(governance, "bmi_low_warning")

  if (!is.null(low_rule)) {

    plausible_min <- low_rule$plausible_min_value[[1]]

    # If BMI is below the plausible minimum, create a WARNING flag.
    if (!is.na(plausible_min) && bmi < plausible_min) {

      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = low_rule,
          issue_description = glue(
            "Derived BMI = {bmi} is below the plausible minimum value ({plausible_min})."
          ),
          fallback_variable_name = "bmi",
          fallback_issue_type = "implausible_value",
          fallback_severity = "WARNING"
        )
      )
    }
  }

  # Retrieve the high-BMI warning rule.
  high_rule <- get_rule_by_name(governance, "bmi_high_warning")

  if (!is.null(high_rule)) {

    plausible_max <- high_rule$plausible_max_value[[1]]

    # If BMI is above the plausible maximum, create a WARNING flag.
    if (!is.na(plausible_max) && bmi > plausible_max) {

      flags <- bind_rows(
        flags,
        make_validation_flag(
          rule = high_rule,
          issue_description = glue(
            "Derived BMI = {bmi} is above the plausible maximum value ({plausible_max})."
          ),
          fallback_variable_name = "bmi",
          fallback_issue_type = "implausible_value",
          fallback_severity = "WARNING"
        )
      )
    }
  }

  return(flags)
}

# ------------------------------------------------------------
# Redundant Flag Reduction
# ------------------------------------------------------------
# If a variable already has a CRITICAL error, WARNING flags for
# that same variable are removed. This keeps the quality_flags
# table clinically readable and avoids redundant warnings.
# ------------------------------------------------------------

# This function simplifies the final list of flags.
# If a variable already has a CRITICAL issue, WARNING messages for that same variable
# are removed because the critical issue is already the most important problem.
remove_redundant_warnings <- function(flags) {

  if (nrow(flags) == 0) {
    return(flags)
  }

  # Identify all variables that have at least one CRITICAL flag.
  critical_variables <- flags %>%
    filter(.data$severity == "CRITICAL") %>%
    pull(.data$variable_name) %>%
    unique()

  # Remove WARNING flags for variables that already have CRITICAL flags.
  filtered_flags <- flags %>%
    filter(
      !(
        .data$severity == "WARNING" &
          .data$variable_name %in% critical_variables
      )
    )

  return(filtered_flags)
}

# ------------------------------------------------------------
# Build Final Validation Result
# ------------------------------------------------------------

# This function converts the accumulated validation flags into the final output structure.
#
# The returned list contains:
# - valid: TRUE if there are no CRITICAL flags
# - overall_severity: highest severity found
# - issues: descriptions of CRITICAL issues
# - warnings: descriptions of WARNING issues
# - flags: complete structured flags table
# - bmi: calculated BMI
# - cleaned_record: normalized patient record
build_validation_result <- function(flags,
                                    bmi,
                                    cleaned_record) {

  # Remove redundant warnings before producing the final result.
  flags <- remove_redundant_warnings(flags)

  # If no flags were detected, the record is valid and severity is INFO.
  if (nrow(flags) == 0) {

    return(list(
      valid = TRUE,
      overall_severity = "INFO",
      issues = character(),
      warnings = character(),
      flags = empty_flags(),
      bmi = bmi,
      cleaned_record = cleaned_record
    ))
  }

  # Determine the highest severity across all remaining flags.
  overall_severity <- max_severity(flags$severity)

  # Extract CRITICAL issue descriptions.
  issues <- flags %>%
    filter(.data$severity == "CRITICAL") %>%
    pull(.data$issue_description)

  # Extract WARNING issue descriptions.
  warnings <- flags %>%
    filter(.data$severity == "WARNING") %>%
    pull(.data$issue_description)

  # A record is considered valid only if it has no CRITICAL flags.
  list(
    valid = !any(flags$severity == "CRITICAL"),
    overall_severity = overall_severity,
    issues = issues,
    warnings = warnings,
    flags = flags,
    bmi = bmi,
    cleaned_record = cleaned_record
  )
}

# ------------------------------------------------------------
# Main Validation Function
# ------------------------------------------------------------
# This is the main function used by 07_insert_patient_pipeline.R.
#
# INPUT:
# record: named list or one-row data.frame containing patient data.
#
# OUTPUT:
# list(
#   valid,
#   overall_severity,
#   issues,
#   warnings,
#   flags,
#   bmi,
#   cleaned_record
# )
# ------------------------------------------------------------

# This is the main entry point of the validation engine.
# It receives one patient record, loads governance rules if needed,
# cleans the record, applies all validation checks, calculates BMI,
# and returns a structured validation result.
validate_patient_record <- function(record,
                                    governance = NULL,
                                    conn = NULL) {

  # This tracks whether validate_patient_record() opened its own database connection.
  # If TRUE, the connection will be closed in the finally block.
  local_connection <- FALSE

  tryCatch({

    # If the input is a data frame, it must contain exactly one row.
    # This prevents accidentally validating multiple patients as if they were one record.
    if (is.data.frame(record)) {

      if (nrow(record) != 1) {
        stop("validate_patient_record() expects a named list or a one-row data.frame.")
      }

      # Convert the one-row data frame into a named list,
      # which is the internal format expected by the validation functions.
      record <- as.list(record[1, ])
    }

    # If the input is not a list after this point, the function cannot validate it.
    if (!is.list(record)) {
      stop("validate_patient_record() expects a named list or a one-row data.frame.")
    }

    # If governance context was not provided, load it from the database.
    if (is.null(governance)) {

      # If no database connection was provided either, open a local connection.
      if (is.null(conn)) {
        conn <- db_connection()
        local_connection <- TRUE
      }

      # Load metadata, vocabularies, and active validation rules.
      governance <- load_governance_context(conn)
    }

    # Normalize the raw input record before validation.
    cleaned_record <- clean_patient_record(record)

    # Calculate BMI from cleaned weight and height values.
    # If weight or height is missing or invalid, BMI will be NA.
    bmi <- calculate_bmi(
      cleaned_record$weight_kg,
      cleaned_record$height_cm
    )

    # Initialize an empty table where validation flags will be accumulated.
    flags <- empty_flags()

    # --------------------------------------------------------
    # Required fields
    # --------------------------------------------------------

    # Check that all manually required fields are present.
    flags <- bind_rows(
      flags,
      validate_required_fields(cleaned_record, governance)
    )

    # --------------------------------------------------------
    # patient_id
    # --------------------------------------------------------

    # Check whether patient_id follows the expected P-XXXX format.
    flags <- bind_rows(
      flags,
      validate_patient_id_format(cleaned_record, governance)
    )

    # --------------------------------------------------------
    # age
    # --------------------------------------------------------

    # Check that age was entered as an integer-like value.
    flags <- bind_rows(
      flags,
      validate_integer_field(
        raw_record = record,
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "age",
        rule_name = "age_integer_validation"
      )
    )

    # Check age hard range and plausibility thresholds.
    flags <- bind_rows(
      flags,
      validate_numeric_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "age"
      )
    )

    # --------------------------------------------------------
    # date_of_birth and DOB-age coherence
    # --------------------------------------------------------

    # Validate that date_of_birth is a real date and is not in the future.
    flags <- bind_rows(
      flags,
      validate_date_of_birth(
        raw_record = record,
        cleaned_record = cleaned_record,
        governance = governance
      )
    )

    # Check whether date_of_birth and recorded age are coherent.
    flags <- bind_rows(
      flags,
      validate_dob_age_consistency(
        cleaned_record = cleaned_record,
        governance = governance
      )
    )

    # --------------------------------------------------------
    # controlled vocabularies
    # --------------------------------------------------------

    # Validate sex against the sex controlled vocabulary.
    flags <- bind_rows(
      flags,
      validate_controlled_vocabulary_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "sex",
        rule_name = "sex_controlled_vocabulary"
      )
    )

    # Validate blood_type against the blood type controlled vocabulary.
    flags <- bind_rows(
      flags,
      validate_controlled_vocabulary_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "blood_type",
        rule_name = "blood_type_controlled_vocabulary"
      )
    )

    # Validate smoker against the smoker controlled vocabulary.
    flags <- bind_rows(
      flags,
      validate_controlled_vocabulary_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "smoker",
        rule_name = "smoker_controlled_vocabulary"
      )
    )

    # --------------------------------------------------------
    # diagnosis_code
    # --------------------------------------------------------

    # Validate diagnosis_code format using the regex rule.
    flags <- bind_rows(
      flags,
      validate_regex_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "diagnosis_code",
        rule_name = "diagnosis_code_regex"
      )
    )

    # --------------------------------------------------------
    # weight_kg
    # --------------------------------------------------------

    # Validate weight hard range and plausibility thresholds.
    flags <- bind_rows(
      flags,
      validate_numeric_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "weight_kg"
      )
    )

    # --------------------------------------------------------
    # height_cm
    # --------------------------------------------------------

    # Validate height hard range and plausibility thresholds.
    flags <- bind_rows(
      flags,
      validate_numeric_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "height_cm"
      )
    )

    # --------------------------------------------------------
    # dosage_mg
    # --------------------------------------------------------

    # Check that dosage_mg was entered as an integer-like value.
    flags <- bind_rows(
      flags,
      validate_integer_field(
        raw_record = record,
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "dosage_mg",
        rule_name = "dosage_integer_validation"
      )
    )

    # Validate dosage hard range and plausibility thresholds.
    flags <- bind_rows(
      flags,
      validate_numeric_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "dosage_mg"
      )
    )

    # --------------------------------------------------------
    # BMI
    # --------------------------------------------------------

    # Validate the derived BMI value.
    # BMI only generates warnings because it is calculated from other measurements.
    flags <- bind_rows(
      flags,
      validate_bmi(
        bmi = bmi,
        governance = governance
      )
    )

    # --------------------------------------------------------
    # Final structured response
    # --------------------------------------------------------

    # Build the final result object with validity status, severity,
    # issues, warnings, flags, BMI, and cleaned record.
    result <- build_validation_result(
      flags = flags,
      bmi = bmi,
      cleaned_record = cleaned_record
    )

    return(result)

  }, error = function(e) {

    # If the validation engine itself fails unexpectedly,
    # return a structured CRITICAL result instead of crashing without context.
    detected_by_user <- Sys.info()[["user"]]

    if (is.null(detected_by_user) || is.na(detected_by_user) || detected_by_user == "") {
      detected_by_user <- "unknown_user"
    }

    # Create a synthetic error flag representing a failure of the validation engine itself.
    error_flags <- tibble(
      rule_id = NA_character_,
      rule_name = NA_character_,
      variable_name = "validation_engine",
      issue_type = "validation_engine_failure",
      severity = "CRITICAL",
      issue_description = glue("Validation engine failed: {e$message}"),
      detected_by_user = detected_by_user
    )

    # Return the same structure as normal validation results,
    # but marked as invalid because the engine failed.
    return(list(
      valid = FALSE,
      overall_severity = "CRITICAL",
      issues = error_flags$issue_description,
      warnings = character(),
      flags = error_flags,
      bmi = NA_real_,
      cleaned_record = record
    ))

  }, finally = {

    # If this function opened its own database connection, close it here.
    # This prevents leaving open connections after validation finishes.
    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in validate_patient_record().")
      })
    }
  })
}