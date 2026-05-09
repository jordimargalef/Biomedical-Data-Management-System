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
# Safe Query Helper
# ------------------------------------------------------------

db_get_query_safe <- function(conn, sql_query, description = "Database query") {

  tryCatch({

    result <- dbGetQuery(conn, sql_query)

    return(result)

  }, error = function(e) {

    message(glue("ERROR during {description}: {e$message}"))

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

load_governance_context <- function(conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.null(conn)) {
      conn <- db_connection()
      local_connection <- TRUE
    }

    existing_tables <- dbListTables(conn)

    required_tables <- c(
      "metadata_table",
      "vocabulary_registry",
      "controlled_vocabularies",
      "validation_rules"
    )

    missing_tables <- setdiff(required_tables, existing_tables)

    if (length(missing_tables) > 0) {

      stop(glue(
        "Missing governance tables: {paste(missing_tables, collapse = ', ')}. ",
        "Run source('04_metadata_setup.R') before using the validation engine."
      ))
    }

    metadata_table <- db_get_query_safe(
      conn,
      "
      SELECT *
      FROM public.metadata_table
      ORDER BY table_name, variable_name;
      ",
      "Loading metadata_table"
    )

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

    list(
      metadata_table = metadata_table,
      vocabulary_registry = vocabulary_registry,
      controlled_vocabularies = controlled_vocabularies,
      validation_rules = validation_rules
    )

  }, error = function(e) {

    stop(glue("Unable to load governance context: {e$message}"))

  }, finally = {

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

as_numeric_safe <- function(x) {

  if (is_missing_value(x)) {
    return(NA_real_)
  }

  suppressWarnings(as.numeric(x[[1]]))
}

as_integer_safe <- function(x) {

  numeric_value <- as_numeric_safe(x)

  if (is.na(numeric_value)) {
    return(NA_integer_)
  }

  if (abs(numeric_value - round(numeric_value)) > .Machine$double.eps^0.5) {
    return(NA_integer_)
  }

  as.integer(round(numeric_value))
}

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

  parsed_date <- suppressWarnings(as.Date(value))

  if (!is.na(parsed_date)) {
    return(parsed_date)
  }

  parsed_date <- suppressWarnings(lubridate::ymd(value))

  if (!is.na(parsed_date)) {
    return(as.Date(parsed_date))
  }

  parsed_date <- suppressWarnings(lubridate::dmy(value))

  if (!is.na(parsed_date)) {
    return(as.Date(parsed_date))
  }

  parsed_date <- suppressWarnings(lubridate::mdy(value))

  if (!is.na(parsed_date)) {
    return(as.Date(parsed_date))
  }

  return(as.Date(NA))
}

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

is_integer_like <- function(x) {

  numeric_value <- as_numeric_safe(x)

  if (is.na(numeric_value)) {
    return(FALSE)
  }

  abs(numeric_value - round(numeric_value)) <= .Machine$double.eps^0.5
}

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

get_rules_by_variable <- function(governance, variable_name) {

  governance$validation_rules %>%
    filter(.data$variable_name == !!variable_name) %>%
    filter(.data$active == TRUE)
}

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

get_rules_by_variable_and_type <- function(governance,
                                           variable_name,
                                           validation_type) {

  governance$validation_rules %>%
    filter(.data$variable_name == !!variable_name) %>%
    filter(.data$validation_type == !!validation_type) %>%
    filter(.data$active == TRUE)
}

get_allowed_values <- function(governance, vocabulary_name) {

  governance$controlled_vocabularies %>%
    filter(.data$vocabulary_name == !!vocabulary_name) %>%
    filter(.data$active == TRUE) %>%
    pull(.data$allowed_value)
}

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

make_validation_flag <- function(rule,
                                 issue_description,
                                 fallback_variable_name = NA_character_,
                                 fallback_issue_type = "validation_failure",
                                 fallback_severity = "CRITICAL") {

  detected_by_user <- Sys.info()[["user"]]

  if (is.null(detected_by_user) || is.na(detected_by_user) || detected_by_user == "") {
    detected_by_user <- "unknown_user"
  }

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

clean_patient_record <- function(record) {

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

  if (!is.na(cleaned_record$sex)) {
    cleaned_record$sex <- str_to_upper(cleaned_record$sex)
  }

  if (!is.na(cleaned_record$blood_type)) {
    cleaned_record$blood_type <- str_to_upper(cleaned_record$blood_type)
  }

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

validate_required_fields <- function(record, governance) {

  required_fields <- governance$metadata_table %>%
    filter(.data$is_required == TRUE) %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  flags <- empty_flags()

  for (field in required_fields) {

    if (is_missing_value(record[[field]])) {

      rule <- get_rule_by_name(
        governance,
        glue("required_{field}")
      )

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

validate_patient_id_format <- function(record, governance) {

  flags <- empty_flags()

  patient_id <- record[["patient_id"]]

  if (is_missing_value(patient_id)) {
    return(flags)
  }

  rule <- get_rule_by_name(governance, "patient_id_format")

  if (is.null(rule)) {
    return(flags)
  }

  regex_pattern <- rule$regex_pattern[[1]]

  if (is.na(regex_pattern) || regex_pattern == "") {
    return(flags)
  }

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

validate_integer_field <- function(raw_record,
                                   cleaned_record,
                                   governance,
                                   variable_name,
                                   rule_name) {

  flags <- empty_flags()

  raw_value <- raw_record[[variable_name]]

  if (is_missing_value(raw_value)) {
    return(flags)
  }

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

validate_numeric_field <- function(cleaned_record,
                                   governance,
                                   variable_name) {

  flags <- empty_flags()

  value <- cleaned_record[[variable_name]]

  if (is_missing_value(value)) {
    return(flags)
  }

  numeric_value <- as_numeric_safe(value)

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

  hard_rules <- get_rules_by_variable_and_type(
    governance,
    variable_name,
    "range"
  )

  for (i in seq_len(nrow(hard_rules))) {

    rule <- hard_rules[i, ]

    hard_min <- rule$hard_min_value[[1]]
    hard_max <- rule$hard_max_value[[1]]

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

  if (any(flags$severity == "CRITICAL")) {
    return(flags)
  }

  # ----------------------------------------------------------
  # Plausibility low validation
  # ----------------------------------------------------------

  plausibility_low_rules <- get_rules_by_variable_and_type(
    governance,
    variable_name,
    "plausibility_low"
  )

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

  plausibility_high_rules <- get_rules_by_variable_and_type(
    governance,
    variable_name,
    "plausibility_high"
  )

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

validate_date_of_birth <- function(raw_record,
                                   cleaned_record,
                                   governance) {

  flags <- empty_flags()

  raw_dob <- raw_record[["date_of_birth"]]
  dob <- cleaned_record[["date_of_birth"]]

  if (is_missing_value(raw_dob)) {
    return(flags)
  }

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

  today <- Sys.Date()

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

validate_dob_age_consistency <- function(cleaned_record,
                                         governance) {

  flags <- empty_flags()

  dob <- cleaned_record[["date_of_birth"]]
  age <- cleaned_record[["age"]]

  if (is_missing_value(dob) || is.na(dob)) {
    return(flags)
  }

  if (is_missing_value(age) || is.na(age)) {
    return(flags)
  }

  if (dob > Sys.Date()) {
    return(flags)
  }

  calculated_age <- as.integer(floor(time_length(interval(dob, Sys.Date()), "years")))

  if (is.na(calculated_age)) {
    return(flags)
  }

  age_difference <- abs(calculated_age - age)

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

validate_controlled_vocabulary_field <- function(cleaned_record,
                                                 governance,
                                                 variable_name,
                                                 rule_name) {

  flags <- empty_flags()

  value <- cleaned_record[[variable_name]]

  if (is_missing_value(value)) {
    return(flags)
  }

  rule <- get_rule_by_name(governance, rule_name)

  if (is.null(rule)) {
    return(flags)
  }

  vocabulary_name <- rule$controlled_vocabulary_name[[1]]

  if (is.na(vocabulary_name) || vocabulary_name == "") {
    return(flags)
  }

  allowed_values <- get_allowed_values(governance, vocabulary_name)

  if (variable_name == "smoker") {
    value_for_vocab <- boolean_to_vocab_value(value)
  } else {
    value_for_vocab <- as.character(value)
  }

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

validate_regex_field <- function(cleaned_record,
                                 governance,
                                 variable_name,
                                 rule_name) {

  flags <- empty_flags()

  value <- cleaned_record[[variable_name]]

  if (is_missing_value(value)) {
    return(flags)
  }

  rule <- get_rule_by_name(governance, rule_name)

  if (is.null(rule)) {
    return(flags)
  }

  regex_pattern <- rule$regex_pattern[[1]]

  if (is.na(regex_pattern) || regex_pattern == "") {
    return(flags)
  }

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

  height_m <- height / 100

  bmi <- weight / (height_m^2)

  round(bmi, 2)
}

# ------------------------------------------------------------
# BMI Validation
# ------------------------------------------------------------
# BMI is derived and is not physically stored in public.patients.
# It generates WARNING flags only.
# ------------------------------------------------------------

validate_bmi <- function(bmi,
                         governance) {

  flags <- empty_flags()

  if (is_missing_value(bmi) || is.na(bmi)) {
    return(flags)
  }

  low_rule <- get_rule_by_name(governance, "bmi_low_warning")

  if (!is.null(low_rule)) {

    plausible_min <- low_rule$plausible_min_value[[1]]

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

  high_rule <- get_rule_by_name(governance, "bmi_high_warning")

  if (!is.null(high_rule)) {

    plausible_max <- high_rule$plausible_max_value[[1]]

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

remove_redundant_warnings <- function(flags) {

  if (nrow(flags) == 0) {
    return(flags)
  }

  critical_variables <- flags %>%
    filter(.data$severity == "CRITICAL") %>%
    pull(.data$variable_name) %>%
    unique()

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

build_validation_result <- function(flags,
                                    bmi,
                                    cleaned_record) {

  flags <- remove_redundant_warnings(flags)

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

  overall_severity <- max_severity(flags$severity)

  issues <- flags %>%
    filter(.data$severity == "CRITICAL") %>%
    pull(.data$issue_description)

  warnings <- flags %>%
    filter(.data$severity == "WARNING") %>%
    pull(.data$issue_description)

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

validate_patient_record <- function(record,
                                    governance = NULL,
                                    conn = NULL) {

  local_connection <- FALSE

  tryCatch({

    if (is.data.frame(record)) {

      if (nrow(record) != 1) {
        stop("validate_patient_record() expects a named list or a one-row data.frame.")
      }

      record <- as.list(record[1, ])
    }

    if (!is.list(record)) {
      stop("validate_patient_record() expects a named list or a one-row data.frame.")
    }

    if (is.null(governance)) {

      if (is.null(conn)) {
        conn <- db_connection()
        local_connection <- TRUE
      }

      governance <- load_governance_context(conn)
    }

    cleaned_record <- clean_patient_record(record)

    bmi <- calculate_bmi(
      cleaned_record$weight_kg,
      cleaned_record$height_cm
    )

    flags <- empty_flags()

    # --------------------------------------------------------
    # Required fields
    # --------------------------------------------------------

    flags <- bind_rows(
      flags,
      validate_required_fields(cleaned_record, governance)
    )

    # --------------------------------------------------------
    # patient_id
    # --------------------------------------------------------

    flags <- bind_rows(
      flags,
      validate_patient_id_format(cleaned_record, governance)
    )

    # --------------------------------------------------------
    # age
    # --------------------------------------------------------

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

    flags <- bind_rows(
      flags,
      validate_date_of_birth(
        raw_record = record,
        cleaned_record = cleaned_record,
        governance = governance
      )
    )

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

    flags <- bind_rows(
      flags,
      validate_controlled_vocabulary_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "sex",
        rule_name = "sex_controlled_vocabulary"
      )
    )

    flags <- bind_rows(
      flags,
      validate_controlled_vocabulary_field(
        cleaned_record = cleaned_record,
        governance = governance,
        variable_name = "blood_type",
        rule_name = "blood_type_controlled_vocabulary"
      )
    )

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

    result <- build_validation_result(
      flags = flags,
      bmi = bmi,
      cleaned_record = cleaned_record
    )

    return(result)

  }, error = function(e) {

    detected_by_user <- Sys.info()[["user"]]

    if (is.null(detected_by_user) || is.na(detected_by_user) || detected_by_user == "") {
      detected_by_user <- "unknown_user"
    }

    error_flags <- tibble(
      rule_id = NA_character_,
      rule_name = NA_character_,
      variable_name = "validation_engine",
      issue_type = "validation_engine_failure",
      severity = "CRITICAL",
      issue_description = glue("Validation engine failed: {e$message}"),
      detected_by_user = detected_by_user
    )

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

    if (local_connection && !is.null(conn)) {

      tryCatch({

        dbDisconnect(conn)

      }, error = function(e) {

        message("Connection closing warning in validate_patient_record().")
      })
    }
  })
}