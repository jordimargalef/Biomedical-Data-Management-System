# ============================================================
# 09_quality_metrics_backend.R
# Biomedical Data Quality Metrics Backend
# PostgreSQL + R
# ============================================================
#
# PURPOSE:
# Backend-only script for biomedical dashboard metrics.
#
# This script calculates:
# - Completeness metrics: null values and populated field ratios.
# - Consistency metrics: internal coherence between variables,
#   controlled vocabulary conformity, DOB-age coherence, and BMI
#   plausibility.
# - Accuracy proxy metrics: true clinical accuracy requires an
#   external gold standard. This system operationalizes proxy
#   accuracy through validation-rule failures, clinical plausibility
#   checks, and statistical outlier detection.
# - Advanced metrics: flag burden, problematic variables, hot/cold
#   data ratio, record confidence score, global quality score.
#
# IMPORTANT:
# - This script defines functions only.
# - This script does not modify public.patients.
# - This script does not insert, update, or delete data.
# - This script does not generate plots.
# - All functions return tibbles/data.frames or lists of tibbles.
#
# ============================================================

# ------------------------------------------------------------
# Load Required Libraries
# ------------------------------------------------------------

# DBI provides the standard database interface used in R.
# It allows the script to connect to PostgreSQL, run queries, and retrieve results.
library(DBI)

# RPostgres is the PostgreSQL-specific driver used together with DBI.
# This is what makes the connection to the PostgreSQL database possible.
library(RPostgres)

# tidyverse provides data manipulation tools such as filter(), mutate(),
# summarise(), group_by(), count(), joins, pipes, and tibble creation.
library(tidyverse)

# stringr provides string utilities.
# It is used here mainly for trimming empty character values and detecting patterns.
library(stringr)

# lubridate provides date/time utilities.
# It is used to calculate ages from date_of_birth and to define hot/cold data thresholds.
library(lubridate)

# uuid is loaded for consistency with the rest of the project package set.
# This backend mainly reads existing UUIDs rather than generating new ones.
library(uuid)

# glue allows readable string interpolation.
# It is used to create dynamic error messages.
library(glue)

# janitor provides data-cleaning utilities.
# It is loaded as part of the common biomedical data-management environment.
library(janitor)

# ------------------------------------------------------------
# Database Connection
# ------------------------------------------------------------

# This function creates a connection to the PostgreSQL database.
# It is similar to the db_connection() function used in the previous scripts,
# but here it is named connect_db() because this file acts as a backend module.
connect_db <- function() {

  # dbConnect() opens a PostgreSQL connection.
  # User credentials are read from environment variables instead of being hardcoded.
  conn <- dbConnect(
    RPostgres::Postgres(),
    dbname   = "biomedical_db",
    host     = "localhost",
    port     = 5432,
    user     = Sys.getenv("PGUSER"),
    password = Sys.getenv("PGPASSWORD")
  )

  # Return the active database connection.
  return(conn)
}

# Backwards-compatible alias, in case previous scripts expect this name.
# This means code that calls db_connection() will still work.
db_connection <- connect_db

# ------------------------------------------------------------
# Safe Query Helpers
# ------------------------------------------------------------

# This function checks whether a table exists in the database.
#
# Parameters:
# - con: active database connection
# - table_name: name of the table to check
# - schema_name: database schema, defaulting to public
table_exists <- function(con, table_name, schema_name = "public") {

  # Query information_schema.tables, which stores metadata about database tables.
  result <- dbGetQuery(
    con,
    "
    SELECT COUNT(*) AS n
    FROM information_schema.tables
    WHERE table_schema = $1
      AND table_name = $2;
    ",
    params = list(schema_name, table_name)
  )

  # If count is greater than 0, the table exists.
  return(result$n[[1]] > 0)
}

# This function safely reads a full table from the database.
#
# It supports both required and optional tables:
# - If required = TRUE and the table is missing, the function stops.
# - If required = FALSE and the table is missing, the function returns an empty tibble.
safe_read_table <- function(con,
                            table_name,
                            schema_name = "public",
                            required = FALSE) {

  tryCatch({

    # First, verify whether the table exists before trying to query it.
    if (!table_exists(con, table_name, schema_name)) {

      # Required tables are essential for the dashboard, so missing ones stop execution.
      if (isTRUE(required)) {
        stop(glue("Required table public.{table_name} does not exist."))
      }

      # Optional missing tables return an empty tibble so downstream functions can continue.
      return(tibble())
    }

    # Build a simple SELECT query to read the full table.
    query <- glue("SELECT * FROM {schema_name}.{table_name};")

    # Read the table and convert it to a tibble for tidyverse compatibility.
    result <- dbGetQuery(con, query) %>%
      as_tibble()

    return(result)

  }, error = function(e) {

    # If a required table fails to load, stop because the metrics cannot be trusted.
    if (isTRUE(required)) {
      stop(glue("Failed to read required table public.{table_name}: {e$message}"))
    }

    # If an optional table fails, show a message and return an empty tibble.
    message(glue("Optional table public.{table_name} could not be loaded: {e$message}"))

    return(tibble())
  })
}

# ------------------------------------------------------------
# Load Dashboard Context
# ------------------------------------------------------------

# This function loads all database tables needed by the metrics backend.
#
# Instead of each metric function reading directly from the database,
# all relevant tables are loaded once into a context list.
#
# This makes the metric functions easier to test and faster to run,
# because they operate on in-memory tibbles.
load_dashboard_context <- function(con) {

  tryCatch({

    # Load the main patients table.
    # This is required because almost all metrics are based on patient records.
    patients <- safe_read_table(
      con = con,
      table_name = "patients",
      required = TRUE
    )

    # Load metadata definitions.
    # This is required to know which fields should be evaluated,
    # which ones are required, optional, system-generated, or derived.
    metadata_table <- safe_read_table(
      con = con,
      table_name = "metadata_table",
      required = TRUE
    )

    # Load validation rules.
    # This is required for the governance context and for interpreting quality dimensions.
    validation_rules <- safe_read_table(
      con = con,
      table_name = "validation_rules",
      required = TRUE
    )

    # Load controlled vocabularies if available.
    # This is optional because some metrics can still run without it.
    controlled_vocabularies <- safe_read_table(
      con = con,
      table_name = "controlled_vocabularies",
      required = FALSE
    )

    # Load quality flags if available.
    # If no flags exist yet, the dashboard can still calculate basic metrics.
    quality_flags <- safe_read_table(
      con = con,
      table_name = "quality_flags",
      required = FALSE
    )

    # Load rejected submissions if available.
    # This is optional because a clean database may not have any rejected records.
    rejected_patient_submissions <- safe_read_table(
      con = con,
      table_name = "rejected_patient_submissions",
      required = FALSE
    )

    # Return all loaded tables as one context object.
    return(list(
      patients = patients,
      metadata_table = metadata_table,
      validation_rules = validation_rules,
      controlled_vocabularies = controlled_vocabularies,
      quality_flags = quality_flags,
      rejected_patient_submissions = rejected_patient_submissions
    ))

  }, error = function(e) {

    # If loading the dashboard context fails, stop with a clear message.
    stop(glue("Failed to load dashboard context: {e$message}"))
  })
}

# ------------------------------------------------------------
# Generic Helpers
# ------------------------------------------------------------

# This helper checks whether a value should be treated as missing.
# It handles NULL, empty vectors, NA values, and empty strings.
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

# This helper performs division safely.
#
# It prevents invalid results when:
# - denominator is zero
# - denominator is NA
# - the result is Inf or NaN
#
# In those cases, it returns NA_real_ instead of an invalid numeric value.
safe_divide <- function(numerator, denominator) {

  numerator <- as.numeric(numerator)
  denominator <- as.numeric(denominator)

  result <- numerator / denominator

  result[
    is.na(denominator) |
      denominator == 0 |
      is.infinite(result) |
      is.nan(result)
  ] <- NA_real_

  return(result)
}

# This helper converts values to numeric while suppressing warnings.
# It is useful when database columns may arrive as character or mixed types.
as_numeric_safe <- function(x) {

  suppressWarnings(as.numeric(x))
}

# This function calculates BMI for vectors of weight and height.
#
# Formula:
# BMI = weight_kg / height_m^2
#
# height_cm is converted to meters before calculation.
calculate_bmi_vector <- function(weight_kg, height_cm) {

  weight <- as_numeric_safe(weight_kg)
  height <- as_numeric_safe(height_cm)

  height_m <- height / 100

  bmi <- weight / (height_m ^ 2)

  # Invalid BMI values are set to NA:
  # - missing weight
  # - missing height
  # - height less than or equal to zero
  bmi[is.na(weight) | is.na(height) | height <= 0] <- NA_real_

  # Round BMI for readable dashboard output.
  round(bmi, 2)
}

# This function calculates dosage normalized by body weight.
#
# dose_per_kg = dosage_mg / weight_kg
#
# This is used as an accuracy/proxy outlier feature,
# not as a definitive clinical dosing safety assessment.
calculate_dose_per_kg_vector <- function(dosage_mg, weight_kg) {

  dosage <- as_numeric_safe(dosage_mg)
  weight <- as_numeric_safe(weight_kg)

  dose_per_kg <- dosage / weight

  # Invalid dose-per-kg values are set to NA.
  dose_per_kg[is.na(dosage) | is.na(weight) | weight <= 0] <- NA_real_

  # Round to four decimals because dose-per-kg can have smaller differences.
  round(dose_per_kg, 4)
}

# This function returns the patient fields that should be evaluated for quality metrics.
#
# It excludes:
# - system-generated variables, such as patient_uuid and created_at
# - derived variables, such as bmi
#
# It also keeps only fields that actually exist in the patients table.
get_evaluated_fields <- function(context) {

  fields <- context$metadata_table %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  fields <- fields[fields %in% names(context$patients)]

  return(fields)
}

# This function returns the required patient fields according to metadata_table.
# System-generated and derived fields are excluded because they are not manually entered.
get_required_fields <- function(context) {

  fields <- context$metadata_table %>%
    filter(.data$is_required == TRUE) %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  fields <- fields[fields %in% names(context$patients)]

  return(fields)
}

# This function returns optional patient fields according to metadata_table.
# It excludes system-generated and derived variables.
get_optional_fields <- function(context) {

  fields <- context$metadata_table %>%
    filter(.data$is_required == FALSE) %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  fields <- fields[fields %in% names(context$patients)]

  return(fields)
}

# This helper creates a simple one-row metric tibble.
# It is useful for returning metrics in a consistent format.
empty_metric <- function(metric_name, metric_value = NA_real_) {

  tibble(
    metric_name = metric_name,
    metric_value = metric_value
  )
}

# ============================================================
# A. COMPLETENESS METRICS
# ============================================================

# This metric returns the total number of patient records.
calculate_total_records <- function(context) {

  tibble(
    metric_name = "total_records",
    metric_value = nrow(context$patients)
  )
}

# This metric returns how many patient fields are included in completeness evaluation.
calculate_total_evaluated_fields <- function(context) {

  tibble(
    metric_name = "total_evaluated_fields",
    metric_value = length(get_evaluated_fields(context))
  )
}

# This metric counts all missing values across all evaluated patient fields.
calculate_total_missing_values <- function(context) {

  patients <- context$patients
  fields <- get_evaluated_fields(context)

  # If there are no patients or no evaluable fields, there are no missing values to count.
  if (nrow(patients) == 0 || length(fields) == 0) {
    return(tibble(metric_name = "total_missing_values", metric_value = 0))
  }

  # Select evaluated fields, count missing values per variable,
  # then sum them into one global missing-value count.
  missing_count <- patients %>%
    select(all_of(fields)) %>%
    summarise(across(everything(), ~sum(is.na(.) | (. == "" & is.character(.)), na.rm = TRUE))) %>%
    pivot_longer(everything(), names_to = "variable_name", values_to = "missing_count") %>%
    summarise(total_missing_values = sum(.data$missing_count)) %>%
    pull(.data$total_missing_values)

  tibble(
    metric_name = "total_missing_values",
    metric_value = missing_count
  )
}

# This metric calculates the global completeness percentage.
#
# Formula:
# completeness = 1 - total_missing / total_expected_values
calculate_overall_completeness <- function(context) {

  total_records <- nrow(context$patients)
  total_fields <- length(get_evaluated_fields(context))
  total_expected_values <- total_records * total_fields

  total_missing <- calculate_total_missing_values(context)$metric_value[[1]]

  completeness <- (1 - safe_divide(total_missing, total_expected_values)) * 100

  # If no expected values exist, completeness is undefined.
  if (is.na(completeness) && total_expected_values == 0) {
    completeness <- NA_real_
  }

  tibble(
    metric_name = "overall_completeness_percentage",
    metric_value = round(completeness, 2)
  )
}

# This function calculates missingness and completeness for each variable.
#
# It helps identify which fields are most often incomplete.
calculate_missingness_by_variable <- function(context) {

  patients <- context$patients
  fields <- get_evaluated_fields(context)
  total_records <- nrow(patients)

  # Return an empty table with the expected columns if there is no data.
  if (total_records == 0 || length(fields) == 0) {

    return(tibble(
      variable_name = character(),
      missing_count = numeric(),
      non_missing_count = numeric(),
      total_records = numeric(),
      missing_percentage = numeric(),
      completeness_percentage = numeric()
    ))
  }

  # Count missing values per evaluated field and calculate percentages.
  result <- patients %>%
    select(all_of(fields)) %>%
    summarise(across(
      everything(),
      ~sum(is.na(.) | (. == "" & is.character(.)), na.rm = TRUE)
    )) %>%
    pivot_longer(
      cols = everything(),
      names_to = "variable_name",
      values_to = "missing_count"
    ) %>%
    mutate(
      non_missing_count = total_records - .data$missing_count,
      total_records = total_records,
      missing_percentage = round(safe_divide(.data$missing_count, total_records) * 100, 2),
      completeness_percentage = round(100 - .data$missing_percentage, 2)
    ) %>%
    arrange(desc(.data$missing_percentage), .data$variable_name)

  return(result)
}

# This function calculates missingness at the patient-record level.
#
# Instead of asking "which variables are incomplete?",
# it asks "which patient records are incomplete?"
calculate_missingness_by_patient <- function(context) {

  patients <- context$patients
  fields <- get_evaluated_fields(context)

  # Return an empty structure if there are no patients or fields to evaluate.
  if (nrow(patients) == 0 || length(fields) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      missing_count = numeric(),
      filled_count = numeric(),
      total_fields = numeric(),
      record_completeness_percentage = numeric()
    ))
  }

  # Create a logical matrix where TRUE means the value is missing.
  missing_matrix <- patients %>%
    select(all_of(fields)) %>%
    mutate(across(
      everything(),
      ~ {
        if (is.character(.x)) {
          is.na(.x) | str_trim(.x) == ""
        } else {
          is.na(.x)
        }
      }
    ))

  # Count how many missing fields each patient has.
  missing_count_vector <- rowSums(
    as.data.frame(missing_matrix),
    na.rm = TRUE
  )

  total_fields <- length(fields)

  # Attach missingness counts and completeness percentages back to each patient.
  result <- patients %>%
    mutate(
      missing_count = missing_count_vector,
      filled_count = total_fields - .data$missing_count,
      total_fields = total_fields,
      record_completeness_percentage = round(
        safe_divide(.data$filled_count, .data$total_fields) * 100,
        2
      )
    ) %>%
    select(
      patient_uuid,
      patient_id,
      missing_count,
      filled_count,
      total_fields,
      record_completeness_percentage
    ) %>%
    arrange(.data$patient_id)

  return(result)
}

# This function calculates completeness for required fields only.
#
# Required fields are clinically and structurally important,
# so their completeness is reported separately from optional fields.
calculate_required_field_completeness <- function(context) {

  patients <- context$patients
  fields <- get_required_fields(context)
  total_records <- nrow(patients)

  # Empty result if there are no records or required fields.
  if (total_records == 0 || length(fields) == 0) {

    return(tibble(
      variable_name = character(),
      required_missing_count = numeric(),
      required_non_missing_count = numeric(),
      total_records = numeric(),
      required_completeness_percentage = numeric()
    ))
  }

  # Count missing required values per variable and calculate completeness.
  patients %>%
    select(all_of(fields)) %>%
    summarise(across(
      everything(),
      ~sum(is.na(.) | (. == "" & is.character(.)), na.rm = TRUE)
    )) %>%
    pivot_longer(
      everything(),
      names_to = "variable_name",
      values_to = "required_missing_count"
    ) %>%
    mutate(
      required_non_missing_count = total_records - .data$required_missing_count,
      total_records = total_records,
      required_completeness_percentage = round(
        safe_divide(.data$required_non_missing_count, total_records) * 100,
        2
      )
    ) %>%
    arrange(.data$variable_name)
}

# This function calculates completeness for optional fields only.
#
# Optional fields are not mandatory, but they still contribute to richness
# and usefulness of the clinical registry.
calculate_optional_field_completeness <- function(context) {

  patients <- context$patients
  fields <- get_optional_fields(context)
  total_records <- nrow(patients)

  # Empty result if there are no records or optional fields.
  if (total_records == 0 || length(fields) == 0) {

    return(tibble(
      variable_name = character(),
      optional_missing_count = numeric(),
      optional_non_missing_count = numeric(),
      total_records = numeric(),
      optional_completeness_percentage = numeric()
    ))
  }

  # Count missing optional values per variable and calculate completeness.
  patients %>%
    select(all_of(fields)) %>%
    summarise(across(
      everything(),
      ~sum(is.na(.) | (. == "" & is.character(.)), na.rm = TRUE)
    )) %>%
    pivot_longer(
      everything(),
      names_to = "variable_name",
      values_to = "optional_missing_count"
    ) %>%
    mutate(
      optional_non_missing_count = total_records - .data$optional_missing_count,
      total_records = total_records,
      optional_completeness_percentage = round(
        safe_divide(.data$optional_non_missing_count, total_records) * 100,
        2
      )
    ) %>%
    arrange(.data$variable_name)
}

# This function combines the main completeness metrics into one summary table.
#
# It includes:
# - total records
# - total evaluated fields
# - total missing values
# - overall completeness
# - average required-field completeness
# - average optional-field completeness
calculate_completeness_summary <- function(context) {

  # Calculate individual completeness metrics.
  total_records <- calculate_total_records(context)$metric_value[[1]]
  total_evaluated_fields <- calculate_total_evaluated_fields(context)$metric_value[[1]]
  total_missing_values <- calculate_total_missing_values(context)$metric_value[[1]]
  overall_completeness <- calculate_overall_completeness(context)$metric_value[[1]]

  # Calculate required and optional completeness tables.
  required_tbl <- calculate_required_field_completeness(context)
  optional_tbl <- calculate_optional_field_completeness(context)

  # Average completeness across required variables.
  required_completeness <- if (nrow(required_tbl) == 0) {
    NA_real_
  } else {
    round(mean(required_tbl$required_completeness_percentage, na.rm = TRUE), 2)
  }

  # Average completeness across optional variables.
  optional_completeness <- if (nrow(optional_tbl) == 0) {
    NA_real_
  } else {
    round(mean(optional_tbl$optional_completeness_percentage, na.rm = TRUE), 2)
  }

  # Return all summary metrics in a consistent metric_name / metric_value format.
  tibble(
    metric_name = c(
      "total_records",
      "total_evaluated_fields",
      "total_missing_values",
      "overall_completeness_percentage",
      "overall_required_completeness_percentage",
      "overall_optional_completeness_percentage"
    ),
    metric_value = c(
      total_records,
      total_evaluated_fields,
      total_missing_values,
      overall_completeness,
      required_completeness,
      optional_completeness
    )
  )
}

# ============================================================
# B. CONSISTENCY METRICS
# ============================================================

# This function checks consistency between date_of_birth and recorded age.
#
# It calculates age from date_of_birth and compares it with the stored age.
# A difference of 2 years or less is considered consistent.
calculate_dob_age_consistency <- function(context) {

  patients <- context$patients

  # If there are no patients, return an empty table with the expected structure.
  if (nrow(patients) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      date_of_birth = as.Date(character()),
      recorded_age = numeric(),
      calculated_age = numeric(),
      age_difference = numeric(),
      dob_age_consistent = logical()
    ))
  }

  result <- patients %>%
    mutate(
      # Ensure date_of_birth is treated as a Date.
      date_of_birth = as.Date(.data$date_of_birth),

      # Convert recorded age to numeric for comparison.
      recorded_age = as.numeric(.data$age),

      # Calculate age from date_of_birth using the interval to today's date.
      calculated_age = as.integer(floor(time_length(interval(.data$date_of_birth, Sys.Date()), "years"))),

      # If DOB or age is missing, or DOB is in the future, calculated age is set to NA.
      calculated_age = ifelse(
        is.na(.data$date_of_birth) | is.na(.data$recorded_age) | .data$date_of_birth > Sys.Date(),
        NA_integer_,
        .data$calculated_age
      ),

      # Calculate absolute difference between recorded and calculated age.
      age_difference = abs(.data$calculated_age - .data$recorded_age),

      # Mark consistency based on the allowed tolerance of 2 years.
      dob_age_consistent = ifelse(
        is.na(.data$age_difference),
        NA,
        .data$age_difference <= 2
      )
    ) %>%
    # Keep only records where age and DOB were available for evaluation.
    filter(!is.na(.data$recorded_age), !is.na(.data$date_of_birth)) %>%
    select(
      patient_uuid,
      patient_id,
      date_of_birth,
      recorded_age,
      calculated_age,
      age_difference,
      dob_age_consistent
    ) %>%
    arrange(.data$patient_id)

  return(result)
}


# This function calculates BMI for all patients and classifies BMI category.
#
# It also checks whether BMI is within the plausibility range used in the project:
# 10 <= BMI <= 70.
calculate_bmi_table <- function(context) {

  patients <- context$patients

  # If no patients exist, return an empty BMI table.
  if (nrow(patients) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      weight_kg = numeric(),
      height_cm = numeric(),
      bmi = numeric(),
      bmi_category = character(),
      bmi_plausible = logical()
    ))
  }

  # Calculate vectorized BMI using weight and height.
  bmi <- calculate_bmi_vector(patients$weight_kg, patients$height_cm)

  tibble(
    patient_uuid = patients$patient_uuid,
    patient_id = patients$patient_id,
    weight_kg = as_numeric_safe(patients$weight_kg),
    height_cm = as_numeric_safe(patients$height_cm),
    bmi = bmi,

    # Assign a broad BMI category for dashboard interpretation.
    bmi_category = case_when(
      is.na(bmi) ~ "unknown",
      bmi < 18.5 ~ "underweight",
      bmi >= 18.5 & bmi < 25 ~ "normal",
      bmi >= 25 & bmi < 30 ~ "overweight",
      bmi >= 30 ~ "obesity",
      TRUE ~ "unknown"
    ),

    # Mark whether the BMI is inside the project plausibility interval.
    bmi_plausible = case_when(
      is.na(bmi) ~ NA,
      bmi >= 10 & bmi <= 70 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
    arrange(.data$patient_id)
}

# This function summarizes how many calculated BMI values are plausible or implausible.
calculate_bmi_plausibility_rate <- function(context) {

  bmi_tbl <- calculate_bmi_table(context)

  # Count evaluated, plausible, and implausible BMI records.
  evaluated_bmi_records <- sum(!is.na(bmi_tbl$bmi_plausible))
  plausible_bmi_records <- sum(bmi_tbl$bmi_plausible == TRUE, na.rm = TRUE)
  implausible_bmi_records <- sum(bmi_tbl$bmi_plausible == FALSE, na.rm = TRUE)

  tibble(
    metric_name = "bmi_plausibility",
    plausible_bmi_records = plausible_bmi_records,
    implausible_bmi_records = implausible_bmi_records,
    evaluated_bmi_records = evaluated_bmi_records,
    bmi_plausibility_rate_percentage = round(
      safe_divide(plausible_bmi_records, evaluated_bmi_records) * 100,
      2
    ),
    bmi_implausibility_rate_percentage = round(
      safe_divide(implausible_bmi_records, evaluated_bmi_records) * 100,
      2
    )
  )
}

# This function groups patients into broad age categories.
#
# Categories:
# - pediatric: age < 18
# - adult: 18 <= age < 65
# - elderly: age >= 65
# - unknown: missing or unclassifiable age
calculate_age_group_distribution <- function(context) {

  patients <- context$patients

  # Empty output if there are no patient records.
  if (nrow(patients) == 0) {

    return(tibble(
      age_group = character(),
      n = numeric(),
      percentage = numeric()
    ))
  }

  patients %>%
    mutate(
      age_group = case_when(
        is.na(.data$age) ~ "unknown",
        .data$age < 18 ~ "pediatric",
        .data$age >= 18 & .data$age < 65 ~ "adult",
        .data$age >= 65 ~ "elderly",
        TRUE ~ "unknown"
      )
    ) %>%
    count(.data$age_group, name = "n") %>%
    mutate(
      percentage = round(safe_divide(.data$n, sum(.data$n)) * 100, 2)
    ) %>%
    arrange(.data$age_group)
}


# ============================================================
# C. ACCURACY / PROXY ACCURACY METRICS
# ============================================================
#
# True clinical accuracy requires an external gold standard.
# This system does not claim true diagnostic correctness,
# overdose detection, toxic dose detection, or pharmacological
# safety assessment.
#
# Proxy accuracy is operationalized using:
# - validation-rule failures
# - clinical plausibility checks
# - statistical outlier detection
# ============================================================

# This function calculates how many inserted patient records have no quality flags,
# warning flags, or critical flags.
#
# It is called a validation pass rate because it measures whether records passed
# the project's validation checks, not whether they are clinically correct in reality.
calculate_validation_pass_rate <- function(context) {

  patients <- context$patients
  flags <- context$quality_flags

  total_patients <- nrow(patients)

  # If no patients exist, rates are undefined.
  if (total_patients == 0) {

    return(tibble(
      records_without_flags = 0,
      records_with_warning_flags = 0,
      records_with_critical_flags = 0,
      total_patients = 0,
      validation_pass_rate_percentage = NA_real_,
      warning_record_rate_percentage = NA_real_,
      critical_record_rate_percentage = NA_real_
    ))
  }

  # Keep only flags linked to inserted patients.
  # Flags linked to rejected submissions are not counted here.
  patient_flags <- flags %>%
    filter(!is.na(.data$patient_uuid))

  # Count patients with at least one WARNING flag.
  warning_patients <- patient_flags %>%
    filter(.data$severity == "WARNING") %>%
    distinct(.data$patient_uuid) %>%
    nrow()

  # Count patients with at least one CRITICAL flag.
  critical_patients <- patient_flags %>%
    filter(.data$severity == "CRITICAL") %>%
    distinct(.data$patient_uuid) %>%
    nrow()

  # Count patients with any flag.
  flagged_patients <- patient_flags %>%
    distinct(.data$patient_uuid) %>%
    nrow()

  # Patients without flags are considered to have passed all stored validation checks.
  records_without_flags <- total_patients - flagged_patients

  tibble(
    records_without_flags = records_without_flags,
    records_with_warning_flags = warning_patients,
    records_with_critical_flags = critical_patients,
    total_patients = total_patients,
    validation_pass_rate_percentage = round(
      safe_divide(records_without_flags, total_patients) * 100,
      2
    ),
    warning_record_rate_percentage = round(
      safe_divide(warning_patients, total_patients) * 100,
      2
    ),
    critical_record_rate_percentage = round(
      safe_divide(critical_patients, total_patients) * 100,
      2
    )
  )
}

# This function retrieves flags that are interpreted as clinical outlier or plausibility signals.
#
# It does not detect new clinical issues itself.
# Instead, it extracts already stored quality_flags whose issue_type or severity
# indicates possible outlier/plausibility problems.
calculate_clinical_outlier_flags <- function(context) {

  flags <- context$quality_flags
  patients <- context$patients

  # If no flags exist, return an empty table with the expected structure.
  if (nrow(flags) == 0) {

    return(tibble(
      flag_id = character(),
      patient_uuid = character(),
      patient_id = character(),
      rule_name = character(),
      variable_name = character(),
      issue_type = character(),
      severity = character(),
      issue_description = character(),
      created_at = as.POSIXct(character())
    ))
  }

  # These issue types are treated as outlier/plausibility-related.
  outlier_issue_types <- c(
    "outlier",
    "suspicious_value",
    "clinical_plausibility_warning",
    "invalid_range",
    "implausible_value"
  )

  flags %>%
    # Only consider flags linked to inserted patients.
    filter(!is.na(.data$patient_uuid)) %>%
    filter(
      # Include explicit outlier issue types or warning flags whose issue_type
      # contains plausibility/outlier/range wording.
      .data$issue_type %in% outlier_issue_types |
        (.data$severity == "WARNING" & str_detect(.data$issue_type, "plaus|outlier|range|implaus", negate = FALSE))
    ) %>%
    # Join patient_id for readability.
    left_join(
      patients %>% select(patient_uuid, patient_id),
      by = "patient_uuid"
    ) %>%
    select(
      flag_id,
      patient_uuid,
      patient_id,
      rule_name,
      variable_name,
      issue_type,
      severity,
      issue_description,
      created_at
    ) %>%
    arrange(desc(.data$created_at))
}

# This function calculates the percentage of patients with at least one
# clinical outlier/plausibility flag.
calculate_clinical_outlier_rate <- function(context) {

  outlier_flags <- calculate_clinical_outlier_flags(context)
  total_patients <- nrow(context$patients)

  # Count distinct patients affected by at least one outlier flag.
  patients_with_outlier_flags <- outlier_flags %>%
    distinct(.data$patient_uuid) %>%
    nrow()

  tibble(
    metric_name = "clinical_outlier_rate",
    patients_with_outlier_flags = patients_with_outlier_flags,
    total_patients = total_patients,
    clinical_outlier_rate_percentage = round(
      safe_divide(patients_with_outlier_flags, total_patients) * 100,
      2
    )
  )
}

# This function summarizes outlier/plausibility flags by variable.
#
# It helps identify which variables most often produce suspicious values.
calculate_outlier_rate_by_variable <- function(context) {

  outlier_flags <- calculate_clinical_outlier_flags(context)
  total_patients <- nrow(context$patients)

  # Empty output if no outlier flags exist.
  if (nrow(outlier_flags) == 0) {

    return(tibble(
      variable_name = character(),
      outlier_flag_count = numeric(),
      affected_patients = numeric(),
      total_patients = numeric(),
      outlier_rate_percentage = numeric()
    ))
  }

  outlier_flags %>%
    group_by(.data$variable_name) %>%
    summarise(
      outlier_flag_count = n(),
      affected_patients = n_distinct(.data$patient_uuid),
      .groups = "drop"
    ) %>%
    mutate(
      total_patients = total_patients,
      outlier_rate_percentage = round(
        safe_divide(.data$affected_patients, total_patients) * 100,
        2
      )
    ) %>%
    arrange(desc(.data$outlier_flag_count), .data$variable_name)
}

# This function detects statistical outliers using z-scores.
#
# It supports numeric variables and derived variables:
# - age
# - weight_kg
# - height_cm
# - dosage_mg
# - bmi
# - dose_per_kg
#
# A record is considered a z-score outlier when abs(z_score) > threshold.
detect_zscore_outliers <- function(context,
                                   variable_name,
                                   threshold = 3) {

  patients <- context$patients

  # Only these variables are allowed for z-score outlier detection.
  allowed_variables <- c(
    "age",
    "weight_kg",
    "height_cm",
    "dosage_mg",
    "bmi",
    "dose_per_kg"
  )

  # Stop if the requested variable is not supported.
  if (!variable_name %in% allowed_variables) {
    stop(glue("Unsupported variable_name: {variable_name}."))
  }

  # Empty result if there are no patients.
  if (nrow(patients) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      variable_name = character(),
      value = numeric(),
      mean_value = numeric(),
      sd_value = numeric(),
      z_score = numeric(),
      threshold = numeric(),
      is_zscore_outlier = logical()
    ))
  }

  # Derived variable: BMI is calculated from weight and height.
  if (variable_name == "bmi") {

    values <- calculate_bmi_vector(
      patients$weight_kg,
      patients$height_cm
    )

  } else if (variable_name == "dose_per_kg") {

    # Derived variable: dose_per_kg is calculated from dosage and weight.
    values <- calculate_dose_per_kg_vector(
      patients$dosage_mg,
      patients$weight_kg
    )

  } else {

    # Direct numeric variable from the patients table.
    values <- as_numeric_safe(patients[[variable_name]])
  }

  # Calculate mean and standard deviation.
  mean_value <- mean(values, na.rm = TRUE)
  sd_value <- sd(values, na.rm = TRUE)

  # If standard deviation is unavailable or zero,
  # z-score outlier detection cannot be performed.
  if (is.na(sd_value) || sd_value == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      variable_name = character(),
      value = numeric(),
      mean_value = numeric(),
      sd_value = numeric(),
      z_score = numeric(),
      threshold = numeric(),
      is_zscore_outlier = logical()
    ))
  }

  # Calculate z-score for every value.
  z_score <- (values - mean_value) / sd_value

  # Return only records whose absolute z-score is above the threshold.
  tibble(
    patient_uuid = patients$patient_uuid,
    patient_id = patients$patient_id,
    variable_name = variable_name,
    value = values,
    mean_value = mean_value,
    sd_value = sd_value,
    z_score = round(z_score, 3),
    threshold = threshold,
    is_zscore_outlier = abs(z_score) > threshold
  ) %>%
    filter(.data$is_zscore_outlier == TRUE) %>%
    arrange(desc(abs(.data$z_score)))
}

# This function runs z-score outlier detection for all supported variables
# and combines the results into one table.
calculate_all_zscore_outliers <- function(context,
                                          threshold = 3) {

  variables <- c(
    "age",
    "weight_kg",
    "height_cm",
    "dosage_mg",
    "bmi",
    "dose_per_kg"
  )

  # Apply detect_zscore_outliers() to each supported variable.
  results <- lapply(
    variables,
    function(v) detect_zscore_outliers(context, v, threshold)
  )

  # Combine all variable-specific outlier tables into one tibble.
  bind_rows(results)
}

# This function calculates the rate of BMI values outside the plausibility interval.
#
# The interval used here is:
# BMI < 10 or BMI > 70 => outlier
calculate_bmi_outlier_rate <- function(context) {

  bmi_tbl <- calculate_bmi_table(context)

  evaluated_bmi_records <- sum(!is.na(bmi_tbl$bmi))
  bmi_outlier_count <- sum(bmi_tbl$bmi < 10 | bmi_tbl$bmi > 70, na.rm = TRUE)

  tibble(
    metric_name = "bmi_outlier_rate",
    bmi_outlier_count = bmi_outlier_count,
    evaluated_bmi_records = evaluated_bmi_records,
    bmi_outlier_rate_percentage = round(
      safe_divide(bmi_outlier_count, evaluated_bmi_records) * 100,
      2
    )
  )
}

# This function calculates dose_per_kg for each patient.
#
# It is used as a derived metric for statistical outlier detection.
# It does not represent a final clinical dosing safety decision.
calculate_dose_per_kg_table <- function(context) {

  patients <- context$patients

  # Empty result if no patient records exist.
  if (nrow(patients) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      dosage_mg = numeric(),
      weight_kg = numeric(),
      dose_per_kg = numeric()
    ))
  }

  tibble(
    patient_uuid = patients$patient_uuid,
    patient_id = patients$patient_id,
    dosage_mg = as_numeric_safe(patients$dosage_mg),
    weight_kg = as_numeric_safe(patients$weight_kg),
    dose_per_kg = calculate_dose_per_kg_vector(
      patients$dosage_mg,
      patients$weight_kg
    )
  ) %>%
    arrange(.data$patient_id)
}


# ============================================================
# D. ADVANCED METRICS
# ============================================================

# This function calculates how many quality flags each patient has.
#
# It reports:
# - total flags
# - warning flags
# - critical flags
# - info flags
calculate_flag_burden_per_patient <- function(context) {

  patients <- context$patients
  flags <- context$quality_flags

  # Empty result if there are no patients.
  if (nrow(patients) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      total_flags = numeric(),
      warning_flags = numeric(),
      critical_flags = numeric(),
      info_flags = numeric()
    ))
  }

  # Count flags per patient_uuid.
  flag_counts <- flags %>%
    filter(!is.na(.data$patient_uuid)) %>%
    group_by(.data$patient_uuid) %>%
    summarise(
      total_flags = n(),
      warning_flags = sum(.data$severity == "WARNING", na.rm = TRUE),
      critical_flags = sum(.data$severity == "CRITICAL", na.rm = TRUE),
      info_flags = sum(.data$severity == "INFO", na.rm = TRUE),
      .groups = "drop"
    )

  # Join flag counts back to the patients table.
  # Patients without flags receive zero counts.
  patients %>%
    select(patient_uuid, patient_id) %>%
    left_join(flag_counts, by = "patient_uuid") %>%
    mutate(
      total_flags = replace_na(.data$total_flags, 0),
      warning_flags = replace_na(.data$warning_flags, 0),
      critical_flags = replace_na(.data$critical_flags, 0),
      info_flags = replace_na(.data$info_flags, 0)
    ) %>%
    arrange(desc(.data$total_flags), .data$patient_id)
}

# This function summarizes flag burden across the full patient table.
#
# It reports:
# - average flags per patient
# - average flags per flagged patient
# - maximum flags on a single patient
# - number of flagged patients
calculate_flag_burden_summary <- function(context) {

  burden <- calculate_flag_burden_per_patient(context)

  # Empty summary if there are no patients.
  if (nrow(burden) == 0) {

    return(tibble(
      metric_name = c(
        "average_flags_per_patient",
        "average_flags_per_flagged_patient",
        "max_flags_single_patient",
        "flagged_patient_count"
      ),
      metric_value = c(NA_real_, NA_real_, NA_real_, 0)
    ))
  }

  # Keep only patients with at least one flag.
  flagged <- burden %>%
    filter(.data$total_flags > 0)

  tibble(
    metric_name = c(
      "average_flags_per_patient",
      "average_flags_per_flagged_patient",
      "max_flags_single_patient",
      "flagged_patient_count"
    ),
    metric_value = c(
      round(mean(burden$total_flags, na.rm = TRUE), 2),
      ifelse(nrow(flagged) == 0, 0, round(mean(flagged$total_flags, na.rm = TRUE), 2)),
      max(burden$total_flags, na.rm = TRUE),
      nrow(flagged)
    )
  )
}

# This function identifies which variables generate the most quality flags.
#
# It is useful for discovering problematic fields in the registry,
# such as fields that are frequently missing, implausible, or incorrectly formatted.
calculate_most_problematic_variables <- function(context) {

  flags <- context$quality_flags

  # Empty result if no quality flags exist.
  if (nrow(flags) == 0) {

    return(tibble(
      variable_name = character(),
      total_flags = numeric(),
      warning_flags = numeric(),
      critical_flags = numeric(),
      info_flags = numeric(),
      affected_patients = numeric()
    ))
  }

  # Summarize flags by variable name.
  flags %>%
    filter(!is.na(.data$variable_name)) %>%
    group_by(.data$variable_name) %>%
    summarise(
      total_flags = n(),
      warning_flags = sum(.data$severity == "WARNING", na.rm = TRUE),
      critical_flags = sum(.data$severity == "CRITICAL", na.rm = TRUE),
      info_flags = sum(.data$severity == "INFO", na.rm = TRUE),
      affected_patients = n_distinct(.data$patient_uuid, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(.data$total_flags), .data$variable_name)
}

# This function classifies records as HOT or COLD based on created_at.
#
# HOT data means records created within the last months_threshold months.
# COLD data means records older than that threshold.
calculate_hot_cold_ratio <- function(context,
                                     months_threshold = 6) {

  patients <- context$patients

  # If there are no records or no created_at field, return an empty result.
  if (nrow(patients) == 0 || !"created_at" %in% names(patients)) {

    return(tibble(
      data_temperature = character(),
      n = numeric(),
      percentage = numeric()
    ))
  }

  # Define the cutoff between hot and cold data.
  threshold_time <- Sys.time() - months(months_threshold)

  patients %>%
    mutate(
      # Convert created_at to POSIXct for comparison.
      created_at_parsed = as.POSIXct(.data$created_at),

      # Classify each record by recency.
      data_temperature = case_when(
        is.na(.data$created_at_parsed) ~ "UNKNOWN",
        .data$created_at_parsed >= threshold_time ~ "HOT",
        .data$created_at_parsed < threshold_time ~ "COLD",
        TRUE ~ "UNKNOWN"
      )
    ) %>%
    count(.data$data_temperature, name = "n") %>%
    mutate(
      percentage = round(safe_divide(.data$n, sum(.data$n)) * 100, 2)
    ) %>%
    arrange(.data$data_temperature)
}

# This function calculates a record-level confidence score.
#
# The score starts at 100 and subtracts penalties for:
# - missing values
# - warning flags
# - critical flags
# - implausible BMI
# - dose_per_kg statistical outlier
#
# This is a data-quality confidence score, not a clinical risk score.
calculate_record_confidence_score <- function(context) {

  patients <- context$patients

  # Empty result if no patients exist.
  if (nrow(patients) == 0) {

    return(tibble(
      patient_uuid = character(),
      patient_id = character(),
      missing_count = numeric(),
      warning_flags = numeric(),
      critical_flags = numeric(),
      bmi_implausible = logical(),
      dose_per_kg_outlier = logical(),
      confidence_score = numeric()
    ))
  }

  # Calculate supporting tables needed to compute confidence.
  missing_tbl <- calculate_missingness_by_patient(context)
  burden_tbl <- calculate_flag_burden_per_patient(context)
  bmi_tbl <- calculate_bmi_table(context)

  # Detect patients that are statistical outliers for dose_per_kg.
  dose_outliers <- detect_zscore_outliers(
    context,
    variable_name = "dose_per_kg",
    threshold = 3
  ) %>%
    distinct(.data$patient_uuid) %>%
    mutate(dose_per_kg_outlier = TRUE)

  result <- patients %>%
    select(patient_uuid, patient_id) %>%
    # Join missingness information.
    left_join(
      missing_tbl %>% select(patient_uuid, missing_count),
      by = "patient_uuid"
    ) %>%
    # Join warning and critical flag counts.
    left_join(
      burden_tbl %>% select(patient_uuid, warning_flags, critical_flags),
      by = "patient_uuid"
    ) %>%
    # Join BMI plausibility information.
    left_join(
      bmi_tbl %>%
        transmute(
          patient_uuid,
          bmi_implausible = case_when(
            is.na(.data$bmi_plausible) ~ FALSE,
            .data$bmi_plausible == FALSE ~ TRUE,
            TRUE ~ FALSE
          )
        ),
      by = "patient_uuid"
    ) %>%
    # Join dose-per-kg outlier information.
    left_join(dose_outliers, by = "patient_uuid") %>%
    mutate(
      # Replace missing joined values with safe defaults.
      missing_count = replace_na(.data$missing_count, 0),
      warning_flags = replace_na(.data$warning_flags, 0),
      critical_flags = replace_na(.data$critical_flags, 0),
      bmi_implausible = replace_na(.data$bmi_implausible, FALSE),
      dose_per_kg_outlier = replace_na(.data$dose_per_kg_outlier, FALSE),

      # Calculate confidence score using predefined penalties.
      confidence_score = 100 -
        (3 * .data$missing_count) -
        (5 * .data$warning_flags) -
        (20 * .data$critical_flags) -
        ifelse(.data$bmi_implausible, 10, 0) -
        ifelse(.data$dose_per_kg_outlier, 10, 0),

      # Prevent negative scores and round the final value.
      confidence_score = pmax(0, round(.data$confidence_score, 2))
    ) %>%
    arrange(.data$patient_id)

  return(result)
}

# This function calculates uniqueness-related metrics.
#
# It checks:
# - duplicate patient_id values
# - duplicate patient_uuid values
# - duplicate patient_id rate
# - uniqueness rate
calculate_uniqueness_metrics <- function(context) {

  patients <- context$patients
  total_patients <- nrow(patients)

  # If there are no patients, duplicate counts are zero and rates are undefined.
  if (total_patients == 0) {

    return(tibble(
      metric_name = c(
        "duplicate_patient_id_count",
        "duplicate_uuid_count",
        "duplicate_patient_id_rate",
        "uniqueness_rate"
      ),
      metric_value = c(0, 0, NA_real_, NA_real_)
    ))
  }

  # Count duplicated patient_id values.
  # The calculation sums the extra occurrences beyond the first one.
  duplicate_patient_id_count <- patients %>%
    filter(!is.na(.data$patient_id)) %>%
    count(.data$patient_id) %>%
    filter(.data$n > 1) %>%
    summarise(total = sum(.data$n - 1)) %>%
    pull(.data$total)

  # If no duplicates are found, force the value to zero.
  if (length(duplicate_patient_id_count) == 0 || is.na(duplicate_patient_id_count)) {
    duplicate_patient_id_count <- 0
  }

  # Count duplicated patient_uuid values.
  # In normal operation this should be zero because patient_uuid is intended to be unique.
  duplicate_uuid_count <- patients %>%
    filter(!is.na(.data$patient_uuid)) %>%
    count(.data$patient_uuid) %>%
    filter(.data$n > 1) %>%
    summarise(total = sum(.data$n - 1)) %>%
    pull(.data$total)

  # If no duplicate UUIDs are found, force the value to zero.
  if (length(duplicate_uuid_count) == 0 || is.na(duplicate_uuid_count)) {
    duplicate_uuid_count <- 0
  }

  # Calculate duplicate patient ID rate.
  duplicate_patient_id_rate <- round(
    safe_divide(duplicate_patient_id_count, total_patients) * 100,
    2
  )

  # Calculate uniqueness rate as the inverse of the duplicate patient ID rate.
  uniqueness_rate <- round(100 - duplicate_patient_id_rate, 2)

  tibble(
    metric_name = c(
      "duplicate_patient_id_count",
      "duplicate_uuid_count",
      "duplicate_patient_id_rate",
      "uniqueness_rate"
    ),
    metric_value = c(
      duplicate_patient_id_count,
      duplicate_uuid_count,
      duplicate_patient_id_rate,
      uniqueness_rate
    )
  )
}


# ------------------------------------------------------------
# Main Dashboard Metrics Generator
# ------------------------------------------------------------

# This is the main function that collects the dashboard metrics into one list.
#
# It does not create plots and it does not modify the database.
# It simply calls the metric functions and returns their results.
generate_dashboard_metrics <- function(context) {

  list(
    # Completeness-related outputs.
    completeness_summary = calculate_completeness_summary(context),
    missingness_by_variable = calculate_missingness_by_variable(context),
    missingness_by_patient = calculate_missingness_by_patient(context),
    required_field_completeness = calculate_required_field_completeness(context),
    optional_field_completeness = calculate_optional_field_completeness(context),

    # Consistency and derived clinical-measurement outputs.
    bmi_table = calculate_bmi_table(context),
    age_group_distribution = calculate_age_group_distribution(context),

    # Proxy accuracy and outlier-related outputs.
    clinical_outlier_flags = calculate_clinical_outlier_flags(context),
    outlier_rate_by_variable = calculate_outlier_rate_by_variable(context),
    zscore_outliers = calculate_all_zscore_outliers(context),
   
    # Advanced quality-monitoring outputs.
    flag_burden_per_patient = calculate_flag_burden_per_patient(context),
    flag_burden_summary = calculate_flag_burden_summary(context),
    most_problematic_variables = calculate_most_problematic_variables(context),
    hot_cold_ratio = calculate_hot_cold_ratio(context),
    record_confidence_score = calculate_record_confidence_score(context)
  )
}