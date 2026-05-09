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

library(DBI)
library(RPostgres)
library(tidyverse)
library(stringr)
library(lubridate)
library(uuid)
library(glue)
library(janitor)

# ------------------------------------------------------------
# Database Connection
# ------------------------------------------------------------

connect_db <- function() {

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

# Backwards-compatible alias, in case previous scripts expect this name.
db_connection <- connect_db

# ------------------------------------------------------------
# Safe Query Helpers
# ------------------------------------------------------------

table_exists <- function(con, table_name, schema_name = "public") {

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

  return(result$n[[1]] > 0)
}

safe_read_table <- function(con,
                            table_name,
                            schema_name = "public",
                            required = FALSE) {

  tryCatch({

    if (!table_exists(con, table_name, schema_name)) {

      if (isTRUE(required)) {
        stop(glue("Required table public.{table_name} does not exist."))
      }

      return(tibble())
    }

    query <- glue("SELECT * FROM {schema_name}.{table_name};")

    result <- dbGetQuery(con, query) %>%
      as_tibble()

    return(result)

  }, error = function(e) {

    if (isTRUE(required)) {
      stop(glue("Failed to read required table public.{table_name}: {e$message}"))
    }

    message(glue("Optional table public.{table_name} could not be loaded: {e$message}"))

    return(tibble())
  })
}

# ------------------------------------------------------------
# Load Dashboard Context
# ------------------------------------------------------------

load_dashboard_context <- function(con) {

  tryCatch({

    patients <- safe_read_table(
      con = con,
      table_name = "patients",
      required = TRUE
    )

    metadata_table <- safe_read_table(
      con = con,
      table_name = "metadata_table",
      required = TRUE
    )

    validation_rules <- safe_read_table(
      con = con,
      table_name = "validation_rules",
      required = TRUE
    )

    controlled_vocabularies <- safe_read_table(
      con = con,
      table_name = "controlled_vocabularies",
      required = FALSE
    )

    quality_flags <- safe_read_table(
      con = con,
      table_name = "quality_flags",
      required = FALSE
    )

    rejected_patient_submissions <- safe_read_table(
      con = con,
      table_name = "rejected_patient_submissions",
      required = FALSE
    )

    return(list(
      patients = patients,
      metadata_table = metadata_table,
      validation_rules = validation_rules,
      controlled_vocabularies = controlled_vocabularies,
      quality_flags = quality_flags,
      rejected_patient_submissions = rejected_patient_submissions
    ))

  }, error = function(e) {

    stop(glue("Failed to load dashboard context: {e$message}"))
  })
}

# ------------------------------------------------------------
# Generic Helpers
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

as_numeric_safe <- function(x) {

  suppressWarnings(as.numeric(x))
}

calculate_bmi_vector <- function(weight_kg, height_cm) {

  weight <- as_numeric_safe(weight_kg)
  height <- as_numeric_safe(height_cm)

  height_m <- height / 100

  bmi <- weight / (height_m ^ 2)

  bmi[is.na(weight) | is.na(height) | height <= 0] <- NA_real_

  round(bmi, 2)
}

calculate_dose_per_kg_vector <- function(dosage_mg, weight_kg) {

  dosage <- as_numeric_safe(dosage_mg)
  weight <- as_numeric_safe(weight_kg)

  dose_per_kg <- dosage / weight

  dose_per_kg[is.na(dosage) | is.na(weight) | weight <= 0] <- NA_real_

  round(dose_per_kg, 4)
}

get_evaluated_fields <- function(context) {

  fields <- context$metadata_table %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  fields <- fields[fields %in% names(context$patients)]

  return(fields)
}

get_required_fields <- function(context) {

  fields <- context$metadata_table %>%
    filter(.data$is_required == TRUE) %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  fields <- fields[fields %in% names(context$patients)]

  return(fields)
}

get_optional_fields <- function(context) {

  fields <- context$metadata_table %>%
    filter(.data$is_required == FALSE) %>%
    filter(.data$is_system_generated == FALSE) %>%
    filter(.data$is_derived == FALSE) %>%
    pull(.data$variable_name)

  fields <- fields[fields %in% names(context$patients)]

  return(fields)
}

empty_metric <- function(metric_name, metric_value = NA_real_) {

  tibble(
    metric_name = metric_name,
    metric_value = metric_value
  )
}

# ============================================================
# A. COMPLETENESS METRICS
# ============================================================

calculate_total_records <- function(context) {

  tibble(
    metric_name = "total_records",
    metric_value = nrow(context$patients)
  )
}

calculate_total_evaluated_fields <- function(context) {

  tibble(
    metric_name = "total_evaluated_fields",
    metric_value = length(get_evaluated_fields(context))
  )
}

calculate_total_missing_values <- function(context) {

  patients <- context$patients
  fields <- get_evaluated_fields(context)

  if (nrow(patients) == 0 || length(fields) == 0) {
    return(tibble(metric_name = "total_missing_values", metric_value = 0))
  }

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

calculate_overall_completeness <- function(context) {

  total_records <- nrow(context$patients)
  total_fields <- length(get_evaluated_fields(context))
  total_expected_values <- total_records * total_fields

  total_missing <- calculate_total_missing_values(context)$metric_value[[1]]

  completeness <- (1 - safe_divide(total_missing, total_expected_values)) * 100

  if (is.na(completeness) && total_expected_values == 0) {
    completeness <- NA_real_
  }

  tibble(
    metric_name = "overall_completeness_percentage",
    metric_value = round(completeness, 2)
  )
}

calculate_missingness_by_variable <- function(context) {

  patients <- context$patients
  fields <- get_evaluated_fields(context)
  total_records <- nrow(patients)

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

calculate_missingness_by_patient <- function(context) {

  patients <- context$patients
  fields <- get_evaluated_fields(context)

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

  missing_count_vector <- rowSums(
    as.data.frame(missing_matrix),
    na.rm = TRUE
  )

  total_fields <- length(fields)

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

calculate_required_field_completeness <- function(context) {

  patients <- context$patients
  fields <- get_required_fields(context)
  total_records <- nrow(patients)

  if (total_records == 0 || length(fields) == 0) {

    return(tibble(
      variable_name = character(),
      required_missing_count = numeric(),
      required_non_missing_count = numeric(),
      total_records = numeric(),
      required_completeness_percentage = numeric()
    ))
  }

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

calculate_optional_field_completeness <- function(context) {

  patients <- context$patients
  fields <- get_optional_fields(context)
  total_records <- nrow(patients)

  if (total_records == 0 || length(fields) == 0) {

    return(tibble(
      variable_name = character(),
      optional_missing_count = numeric(),
      optional_non_missing_count = numeric(),
      total_records = numeric(),
      optional_completeness_percentage = numeric()
    ))
  }

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

calculate_completeness_summary <- function(context) {

  total_records <- calculate_total_records(context)$metric_value[[1]]
  total_evaluated_fields <- calculate_total_evaluated_fields(context)$metric_value[[1]]
  total_missing_values <- calculate_total_missing_values(context)$metric_value[[1]]
  overall_completeness <- calculate_overall_completeness(context)$metric_value[[1]]

  required_tbl <- calculate_required_field_completeness(context)
  optional_tbl <- calculate_optional_field_completeness(context)

  required_completeness <- if (nrow(required_tbl) == 0) {
    NA_real_
  } else {
    round(mean(required_tbl$required_completeness_percentage, na.rm = TRUE), 2)
  }

  optional_completeness <- if (nrow(optional_tbl) == 0) {
    NA_real_
  } else {
    round(mean(optional_tbl$optional_completeness_percentage, na.rm = TRUE), 2)
  }

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

calculate_dob_age_consistency <- function(context) {

  patients <- context$patients

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
      date_of_birth = as.Date(.data$date_of_birth),
      recorded_age = as.numeric(.data$age),
      calculated_age = as.integer(floor(time_length(interval(.data$date_of_birth, Sys.Date()), "years"))),
      calculated_age = ifelse(
        is.na(.data$date_of_birth) | is.na(.data$recorded_age) | .data$date_of_birth > Sys.Date(),
        NA_integer_,
        .data$calculated_age
      ),
      age_difference = abs(.data$calculated_age - .data$recorded_age),
      dob_age_consistent = ifelse(
        is.na(.data$age_difference),
        NA,
        .data$age_difference <= 2
      )
    ) %>%
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


calculate_bmi_table <- function(context) {

  patients <- context$patients

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

  bmi <- calculate_bmi_vector(patients$weight_kg, patients$height_cm)

  tibble(
    patient_uuid = patients$patient_uuid,
    patient_id = patients$patient_id,
    weight_kg = as_numeric_safe(patients$weight_kg),
    height_cm = as_numeric_safe(patients$height_cm),
    bmi = bmi,
    bmi_category = case_when(
      is.na(bmi) ~ "unknown",
      bmi < 18.5 ~ "underweight",
      bmi >= 18.5 & bmi < 25 ~ "normal",
      bmi >= 25 & bmi < 30 ~ "overweight",
      bmi >= 30 ~ "obesity",
      TRUE ~ "unknown"
    ),
    bmi_plausible = case_when(
      is.na(bmi) ~ NA,
      bmi >= 10 & bmi <= 70 ~ TRUE,
      TRUE ~ FALSE
    )
  ) %>%
    arrange(.data$patient_id)
}

calculate_bmi_plausibility_rate <- function(context) {

  bmi_tbl <- calculate_bmi_table(context)

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

calculate_age_group_distribution <- function(context) {

  patients <- context$patients

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

calculate_validation_pass_rate <- function(context) {

  patients <- context$patients
  flags <- context$quality_flags

  total_patients <- nrow(patients)

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

  patient_flags <- flags %>%
    filter(!is.na(.data$patient_uuid))

  warning_patients <- patient_flags %>%
    filter(.data$severity == "WARNING") %>%
    distinct(.data$patient_uuid) %>%
    nrow()

  critical_patients <- patient_flags %>%
    filter(.data$severity == "CRITICAL") %>%
    distinct(.data$patient_uuid) %>%
    nrow()

  flagged_patients <- patient_flags %>%
    distinct(.data$patient_uuid) %>%
    nrow()

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

calculate_clinical_outlier_flags <- function(context) {

  flags <- context$quality_flags
  patients <- context$patients

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

  outlier_issue_types <- c(
    "outlier",
    "suspicious_value",
    "clinical_plausibility_warning",
    "invalid_range",
    "implausible_value"
  )

  flags %>%
    filter(!is.na(.data$patient_uuid)) %>%
    filter(
      .data$issue_type %in% outlier_issue_types |
        (.data$severity == "WARNING" & str_detect(.data$issue_type, "plaus|outlier|range|implaus", negate = FALSE))
    ) %>%
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

calculate_clinical_outlier_rate <- function(context) {

  outlier_flags <- calculate_clinical_outlier_flags(context)
  total_patients <- nrow(context$patients)

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

calculate_outlier_rate_by_variable <- function(context) {

  outlier_flags <- calculate_clinical_outlier_flags(context)
  total_patients <- nrow(context$patients)

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

detect_zscore_outliers <- function(context,
                                   variable_name,
                                   threshold = 3) {

  patients <- context$patients

  allowed_variables <- c(
    "age",
    "weight_kg",
    "height_cm",
    "dosage_mg",
    "bmi",
    "dose_per_kg"
  )

  if (!variable_name %in% allowed_variables) {
    stop(glue("Unsupported variable_name: {variable_name}."))
  }

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

  if (variable_name == "bmi") {

    values <- calculate_bmi_vector(
      patients$weight_kg,
      patients$height_cm
    )

  } else if (variable_name == "dose_per_kg") {

    values <- calculate_dose_per_kg_vector(
      patients$dosage_mg,
      patients$weight_kg
    )

  } else {

    values <- as_numeric_safe(patients[[variable_name]])
  }

  mean_value <- mean(values, na.rm = TRUE)
  sd_value <- sd(values, na.rm = TRUE)

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

  z_score <- (values - mean_value) / sd_value

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

  results <- lapply(
    variables,
    function(v) detect_zscore_outliers(context, v, threshold)
  )

  bind_rows(results)
}

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

calculate_dose_per_kg_table <- function(context) {

  patients <- context$patients

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

calculate_flag_burden_per_patient <- function(context) {

  patients <- context$patients
  flags <- context$quality_flags

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

calculate_flag_burden_summary <- function(context) {

  burden <- calculate_flag_burden_per_patient(context)

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

calculate_most_problematic_variables <- function(context) {

  flags <- context$quality_flags

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

calculate_hot_cold_ratio <- function(context,
                                     months_threshold = 6) {

  patients <- context$patients

  if (nrow(patients) == 0 || !"created_at" %in% names(patients)) {

    return(tibble(
      data_temperature = character(),
      n = numeric(),
      percentage = numeric()
    ))
  }

  threshold_time <- Sys.time() - months(months_threshold)

  patients %>%
    mutate(
      created_at_parsed = as.POSIXct(.data$created_at),
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

calculate_record_confidence_score <- function(context) {

  patients <- context$patients

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

  missing_tbl <- calculate_missingness_by_patient(context)
  burden_tbl <- calculate_flag_burden_per_patient(context)
  bmi_tbl <- calculate_bmi_table(context)

  dose_outliers <- detect_zscore_outliers(
    context,
    variable_name = "dose_per_kg",
    threshold = 3
  ) %>%
    distinct(.data$patient_uuid) %>%
    mutate(dose_per_kg_outlier = TRUE)

  result <- patients %>%
    select(patient_uuid, patient_id) %>%
    left_join(
      missing_tbl %>% select(patient_uuid, missing_count),
      by = "patient_uuid"
    ) %>%
    left_join(
      burden_tbl %>% select(patient_uuid, warning_flags, critical_flags),
      by = "patient_uuid"
    ) %>%
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
    left_join(dose_outliers, by = "patient_uuid") %>%
    mutate(
      missing_count = replace_na(.data$missing_count, 0),
      warning_flags = replace_na(.data$warning_flags, 0),
      critical_flags = replace_na(.data$critical_flags, 0),
      bmi_implausible = replace_na(.data$bmi_implausible, FALSE),
      dose_per_kg_outlier = replace_na(.data$dose_per_kg_outlier, FALSE),
      confidence_score = 100 -
        (3 * .data$missing_count) -
        (5 * .data$warning_flags) -
        (20 * .data$critical_flags) -
        ifelse(.data$bmi_implausible, 10, 0) -
        ifelse(.data$dose_per_kg_outlier, 10, 0),
      confidence_score = pmax(0, round(.data$confidence_score, 2))
    ) %>%
    arrange(.data$patient_id)

  return(result)
}

calculate_uniqueness_metrics <- function(context) {

  patients <- context$patients
  total_patients <- nrow(patients)

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

  duplicate_patient_id_count <- patients %>%
    filter(!is.na(.data$patient_id)) %>%
    count(.data$patient_id) %>%
    filter(.data$n > 1) %>%
    summarise(total = sum(.data$n - 1)) %>%
    pull(.data$total)

  if (length(duplicate_patient_id_count) == 0 || is.na(duplicate_patient_id_count)) {
    duplicate_patient_id_count <- 0
  }

  duplicate_uuid_count <- patients %>%
    filter(!is.na(.data$patient_uuid)) %>%
    count(.data$patient_uuid) %>%
    filter(.data$n > 1) %>%
    summarise(total = sum(.data$n - 1)) %>%
    pull(.data$total)

  if (length(duplicate_uuid_count) == 0 || is.na(duplicate_uuid_count)) {
    duplicate_uuid_count <- 0
  }

  duplicate_patient_id_rate <- round(
    safe_divide(duplicate_patient_id_count, total_patients) * 100,
    2
  )

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

generate_dashboard_metrics <- function(context) {

  list(
    completeness_summary = calculate_completeness_summary(context),
    missingness_by_variable = calculate_missingness_by_variable(context),
    missingness_by_patient = calculate_missingness_by_patient(context),
    required_field_completeness = calculate_required_field_completeness(context),
    optional_field_completeness = calculate_optional_field_completeness(context),

    bmi_table = calculate_bmi_table(context),
    age_group_distribution = calculate_age_group_distribution(context),

    clinical_outlier_flags = calculate_clinical_outlier_flags(context),
    outlier_rate_by_variable = calculate_outlier_rate_by_variable(context),
    zscore_outliers = calculate_all_zscore_outliers(context),
   
    flag_burden_per_patient = calculate_flag_burden_per_patient(context),
    flag_burden_summary = calculate_flag_burden_summary(context),
    most_problematic_variables = calculate_most_problematic_variables(context),
    hot_cold_ratio = calculate_hot_cold_ratio(context),
    record_confidence_score = calculate_record_confidence_score(context)
  )
}