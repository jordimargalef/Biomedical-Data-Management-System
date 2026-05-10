############################################################
# Project: Biomedical Data Management System
# Filename: 012_report_generation.R
# Description: Generates automated HTML reports summarizing
# the current state of the biomedical database.
# Author: Carolina López, Jordi Margalef & Sara Vaquero
# Date: 11-05-2026
############################################################

library(DBI)
library(RPostgres)
library(dplyr)
library(rmarkdown)

# ------------------------------------------------------------
# 0. ENVIRONMENT VARIABLES AND SOURCE PROJECT FUNCTIONS
# ------------------------------------------------------------
Sys.setenv(PGUSER = "postgres")
Sys.setenv(PGPASSWORD = "postgres")

source("09_quality_metrics_backend.R")

# ------------------------------------------------------------
# 1. DATABASE CONNECTION FUNCTION
# ------------------------------------------------------------
connect_db_report <- function() {
  dbConnect(
    RPostgres::Postgres(),
    dbname = "biomedical_db",
    host = "localhost",
    port = 5432,
    user = "postgres",
    password = "postgres"
  )
}

# ------------------------------------------------------------
# 2. GENERATE BIOMEDICAL REPORT
# ------------------------------------------------------------
generate_biomedical_report <- function(report_format = "HTML") {
  
  con <- connect_db_report()
  
  patients <- dbReadTable(con, "patients")
  audit_log <- dbReadTable(con, "audit_log")
  
  context <- load_dashboard_context(con)
  
  if (!"patient_uuid" %in% names(context$quality_flags)) {
    context$quality_flags <- tibble::tibble(
      flag_id = character(),
      patient_uuid = character(),
      submission_id = character(),
      rule_id = character(),
      rule_name = character(),
      variable_name = character(),
      issue_type = character(),
      severity = character(),
      issue_description = character(),
      detected_by_user = character(),
      created_at = as.POSIXct(character())
    )
  }
  
  metrics <- generate_dashboard_metrics(context)
  
  dbDisconnect(con)
  
  report_format <- toupper(report_format)
  
  output_ext <- ifelse(report_format == "PDF", ".pdf", ".html")
  output_format <- ifelse(report_format == "PDF", "pdf_document", "html_document")
  
  output_file <- paste0(
    "biomedical_database_report_",
    format(Sys.time(), "%Y%m%d_%H%M%S"),
    output_ext
  )
  
  rmarkdown::render(
    input = "report_template.Rmd",
    output_format = output_format,
    output_file = output_file,
    params = list(
      patients = patients,
      audit_log = audit_log,
      metrics = metrics
    ),
    envir = new.env(parent = globalenv())
  )
  
  return(output_file)
}