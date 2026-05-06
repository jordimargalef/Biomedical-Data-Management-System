############################################################
# Project: Biomedical Data Management System
# Filename: 01_backend_setup.R
# Description: Initializes PostgreSQL database, loads raw 
# data, and defines core relational schema.
# Author: Carolina López, Jordi Margalef & Sara Vaquero
# Date: 11-05-2026
############################################################

library(DBI)
library(RPostgres)
library(readxl)

# ------------------------------------------------------------
# 1. CONNECT TO POSTGRESQL
# ------------------------------------------------------------
# Connect to PostgreSQL
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "biomedical_db",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "%s"
)

dbExecute(con, "SET TIME ZONE 'Europe/Madrid'")

# ------------------------------------------------------------
# 2. LOAD RAW DATA
# ------------------------------------------------------------
# Load Excel and create raw_data table
df <- read_excel("Hospital_Admissions_ULTIMATE.xlsx")

dbWriteTable(
  con,
  "raw_data",
  df,
  overwrite = TRUE
)

# ------------------------------------------------------------
# 3. CREATE PATIENTS TABLE
# ------------------------------------------------------------
sql_create_patients <- "
CREATE TABLE IF NOT EXISTS patients (
  patient_uuid UUID PRIMARY KEY,
  patient_id TEXT UNIQUE NOT NULL,
  date_of_birth DATE NOT NULL,
  age INTEGER CHECK (age BETWEEN 0 AND 120),
  sex TEXT CHECK (sex IN ('M', 'F', 'X')),
  weight_kg NUMERIC,
  height_cm NUMERIC,
  blood_type TEXT,
  diagnosis_code TEXT,
  dosage_mg INTEGER CHECK (dosage_mg > 0),
  smoker BOOLEAN,
  doctor_name TEXT,
  created_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
"

# ------------------------------------------------------------
# 4. CREATE AUDIT LOG TABLE
# ------------------------------------------------------------
sql_create_audit_log <- "
CREATE TABLE IF NOT EXISTS audit_log (
  audit_id UUID PRIMARY KEY,
  table_name TEXT NOT NULL,
  record_id UUID NOT NULL,
  action TEXT NOT NULL,
  old_value JSONB,
  new_value JSONB,
  changed_by TEXT,
  changed_at TIMESTAMPTZ DEFAULT CURRENT_TIMESTAMP
);
"

dbExecute(con, sql_create_patients)
dbExecute(con, sql_create_audit_log)

dbListTables(con)
dbDisconnect(con)