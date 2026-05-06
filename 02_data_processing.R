############################################################
# Project: Biomedical Data Management System
# Filename: 02_data_processing.R
# Description: Cleans, standardizes, and transforms raw 
# patient data before database insertion.
# Author: Carolina López, Jordi Margalef & Sara Vaquero
# Date: 11-05-2026
############################################################

library(DBI)
library(RPostgres)
library(tidyverse)
library(uuid)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "biomedical_db",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "%s"
)

raw_df <- dbReadTable(con, "raw_data")

# ------------------------------------------------------------
# 2. DATA CLEANING (PRE-PROCESSING)
# ------------------------------------------------------------
# Deduplicate and normalize Patient_ID
df_clean <- raw_df %>%
  distinct(Patient_ID, .keep_all = TRUE) %>%
  mutate(
    Patient_ID = Patient_ID %>%
      str_to_upper() %>%
      str_trim() %>%
      str_remove_all("L") %>%
      str_remove_all("[^0-9]") %>%
      str_pad(width = 4, side = "left", pad = "0") %>%
      paste0("P-", .)
  ) %>%
  distinct(Patient_ID, .keep_all = TRUE) %>%
  filter(!is.na(Patient_ID))

# Surrogate key
df_clean <- df_clean %>%
  mutate(patient_uuid = UUIDgenerate(n = n(), use.time = TRUE)) %>%
  relocate(patient_uuid, Patient_ID)

# Dates validation
df_clean <- df_clean %>%
  mutate(Date_of_Birth = na_if(tolower(as.character(Date_of_Birth)), "unknown")) %>%
  filter(!is.na(Date_of_Birth))

df_clean <- df_clean %>%
  mutate(
    date_of_birth = case_when(
      str_detect(Date_of_Birth, "^[0-9]+$") ~ as.Date(as.numeric(Date_of_Birth), origin = "1899-12-30"),
      TRUE ~ as.Date(Date_of_Birth, format = "%d/%m/%Y")
    )
  )

# Sex normalization
df_clean <- df_clean %>%
  mutate(
    sex = case_when(
      str_to_lower(Sex) %in% c("m", "male") ~ "M",
      str_to_lower(Sex) %in% c("f", "female") ~ "F",
      str_to_lower(Sex) == "x" ~ "X",
      TRUE ~ NA_character_
    )
  )

# Weight and height
df_clean <- df_clean %>%
  mutate(
    weight_kg = as.numeric(str_extract(Weight, "[0-9]+\\.?[0-9]*")),
    height_num = as.numeric(str_extract(Height, "[0-9]+\\.?[0-9]*")),
    height_cm = ifelse(height_num < 3, height_num * 100, height_num)
  )

# Blood type
df_clean <- df_clean %>%
  mutate(
    blood_type = case_when(
      str_to_lower(Blood_Type) %in% c("a pos", "a+", "a positive") ~ "A+",
      str_to_lower(Blood_Type) %in% c("a neg", "a-", "a negative") ~ "A-",
      str_to_lower(Blood_Type) %in% c("b pos", "b+", "b positive") ~ "B+",
      str_to_lower(Blood_Type) %in% c("b neg", "b-", "b negative") ~ "B-",
      str_to_lower(Blood_Type) %in% c("ab+", "ab pos") ~ "AB+",
      str_to_lower(Blood_Type) %in% c("o+", "o pos", "o positive") ~ "O+",
      TRUE ~ NA_character_
    )
  )

# Diagnosis, dosage, smoker, doctor
df_clean <- df_clean %>%
  mutate(
    diagnosis_code = str_to_upper(str_trim(Diagnosis_Code)),
    dosage_mg = as.numeric(Dosage_mg),
    smoker = case_when(
      str_to_lower(Smoker) %in% c("y", "yes", "smoker") ~ TRUE,
      str_to_lower(Smoker) %in% c("n", "no", "non-smoker") ~ FALSE,
      TRUE ~ NA
    ),
    doctor_name = Doctor_Name %>%
      str_trim() %>%
      str_replace_all("\\s+", " ") %>%
      str_replace("^Doctor\\s+", "") %>%
      str_replace("^Dr\\.\\s*", "") %>%
      str_to_title() %>%
      paste0("Dr. ", .)
  )

# ------------------------------------------------------------
# 3. INSERT INTO PATIENTS
# ------------------------------------------------------------
patients_df <- df_clean %>%
  transmute(
    patient_uuid = patient_uuid,
    patient_id = Patient_ID,
    date_of_birth = date_of_birth,
    age = Age,
    sex = sex,
    weight_kg = weight_kg,
    height_cm = height_cm,
    blood_type = blood_type,
    diagnosis_code = diagnosis_code,
    dosage_mg = dosage_mg,
    smoker = smoker,
    doctor_name = doctor_name
  )

dbWriteTable(
  con,
  "patients",
  patients_df,
  append = TRUE,
  row.names = FALSE
)

dbDisconnect(con)