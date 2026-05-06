############################################################
# Project: Biomedical Data Management System
# Filename: 03_audit_triggers.R
# Description: Implements audit logging with triggers to 
# track data changes and user actions.
# Author: Carolina López, Jordi Margalef & Sara Vaquero
# Date: 11-05-2026
############################################################

library(DBI)
library(RPostgres)

# ------------------------------------------------------------
# 1. CONNECT TO DATABASE
# ------------------------------------------------------------
con <- dbConnect(
  RPostgres::Postgres(),
  dbname = "biomedical_db",
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = "%s"
)

# ------------------------------------------------------------
# 2. CREATE AUDIT FUNCTION
# ------------------------------------------------------------
sql_create_audit_function <- "
CREATE OR REPLACE FUNCTION log_patient_changes()
RETURNS TRIGGER AS $$
BEGIN
  INSERT INTO audit_log (
    audit_id,
    table_name,
    record_id,
    action,
    old_value,
    new_value,
    changed_by
  )
  VALUES (
    gen_random_uuid(),
    TG_TABLE_NAME,
    COALESCE(NEW.patient_uuid, OLD.patient_uuid),
    TG_OP,
    row_to_json(OLD),
    row_to_json(NEW),
    current_user
  );
  
  IF TG_OP = 'DELETE' THEN
    RETURN OLD;
  ELSE
    RETURN NEW;
  END IF;
END;
$$ LANGUAGE plpgsql;
"

dbExecute(con, sql_create_audit_function)

# ------------------------------------------------------------
# 3. CREATE TRIGGERS ON PATIENTS TABLE
# ------------------------------------------------------------
dbExecute(con, "DROP TRIGGER IF EXISTS patients_insert_audit ON patients;")
dbExecute(con, "DROP TRIGGER IF EXISTS patients_update_audit ON patients;")
dbExecute(con, "DROP TRIGGER IF EXISTS patients_delete_audit ON patients;")

sql_create_trigger_insert <- "
CREATE TRIGGER patients_insert_audit
AFTER INSERT ON patients
FOR EACH ROW
EXECUTE FUNCTION log_patient_changes();
"
sql_create_trigger_update <- "
CREATE TRIGGER patients_update_audit
AFTER UPDATE ON patients
FOR EACH ROW
EXECUTE FUNCTION log_patient_changes();
"

sql_create_trigger_delete <- "
CREATE TRIGGER patients_delete_audit
AFTER DELETE ON patients
FOR EACH ROW
EXECUTE FUNCTION log_patient_changes();
"

dbExecute(con, sql_create_trigger_insert)
dbExecute(con, sql_create_trigger_update)
dbExecute(con, sql_create_trigger_delete)

# ------------------------------------------------------------
# 4. DISCONNECT
# ------------------------------------------------------------
dbDisconnect(con)