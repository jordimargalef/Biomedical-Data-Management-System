############################################################
# Project: Biomedical Data Management System
# Filename: 011_shiny_application.R
# Description: Builds the final Shiny user interface for
# patient visualization, secure data entry, quality metrics,
# audit trail monitoring, and report generation.
# Author: Carolina López, Jordi Margalef & Sara Vaquero
# Date: 11-05-2026
############################################################

library(shiny)
library(DBI)
library(RPostgres)
library(DT)
library(dplyr)
library(shinydashboard)
library(plotly)
library(ggplot2)

# ------------------------------------------------------------
# 0. ENVIRONMENT VARIABLES AND SOURCE PROJECT FUNCTIONS
# ------------------------------------------------------------
Sys.setenv(PGUSER = "postgres")
Sys.setenv(PGPASSWORD = "postgres")

source("05_validation_engine.R")
source("06_quality_flagging.R")
source("07_insert_patient_pipeline.R")
source("09_quality_metrics_backend.R")
source("012_report_generation.R")

# ------------------------------------------------------------
# 1. VISUAL STYLE CONSTANTS
# ------------------------------------------------------------
medical_blue <- "#1F4E79"
medical_teal <- "#2A9D8F"
medical_grey <- "#5C6770"
medical_light <- "#F8FAFC"
medical_dark <- "#1E293B"

# ------------------------------------------------------------
# 2. DATABASE CONNECTION FUNCTION
# ------------------------------------------------------------
connect_db <- function() {
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
# 3. DATA ACCESS FUNCTIONS
# ------------------------------------------------------------
get_patients <- function() {
  con <- connect_db()
  patients <- dbReadTable(con, "patients")
  dbDisconnect(con)
  patients
}

get_audit_log <- function() {
  con <- connect_db()
  audit_log <- dbReadTable(con, "audit_log")
  dbDisconnect(con)
  audit_log
}

get_dashboard_metrics <- function() {
  con <- connect_db()
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
  metrics
}

# ------------------------------------------------------------
# 4. USER INTERFACE
# ------------------------------------------------------------
ui <- fluidPage(
  
  titlePanel("Biomedical Data Management System"),
  
  tabsetPanel(
    
    tabPanel(
      "Patient Records",
      br(),
      h3("Patient Records"),
      p("Cleaned and standardized patient data retrieved from PostgreSQL."),
      DTOutput("patients_table")
    ),
    
    tabPanel(
      "Data Entry Form",
      br(),
      h3("Clinical Data Entry Form"),
      p("Structured Case Report Form for secure patient insertion."),
      
      fluidRow(
        column(
          width = 6,
          textInput("patient_id", "Patient ID", placeholder = "Example: P-9999"),
          dateInput("date_of_birth", "Date of Birth"),
          numericInput("age", "Age", value = NA, min = 0, max = 120),
          selectInput("sex", "Sex", choices = c("M", "F", "X")),
          numericInput("weight_kg", "Weight (kg)", value = NA),
          numericInput("height_cm", "Height (cm)", value = NA)
        ),
        column(
          width = 6,
          selectInput(
            "blood_type",
            "Blood Type",
            choices = c("", "A+", "A-", "B+", "B-", "AB+", "AB-", "O+", "O-")
          ),
          textInput("diagnosis_code", "Diagnosis Code", placeholder = "Example: E11"),
          numericInput("dosage_mg", "Dosage (mg)", value = NA),
          selectInput("smoker", "Smoker", choices = c("", "TRUE", "FALSE")),
          textInput("doctor_name", "Doctor Name", placeholder = "Example: Dr. Smith"),
          br(),
          actionButton("submit_patient", "Submit Patient")
        )
      ),
      
      br(),
      h4("Insertion Result"),
      DTOutput("insertion_summary")
    ),
    
    tabPanel(
      "Quality Dashboard",
      br(),
      h3("Data Quality Dashboard"),
      p("Reactive overview of completeness, missingness, outliers, and record confidence."),
      
      fluidRow(
        valueBoxOutput("total_patients_box", width = 3),
        valueBoxOutput("missing_values_box", width = 3),
        valueBoxOutput("average_age_box", width = 3),
        valueBoxOutput("smoker_percentage_box", width = 3)
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 6,
          h4("Sex Distribution"),
          plotlyOutput("sex_distribution_plot", height = "280px")
        ),
        column(
          width = 6,
          h4("Blood Type Distribution"),
          plotlyOutput("blood_distribution_plot", height = "280px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 6,
          h4("Age Distribution"),
          plotlyOutput("age_distribution_plot", height = "280px")
        ),
        column(
          width = 6,
          h4("Missing Values by Variable"),
          plotlyOutput("missing_values_plot", height = "280px")
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          width = 6,
          h4("Record Confidence Score"),
          DTOutput("confidence_score_table")
        ),
        column(
          width = 6,
          h4("Z-Score Outliers"),
          DTOutput("zscore_outliers_table")
        )
      ),

      br(),

      fluidRow(
        column(
          width = 6,
          h4("BMI Quality Table"),
          DTOutput("bmi_table")
        ),
        column(
          width = 6,
          h4("Missingness by Patient"),
          DTOutput("missingness_patient_table")
        )
      ),
    ),
    
    tabPanel(
      "Audit Trail",
      br(),
      h3("Audit Trail"),
      p("Automatic record of changes performed on the patients table."),
      DTOutput("audit_table")
    ),
    
    tabPanel(
      "Reports",
      br(),
      h3("Automated Reports"),
      p("This section generates automated HTML or PDF summaries of the current biomedical database state."),
      
      selectInput(
        "report_format",
        "Report Format",
        choices = c("HTML", "PDF"),
        selected = "HTML"
      ),
      
      actionButton(
        "generate_report",
        "Generate Report",
        class = "btn-primary"
      )
    )
  )
)

# ------------------------------------------------------------
# 5. SERVER LOGIC
# ------------------------------------------------------------
server <- function(input, output, session) {
  
  refresh_data <- reactiveVal(0)
  insertion_result <- reactiveVal(data.frame())
  
  patients_data <- reactive({
    refresh_data()
    get_patients()
  })
  
  audit_data <- reactive({
    refresh_data()
    get_audit_log()
  })
  
  dashboard_metrics <- reactive({
    refresh_data()
    get_dashboard_metrics()
  })
  
  # ------------------------------------------------------------
  # PATIENT RECORDS TABLE
  # ------------------------------------------------------------
  output$patients_table <- renderDT({
    datatable(
      patients_data(),
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # ------------------------------------------------------------
  # AUDIT LOG TABLE
  # ------------------------------------------------------------
  output$audit_table <- renderDT({
    audit_clean <- audit_data() %>%
      arrange(desc(changed_at)) %>%
      select(
        audit_id,
        table_name,
        record_id,
        action,
        changed_by,
        changed_at
      ) %>%
      head(20)
    
    datatable(
      audit_clean,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
  
  # ------------------------------------------------------------
  # QUALITY DASHBOARD KPIs
  # ------------------------------------------------------------
  output$total_patients_box <- renderValueBox({
    valueBox(
      value = nrow(patients_data()),
      subtitle = "Total Patients",
      icon = icon("hospital-user"),
      color = "blue"
    )
  })
  
  output$missing_values_box <- renderValueBox({
    completeness <- dashboard_metrics()$completeness_summary
    
    missing_value <- completeness %>%
      filter(metric_name == "total_missing_values") %>%
      pull(metric_value)

    if (length(missing_value) == 0) {
      missing_value <- sum(is.na(patients_data()))
    }
    
    valueBox(
      value = missing_value,
      subtitle = "Missing Values",
      icon = icon("triangle-exclamation"),
      color = "red"
    )
  })
  
  output$average_age_box <- renderValueBox({
    valueBox(
      value = round(mean(patients_data()$age, na.rm = TRUE), 1),
      subtitle = "Average Age",
      icon = icon("user"),
      color = "green"
    )
  })
  
  output$smoker_percentage_box <- renderValueBox({
    smoker_percentage <- mean(
      patients_data()$smoker,
      na.rm = TRUE
    ) * 100
    
    valueBox(
      value = paste0(round(smoker_percentage, 1), "%"),
      subtitle = "Smokers",
      icon = icon("smoking"),
      color = "yellow"
    )
  })
  
  # ------------------------------------------------------------
  # QUALITY DASHBOARD PLOTS
  # ------------------------------------------------------------
  output$sex_distribution_plot <- renderPlotly({
    sex_data <- patients_data() %>%
      count(sex)
    
    p <- ggplot(sex_data, aes(x = sex, y = n)) +
      geom_col(fill = medical_blue, width = 0.7) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = medical_light, color = NA),
        panel.background = element_rect(fill = medical_light, color = NA),
        text = element_text(color = medical_dark)
      ) +
      labs(
        x = "Sex",
        y = "Number of patients"
      )
    
    ggplotly(p)
  })
  
  output$blood_distribution_plot <- renderPlotly({
    blood_data <- patients_data() %>%
      count(blood_type)
    
    p <- ggplot(blood_data, aes(x = blood_type, y = n)) +
      geom_col(fill = medical_teal, width = 0.7) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = medical_light, color = NA),
        panel.background = element_rect(fill = medical_light, color = NA),
        text = element_text(color = medical_dark)
      ) +
      labs(
        x = "Blood type",
        y = "Number of patients"
      )
    
    ggplotly(p)
  })
  
  output$age_distribution_plot <- renderPlotly({
    p <- ggplot(patients_data(), aes(x = age)) +
      geom_histogram(
        bins = 15,
        fill = medical_blue,
        color = "white"
      ) +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = medical_light, color = NA),
        panel.background = element_rect(fill = medical_light, color = NA),
        text = element_text(color = medical_dark)
      ) +
      labs(
        x = "Age",
        y = "Number of patients"
      )
    
    ggplotly(p)
  })
  
  output$missing_values_plot <- renderPlotly({
    missing_data <- dashboard_metrics()$missingness_by_variable
    
    if (!all(c("variable_name", "missing_count") %in% names(missing_data))) {
      missing_data <- data.frame(
        variable_name = names(colSums(is.na(patients_data()))),
        missing_count = as.numeric(colSums(is.na(patients_data())))
      )
    }
    
    missing_data <- missing_data %>%
      filter(missing_count > 0)
    
    p <- ggplot(missing_data, aes(x = reorder(variable_name, missing_count), y = missing_count)) +
      geom_col(fill = medical_grey, width = 0.7) +
      coord_flip() +
      theme_minimal() +
      theme(
        plot.background = element_rect(fill = medical_light, color = NA),
        panel.background = element_rect(fill = medical_light, color = NA),
        text = element_text(color = medical_dark)
      ) +
      labs(
        title = "Variables with Missing Values",
        x = "Variable",
        y = "Missing values"
      )
    
    ggplotly(p)
  })
  
  # ------------------------------------------------------------
  # ADVANCED QUALITY TABLES FROM METRICS
  # ------------------------------------------------------------
  output$confidence_score_table <- renderDT({
    confidence_table <- dashboard_metrics()$record_confidence_score
    
    datatable(
      confidence_table,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })
  
  output$zscore_outliers_table <- renderDT({
    outliers_table <- dashboard_metrics()$zscore_outliers
    
    datatable(
      outliers_table,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })

  output$bmi_table <- renderDT({
    bmi_table <- dashboard_metrics()$bmi_table
    
    datatable(
      bmi_table,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })

  output$missingness_patient_table <- renderDT({
    missingness_patient_table <- dashboard_metrics()$missingness_by_patient
    
    datatable(
      missingness_patient_table,
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })
  
  # ------------------------------------------------------------
  # DATA ENTRY FORM INSERTION USING THE PIPELINE
  # ------------------------------------------------------------
  observeEvent(input$submit_patient, {
    
    patient_record <- list(
      patient_id = input$patient_id,
      date_of_birth = input$date_of_birth,
      age = input$age,
      sex = input$sex,
      weight_kg = input$weight_kg,
      height_cm = input$height_cm,
      blood_type = input$blood_type,
      diagnosis_code = input$diagnosis_code,
      dosage_mg = input$dosage_mg,
      smoker = input$smoker,
      doctor_name = input$doctor_name
    )
    
    tryCatch({
      result <- insert_patient_secure(patient_record)
      summary <- summarize_pipeline_result(result)
      
      insertion_result(summary)
      
      if (isTRUE(result$inserted)) {
        showNotification(
          "Patient record successfully inserted through the secure validation pipeline.",
          type = "message"
        )
      } else {
        showNotification(
          "Patient record was rejected by the validation pipeline.",
          type = "error"
        )
      }
      
      refresh_data(refresh_data() + 1)
      
    }, error = function(e) {
      showNotification(
        paste("Secure insertion failed:", e$message),
        type = "error"
      )
    })
  })
  
  output$insertion_summary <- renderDT({
    datatable(
      insertion_result(),
      options = list(
        pageLength = 5,
        scrollX = TRUE
      )
    )
  })
  
  # ------------------------------------------------------------
  # REPORT GENERATION
  # ------------------------------------------------------------
  observeEvent(input$generate_report, {
    
    tryCatch({
      
      report_path <- generate_biomedical_report(
        report_format = input$report_format
      )
      
      showNotification(
        paste(input$report_format, "report generated successfully:", report_path),
        type = "message",
        duration = 8
      )
      
    }, error = function(e) {
      
      showNotification(
        paste(input$report_format, "report generation failed:", e$message),
        type = "error",
        duration = 10
      )
    })
  })
  
}

# ------------------------------------------------------------
# 6. RUN APPLICATION
# ------------------------------------------------------------
shinyApp(ui = ui, server = server)