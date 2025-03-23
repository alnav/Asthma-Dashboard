library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(flexdashboard)

# Load the dataset
patient_data <- read_csv("patient_dataset.csv")

# Define UI
ui <- fluidPage(
  titlePanel("ASTHMA DASHBOARD v1.0"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("patient_id",
        "Select Patient ID:",
        choices = unique(patient_data$ID)
      ),
      h3("Patient", style = "font-weight: bold;"),
      h5("Name:", style = "font-weight: bold;"),
      h5("Date of Birth:", style = "font-weight: bold;"),
      h5("NHS Number:", style = "font-weight: bold;"),
      tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"),
      htmlOutput("patient_info"),
      tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"),
      h3("Lung Function", style = "font-weight: bold;"),
      htmlOutput("latest_lung_function"),
      tags$hr(style = "border-top: 1px solid #ddd; margin: 10px 0;"),
      h3("Other Information", style = "font-weight: bold;"),
      h5("Hover for normal ranges", style = "font-style: italic;"),
      htmlOutput("latest_other_info"),
      div(
        style = "position: absolute; bottom:0px; width: 50%;",
        actionButton("edit_options", "Edit Patient",
          style = "
        background-color: #880808;
        color: white;
        font-weight: bold;
        padding: 10px 15px;
        width: 100%;
        border-radius: 5px;
        border: none;
        transition: background-color 0.3s;
      "
        )
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(12,
          align = "center",
          h3("Risk of Exacerbation in Next Month"),
          div(
            style = "width: 100%;
            height: 200px;
            display: flex;
            justify-content: center;
            align-items: center;",
            gaugeOutput("risk_gauge")
          )
        )
      ),
      fluidRow(
        column(
          6,
          h3("FEV1"),
          plotlyOutput("lung_function_plot")
        ),
        column(
          6,
          h3("Peak Expiratory Flow"),
          plotlyOutput("pef_plot")
        )
      ),
      fluidRow(
        column(
          6,
          h3("Adherence"),
          plotlyOutput("adherence_plot"),
        ),
        column(
          6,
          h3("Eosinophils and FeNO"),
          plotlyOutput("eosinophils_feno_plot")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Create reactive values storage
  rv <- reactiveValues(
    patient_data = patient_data
  )

  # Update selected_patient to use reactive data
  selected_patient <- reactive({
    rv$patient_data[rv$patient_data$ID == input$patient_id, ]
  })

  calculate_risk <- function(patient) {
    # Print debug info
    print(sprintf("FEV1: %f", patient$fev1_percent_predicted))
    print(sprintf("Eos: %f", patient$eosinophil_level))
    print(sprintf("IgE: %f", patient$total_ige))
    print(sprintf("FeNO: %f", patient$FeNO_ppb))
    print(sprintf("Adherence: %f", patient$Adherence))
    print(sprintf("Smoking: %s", patient$`Smoking Status`))
    print(sprintf("Severity: %s", patient$`Asthma Severity`))

    # Coefficients
    beta_0 <- -0 # Baseline risk
    beta_fev1 <- -0.03 # Protective effect of higher FEV1 % predicted
    beta_eos <- 0.001 # Higher eosinophils increase risk
    beta_ige <- 0.001 # IgE contribution to risk
    beta_severity <- 1.2 # Severe asthma significantly increases risk
    beta_smoking <- c(0.0, 0.5, 1.0) # Coefficients for smoking status
    beta_feno <- 0.01 # FeNO contribution to risk
    beta_adherence <- -0.02 # Better adherence reduces risk

    # Input validation with defaults
    fev1 <- ifelse(is.na(patient$fev1_percent_predicted),
      80, patient$fev1_percent_predicted
    )
    eos <- ifelse(is.na(patient$eosinophil_level),
      150, patient$eosinophil_level
    )
    ige <- ifelse(is.na(patient$total_ige), 100, patient$total_ige)
    feno <- ifelse(is.na(patient$FeNO_ppb), 25, patient$FeNO_ppb)
    adherence <- ifelse(is.na(patient$Adherence), 50, patient$Adherence) / 100

    # Convert categorical to numeric
    smoking_numeric <- switch(patient$`Smoking Status`,
      "Never Smoked" = 0,
      "Former Smoker" = 1,
      "Current Smoker" = 2,
      0 # Default
    )

    severity_numeric <- switch(patient$`Asthma Severity`,
      "Mild" = 0,
      "Moderate" = 1,
      "Severe" = 2,
      0 # Default
    )
    # Calculate log odds
    log_odds <- beta_0 +
      beta_fev1 * patient$fev1_percent_predicted +
      beta_eos * patient$eosinophil_level +
      beta_ige * patient$total_ige +
      beta_severity * severity_numeric +
      beta_smoking[smoking_numeric + 1] +
      beta_feno * patient$FeNO_ppb +
      beta_adherence * patient$Adherence

    # Print debug info
    print(sprintf("Log odds: %f", log_odds))

    # Convert to probability
    prob <- 1 / (1 + exp(-log_odds))
    print(sprintf("Probability: %f", prob))

    return(prob * 100)
  }

  # Update risk gauge:
  selected_patient <- reactive({
    req(input$patient_id)
    rv$patient_data[rv$patient_data$ID == input$patient_id, ]
  })

  # Update risk gauge
  output$risk_gauge <- renderGauge({
    req(selected_patient())
    patient <- selected_patient() %>% tail(1)
    risk_value <- calculate_risk(patient)

    gauge(
      risk_value,
      min = 0,
      max = 100,
      symbol = "%",
      gaugeSectors(
        success = c(0, 30),
        warning = c(30, 70),
        danger = c(70, 100)
      )
    )
  })

  output$patient_info <- renderUI({
    patient <- selected_patient() %>% tail(1)
    tags$div(
      tags$p(paste("Sex:", patient$Sex)),
      tags$p(paste("Age:", patient$Age)),
      tags$p(paste("Height (cm):", patient$`Height (cm)`)),
      tags$p(paste("Weight (kg):", patient$`Weight (kg)`)),
      tags$p(HTML(paste(
        "BMI:",
        sprintf(
          "<span style='color: %s'>%.1f</span>",
          ifelse(patient$BMI > 30, "red",
            ifelse(patient$BMI > 25, "#ff9100", "black")
          ),
          patient$BMI
        )
      ))),
      tags$p(paste("Ethnicity:", patient$Ethnicity)),
      tags$p(HTML(paste(
        "Smoking Status:",
        sprintf(
          "<span style='color: %s; font-weight: %s'>%s</span>",
          ifelse(patient$`Smoking Status` == "Current Smoker", "red", "black"),
          ifelse(patient$`Smoking Status` == "Current Smoker", "bold", "normal"),
          patient$`Smoking Status`
        )
      ))),
      tags$p(paste("Asthma Severity:", patient$`Asthma Severity`))
    )
  })

  output$latest_lung_function <- renderUI({
    patient <- selected_patient()
    latest <- tail(patient, 1)
    previous <- tail(patient, 2)[1, ]

    # Calculate FEV1 percent change
    fev1_change <- if (nrow(patient) > 1) {
      change <- ((latest$fev1_actual - previous$fev1_actual)
      / previous$fev1_actual) * 100
      sprintf(
        "<span style='color: %s'> (%+.1f%%)</span>",
        ifelse(change >= 0, "green", "red"),
        change
      )
    } else {
      ""
    }

    # Calculate PEF percent change
    pef_change <- if (nrow(patient) > 1) {
      change_pef <- ((latest$pef - previous$pef) / previous$pef) * 100
      sprintf(
        "<span style='color: %s'> (%+.1f%%)</span>",
        ifelse(change_pef >= 0, "green", "red"),
        change_pef
      )
    } else {
      ""
    }

    tags$div(
      tags$p(HTML(paste(
        "FEV1:", latest$fev1_actual,
        sprintf("(%d%%)", round(latest$fev1_percent_predicted)),
        fev1_change
      ))),
      tags$p(HTML(paste(
        "FVC:", latest$fvc_actual,
        sprintf("(%d%%)", round(latest$fvc_percent_predicted))
      ))),
      tags$p(HTML(paste("FEV1/FVC Ratio:", latest$fev1_fvc_ratio * 100, "%"))),
      tags$p(HTML(paste(
        "PEF:", latest$pef,
        pef_change
      )))
    )
  })

  output$latest_other_info <- renderUI({
    patient <- selected_patient() %>% tail(1)
    tags$div(
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <0.45 x 10^9/L'>Eosinophils:<span style='color: %s'>%s</span> x 10^9/L</span>",
          ifelse(patient$eosinophil_level > 0.45, "red", "black"),
          patient$eosinophil_level
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <25 ppb'>FeNo: <span style='color: %s'>%s</span> ppb</span>",
          ifelse(patient$FeNO_ppb > 25, "red", "black"),
          patient$FeNO_ppb
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range 5-120 kU/L'>Total IgE: <span style='color: %s'>%s</span> kU/L</span>",
          ifelse(patient$total_ige > 120, "red", "black"),
          patient$total_ige
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <0.35 kU/L'>IgE Pollen: <span style='color: %s'>%s</span> kU/L</span>",
          ifelse(patient$ige_pollen > 0.35, "red", "black"),
          patient$ige_pollen
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <0.35 kU/L'>IgE Cats: <span style='color: %s'>%s</span> kU/L</span>",
          ifelse(patient$ige_cats > 0.35, "red", "black"),
          patient$ige_cats
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <0.35 kU/L'>IgE Dogs: <span style='color: %s'>%s</span> kU/L</span>",
          ifelse(patient$ige_dogs > 0.35, "red", "black"),
          patient$ige_dogs
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <0.35 kU/L'>IgE Mould: <span style='color: %s'>%s</span> kU/L</span>",
          ifelse(patient$ige_grass > 0.35, "red", "black"),
          patient$ige_grass
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range <0.35 kU/L'>IgE House Dust Mites: <span style='color: %s'>%s</span> kU/L</span>",
          ifelse(patient$ige_house_dust_mites > 0.35, "red", "black"),
          patient$ige_house_dust_mites
        )
      ))),
      tags$p(paste("Treatment:", patient$treatment))
    )
  })

  output$lung_function_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = Age)) +
      geom_line(aes(y = fev1_actual, color = "FEV1 (Litres)")) +
      labs(y = "FEV1", color = "") +
      theme_minimal()
    ggplotly(p)
  })

  output$adherence_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = Age)) +
      geom_line(aes(y = Adherence, color = "Adherence (%)")) +
      labs(y = "Adherence (%)", color = "") +
      scale_color_manual(values = c("Adherence (%)" = "#0000FF")) +
      ylim(0, 100) +
      theme_minimal()
    ggplotly(p)
  })

  output$pef_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = Age)) +
      geom_line(aes(y = pef, color = "PEF (L/s)")) +
      labs(y = "PEF", color = "") +
      theme_minimal()
    ggplotly(p)
  })

  output$eosinophils_feno_plot <- renderPlotly({
    patient <- selected_patient()

    plot_ly(data = patient) %>%
      add_trace(
        x = ~Age,
        y = ~eosinophil_level,
        name = "Eosinophil Level",
        type = "scatter",
        mode = "lines",
        line = list(color = "#1f77b4")
      ) %>%
      add_trace(
        x = ~Age,
        y = ~FeNO_ppb,
        name = "FeNO (ppb)",
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        line = list(color = "#ff7f0e")
      ) %>%
      layout(
        yaxis = list(
          title = "Eosinophil Level",
          side = "left"
        ),
        yaxis2 = list(
          title = "FeNO (ppb)",
          overlaying = "y",
          side = "right"
        ),
        showlegend = TRUE
      )
  })

  observeEvent(input$edit_options, {
    showModal(modalDialog(
      title = "Edit Patient Options",
      div(
        style = "text-align: center;",
        actionButton("edit_smoking", "Edit Smoking Status",
          style = "margin: 5px;"
        ),
        actionButton("perfect_adherence", "Set Perfect Adherence",
          style = "margin: 5px;"
        ),
        actionButton("avoid_allergens", "Avoid Allergens",
          style = "margin: 5px;"
        )
      ),
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$edit_smoking, {
    removeModal()
    patient <- selected_patient() %>% tail(1)
    showModal(modalDialog(
      title = "Edit Smoking Status",
      selectInput("new_smoking_status", "Smoking Status:",
        choices = c("Never Smoked", "Former Smoker", "Current Smoker"),
        selected = patient$`Smoking Status`
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_smoking", "Save")
      )
    ))
  })

  observeEvent(input$perfect_adherence, {
    removeModal()
    last_row <- which(rv$patient_data$ID == input$patient_id)[length(which(rv$patient_data$ID == input$patient_id))]
    rv$patient_data$Adherence[last_row] <- 100
    showModal(modalDialog(
      title = "Success",
      "Last adherence value set to 100%",
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$avoid_allergens, {
    removeModal()
    # Reduce all IgE values by 50%
    allergen_cols <- c(
      "ige_pollen", "ige_cats", "ige_dogs", "ige_mould",
      "ige_grass", "ige_house_dust_mites"
    )
    for (col in allergen_cols) {
      patient_data[[col]][patient_data$ID == input$patient_id] <-
        patient_data[[col]][patient_data$ID == input$patient_id] * 0.5
    }
    showModal(modalDialog(
      title = "Success",
      "Allergen exposure reduced by 50%",
      footer = modalButton("Close")
    ))
  })
}

# Run the application
shinyApp(ui = ui, server = server)
