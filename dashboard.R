library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(flexdashboard)
library(rmarkdown)
library(knitr)
library(tinytex)

# Load the dataset
patient_data <- read_csv("patient_dataset.csv")

# UI
ui <- fluidPage(
  titlePanel("ASTHMA DASHBOARD v1.0"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectizeInput("patient_id", "Select Patient ID:",
        choices = unique(patient_data$ID),
        selected = unique(patient_data$ID)[1],
      ),
      div(
        style = "display: flex; align-items: center;",
        dateRangeInput("date_range",
          "Select Date Range:",
          start = min(as.Date(patient_data$review_date)),
          end = max(as.Date(patient_data$review_date)),
          min = min(as.Date(patient_data$review_date)),
          max = max(as.Date(patient_data$review_date))
        ),
        tags$em(
          "Date range selection not currently available",
          style = "margin-left: 10px; color: #666; font-size: 12px;"
        )
      ),
      h3("Patient", style = "font-weight: bold;"),
      div(
        style = "display: flex; align-items: center;",
        h5("Name:", style = "font-weight: bold; margin-right: 5px;"),
        htmlOutput("patient_name")
      ),
      div(
        style = "display: flex; align-items: center;",
        h5("Date of Birth:", style = "font-weight: bold; margin-right: 5px;"),
        htmlOutput("patient_dob")
      ),
      div(
        style = "display: flex; align-items: center;",
        h5("NHS Number:", style = "font-weight: bold; margin-right: 5px;"),
        htmlOutput("patient_nhs")
      ),
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
        style = "position: absolute; bottom:-50px; width: 50%;",
        actionButton("generate_report", "Generate Report",
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
      ),
      downloadButton("download_report", "Download Report", style = "display: none;")
    ),
    mainPanel(
      width = 9,
      fluidRow(
  column(
    8, 
    h3("Asthma Control Test (ACT) Score"),
    plotlyOutput("act_plot")
  ),
  column(
  4,
  align = "center",
  div(
    style = "text-align: center;",
    h3("Risk of exacerbation in the next month"),
    tags$span(
      style = "color: #666; font-style: italic; font-size: 14px;",
      "(proof of concept)"
    )
  ),
  div(
    style = "width: 100%;
    height: 200px;
    display: flex;
    justify-content: center;
    align-items: center; padding-left: 0;",
    gaugeOutput("risk_gauge")
  ),
  uiOutput("risk_factors")
)
),
      fluidRow(
        column(
          6,
          h3("FEV1 and FVC"),
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
          h3("Adherence %"),
          plotlyOutput("adherence_plot"),
        ),
        column(
          6,
          h3("Eosinophils and FeNO"),
          plotlyOutput("eosinophils_feno_plot")
        )
      ),
      fluidRow(
        column(
          12,
          h3("Exacerbation Timeline"),
          plotlyOutput("exacerbation_timeline")
        )
      )
    )
  )
)

# server logic
server <- function(input, output, session) {
  # Create reactive values storage
  rv <- reactiveValues(
    patient_data = patient_data
  )

  # Update selected_patient to use reactive data
  selected_patient <- reactive({
    rv$patient_data[rv$patient_data$ID == input$patient_id, ]
  })

  output$patient_name <- renderUI({
    patient <- selected_patient() %>% tail(1)
    tags$span(patient$Name)
  })

  output$patient_dob <- renderUI({
    patient <- selected_patient() %>% tail(1)
    formatted_date <- format(as.Date(patient$birth_date), "%d/%m/%Y")
    tags$span(formatted_date)
  })

  output$patient_nhs <- renderUI({
    patient <- selected_patient() %>% tail(1)
    tags$span(patient$nhs_number)
  })

  calculate_risk <- function(patient) {
    # Print debug info
    print(sprintf("FEV1: %f", patient$fev1_percent_predicted))
    print(sprintf("Eos: %f", patient$eosinophil_level))
    print(sprintf("IgE: %f", patient$total_ige))
    print(sprintf("FeNO: %f", patient$FeNO_ppb))
    print(sprintf("Adherence: %f", patient$Adherence))
    print(sprintf("Smoking: %s", patient$`Smoking Status`))
    print(sprintf("BMI: %s", patient$BMI))
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
    beta_bmi <- 0.01 # BMI contribution to risk

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
      0 
    )

    severity_numeric <- switch(patient$`Asthma Severity`,
      "Mild" = 0,
      "Moderate" = 1,
      "Severe" = 2,
      0 
    )
    # Calculate log odds
    log_odds <- beta_0 +
      beta_fev1 * patient$fev1_percent_predicted +
      beta_eos * patient$eosinophil_level +
      beta_ige * patient$total_ige +
      beta_severity * severity_numeric +
      beta_smoking[smoking_numeric + 1] +
      beta_feno * patient$FeNO_ppb +
      beta_adherence * patient$Adherence +
      beta_bmi * patient$BMI

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

  output$risk_factors <- renderUI({
    patient <- selected_patient() %>% tail(1)
    factors <- c()

    if (!is.na(patient$eosinophil_level) && patient$eosinophil_level > 0.45) {
      factors <- c(factors, "High eosinophil levels")
    }
    if (!is.na(patient$FeNO_ppb) && patient$FeNO_ppb > 25) {
      factors <- c(factors, "Elevated FeNO")
    }
    if (!is.na(patient$Adherence) && patient$Adherence < 80) {
      factors <- c(factors, "Poor adherence")
    }
    if (!is.na(patient$`Smoking Status`) && patient$`Smoking Status` == "Current Smoker") {
      factors <- c(factors, "Smoking status")
    }
    if (!is.na(patient$`Asthma Severity`) && patient$`Asthma Severity` == "Severe") {
      factors <- c(factors, "Asthma severity")
    }
    if (!is.na(patient$BMI) && patient$BMI > 24) {
      factors <- c(factors, "High BMI")
    }

    if (length(factors) == 0) {
      factors_text <- "No significant risk factors identified."
    } else {
      factors_text <- paste(factors, collapse = ", ")
    }

    div(
      style = "margin-top: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9;",
      h5("Risk Factors:", style = "font-weight: bold; color: red;"),
      p(factors_text, style = "font-size: 12px; color: #666; margin-bottom: 15px;"),

      actionButton("edit_options", "Modify Risk Factors",
        style = "
          background-color: #880808;
          color: white;
          font-weight: bold;
          padding: 10px 15px;
          width: 100%;
          border-radius: 5px;
          border: none;
          transition: background-color 0.3s;
          margin-top: 10px;
        "
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
      tags$p(paste("Asthma Severity:", patient$`Asthma Severity`)),
      tags$p(paste("Job:", patient$Job)),
      tags$p(paste("Pets:", paste(patient$Pet, collapse = ", "))),
    )
  })

  output$latest_lung_function <- renderUI({
    patient <- selected_patient()
    latest <- tail(patient, 1)
    previous <- tail(patient, 2)[1, ]


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
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range >0.35'>Pneumococcal Antibodies: <span style='color: %s'>%s</span></span>",
          ifelse(patient$pneumococcal_abs < 0.35, "red", "black"),
          patient$pneumococcal_abs
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range >0.35'>Tetanus Antibodies: <span style='color: %s'>%s</span></span>",
          ifelse(patient$tetanus_abs < 0.35, "red", "black"),
          patient$tetanus_abs
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range 0.9-1.8 g/L'>C3: <span style='color: %s'>%s</span> g/L</span>",
          ifelse(patient$c3 < 0.9 | patient$c3 > 1.8, "red", "black"),
          patient$c3
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range 0.1-0.4 g/L'>C4: <span style='color: %s'>%s</span> g/L</span>",
          ifelse(patient$c4 < 0.1 | patient$c4 > 0.4, "red", "black"),
          patient$c4
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range Negative'>ANCA: <span style='color: %s'>%s</span></span>",
          ifelse(patient$anca != "Negative", "red", "black"),
          patient$anca
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range Negative'>ANA: <span style='color: %s'>%s</span></span>",
          ifelse(patient$ana != "Negative", "red", "black"),
          patient$ana
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='Normal range Negative'>Quantiferon: <span style='color: %s'>%s</span></span>",
          ifelse(patient$quantiferon != "Negative", "red", "black"),
          patient$quantiferon
        )
      ))),
      tags$p(HTML(paste(
        sprintf(
          "<span title='ACT Score: 5-15 (Poor), 16-19 (Not well), 20-25 (Well controlled)'>ACT Score: <span style='color: %s; font-weight: bold'>%s</span>/25</span>",
          ifelse(patient$act_score >= 20, "green",
            ifelse(patient$act_score >= 16, "orange", "red")
          ),
          patient$act_score
        )
      ))),
      tags$p(paste("Treatment:", patient$treatment))
    )
  })

  output$lung_function_plot <- renderPlotly({
    patient <- selected_patient()


    patient$review_date <- as.Date(patient$review_date)

    plot_ly(data = patient) %>%
      add_trace(
        x = ~review_date,
        y = ~fev1_actual,
        name = "FEV1",
        type = "scatter",
        mode = "lines+markers+text",
        line = list(color = "red"),
        text = ~ sprintf("%.1f, %.0f%%", fev1_actual, as.numeric(fev1_percent_predicted)),
        textposition = "top",
        hovertemplate = paste0(
          "FEV1: %{y:.2f} L (%{text})",
          "<extra></extra>"
        ),
        textfont = list(size = 12)
      ) %>%
      add_trace(
        x = ~review_date,
        y = ~fvc_actual,
        name = "FVC",
        yaxis = "y2",
        type = "scatter",
        mode = "lines+markers+text",
        line = list(color = "blue"),
        text = ~ sprintf("%.1f, %.0f%%", fvc_actual, as.numeric(fvc_percent_predicted)),
        textposition = "top",
        hovertemplate = paste0(
          "FVC: %{y:.2f} L (%{text})",
          "<extra></extra>"
        ),
        textfont = list(size = 12)
      ) %>%
      layout(
        yaxis = list(
          title = "FEV1 (L)",
          side = "left"
        ),
        yaxis2 = list(
          title = "FVC (L)",
          overlaying = "y",
          side = "right"
        ),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%d/%m/%Y",
          showgrid = TRUE,
          ticktext = ~ format(review_date, "%d/%m/%Y"),
          tickvals = ~review_date,
          tickmode = "array"
        ),
        showlegend = TRUE,
        hovermode = "x unified"
      )
  })

  output$adherence_plot <- renderPlotly({
    patient <- selected_patient()


    patient$review_date <- as.Date(patient$review_date)

    plot_ly(data = patient) %>%

      add_trace(
        x = c(min(patient$review_date), max(patient$review_date)),
        y = c(80, 80),
        type = "scatter",
        mode = "lines",
        line = list(color = "red", dash = "dash"),
        showlegend = FALSE,
        hoverinfo = "none"
      ) %>%

      add_trace(
        x = ~review_date,
        y = ~Adherence,
        name = "Adherence (%)",
        type = "scatter",
        mode = "lines+markers+text",
        line = list(color = "#0000FF"),
        text = ~ sprintf("%.0f%%", Adherence),
        textposition = "top center",
        marker = list(color = "#0000FF", size = 8),
        hovertemplate = paste0(
          "Date: %{x|%d/%m/%Y}<br>",
          "Adherence: %{y:.0f}%",
          "<extra></extra>"
        ),
        textfont = list(size = 12)
      ) %>%
      layout(
        yaxis = list(
          title = "Adherence (%)",
          range = c(0, 100),
          dtick = 20
        ),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%d/%m/%Y",
          showgrid = TRUE,
          ticktext = ~ format(review_date, "%d/%m/%Y"),
          tickvals = ~review_date,
          tickmode = "array"
        ),
        showlegend = FALSE,
        hovermode = "x unified"
      )
  })

  output$pef_plot <- renderPlotly({
    patient <- selected_patient()


    patient$review_date <- as.Date(patient$review_date)

    plot_ly(data = patient) %>%
      add_trace(
        x = ~review_date,
        y = ~pef,
        name = "PEF (L/s)",
        type = "scatter",
        mode = "lines+markers+text",
        line = list(color = "red"),
        text = ~ sprintf("%.0f", pef),
        textposition = "top center",
        marker = list(color = "red", size = 8),
        hovertemplate = paste0(
          "Date: %{x|%d/%m/%Y}<br>",
          "PEF: %{y:.0f} L/s",
          "<extra></extra>"
        ),
        textfont = list(size = 12)
      ) %>%
      layout(
        yaxis = list(
          title = "PEF (L/s)",
          side = "left"
        ),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%d/%m/%Y",
          showgrid = TRUE,
          ticktext = ~ format(review_date, "%d/%m/%Y"),
          tickvals = ~review_date,
          tickmode = "array"
        ),
        showlegend = FALSE,
        hovermode = "x unified"
      )
  })

output$eosinophils_feno_plot <- renderPlotly({
    patient <- selected_patient()

    patient$review_date <- as.Date(patient$review_date)

    plot_ly(data = patient) %>%
      add_trace(
        x = ~review_date,
        y = ~eosinophil_level,
        name = "Eosinophil Level",
        type = "scatter",
        mode = "lines+markers+text",
        line = list(color = "red"),
        text = ~ sprintf("%.2f", eosinophil_level),
        textposition = "top center",
        hovertemplate = paste0(
          "Eosinophils: %{y:.2f} x 10^9/L",
          "<extra></extra>"
        ),
        textfont = list(size = 12)
      ) %>%
      add_trace(
        x = ~review_date,
        y = ~FeNO_ppb,
        name = "FeNO (ppb)",
        yaxis = "y2",
        type = "scatter",
        mode = "lines+markers+text",
        line = list(color = "blue"),
        text = ~ sprintf("%.0f", FeNO_ppb),
        textposition = "top center",
        hovertemplate = paste0(
          "FeNO: %{y:.0f} ppb",
          "<extra></extra>"
        ),
        textfont = list(size = 12)
      ) %>%
      layout(
        yaxis = list(
          title = "Eosinophil Level (10^9/L)",
          side = "left"
        ),
        yaxis2 = list(
          title = "FeNO (ppb)",
          overlaying = "y",
          side = "right"
        ),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%d/%m/%Y",
          showgrid = TRUE,
          ticktext = ~ format(review_date, "%d/%m/%Y"),
          tickvals = ~review_date,
          tickmode = "array"
        ),
        showlegend = TRUE,
        hovermode = "x unified"
      )
  })

  output$act_plot <- renderPlotly({
    patient <- selected_patient()

    patient$review_date <- as.Date(patient$review_date)

    poor_control <- data.frame(
      x = c(min(patient$review_date), max(patient$review_date)),
      y = c(15, 15)
    )
    not_well_controlled <- data.frame(
      x = c(min(patient$review_date), max(patient$review_date)),
      y = c(19, 19)
    )

    p <- ggplot(patient, aes(x = review_date)) +
      geom_rect(aes(
        xmin = min(review_date),
        xmax = max(review_date),
        ymin = 5,
        ymax = 15
      ), fill = "#ffcccb", alpha = 0.3) +
      geom_rect(aes(
        xmin = min(review_date),
        xmax = max(review_date),
        ymin = 15,
        ymax = 19
      ), fill = "#ffffcc", alpha = 0.3) +
      geom_rect(aes(
        xmin = min(review_date),
        xmax = max(review_date),
        ymin = 19,
        ymax = 25
      ), fill = "#ccffcc", alpha = 0.3) +
      geom_line(
        data = poor_control, aes(x = x, y = y),
        linetype = "dashed", color = "red"
      ) +
      geom_line(
        data = not_well_controlled, aes(x = x, y = y),
        linetype = "dashed", color = "orange"
      ) +
      geom_line(aes(y = act_score, color = "ACT Score")) +
      geom_point(
        aes(
          y = act_score,
          text = paste0(
            "Date: ", format(review_date, "%d/%m/%Y"), "<br>",
            "ACT Score: ", act_score, "/25"
          )
        ),
        color = "red",
        size = 3
      ) +
      geom_text(
        aes(y = act_score + 1, label = sprintf("%d", act_score)),
        size = 3.5
      ) +
      annotate("text",
        x = min(patient$review_date) + 120,
        y = c(10, 17, 22),
        label = c("Poor Control", "Not Well Controlled", "Well Controlled"),
        hjust = 0,
        vjust = 0,
        color = c("red", "orange", "#049404")
      ) +
      scale_y_continuous(limits = c(5, 25), breaks = seq(5, 25, 5)) +
      scale_color_manual(values = c("ACT Score" = "red")) +
      labs(y = "ACT Score", color = "") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "none"
      )

    ggplotly(p, tooltip = "text") %>%
      layout(
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%d/%m/%Y"
        )
      )
  })


  observeEvent(input$edit_bmi, {
    removeModal()
    patient <- selected_patient() %>% tail(1)

    stones <- floor(patient$`Weight (kg)` * 2.20462 / 14)
    pounds <- round((patient$`Weight (kg)` * 2.20462) %% 14, 1)

    showModal(modalDialog(
      title = "Edit BMI",
      div(
        div(
          style = "display: flex; justify-content: space-between; align-items: center;",
          div(
            style = "flex: 1; margin-right: 10px;",
            numericInput("new_weight", "Weight (kg):",
              value = patient$`Weight (kg)`,
              min = 30, max = 200, step = 0.1
            )
          ),
          div(
            style = "flex: 1;",
            verbatimTextOutput("weight_in_stones")
          )
        ),
        numericInput("new_height", "Height (cm):",
          value = patient$`Height (cm)`,
          min = 120, max = 220, step = 0.1
        ),
        verbatimTextOutput("bmi_calculation")
      ),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_bmi", "Save")
      )
    ))
  })

  output$weight_in_stones <- renderText({
    weight_kg <- input$new_weight
    stones <- floor(weight_kg * 2.20462 / 14)
    pounds <- round((weight_kg * 2.20462) %% 14, 1)
    sprintf("(%.0fst %.1flb)", stones, pounds)
  })

  output$bmi_calculation <- renderText({
    weight <- input$new_weight
    height <- input$new_height / 100 
    bmi <- weight / (height^2)
    status <- if (bmi < 18.5) {
      "Underweight"
    } else if (bmi < 25) {
      "Normal"
    } else if (bmi < 30) {
      "Overweight"
    } else {
      "Obese"
    }

    sprintf("Calculated BMI: %.1f (%s)", bmi, status)
  })

  observeEvent(input$save_bmi, {
    req(input$patient_id)

    last_row <- which(rv$patient_data$ID == input$patient_id)
    last_row <- last_row[length(last_row)]

    weight <- input$new_weight
    height <- input$new_height / 100
    bmi <- weight / (height^2)

    rv$patient_data$`Weight (kg)`[last_row] <- weight
    rv$patient_data$`Height (cm)`[last_row] <- height * 100
    rv$patient_data$BMI[last_row] <- bmi

    removeModal()
    showModal(modalDialog(
      title = "Success",
      sprintf("BMI updated to %.1f", bmi),
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

  observeEvent(input$save_smoking, {
    last_row <- which(rv$patient_data$ID == input$patient_id)[length(which(rv$patient_data$ID == input$patient_id))]
    rv$patient_data$`Smoking Status`[last_row] <- input$new_smoking_status
    removeModal()
  })

  observeEvent(input$perfect_adherence, {
    req(input$patient_id)
    removeModal()

    last_row <- which(rv$patient_data$ID == input$patient_id)
    last_row <- last_row[length(last_row)]
    rv$patient_data$Adherence[last_row] <- 100

    showModal(modalDialog(
      title = "Success",
      "Last adherence value set to 100%",
      footer = modalButton("Close")
    ))
  })

  observeEvent(input$avoid_allergens, {
    removeModal()
    current_patient <- input$patient_id

    allergen_cols <- c(
      "ige_pollen", "ige_cats", "ige_dogs", "ige_mould",
      "ige_grass", "ige_house_dust_mites"
    )

    patient_indices <- which(rv$patient_data$ID == current_patient)
    last_row <- patient_indices[length(patient_indices)]

    for (col in allergen_cols) {
      rv$patient_data[[col]][last_row] <- rv$patient_data[[col]][last_row] * 0.5
    }

    rv$patient_data$total_ige[last_row] <- rv$patient_data$total_ige[last_row] * 0.5

    showModal(modalDialog(
      title = "Success",
      "Allergen exposure reduced by 50%",
      footer = modalButton("Close")
    ))
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
        ),
        actionButton("update_act", "Update ACT Score",
          style = "margin: 5px;"
        ),
        actionButton("edit_bmi", "Edit BMI",
          style = "margin: 5px;"
        ),
        footer = modalButton("Close")
      )
    ))
  })


  observeEvent(input$update_act, {
    removeModal()
    patient <- selected_patient() %>% tail(1)
    showModal(modalDialog(
      title = "Update ACT Score",

      div(
        h4("In the past 4 weeks:"),

        # Question 1
        div(
          style = "margin-bottom: 15px;",
          p("1. How much of the time did your asthma keep you from getting as much done at work, school or at home?"),
          radioButtons("act_q1", "",
            choices = c(
              "All of the time" = 1,
              "Most of the time" = 2,
              "Some of the time" = 3,
              "A little of the time" = 4,
              "None of the time" = 5
            ),
            selected = character(0),
            inline = FALSE
          )
        ),

        # Question 2
        div(
          style = "margin-bottom: 15px;",
          p("2. How often have you had shortness of breath?"),
          radioButtons("act_q2", "",
            choices = c(
              "More than once a day" = 1,
              "Once a day" = 2,
              "3 to 6 times a week" = 3,
              "Once or twice a week" = 4,
              "Not at all" = 5
            ),
            selected = character(0),
            inline = FALSE
          )
        ),

        # Question 3
        div(
          style = "margin-bottom: 15px;",
          p("3. How often did your asthma symptoms wake you up at night or earlier than usual?"),
          radioButtons("act_q3", "",
            choices = c(
              "4 or more nights a week" = 1,
              "2-3 nights a week" = 2,
              "Once a week" = 3,
              "Once or twice" = 4,
              "Not at all" = 5
            ),
            selected = character(0),
            inline = FALSE
          )
        ),

        # Question 4
        div(
          style = "margin-bottom: 15px;",
          p("4. How often have you used your rescue inhaler or nebulizer medication?"),
          radioButtons("act_q4", "",
            choices = c(
              "3 or more times per day" = 1,
              "1-2 times per day" = 2,
              "2-3 times per week" = 3,
              "Once a week or less" = 4,
              "Not at all" = 5
            ),
            selected = character(0),
            inline = FALSE
          )
        ),

        # Question 5
        div(
          style = "margin-bottom: 15px;",
          p("5. How would you rate your asthma control?"),
          radioButtons("act_q5", "",
            choices = c(
              "Not controlled at all" = 1,
              "Poorly controlled" = 2,
              "Somewhat controlled" = 3,
              "Well controlled" = 4,
              "Completely controlled" = 5
            ),
            selected = character(0),
            inline = FALSE
          )
        )
      ),

      uiOutput("act_calculation"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_act", "Save ACT Score")
      )
    ))
  })



  output$download_report <- downloadHandler(
    filename = function() {
      patient <- selected_patient() %>% tail(1)
      paste0("patient_report_", patient$ID, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      tempReport <- file.path(tempdir(), "report.Rmd")

      patient <- selected_patient()
      latest <- tail(patient, 1)
      previous <- if (nrow(patient) > 1) patient[nrow(patient) - 1, ] else latest

      writeLines(sprintf(
        '
      ---
      title: "Patient Report"
      output: pdf_document
      ---

      ## Patient Information
      - Name: %s
      - NHS Number: %s
      - Date of Birth: %s
      - Age: %d
      - Sex: %s
      - Ethnicity: %s
      - Asthma Severity: %s

      ## Clinical Metrics
      - ACT Score: %d/25 (%s) [Previous: %d/25 (%s)]
      - FEV1: %.2f L (%d%% predicted) (%s) [Previous: %.2f L (%d%% predicted) (%s)]
      - FeNO: %.1f ppb (%s) [Previous: %.1f ppb (%s)]
      - Eosinophil Count: %.2f (%s) [Previous: %.2f (%s)]

      ## Risk Factors
      - BMI: %.1f
      - Smoking Status: %s
      - Current Risk Score: %.1f%%

      ## Current Treatment
      %s
      ',
        latest$Name, latest$nhs_number,
        format(as.Date(latest$birth_date), "%d/%m/%Y"),
        latest$Age, latest$Sex, latest$Ethnicity,
        latest$`Asthma Severity`,

        latest$act_score,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$act_score,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$fev1_actual,
        latest$fev1_percent_predicted,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$fev1_actual,
        previous$fev1_percent_predicted,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$FeNO_ppb,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$FeNO_ppb,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$eosinophil_level,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$eosinophil_level,
        format(as.Date(previous$review_date), "%d/%m/%Y"),

        latest$BMI,
        latest$`Smoking Status`,
        calculate_risk(latest),

        latest$treatment
      ), tempReport)

      rmarkdown::render(tempReport,
        output_file = file,
        quiet = TRUE
      )
    }
  )


  output$exacerbation_timeline <- renderPlotly({
    patient <- selected_patient()

    date_range <- range(as.Date(patient$review_date))
    start_date <- date_range[1]
    end_date <- date_range[2]

    exacerbation_dates <- NULL
    if (!is.null(patient$exacerbation_dates)) {
      dates_str <- patient$exacerbation_dates[patient$exacerbation_dates != "None"]
      if (length(dates_str) > 0) {
        date_vector <- unlist(strsplit(dates_str, ","))
        exacerbation_dates <- as.Date(trimws(date_vector), format = "%Y-%m-%d")
      }
    }

    p <- plot_ly() %>%
      layout(
        yaxis = list(
          showticklabels = FALSE,
          showgrid = FALSE,
          zeroline = FALSE,
          range = c(0.5, 1.5)
        ),
        xaxis = list(
          title = "",
          type = "date",
          tickformat = "%d/%m/%Y",
          showgrid = TRUE,
          range = list(start_date, end_date)
        ),
        showlegend = FALSE
      )

    if (!is.null(exacerbation_dates) && length(exacerbation_dates) > 0) {
      p <- p %>%
        add_trace(
          type = "scatter",
          mode = "markers+text",
          x = exacerbation_dates,
          y = rep(1, length(exacerbation_dates)),
          text = format(exacerbation_dates, "%d/%m/%Y"),
          textposition = "bottom",
          marker = list(
            color = "red",
            size = 10,
            symbol = "diamond"
          ),
          textfont = list(
            size = 10
          ),
          hovertemplate = paste0(
            "Date: %{x|%d/%m/%Y}",
            "<extra></extra>"
          )
        )
    }

    p
  })


  output$act_calculation <- renderUI({
    q1 <- as.numeric(input$act_q1)
    q2 <- as.numeric(input$act_q2)
    q3 <- as.numeric(input$act_q3)
    q4 <- as.numeric(input$act_q4)
    q5 <- as.numeric(input$act_q5)

    q1 <- if (is.na(q1)) 0 else q1
    q2 <- if (is.na(q2)) 0 else q2
    q3 <- if (is.na(q3)) 0 else q3
    q4 <- if (is.na(q4)) 0 else q4
    q5 <- if (is.na(q5)) 0 else q5

    total <- q1 + q2 + q3 + q4 + q5

    if (total == 0) {
      status <- "Please answer all questions"
      color <- "black"
    } else if (total < 16) {
      status <- "Poorly controlled asthma"
      color <- "red"
    } else if (total < 20) {
      status <- "Not well controlled asthma"
      color <- "orange"
    } else {
      status <- "Well controlled asthma"
      color <- "green"
    }

    tags$div(
      style = "text-align: center; margin-top: 15px; border-top: 1px solid #eee; padding-top: 15px;",
      h4("ACT Score Calculation"),
      tags$p(sprintf("Current total: %d/25", total)),
      tags$p(style = sprintf("font-weight: bold; color: %s;", color), status)
    )
  })

  observeEvent(input$save_act, {
    q1 <- as.numeric(input$act_q1)
    q2 <- as.numeric(input$act_q2)
    q3 <- as.numeric(input$act_q3)
    q4 <- as.numeric(input$act_q4)
    q5 <- as.numeric(input$act_q5)

    if (any(is.na(c(q1, q2, q3, q4, q5)))) {
      showModal(modalDialog(
        title = "Error",
        "Please answer all questions to calculate ACT score",
        footer = modalButton("OK")
      ))
      return()
    }

    total <- q1 + q2 + q3 + q4 + q5

    last_row <- which(rv$patient_data$ID == input$patient_id)
    last_row <- last_row[length(last_row)]
    rv$patient_data$act_score[last_row] <- total

    removeModal()

    showModal(modalDialog(
      title = "Success",
      sprintf("ACT Score updated to %d/25", total),
      footer = modalButton("Close")
    ))
  })



  generateReportContent <- function(latest, previous, format = "txt") {
    if (format == "html") {
      return(tags$div(
        style = "font-family: Arial; line-height: 1.5; padding: 20px;",
        tags$h3("Patient Information"),
        tags$p(HTML(paste("<strong>Name:</strong>", latest$Name))),
        tags$p(HTML(paste("<strong>NHS Number:</strong>", latest$nhs_number))),
        tags$p(HTML(paste("<strong>Date of Birth:</strong>", format(as.Date(latest$birth_date), "%d/%m/%Y")))),
        tags$p(HTML(paste("<strong>Age:</strong>", latest$Age))),
        tags$p(HTML(paste("<strong>Sex:</strong>", latest$Sex))),
        tags$p(HTML(paste("<strong>Ethnicity:</strong>", latest$Ethnicity))),
        tags$h3("Clinical Metrics"),
        tags$p(HTML(paste(
          "<strong>ACT Score:</strong>", sprintf("%.0f/25", latest$act_score), "-", format(as.Date(latest$review_date), "%d/%m/%Y"),
          sprintf("(%.0f/25 on %s)", previous$act_score, format(as.Date(previous$review_date), "%d/%m/%Y"))
        ))),
        tags$p(HTML(paste(
          "<strong>FEV1:</strong>", sprintf("%.2fL (%.0f%%)", latest$fev1_actual, latest$fev1_percent_predicted),
          sprintf(
            "- %s (%.2fL (%.0f%%) on %s)", format(as.Date(latest$review_date), "%d/%m/%Y"),
            previous$fev1_actual, previous$fev1_percent_predicted, format(as.Date(previous$review_date), "%d/%m/%Y")
          )
        ))),
        tags$p(HTML(paste(
          "<strong>PEF:</strong>", sprintf("%.0f L/min", latest$pef), "-", format(as.Date(latest$review_date), "%d/%m/%Y"),
          sprintf("(%.0f on %s)", previous$pef, format(as.Date(previous$review_date), "%d/%m/%Y"))
        ))),
        tags$p(HTML(paste("<strong>FeNO:</strong>", sprintf("%.1fppb", latest$FeNO_ppb), sprintf(
          "- %s (%.1fppb on %s)",
          format(as.Date(latest$review_date), "%d/%m/%Y"), previous$FeNO_ppb, format(as.Date(previous$review_date), "%d/%m/%Y")
        )))),
        tags$p(HTML(paste(
          "<strong>Eosinophil Count:</strong>", sprintf("%.2f", latest$eosinophil_level),
          sprintf(
            "- %s (%.2f on %s)", format(as.Date(latest$review_date), "%d/%m/%Y"),
            previous$eosinophil_level, format(as.Date(previous$review_date), "%d/%m/%Y")
          )
        ))),
        tags$p(HTML(paste("<strong>Current treatment:</strong>", latest$treatment))),
        tags$h3("Risk Factors"),
        tags$p(HTML(paste("<strong>Asthma Severity:</strong>", latest$`Asthma Severity`))),
        tags$p(HTML(paste("<strong>BMI:</strong>", sprintf("%.1f", latest$BMI)))),
        tags$p(HTML(paste("<strong>Smoking Status:</strong>", latest$`Smoking Status`))),
        tags$p(HTML(paste("<strong>Adherence:</strong>", sprintf("%.0f%%", latest$Adherence)))),
        tags$p(HTML(paste("<strong>Current Risk Score:</strong>", sprintf("%.1f%%", calculate_risk(latest))))),
        tags$br(),
        tags$br(),
        tags$p(HTML("<em>Created by Asthma Dashboard 1.0</em>"))
      ))
    } else {
      sprintf(
        "PATIENT REPORT

PATIENT INFORMATION
- Name: %s
- NHS Number: %s
- Date of Birth: %s
- Age: %.0f
- Sex: %s
- Ethnicity: %s

CLINICAL METRICS
* ACT Score: %.0f/25 - %s (%.0f/25 on %s)
* FEV1: %.2fL (%.0f%%) - %s (%.2fL (%.0f%%) on %s)
* PEF: %.0f L/min - %s (%.0f L/min on %s)
* FeNO: %.1f ppb - %s (%.1f ppb on %s)
* Eosinophil Count: %.2f - %s (%.2f on %s)
* Current treatment: %s

RISK FACTORS
- Asthma Severity: %s
- BMI: %.1f
- Smoking Status: %s
- Adherence: %.0f%%
- Current Risk Score: %.1f%%

Created by Asthma Dashboard 1.0
",
        latest$Name, latest$nhs_number,
        format(as.Date(latest$birth_date), "%d/%m/%Y"),
        as.numeric(latest$Age), latest$Sex, latest$Ethnicity,
        as.numeric(latest$act_score),
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        as.numeric(previous$act_score),
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$fev1_actual,
        latest$fev1_percent_predicted,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$fev1_actual,
        previous$fev1_percent_predicted,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$pef,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$pef,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$FeNO_ppb,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$FeNO_ppb,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$eosinophil_level,
        format(as.Date(latest$review_date), "%d/%m/%Y"),
        previous$eosinophil_level,
        format(as.Date(previous$review_date), "%d/%m/%Y"),
        latest$treatment,
        latest$`Asthma Severity`,
        latest$BMI,
        latest$`Smoking Status`,
        latest$Adherence,
        calculate_risk(latest)
      )
    }
  }


  observeEvent(input$generate_report, {
    patient <- selected_patient()
    latest <- tail(patient, 1)
    previous <- if (nrow(patient) > 1) patient[nrow(patient) - 1, ] else latest

    showModal(modalDialog(
      title = "Patient Report",
      size = "l",
      generateReportContent(latest, previous, format = "html"),
      easyClose = TRUE,
      footer = tagList(
        downloadButton("download_report", "Download Report"),
        modalButton("Close")
      )
    ))
  })

  output$download_report <- downloadHandler(
    filename = function() {
      patient <- selected_patient() %>% tail(1)
      paste0("patient_report_", patient$ID, "_", format(Sys.Date(), "%Y%m%d"), ".txt")
    },
    content = function(file) {
      patient <- selected_patient()
      latest <- tail(patient, 1)
      previous <- if (nrow(patient) > 1) patient[nrow(patient) - 1, ] else latest

      writeLines(generateReportContent(latest, previous, format = "text"), file)
    }
  )
}

shinyApp(ui = ui, server = server)
