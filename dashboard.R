library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(flexdashboard)
library(rmarkdown)

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
      dateRangeInput("date_range",
        "Select Date Range:",
        start = min(patient_data$Date),
        end = max(patient_data$Date),
        min = min(patient_data$Date),
        max = max(patient_data$Date)
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
      ),
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
      )
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(
          8, # Increase the width of the ACT chart
          h3("Asthma Control Test (ACT) Score"),
          plotlyOutput("act_plot")
        ),
        column(
          4, # Gauge column
          align = "center",
          h3("Risk of exacerbation in the next month"),
          div(
            style = "width: 100%;
            height: 200px;
            display: flex;
            justify-content: center;
            align-items: center; padding-left: 0;",
            gaugeOutput("risk_gauge")
          ),
          uiOutput("risk_factors") # Add the dynamic risk factors box
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
      style = "margin-top: 10px; padding: 10px; border: 1px solid #ddd; border-radius: 5px; background-color: #f9f9f9; font-size: 12px; color: #666;",
      h5("Risk Factors:", style = "font-weight: bold; color: red;"),
      p(factors_text)
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
    

    
p <- ggplot(patient, aes(x = review_date)) +
  geom_line(aes(y = fev1_actual, color = "FEV1 (Litres)")) +
  geom_point(
    aes(
      y = fev1_actual,
      text = paste0(
        "Date: ", format(review_date, "%d/%m/%Y"), "<br>",
        "FEV1: ", sprintf("%.2f L", fev1_actual), "<br>",
        "FEV1 % predicted: ", sprintf("%.0f%%", fev1_percent_predicted)
      )
    ),
    color = "red"
  ) +
  geom_text(
    aes(
      y = fev1_actual + 0.05,
      label = sprintf("%.2f (%.0f%%)", fev1_actual, fev1_percent_predicted)
    ),
    size = 3.5
  ) +
  labs(y = "FEV1", color = "") +
  theme_minimal()

ggplotly(p, tooltip = "text") %>%
  layout(
    showlegend = FALSE,
    xaxis = list(title = "")
  )

  })

  output$adherence_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = review_date)) +
      geom_hline(yintercept = 80, color = "red", linetype = "dashed") +
      geom_line(aes(y = Adherence, color = "Adherence (%)")) +
      geom_point(aes(y = Adherence), color = "#0000FF") +
      geom_text(aes(y = Adherence + 10, label = sprintf("%.0f%%", Adherence)),
        size = 3.5
      ) +
      labs(y = "Adherence (%)", color = "") +
      scale_color_manual(values = c("Adherence (%)" = "#0000FF")) +
      scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +
      theme_minimal()
    ggplotly(p)%>%
    layout(
      showlegend = FALSE,
      xaxis = list(
        title = ""
      )
    )
})

  output$pef_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = review_date)) +
      geom_line(aes(y = pef, color = "PEF (L/s)")) +
      geom_point(aes(y = pef), color = "red") +
      geom_text(
        aes(y = pef + 10, label = sprintf("%.0f", pef)),
        size = 3.5
      ) +
      labs(y = "PEF", color = "") +
      theme_minimal()
    ggplotly(p)%>%
    layout(
      showlegend = FALSE,
      xaxis = list(
        title = ""
      )
    )
})

  output$eosinophils_feno_plot <- renderPlotly({
    patient <- selected_patient()

    plot_ly(data = patient) %>%
      add_trace(
        x = ~review_date,
        y = ~eosinophil_level,
        name = "Eosinophil Level",
        type = "scatter",
        mode = "lines",
        line = list(color = "red")
      ) %>%
      add_trace(
        x = ~review_date,
        y = ~FeNO_ppb,
        name = "FeNO (ppb)",
        yaxis = "y2",
        type = "scatter",
        mode = "lines",
        line = list(color = "blue")
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
        xaxis = list(
          title = "",
          showgrid = TRUE
        ),
        showlegend = TRUE
      )
  })

  output$act_plot <- renderPlotly({
    patient <- selected_patient()

    # Define control zones for visualization
    poor_control <- data.frame(x = c(min(patient$Age), max(patient$Age)), y = c(15, 15))
    not_well_controlled <- data.frame(x = c(min(patient$Age), max(patient$Age)), y = c(19, 19))

    p <- ggplot(patient, aes(x = Age)) +
      # Add colored background zones
      geom_rect(aes(xmin = min(Age), xmax = max(Age), ymin = 5, ymax = 15),
        fill = "#ffcccb", alpha = 0.3
      ) +
      geom_rect(aes(xmin = min(Age), xmax = max(Age), ymin = 16, ymax = 19),
        fill = "#ffffcc", alpha = 0.3
      ) +
      geom_rect(aes(xmin = min(Age), xmax = max(Age), ymin = 20, ymax = 25),
        fill = "#ccffcc", alpha = 0.3
      ) +
      # Add zone separator lines
      geom_line(data = poor_control, aes(x = x, y = y), linetype = "dashed", color = "red") +
      geom_line(data = not_well_controlled, aes(x = x, y = y), linetype = "dashed", color = "orange") +
      # Add ACT scores line and points
      geom_line(aes(y = act_score, color = "ACT Score")) +
      geom_point(aes(y = act_score), size = 3) +
      # Add text annotations for latest value
      geom_text(
        data = tail(patient, 1),
        aes(y = act_score + 1, label = sprintf("%d", act_score)),
        size = 3.5
      ) +
      # Formatting
      scale_y_continuous(limits = c(5, 25), breaks = seq(5, 25, 5)) +
      scale_color_manual(values = c("ACT Score" = "#9467bd")) +
      labs(y = "ACT Score", color = "") +
      theme_minimal() +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "none" 
      )

    ggplotly(p) %>%
      layout(
        annotations = list(
          list(
            x = min(patient$Age)+0.4,
            y = 10,
            text = "Poor Control",
            showarrow = FALSE,
            font = list(color = "red")
          ),
          list(
            x = min(patient$Age)+0.4,
            y = 17.5,
            text = "Not Well Controlled",
            showarrow = FALSE,
            font = list(color = "orange")
          ),
          list(
            x = min(patient$Age)+0.4,
            y = 22.5,
            text = "Well Controlled",
            showarrow = FALSE,
            font = list(color = "green")
          )
        )
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

  observeEvent(input$save_smoking, {
    last_row <- which(rv$patient_data$ID == input$patient_id)[length(which(rv$patient_data$ID == input$patient_id))]
    rv$patient_data$`Smoking Status`[last_row] <- input$new_smoking_status
    removeModal()
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

  # 4. Add a button to update ACT score in your Edit Patient modal
  # Modify your observeEvent(input$edit_options, {...}) function

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
        # Add this new button
        actionButton("update_act", "Update ACT Score",
          style = "margin: 5px;"
        )
      ),
      footer = modalButton("Close")
    ))
  })

  # 5. Add the event handler for the ACT update button

  observeEvent(input$update_act, {
    removeModal()
    patient <- selected_patient() %>% tail(1)
    showModal(modalDialog(
      title = "Update ACT Score",

      # Create a small form for the 5 ACT questions
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

      # Add calculation summary that updates when options are selected
      uiOutput("act_calculation"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_act", "Save ACT Score")
      )
    ))
  })

  

# Modify the download handler
output$download_report <- downloadHandler(
  filename = function() {
    # Generate unique filename with patient ID and date
    patient <- selected_patient() %>% tail(1)
    paste0("patient_report_", patient$ID, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
  },
  content = function(file) {
    # Create a temporary Rmd file
    tempReport <- file.path(tempdir(), "report.Rmd")
    
    # Get patient data
    patient <- selected_patient()
    latest <- tail(patient, 1)
    
    # Write the report content
    writeLines(sprintf('
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
      - Latest ACT Score: %d/25
      - Latest FEV1: %.2f L (%d%% predicted)
      - Latest FeNO: %.1f ppb
      - Latest Eosinophil Count: %.2f
      
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
      latest$act_score, latest$fev1_actual, 
      latest$fev1_percent_predicted,
      latest$FeNO_ppb, latest$eosinophil_level,
      latest$BMI, latest$`Smoking Status`,
      calculate_risk(latest),
      latest$treatment
    ), tempReport)
    
    # Render the report
    rmarkdown::render(tempReport, output_file = file,
                     quiet = TRUE)
  }
)
    
    # Add download handler for the report
    output$download_report <- downloadHandler(
      filename = function() {
        paste("patient_report_", Sys.Date(), ".pdf", sep = "")
      },
      content = function(file) {
        pdf(file, width = 8, height = 10)
        print(report_text())
        dev.off()
      }
    )
  
    # 5. Add a plotly timeline for exacerbation dates
output$exacerbation_timeline <- renderPlotly({
  patient <- selected_patient()
  
  # Get date range from patient data
  date_range <- range(as.Date(patient$review_date))
  start_date <- date_range[1]
  end_date <- date_range[2]
  
  # Convert comma-separated dates to vector safely
  exacerbation_dates <- NULL
  if (!is.null(patient$exacerbation_dates)) {
    dates_str <- patient$exacerbation_dates[patient$exacerbation_dates != "None"]
    if (length(dates_str) > 0) {
      date_vector <- unlist(strsplit(dates_str, ","))
      exacerbation_dates <- as.Date(trimws(date_vector), format = "%Y-%m-%d")
    }
  }
  
  # Create base plot with date range
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
        range = list(start_date, end_date)  # Use actual date range
      ),
      showlegend = FALSE
    )
  
  # Add markers and text labels if there are exacerbation dates
  if (!is.null(exacerbation_dates) && length(exacerbation_dates) > 0) {
    p <- p %>% add_trace(
      type = "scatter",
      mode = "markers+text",
      x = exacerbation_dates,
      y = rep(1, length(exacerbation_dates)),
      text = format(exacerbation_dates, "%d/%m/%Y"),
      textposition = "top center",
      marker = list(
        color = "red",
        size = 10,
        symbol = "diamond"
      ),
      hovertemplate = paste0(
        "Date: %{x|%d/%m/%Y}",
        "<extra></extra>"
      )
    )
  }
  
  p
})
  
  

  # 6. Add reactive calculation of ACT score
  output$act_calculation <- renderUI({
    # Calculate total from all questions
    q1 <- as.numeric(input$act_q1)
    q2 <- as.numeric(input$act_q2)
    q3 <- as.numeric(input$act_q3)
    q4 <- as.numeric(input$act_q4)
    q5 <- as.numeric(input$act_q5)

    # Handle NAs
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

  # 7. Add event handler to save the ACT score
  observeEvent(input$save_act, {
    # Calculate total from all questions
    q1 <- as.numeric(input$act_q1)
    q2 <- as.numeric(input$act_q2)
    q3 <- as.numeric(input$act_q3)
    q4 <- as.numeric(input$act_q4)
    q5 <- as.numeric(input$act_q5)

    # Check if all questions are answered
    if (any(is.na(c(q1, q2, q3, q4, q5)))) {
      showModal(modalDialog(
        title = "Error",
        "Please answer all questions to calculate ACT score",
        footer = modalButton("OK")
      ))
      return()
    }

    total <- q1 + q2 + q3 + q4 + q5

    # Update the ACT score in the dataset
    last_row <- which(rv$patient_data$ID == input$patient_id)
    last_row <- last_row[length(last_row)]
    rv$patient_data$act_score[last_row] <- total

    removeModal()

    # Show confirmation
    showModal(modalDialog(
      title = "Success",
      sprintf("ACT Score updated to %d/25", total),
      footer = modalButton("Close")
    ))
  })

  # Add this after other observers in the server function
  observeEvent(input$generate_report, {
    patient <- selected_patient()
    latest <- tail(patient, 1)
    
    # Calculate yearly statistics
    one_year_ago <- Sys.Date() - 365
    yearly_data <- patient[as.Date(patient$review_date) >= one_year_ago, ]
    
    avg_adherence <- mean(yearly_data$Adherence, na.rm = TRUE)
    n_exacerbations <- length(unlist(strsplit(latest$exacerbation_dates[latest$exacerbation_dates != "None"], ",")))
    
    # Generate report text
    report_text <- tags$div(
      style = "font-family: Arial; line-height: 1.5; padding: 20px;",
      
      tags$h3("Patient Summary Report", 
             style = "color: #2c3e50; border-bottom: 2px solid #2c3e50;"),
      
      tags$h4("Patient Information"),
      tags$p(HTML(sprintf(
        "Name: %s<br>
         NHS Number: %s<br>
         Date of Birth: %s<br>
         Age: %.0f<br>
         Sex: %s<br>
         Ethnicity: %s<br>
         Asthma Severity: %s",
        latest$Name, 
        latest$nhs_number,
        format(as.Date(latest$birth_date), "%d/%m/%Y"),
        as.numeric(latest$Age),
        latest$Sex,
        latest$Ethnicity,
        latest$`Asthma Severity`
      ))),
      
      tags$h4("Clinical Metrics (Last 12 Months)"),
      tags$p(HTML(sprintf(
        "Average Adherence: %.1f%%<br>
         Number of Exacerbations: %.0f<br>
         Latest ACT Score: %.0f/25<br>
         Latest FEV1: %.2f L (%.0f%% predicted)<br>
         Latest FeNO: %.1f ppb<br>
         Latest Eosinophil Count: %.2f",
        avg_adherence,
        n_exacerbations,
        as.numeric(latest$act_score),
        latest$fev1_actual,
        latest$fev1_percent_predicted,
        latest$FeNO_ppb,
        latest$eosinophil_level
      ))),
      
      tags$h4("Risk Factors"),
      tags$p(HTML(sprintf(
        "BMI: %.1f<br>
         Smoking Status: %s<br>
         Current Risk Score: %.1f%%",
        latest$BMI,
        latest$`Smoking Status`,
        calculate_risk(latest)
      ))),
      
      tags$h4("Current Treatment"),
      tags$p(latest$treatment)
    )
    
    # Show modal with report
    showModal(modalDialog(
      title = "Patient Report",
      size = "l",
      report_text,
      easyClose = TRUE,
      footer = tagList(
        downloadButton("download_report_pdf", "Download PDF"),
        modalButton("Close")
      )
    ))
  })

  # Add the download handler for PDF export
  output$download_report_pdf <- downloadHandler(
    filename = function() {
      patient <- selected_patient() %>% tail(1)
      paste0("patient_report_", patient$ID, "_", format(Sys.Date(), "%Y%m%d"), ".pdf")
    },
    content = function(file) {
      # Create a temporary Rmd file
      tempReport <- file.path(tempdir(), "report.Rmd")
      patient <- selected_patient()
      latest <- tail(patient, 1)
      
      writeLines(sprintf('
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
  - Latest ACT Score: %d/25
  - Latest FEV1: %.2f L (%d%% predicted)
  - Latest FeNO: %.1f ppb
  - Latest Eosinophil Count: %.2f
  
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
        latest$act_score, latest$fev1_actual,
        latest$fev1_percent_predicted,
        latest$FeNO_ppb, latest$eosinophil_level,
        latest$BMI, latest$`Smoking Status`,
        calculate_risk(latest),
        latest$treatment
      ), tempReport)
      
      # Render the report
      rmarkdown::render(tempReport, output_file = file,
                       quiet = TRUE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
