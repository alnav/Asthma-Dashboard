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
  titlePanel("Asthma Patient Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      selectInput("patient_id", "Select Patient ID:", choices = unique(patient_data$ID)),
      h3("Patient Information"),
      htmlOutput("patient_info"),
      actionButton("edit_smoking", "Edit Smoking Status"),
      h3("Lung Function"),
      htmlOutput("latest_lung_function"),
      h3("Other Information"),
      htmlOutput("latest_other_info")
    ),
    
    mainPanel(
  width = 9,
  fluidRow(
column(12, align = "center",
      h3("Risk of Exacerbation in Next Month"),
      div(
        style = "width: 100%; height: 200px; display: flex; justify-content: center; align-items: center;",
        gaugeOutput("risk_gauge")
    ))
  ),
  
  fluidRow(
    column(6,
      h3("FEV1 over time"),
      plotlyOutput("lung_function_plot")
    ),
    column(6,
      h3("PEF Over Time"),
      plotlyOutput("pef_plot")
    )
  ),

    fluidRow(
      column(6,
        h3("Adherence Over Time"),
        plotlyOutput("adherence_plot"),
      ),
      column(6,
        h3("Eosinophils and FeNO Over Time"),
      plotlyOutput("eosinophils_feno_plot")
      )
    )  
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  selected_patient <- reactive({
    patient_data[patient_data$ID == input$patient_id, ]
  })
  
  output$risk_gauge <- renderGauge({
    patient <- selected_patient() %>% tail(1)
    # Placeholder risk calculation - replace with actual model
    risk_value <- runif(1, 0, 100)
  gauge(
    risk_value, 
    min = 0, 
    max = 100, 
    symbol = '%',
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
      tags$p(paste("BMI:", patient$BMI)),
      tags$p(paste("Ethnicity:", patient$Ethnicity)),
      tags$p(paste("Smoking Status:", patient$`Smoking Status`)),
      tags$p(paste("Asthma Severity:", patient$`Asthma Severity`))
    )
  })
  
  output$latest_lung_function <- renderUI({
    patient <- selected_patient() %>% tail(1)
    tags$div(
      tags$p(paste("FVC Actual:", patient$fvc_actual)),
      tags$p(paste("FEV1 Actual:", patient$fev1_actual)),
      tags$p(paste("FEV1/FVC Ratio:", patient$fev1_fvc_ratio)),
      tags$p(paste("PEF:", patient$pef))
    )
  })
  
  output$latest_other_info <- renderUI({
    patient <- selected_patient() %>% tail(1)
    tags$div(
      tags$p(paste("Eosinophil Level:", patient$eosinophil_level)),
      tags$p(paste("FeNO (ppb):", patient$FeNO_ppb)),
      tags$p(paste("Total IgE:", patient$total_ige)),
      tags$p(paste("IgE Pollen:", patient$ige_pollen)),
      tags$p(paste("IgE Cats:", patient$ige_cats)),
      tags$p(paste("IgE Dogs:", patient$ige_dogs)),
      tags$p(paste("IgE Mould:", patient$ige_mould)),
      tags$p(paste("IgE Grass:", patient$ige_grass)),
      tags$p(paste("IgE House Dust Mites:", patient$ige_house_dust_mites)),
      tags$p(paste("Treatment:", patient$treatment))
    )
  })
  
  output$lung_function_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = Age)) +
      #geom_line(aes(y = fvc_actual, color = "FVC Actual")) +
      geom_line(aes(y = fev1_actual, color = "FEV1 (Litres)")) +
      #geom_line(aes(y = fev1_fvc_ratio, color = "FEV1/FVC Ratio")) +
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
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#1f77b4')
    ) %>%
    add_trace(
      x = ~Age,
      y = ~FeNO_ppb,
      name = "FeNO (ppb)",
      yaxis = "y2",
      type = 'scatter',
      mode = 'lines',
      line = list(color = '#ff7f0e')
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

observeEvent(input$edit_smoking, {
    patient <- selected_patient() %>% tail(1)
    showModal(modalDialog(
      title = "Edit Smoking Status",
      selectInput("new_smoking_status", "Smoking Status:",
                 choices = c("Never Smoked", "Former Smoker", "Current Smoker"),
                 selected = patient$`Smoking Status`),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_smoking", "Save")
      )
    ))
  })
  
  observeEvent(input$save_smoking, {
    patient_data$`Smoking Status`[patient_data$ID == input$patient_id] <- input$new_smoking_status
    removeModal()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)