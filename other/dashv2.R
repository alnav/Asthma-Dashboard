library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)

# Load the dataset
patient_data <- read_csv("patient_dataset.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Asthma Patient Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("patient_id", "Select Patient ID:", choices = unique(patient_data$ID)),
      h3("Patient Information"),
      htmlOutput("patient_info")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Lung Function",
          h3("Latest Lung Function"),
          htmlOutput("latest_lung_function"),
          h3("Lung Function Over Time"),
          plotlyOutput("lung_function_plot")
        ),
        tabPanel("PEF",
          h3("PEF Over Time"),
          plotlyOutput("pef_plot")
        ),
        tabPanel("Eosinophils and FeNO",
          h3("Latest Eosinophils and FeNO"),
          htmlOutput("latest_other_info"),
          h3("Eosinophils and FeNO Over Time"),
          plotlyOutput("eosinophils_feno_plot")
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  selected_patient <- reactive({
    req(input$patient_id)
    patient_data[patient_data$ID == input$patient_id, ]
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
      geom_line(aes(y = fvc_actual, color = "FVC Actual")) +
      geom_line(aes(y = fev1_actual, color = "FEV1 Actual")) +
      geom_line(aes(y = fev1_fvc_ratio, color = "FEV1/FVC Ratio")) +
      labs(y = "Value", color = "Lung Function") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$pef_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = Age)) +
      geom_line(aes(y = pef, color = "PEF")) +
      labs(y = "PEF", color = "PEF") +
      theme_minimal()
    ggplotly(p)
  })
  
  output$eosinophils_feno_plot <- renderPlotly({
    patient <- selected_patient()
    p <- ggplot(patient, aes(x = Age)) +
      geom_line(aes(y = eosinophil_level, color = "Eosinophil Level")) +
      geom_line(aes(y = FeNO_ppb, color = "FeNO (ppb)")) +
      labs(y = "Value", color = "Eosinophils and FeNO") +
      theme_minimal()
    ggplotly(p)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)