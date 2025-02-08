library(shiny)
library(DT)

# Load the patient data
patient_data <- read.csv("patient_dataset.csv")

# Define UI for the dashboard
ui <- fluidPage(
  titlePanel("Patient Data Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("patient_id", "Select Patient ID:", choices = patient_data$ID)
    ),
    
    mainPanel(
      h3("Patient Information"),
      tableOutput("patient_info"),
      
      h3("Lung Function"),
      tableOutput("lung_function"),
      
      h3("IgE Levels"),
      tableOutput("ige_levels"),
      
      h3("Other Data"),
      tableOutput("other_data")
    )
  )
)

# Define server logic
server <- function(input, output) {
  selected_patient <- reactive({
    patient_data[patient_data$ID == input$patient_id, ]
  })
  
  output$patient_info <- renderTable({
    selected_patient()[, c("Sex", "Age", "Height..cm.", "Weight..kg.", "BMI", "Ethnicity", "Smoking.Status", "Asthma.Severity")]
  })
  
  output$lung_function <- renderTable({
    selected_patient()[, c("fvc_actual", "fvc_predicted", "fvc_percent_predicted", "fev1_actual", "fev1_predicted", "fev1_fvc_ratio", "fev1_percent_predicted", "pef")]
  })
  
  output$ige_levels <- renderTable({
    selected_patient()[, c("total_ige", "ige_pollen", "ige_cats", "ige_dogs", "ige_mould", "ige_grass", "ige_house_dust_mites")]
  })
  
  output$other_data <- renderTable({
    selected_patient()[, c("eosinophil_level")]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)