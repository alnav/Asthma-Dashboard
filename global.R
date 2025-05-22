# Load required packages
library(shiny)
library(readr)
library(ggplot2)
library(dplyr)
library(plotly)
library(flexdashboard)
library(rmarkdown)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(tidyr)

# Load the dataset
patient_data <- read_csv("patient_dataset.csv")