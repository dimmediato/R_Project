# Load packages ----
library(shiny)
# User interface ----
fluidPage(
  mainPanel(leafletOutput(outputId = 'map')
  ))