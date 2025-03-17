library(shiny)
library(tidyverse)
library(bs4Dash)

ui <- dashboardPage(
  title = "Macroeconomic and Trade Dashboard",
  header = dashboardHeader(),
  sidebar = dashboardSidebar(),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(),
  body = dashboardBody()
)

server <- function(input, output) {}

shinyApp(ui, server)