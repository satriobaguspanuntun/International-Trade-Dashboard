library(shiny)
library(tidyverse)
library(bs4Dash)

## OBJECTIVE -----
# design macro-dahsboard
# design drilldown analysis for trade
# design forecasting and modelling dashboard (particularly for macro)

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