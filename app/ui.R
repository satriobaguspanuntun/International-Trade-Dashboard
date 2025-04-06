library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bs4Dash)


## OBJECTIVE -----
# design macro-dahsboard
# design drilldown analysis for trade
# design forecasting and modelling dashboard (particularly for macro)

### Pre-requisite parameters and values ###
# country code
country_code <- readRDS("~/international-Trade-Dashboard/data/countrycode.rds")

# max year in database
max_year_macro <- 

# min year in database

### load dataset
# macro
# trade (not a wise thing to do due here due to the amount of data it has.)

ui <- dashboardPage(
  title = "Macroeconomic and Trade Dashboard",
  header = dashboardHeader(
    title = "Macroeconomic and Trade Dashboard"),
  sidebar = dashboardSidebar(
    width = "350px",
    sidebarMenu(
      id = "sidebar_menu",
      menuItem(
        text = "Home",
        tabName = "home",
        icon = icon("home")
      ),
      menuItem(
        text = "Macroeconomic Statistics",
        tabName = "macro",
        icon = icon("bar-chart"),
        startExpanded = TRUE,
        menuSubItem(
          text = "Main Statistics",
          tabName = "macro_main_stats",
          icon = icon("circle")
        ),
        menuSubItem(
          text = "Comparison Section",
          tabName = "macro_comp_stats",
          icon = icon("circle")
        )
      ),
      menuItem(
        text = "International Trade",
        tabName = "trade",
        icon = icon("ship"),
        startExpanded = TRUE,
        menuSubItem(
          text = "Summary Statistics",
          tabName = "summary_trade_stats",
          icon = icon("chevron-right")
        ),
        menuSubItem(
          text = "Drilldown Analysis",
          tabName = "drilldown_trade",
          icon = icon("chevron-right")
        )
      ),
      menuItem(
        text = "Macro-Forecasting",
        tabName = "macro_forecast",
        icon = icon("line-chart"),
        startExpanded = TRUE,
        menuSubItem(
          text = "ARIMA Method",
          tabName = "arima_macro",
          icon = icon("square")
        ),
        menuSubItem(
          text = "Dynamic Factor Model",
          tabName = "dynamic_macro",
          icon = icon("square")
        )
      )
    )
  ),
  controlbar = dashboardControlbar(),
  footer = dashboardFooter(
    left = "Satrio Bagus Panuntun",
    right = "2025"
  ),
  body = dashboardBody(
    tabItems(
      
      tabItem(
        tabName = "home",
        
        jumbotron(
          title = "Welcome to Macroeconomic and Trade Dashboard!",
          status = "info",
          lead = "Visualising and Forecasting Macroeonomic & Trade statistics for 51 seleccted countries.",
          btnName = "Github Repo",
          href = "https://github.com/satriobaguspanuntun/International-Trade-Dashboard",
          "Please check out my Github repository for more details!"
        ),
        
        fluidRow(
          
          userBox(
            collapsible = FALSE,
            title = userDescription(
              title = "Satrio Bagus Panuntun",
              subtitle = "Developer Wannabe",
              image = "uiauiacat.jpeg",
              imageElevation = 2,
              type = 2,
            ),
            status = "navy",
            "A Honours graduate from Victoria University of Wellington and University of Indonesia,
            majoring in Economics and Econometrics. Skill set: Master of procastination and a certified
            doomed scroller."
          ),
          
          box(
            collapsible = FALSE,
            title = "Favourite Quote",
            blockQuote(
              "In your actions, don’t procrastinate. In your conversations, don’t confuse. In your thoughts, don’t wander. In your soul, don’t be passive or aggressive. In your life, don’t be all about business. - Marcus Aurelius",
              color = "navy"
            )
          
          )
        )
        
      ),
      tabItem(
        tabName = "macro_main_stats",
        fluidRow(
            column(
              width = 3,
              offset = 0,
              
              # Select country option
              virtualSelectInput(
                inputId = "macro_stats_country_input",
                label = "Select Country",
                choices = countrycode$iso.name.en,
                search = TRUE,
                width = "100%"
                )
            ),
            column(
              width = 3,
              offset = 0,
              
              # Start date
              airDatepickerInput(
                inputId = "macro_stats_country_start_date",
                label = "Start Year",
                value = Sys.Date(),
                dateFormat = "yyyy",
                view = "years",
                minView = "years",
                width = "100%"
              )
            ),
            column(
              width = 3,
              offset = 0,
              
              # end date
              airDatepickerInput(
                inputId = "macro_stats_country_end_date",
                label = "End Year",
                value = Sys.Date(),
                dateFormat = "yyyy",
                view = "years",
                minView = "years",
                width = "100%"
              )
            ),
            
            # run action button
            column(
              width = 2,
              offset = 0,
              div(style = "margin-top:29px; margin-right:50px"),
              actionBttn(
                inputId = "run_macro_main_stats",
                label = "Run Report",
                style = "jelly"
              )
            )
        ),
        fluidRow(
          column(
            width = 3,
                 valueBox(
                   value = h4("3.5%"),
                   subtitle = "Inflation",
                   color = "primary",
                   icon = icon("shopping-cart"),
                   width = 12
                 )
            ),
          column(
            width = 3,
            valueBox(
              value = h4("$100 Billion"),
              subtitle = "GDP",
              color = "info",
              icon = icon("bar-chart"),
              width = 12
            )
          ),
          column(
            width = 3,
            valueBox(
              value = h4("$-35 Million"),
              subtitle = "Current Account",
              color = "teal",
              icon = icon("gear"),
              width = 12
            )
          ),
          column(
            width = 3,
            valueBox(
              value = h4("5%"),
              subtitle = "Unemployment",
              color = "lightblue",
              icon = icon("line-chart"),
              width = 12
            )
          )
        ),
        fluidRow(
          sortable(
            width = 6,

            box(
              width = 12,
              status = "olive",
              title = "Output Statistics",
              plotlyOutput(outputId = "output_stats"),
              sidebar = boxSidebar(
                id = "output_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "output_sidebar_input",
                  label = "Select Indicator",
                  choices = c("GDP Nominal", "GDP Per Capita", "GDP Per Capita Growth", "Export to GDP")
                )
               )
              ),
            
            box(
              width = 12,
              status = "olive",
              title = "Price Statistics",
              plotOutput(outputId = "price_stats"),
              sidebar = boxSidebar(
                id = "price_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "price_sidebar_input",
                  label = "Select Indicator",
                  choices = c("Inlation")
                )
              )
            )
          ),
          sortable(
            width = 6,
            
            box(
              width = 12,
              status = "olive",
              title = "Labour Statistics",
              plotOutput(outputId = "labour_stats"),
              sidebar = boxSidebar(
                id = "labour_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "labour_sidebar_input",
                  label = "Select Indicator",
                  choices = c("Unemployment", "Population", "Net Migration")
                )
              )
            ),
            
            box(
              width = 12,
              title = "Finance & Debt Statistics",
              status = "olive",
              plotOutput(outputId = "fin_debt_stats"),
              sidebar = boxSidebar(
                id = "fin_debt_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "fin_debt_sidebar_input",
                  label = "Select Indicator",
                  choices = c("Current Account", "FDI", "Gross Capital Formation")
                )
              )
            )
          )
        )
      )
    )
  )
)

server <- function(input, output) {
  
  #### MACRO TABS SERVER ####
  
  ### Compile necessary dataset and make it reactivevals - so that it'll keep on updating
  ### based on user input.
  
  ### Data Manipulation if necessary before plotting
  
  ### Plotting
}

shinyApp(ui, server)