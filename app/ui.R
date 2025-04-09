library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bs4Dash)
library(lubridate)
library(RSQLite)
library(plotly)
library(DT)

#source("~/international-Trade-Dashboard/app/ui.R")
#source("~/international-Trade-Dashboard/app/server.R")
source("~/international-Trade-Dashboard/sql_queries.R")
options(scipen = 999)

## OBJECTIVE -----
# design macro-dahsboard
# design drilldown analysis for trade
# design forecasting and modelling dashboard (particularly for macro)

### Pre-requisite parameters and values ###

# fetch year query
year_range_table <- sql_year_range(conn)

# country code
countrycode <- readRDS("~/international-Trade-Dashboard/data/countrycode.rds")

# max year in database
max_year_macro <- as.character(max(year_range_table[["macro_year"]]$year))
max_year_trade <- as.character(max(year_range_table[["trade_year"]]$goods_years))

# min year in database
min_year_macro <- as.character(min(year_range_table[["macro_year"]]$year))
min_year_trade <- as.character(min(year_range_table[["trade_year"]]$goods_years))

### load dataset
# macro
macro_main_dataset <- sql_macro_query(conn, start = min_year_macro, end = max_year_macro)

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
                value = as.Date(paste0(min_year_macro, "-", "01", "-", "01")),
                dateFormat = "yyyy",
                view = "years",
                minView = "years",
                minDate = as.Date(paste0(min_year_macro, "-", "01", "-", "01")),
                maxDate = as.Date(paste0(max_year_macro, "-", "01", "-", "01")),
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
                value = as.Date(paste0(max_year_macro, "-", "01", "-", "01")),
                dateFormat = "yyyy",
                view = "years",
                minView = "years",
                minDate = as.Date(paste0(min_year_macro, "-", "01", "-", "01")),
                maxDate = as.Date(paste0(max_year_macro, "-", "01", "-", "01")),
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
        ),
        fluidRow(
          tabBox(id = "macro_tab",
                 width = 12,
                 title = "Macroeconomic Indicator Table",
                 status = "olive",
                 type = "tabs",
                 solidHeader = TRUE,
                 tabPanel(
                   title = "Data Table",
                   DTOutput("macro_data_table")
                 ),
                 
                 tabPanel(
                   title = "5 Year CAGR",
                   DTOutput("macro_cagr_table")
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
  macro_input <- reactiveValues(country = "Argentina",
                                start_year = 2015,
                                end_year = 2023)
  
  ### Update reactive values when inputs change
  observe({
    macro_input$country <- input$macro_stats_country_input
    macro_input$start_year <- as.numeric(substr(input$macro_stats_country_start_date, 1, 4))
    macro_input$end_year <- as.numeric(substr(input$macro_stats_country_end_date, 1, 4))
  })
  
  ### Data Manipulation if necessary before plotting
  # filter macro data based on inputs
  
  macro_data_filter <- eventReactive(input$run_macro_main_stats, {
    
    # year range input
    year_input <- seq(macro_input$start_year, macro_input$end_year, by = 1)
    
    # Filter data based on country and year
    data_filter <- data_macro %>% 
      filter(country == macro_input$country, year %in% year_input) 
    
    return(data_filter)
  })
  
  # Debugging: Print the first few rows of the filtered data
  observe({
    print(macro_data_filter() %>% select(country, year, seq))
  })
  
  ### valueboxes
  calc_valuebox_macro <- function(data, type) {
    
    # sense check 
    if (!is.data.frame(data)) {
      stop("Please supply the data in a dataframe format")
    }
    
    if (!is.character(type)) {
      stop("Type has to be in character or string")
    }
    
    # switch function
    output_valuebox <- switch(type, 
                              # inflation
                              "inflation" = {x <- data %>% 
                                mutate(year = as.numeric(year)) %>% 
                                select(country, year, inflation) %>% 
                                filter(year == max(year)) %>% 
                                mutate(year = as.character(year),
                                       inflation = round(inflation, 2))
                              
                              x <- paste0(x$inflation, " %")},
                              
                              # GDP
                              "gdp" = {x <- data %>% 
                                mutate(year = as.numeric(year)) %>% 
                                select(country, year, gdp_nominal) %>% 
                                filter(year == max(year)) %>% 
                                mutate(year = as.character(year),
                                       gdp_nominal = format(round(gdp_nominal/1000000000, 2), digits = 1 ,big.mark=","))
                              
                              x <- paste0("$",x$gdp_nominal, " Billion")},
                              
                              # current account
                              "current" = {x <- data %>% 
                                mutate(year = as.numeric(year)) %>% 
                                select(country, year, current_account) %>% 
                                filter(year == max(year)) %>% 
                                mutate(year = as.character(year),
                                       current_account = format(round(current_account/1000000000, 2), digits = 1,big.mark=","))
                              
                              x <- paste0("$", x$current_account, " Billion")},
                              
                              # unemployment
                              "unemployment" = {x <- data %>% 
                                mutate(year = as.numeric(year)) %>% 
                                select(country, year, unemployment) %>% 
                                filter(year == max(year)) %>% 
                                mutate(year = as.character(year),
                                       unemployment = round(unemployment, 2))
                              
                              x <- paste0(x$unemployment, " %")}
    )
    return(output_valuebox)
  }
  
  
  
  ### Plotting
}

shinyApp(ui, server)