library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bs4Dash)
library(lubridate)
library(RSQLite)
library(plotly)
library(DT)
library(RColorBrewer)
library(shinycssloaders)
library(sf)
library(leaflet)

#source("~/international-Trade-Dashboard/app/ui.R")
#source("~/international-Trade-Dashboard/app/server.R")
source("~/international-Trade-Dashboard/sql_queries.R")
options(scipen = 999)

## OBJECTIVE -----
# design macro-dahsboard
# design drilldown analysis for trade
# design forecasting and modelling dashboard (particularly for macro)

### Pre-requisite parameters and values ###

# world map data
world_sf <- read_sf("~/international-Trade-Dashboard/data/world-administrative-boundaries.geojson")

# fetch year query
year_range_table <- sql_year_range(conn)

# country code
countrycode <- readRDS("~/international-Trade-Dashboard/data/countrycode.rds")

# max year in database
max_year_macro <- as.character(max(year_range_table[["macro_year"]]$year, na.rm = TRUE))
max_year_trade <- as.character(max(year_range_table[["trade_year"]]$goods_years, na.rm = TRUE))

# min year in database
min_year_macro <- as.character(min(year_range_table[["macro_year"]]$year))
min_year_trade <- as.character(min(year_range_table[["trade_year"]]$goods_years, na.rm = TRUE))

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
            valueBoxOutput(outputId = "macro_valuebox_inflation",width = 12)
            ),
          column(
            width = 3,
            valueBoxOutput(outputId = "macro_valuebox_gdp",width = 12)
          ),
          column(
            width = 3,
            valueBoxOutput(outputId = "macro_valuebox_current",width = 12)
          ),
          column(
            width = 3,
            valueBoxOutput(outputId = "macro_valuebox_unemployment",width = 12)
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
              maximizable = TRUE,
              sidebar = boxSidebar(
                id = "output_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "output_sidebar_input",
                  label = "Select Indicator",
                  selected = "GDP Nominal",
                  choices = c("GDP Nominal", "GDP Per Capita", "GDP Per Capita Growth", "Export to GDP")
                )
               )
              ),
            
            box(
              width = 12,
              status = "olive",
              title = "Price Statistics",
              plotlyOutput(outputId = "price_stats"),
              maximizable = TRUE,
              sidebar = boxSidebar(
                id = "price_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "price_sidebar_input",
                  label = "Select Indicator",
                  selected = "Inflation",
                  choices = c("Inflation")
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
              plotlyOutput(outputId = "labour_stats"),
              maximizable = TRUE,
              sidebar = boxSidebar(
                id = "labour_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "labour_sidebar_input",
                  label = "Select Indicator",
                  selected = "Unemployment",
                  choices = c("Unemployment", "Population", "Net Migration")
                )
              )
            ),
            
            box(
              width = 12,
              title = "Finance & Debt Statistics",
              status = "olive",
              plotlyOutput(outputId = "fin_debt_stats"),
              maximizable = TRUE,
              sidebar = boxSidebar(
                id = "fin_debt_sidebar",
                background = "#7f7f7f",
                pickerInput(
                  inputId = "fin_debt_sidebar_input",
                  label = "Select Indicator",
                  selected = "Current Account",
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
                   radioGroupButtons(inputId = "select_macro_table_grouping",
                                     label = "Select Grouping",
                                     choiceNames = c("Output", "Price", "Labour", "Finance & Debt"),
                                     choiceValues = c("output", "price", "labour", "financial"),
                                     selected = "output",
                                     justified = TRUE,
                                     status = "primary"),
                  withSpinner(DTOutput("macro_data_table"),
                              caption = "Please Wait",
                              type = 8)
                 )
              )
        )
      ),
      tabItem(
        tabName = "summary_trade_stats",
        fluidRow(
          column(
            width = 4,
            pickerInput(inputId = "trade_stats_country",
                        label = "Select Country",
                        choices = countrycode$iso.name.en,
                        multiple = FALSE,
                        options = pickerOptions(style = "btn-outline-dark"),
                        width = "100%"
                       )
        ),
        column(
          width = 4,
          airDatepickerInput(
            inputId = "trade_stats_year",
            label = "Year",
            value = as.Date(paste0(max_year_trade, "-", "01", "-", "01")),
            dateFormat = "yyyy",
            view = "years",
            minView = "years",
            minDate = as.Date(paste0(min_year_trade, "-", "01", "-", "01")),
            maxDate = as.Date(paste0(max_year_trade, "-", "01", "-", "01")),
            width = "100%",
          )
        ),
        column(
          width = 4,
           radioGroupButtons(
            inputId = "trade_flow_input",
            label = "Trade Flow",
            choiceNames = c("Export", "Import"),
            choiceValues = c("X","M"),
            status = "success",
            selected = "X",
            justified = TRUE
          )
        )
      ),
      fluidRow(
        column(
          width = 12,
          leafletOutput(outputId = "trade_map_leaflet", height = "500px")
        )
      ),
        fluidRow(
          column(
            width = 3,
            descriptionBlock(
              number = "17%",
              numberColor = "success",
              numberIcon = icon("caret-up"),
              header = "$999999",
              text = "Total Two-Way Trade",
              marginBottom = TRUE
            )
          ),
            column(
              width = 3,
              descriptionBlock(
                number = "-0.75%",
                numberColor = "danger",
                numberIcon = icon("caret-down"),
                header = "$888888",
                text = "Total Export",
                marginBottom = TRUE
              )
            ),
          column(
            width = 3,
            descriptionBlock(
              number = "51%",
              numberColor = "success",
              numberIcon = icon("caret-up"),
              header = "$555555",
              text = "Total Import",
              marginBottom = TRUE
            )
          ),
          column(
            width = 3,
            descriptionBlock(
              number = "4%",
              numberColor = "success",
              numberIcon = icon("caret-up"),
              header = "$6666666",
              text = "Trade Balance",
              rightBorder = FALSE,
              marginBottom = TRUE
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
    
    # show spinner
    showPageSpinner()
    
    # year range input
    year_input <- seq(macro_input$start_year, macro_input$end_year, by = 1)
    Sys.sleep(1)
    
    # Filter data based on country and year
    data_filter <- macro_main_dataset %>% 
      filter(country == macro_input$country, year %in% year_input) 
    
    # hide spinner
    hidePageSpinner()
    
    return(data_filter)
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
  
  # valuebox creation
  # inflation
  output$macro_valuebox_inflation <- renderValueBox({
    valueBox(
      value = h4(calc_valuebox_macro(macro_data_filter(), type = "inflation")),
      subtitle = paste0("Inflation (", max_year_macro,")"),
      color = "primary",
      icon = icon("shopping-cart"),
      width = 12
    )
  })
  
  # GDP
  output$macro_valuebox_gdp <- renderValueBox({
    valueBox(
      value = h4(calc_valuebox_macro(macro_data_filter(), type = "gdp")),
      subtitle = paste0("GDP (", max_year_macro, ")"),
      color = "info",
      icon = icon("bar-chart"),
      width = 12
    )
  })
  
  # current account
  output$macro_valuebox_current <- renderValueBox({
    valueBox(
      value = h4(calc_valuebox_macro(macro_data_filter(), type = "current")),
      subtitle = paste0("Current Account (", max_year_macro, ")"),
      color = "teal",
      icon = icon("gear"),
      width = 12
    )
  })
  
  # unemployment
  output$macro_valuebox_unemployment <- renderValueBox({
    valueBox(
      value = h4(calc_valuebox_macro(macro_data_filter(), type = "unemployment")),
      subtitle = paste0("Unemployment (", max_year_macro, ")"),
      color = "lightblue",
      icon = icon("line-chart"),
      width = 12
    )
  })
  
  # filter data by column
  macro_data_final <- reactive({
    
    # wide to long format
    macro_data_long <- pivot_longer(data = macro_data_filter(),
                                    cols = gdp_nominal:gross_capital_formation,
                                    names_to = "var",
                                    values_to = "values") %>% 
      group_by(country, year) %>% 
      mutate(var_grouping = case_when(var %in% c("gdp_nominal", "gdp_per_capita_current",
                                                 "gdp_per_capita_growth", "export_gdp_percent") ~ "output",
                                      var %in% c("inflation") ~ "price",
                                      var %in% c("unemployment", "population", "net_migration") ~ "labour",
                                      var %in% c("current_account", "fdi", "gross_capital_formation") ~ "financial",
                                      .default = "NA"),
             
             var_name = case_when(var == "gdp_nominal" ~ "GDP Nominal",
                                  var == "gdp_per_capita_current" ~ "GDP Per Capita",
                                  var == "gdp_per_capita_growth" ~ "GDP Per Capita Growth",
                                  var == "export_gdp_percent" ~ "Export to GDP",
                                  var == "inflation" ~ "Inflation",
                                  var == "unemployment" ~ "Unemployment",
                                  var == "population" ~ "Population",
                                  var == "net_migration" ~ "Net Migration",
                                  var == "current_account" ~ "Current Account",
                                  var == "fdi" ~ "FDI",
                                  var == "gross_capital_formation" ~ "Gross Capital Formation",
                                  .default = "NA"))
    
  })
  
  # data for plot
  macro_data_plot <- reactive({
    
    macro_data_wide <- macro_data_final() %>%
      select(-c(var, var_grouping)) %>% 
      relocate(country, iso3c, year, var_name, values) %>% 
      group_by(year, country, var_name) %>% 
      mutate(row = row_number()) %>% 
      ungroup() %>% 
      pivot_wider(names_from = var_name, values_from = values)
      
  })
  
  # data for the table
  macro_data_table <- reactive({
    
    macro_table <- macro_data_final() %>% 
      ungroup() %>% 
      mutate(values = case_when(var_name == "GDP Nominal" ~ paste0("$ ",format(round(values/1000000000, 2), big.mark = ","), "B"),
                               var_name == "GDP Per Capita" ~ paste0("$ ",format(round(values, 2), big.mark = ",")),
                               var_name == "GDP Per Capita Growth" ~ paste0(round(values, 2), " %"),
                               var_name == "Export to GDP" ~ paste0(round(values, 2), " %"),
                               var_name == "Inflation" ~ paste0(round(values, 2), " %"),
                               var_name == "Unemployment" ~ paste0(round(values, 2), " %"),
                               var_name == "Population" ~ paste0(format(round(values/1000000, 2), big.mark = ","), "M"),
                               var_name == "Net Migration" ~ paste0(format(round(values, 2), big.mark = ",")),
                               var_name == "Current Account" ~ paste0("$ ",format(round(values/1000000000, 2), big.mark = ","), "B"),
                               var_name == "FDI" ~ paste0("$ ",format(round(values/1000000000, 2), big.mark = ","), "B"),
                               var_name == "Gross Capital Formation" ~ paste0(round(values, 2), " %"),
                               .default = "NA")) %>% 
      filter(var_grouping == as.character(input$select_macro_table_grouping)) %>% 
      select(-c(var, var_grouping)) %>% 
      rename("Country" = country,
             "ISO3" = iso3c,
             "Year" = year) %>% 
      pivot_wider(names_from = var_name, values_from = values)
    
  })
  
  ### Plotting
  # output plot 
  output$output_stats <- renderPlotly({
    
    output_plot <- plot_ly(
      macro_data_plot(),
      x = ~year,
      y = ~select(macro_data_plot(), input$output_sidebar_input) %>% pull(),
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#1f77b4', width = 2),
      marker = list(color = '#1f77b4', width = 4),
      connectgaps = TRUE,
      symbol = I('diamond')
    ) %>%
      layout(
        title = list(
          text = paste0(input$output_sidebar_input)
        ),
        xaxis = list(
          title = "",
          tickmode = "linear",
          dtick = 1,
          tickangle = -45,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = ifelse(input$output_sidebar_input %in% c("GDP Nominal", "GDP Per Capita"), 
                         "$ USD", 
                         "% Percentage"),
          showgrid = TRUE,
          gridcolor = 'rgba(200,200,200,0.3)',
          zeroline = FALSE
        ),
        margin = list(l = 30, r = 15, b = 15, t = 30)
      )
  })
  
  #  price plot 
  output$price_stats <- renderPlotly({
    
    value <- select(macro_data_plot(), input$price_sidebar_input) %>% pull()
    
    output_plot <- plot_ly(
      macro_data_plot(),
      x = ~year,
      y = ~value,
      type = 'bar',
      marker = list(
        color = value,
        cmin = 0,
        cmax = 10,
        colorscale = "Viridis",  # Scaled gradient
        colorbar = list(title = ""),
        showscale = FALSE
      )
    ) %>%
      layout(
        title = list(
          text = paste0("Annual ", input$price_sidebar_input)
        ),
        xaxis = list(
          title = "",
          tickmode = "linear",
          dtick = 1,
          tickangle = -45,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = "% Percentage",
          showgrid = TRUE,
          gridcolor = 'rgba(200,200,200,0.3)',
          zeroline = FALSE
        ),
        margin = list(l = 30, r = 15, b = 15, t = 30)
      )
  })
  
  # labour market plot
  output$labour_stats <- renderPlotly({
    
    output_plot <- plot_ly(
      macro_data_plot(),
      x = ~year,
      y = ~select(macro_data_plot(), input$labour_sidebar_input) %>% pull(),
      type = 'scatter',
      mode = 'lines+markers',
      line = list(color = '#de6d2c', width = 2),
      marker = list(color = '#de6d2c', width = 4),
      connectgaps = TRUE,
      symbol = I('circle')
    ) %>%
      layout(
        title = list(
          text = paste0(input$labour_sidebar_input)
        ),
        xaxis = list(
          title = "",
          tickmode = "linear",
          dtick = 1,
          tickangle = -45,
          showgrid = FALSE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = ifelse(input$labour_sidebar_input %in% c("Unemployment"), 
                         "% Percentage", 
                         "Headcount"),
          showgrid = TRUE,
          gridcolor = 'rgba(200,200,200,0.3)',
          zeroline = FALSE
        ),
        margin = list(l = 30, r = 15, b = 15, t = 30)
      )
  })
  
  # finance and debt plot
  
  output$fin_debt_stats <- renderPlotly({
    
    if (input$fin_debt_sidebar_input %in% c("Current Account")) {
      
      curr_account_data <- macro_data_plot() %>% 
        select(year, `Current Account`) %>% 
        mutate(curr_label = factor(if_else(`Current Account` < 0, "Deficit", "Surplus"), levels = c("Surplus", "Deficit")))
      
      output_plot <- plot_ly(
        curr_account_data,
        x = ~year,
        y = ~`Current Account`,
        name = ~curr_label,
        type = 'bar',
        color = ~curr_label, 
        colors = c("#28a745", "#dc3545")) %>%
        layout(
          title = list(
            text = "Current Account"
          ),
          xaxis = list(
            title = "",
            tickmode = "linear",
            dtick = 1,
            tickangle = -45,
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "$ USD",
            showgrid = TRUE,
            gridcolor = 'rgba(200,200,200,0.3)',
            zeroline = FALSE
          ),
          margin = list(l = 30, r = 15, b = 15, t = 30)
        )
      
    } else {
      
      output_plot <- plot_ly(
        macro_data_plot(),
        x = ~year,
        y = ~select(macro_data_plot(), input$fin_debt_sidebar_input) %>% pull(),
        type = 'scatter',
        mode = 'lines+markers',
        line = list(color = '#08bfa4', width = 2),
        marker = list(color = '#08bfa4', width = 4),
        fill = 'tozeroy',
        fillcolor = '#94f7dd',
        connectgaps = TRUE,
        symbol = I('square')
      ) %>%
        layout(
          title = list(
            text = paste0(input$fin_debt_sidebar_input)
          ),
          xaxis = list(
            title = "",
            tickmode = "linear",
            dtick = 1,
            tickangle = -45,
            showgrid = FALSE,
            zeroline = FALSE
          ),
          yaxis = list(
            title = "$ USD",
            showgrid = TRUE,
            gridcolor = 'rgba(200,200,200,0.3)',
            zeroline = FALSE
          ),
          margin = list(l = 30, r = 15, b = 15, t = 30)
        )
    }
  })
  
  # Data Table Section and 5-year CAGR for each variable
  output$macro_data_table <- renderDT({
    Sys.sleep(1)
    datatable(macro_data_table(),
              rownames = FALSE,
              extensions = "Buttons",
              options = list(
                scrollY = 400,
                scroller = TRUE,
                paging = TRUE,
                autoWidth = TRUE,
                dom = 'Bfrtip',
                buttons = c('csv', 'excel')))
    
  })
  
  #### TRADE TAB SERVER ####
  ### Summary Statistic page
  ## user input.
  trade_input <- reactiveValues(country = "Argentina",
                                trade_flow_select = "X",
                                year = "2023")
  
  ### Update reactive values when inputs change
  observe({
    trade_input$country <- input$trade_stats_country
    trade_input$trade_flow_select <- input$trade_flow_input
    trade_input$year <- as.numeric(substr(input$trade_stats_year, 1, 4))
  })
  
  observe({print(trade_input$country)})
  observe({print(trade_input$trade_flow_select)})
  observe({print(trade_input$year)})
  
  
  # ### call trade data
  trade_data <- reactive({

    trade_data_test <- sql_export_query(conn,
                                        start =  trade_input$year,
                                        end = trade_input$year,
                                        trade_flow = trade_input$trade_flow_select,
                                        type = "goods",
                                        country = trade_input$country)

  })
  
  trade_map_function <- function(world_map_data, trade_data, reporter_iso_select, trade_flow, year_select) {
    
    # calculate totals for each partner countries
    total_data <- trade_data %>% 
      filter( partner_iso != reporter_iso_select) %>% 
      group_by(period, reporter_iso, reporter_desc, partner_iso, flow_code, flow_desc) %>% 
      summarise(total_value_country = sum(primary_value)/1000000000) %>% 
      ungroup() %>% 
      filter(flow_code == trade_flow)
    
    # joined with world map sf data
    joined_data_sf <- left_join(world_map_data, total_data, join_by("iso3" == "partner_iso"))
    joined_data_sf <- st_as_sf(joined_data_sf)
    
    # bins & and color
    mybins <- quantile(joined_data_sf$total_value_country, probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE)
    
    if (trade_flow == "M") {
      
      mypalette <- colorBin(
        palette = "YlOrBr", domain = joined_data_sf$total_value_country,
        na.color = "transparent", bins = mybins
      )
      
    } else if (trade_flow == "X") {
      
      mypalette <- colorBin(
        palette = "YlGnBu", domain = joined_data_sf$total_value_country,
        na.color = "transparent", bins = mybins
      )
    }
    
    text_label <- paste("Country: ", joined_data_sf$name, "<br/>",
                        "Area: ", joined_data_sf$continent, "<br/>",
                        "Trade Flow: ", joined_data_sf$flow_desc, "<br/>",
                        "Value: ", paste0("$",round(joined_data_sf$total_value_country, 4), "B")) %>%
      lapply(htmltools::HTML)
    
    # leaflet map
    trade_map <- leaflet(joined_data_sf) %>%
      addTiles() %>%
      setView(10, 0, zoom = 2) %>% 
      addPolygons(
        stroke = FALSE,
        fillOpacity = 1,
        smoothFactor = 0.5,
        color = ~ mypalette(total_value_country),
        label = text_label,
        labelOptions = labelOptions(
          textsize = "13px",
          direction = "auto"
        ),
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.5,
          bringToFront = TRUE)
      ) %>%
      addLegend(
        pal = mypalette,
        values = ~total_value_country, 
        title = ifelse(trade_flow=="M", "Import Value", "Export Value"),
        opacity = 1,
        labFormat = labelFormat(prefix = "$", suffix = "B"),
        position = "bottomright"
      )
    
    return(trade_map)
  }
  
  output$trade_map_leaflet <- renderLeaflet({
    
    trade_world_map <- trade_map_function(world_map_data = world_sf,
                                          trade_data = trade_data(),
                                          reporter_iso_select = trade_input$country,
                                          trade_flow = trade_input$trade_flow_select,
                                          year_select = trade_input$year )
  })

}

shinyApp(ui, server)