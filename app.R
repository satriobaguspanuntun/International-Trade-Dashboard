library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bs4Dash)
library(RSQLite)
library(highcharter)
library(plotly)
library(DT)
library(RColorBrewer)
library(shinycssloaders)
library(sf)
library(leaflet)
library(glue)
library(waiter)

#source("~/international-Trade-Dashboard/app/ui.R")
#source("~/international-Trade-Dashboard/app/server.R")
source("~/international-Trade-Dashboard/R/sql_queries.R")
options(scipen = 999)

## OBJECTIVE -----
# design macro-dahsboard
# design drilldown analysis for trade
# design forecasting and modelling dashboard (particularly for macro)

### Pre-requisite parameters and values ###
# nomenclature and concordances
hs_concord <- readRDS("~/international-Trade-Dashboard/data/hs_concord.rds")
serv_concord <- readRDS("~/international-Trade-Dashboard/data/serv_concord.rds")

# country code
countrycode <- readRDS("~/international-Trade-Dashboard/data/countrycode.rds")
all_countrycode <- readRDS("~/international-Trade-Dashboard/data/all_countrycode.rds")

# world map data
world_sf <- read_sf("~/international-Trade-Dashboard/data/world-administrative-boundaries.geojson")

# cleaning and harmonising HS nomenclature
clean_hs2 <- function(hs) {
  hs <- hs %>% filter(parent == "TOTAL")
  hs$text <- gsub("^\\d+\\s*-\\s*", "", hs$text)
  clean_hs <- hs %>% 
    mutate(text = case_when(id == "03" ~ "Fish and other aquatic invertebrates",
                            id == "04" ~ "Dairy produce; birds' eggs; natural honey; n.e.c",
                            id == "05" ~ "Animal originated products; n.e.c",
                            id == "06" ~ "Trees and other plants",
                            id == "08" ~ "Fruit and nuts",
                            id == "11" ~ "Products of the milling industry; malt, starches",
                            id == "12" ~ "Oil seeds and oleaginous fruits",
                            id == "14" ~ "Vegetable plaiting materials; n.e.c",
                            id == "15" ~ "Animal, vegetable or microbial fats and oils",
                            id == "16" ~ "Meat, fish, other aquatic invertebrates, preparations thereof",
                            id == "19" ~ "Preparations of cereals, flour, starch or milk",
                            id == "24" ~ "Tobacco and manufactured tobacco substitutes",
                            id == "27" ~ "Mineral fuels or oils and their distillation; bituminous substances",
                            id == "28" ~ "Inorganic chemicals; compounds of precious metals; rare earth metals; radio-active elements",
                            id == "32" ~ "Tanning or dyeing extracts",
                            id == "34" ~ "Soap, organic surface-active agents; washing, lubricating, polishing or scouring preparations",
                            id == "42" ~ "Articles of leather; saddlery and harness; travel goods, handbags and similar container",
                            id == "49" ~ "Printed books, newspapers, pictures and other products of the printing industry",
                            id == "56" ~ "Wadding, felt and nonwovens, special yarns",
                            id == "58" ~ "Fabrics; special woven fabrics, tufted textile fabrics",
                            id == "59" ~ "Textile fabrics; impregnated, coated, covered or laminated",
                            id == "71" ~ "Natural, cultured pearls; precious, semi-precious stones; precious metals",
                            id == "82" ~ "Tools, implements, cutlery, spoons and forks, of base metal",
                            id == "85" ~ "Electrical machinery and equipment and parts thereof",
                            id == "86" ~ "Railway, tramway locomotives, rolling-stock and parts thereof",
                            id == "87" ~ "Vehicles; other than railway or tramway rolling stock",
                            id == "90" ~ "Optical, photographic, medical or surgical instruments and apparatus",
                            id == "94" ~ "Furniture; bedding, mattresses, mattress supports, cushions and similar stuffed furnishings",
                            .default = text))
  return(clean_hs)
}

clean_hs_commod <- clean_hs2(hs_concord)

# services concordance
service_commod <- sql_service_concordance(conn)

# combined hs + services concordances
combined_concord <- clean_hs_commod %>% select(id, text) %>% bind_rows(service_commod)

# fetch year query
year_range_table <- sql_year_range(conn)

# max year in database
max_year_macro <- as.character(max(year_range_table[["macro_year"]]$year, na.rm = TRUE))
max_year_trade <- as.character(max(year_range_table[["trade_year"]]$goods_years, na.rm = TRUE))

# min year in database
min_year_macro <- as.character(min(year_range_table[["macro_year"]]$year))
min_year_trade <- as.character(min(year_range_table[["trade_year"]]$goods_years, na.rm = TRUE))

### load dataset
# macro
macro_main_dataset <- sql_macro_query(conn, start = min_year_macro, end = max_year_macro)

# available countries in the database (use goods in the meantime)
available_countries <- sql_country_code(conn)

# limit the country code options by matching the original codes with the one in the database
countrycode <- countrycode[countrycode$ISO3_CODE %in% available_countries[["goods_avail_country"]], ]


ui <- dashboardPage(
  help = NULL,
  preloader = list(html = tagList(spin_2(), "Loading ..."), color = "#343a40"),
  header = dashboardHeader(
    tags$head(
      tags$style(HTML("
    .main-header .navbar .navbar-brand {
      font-weight: 600;
      font-size: 1.5rem;
      letter-spacing: 0.5px;
    }
  "))
    ),
    title = dashboardBrand(
      title = h5("MacroTrade Insight"),
      color = "navy",
      image = "logo.jpg",
      opacity = 0.9
    ),
    skin = "light",  
    status = "white", 
    border = TRUE,  
    compact = FALSE,  
    controlbarIcon = icon("sliders-h"),  
    sidebarIcon = icon("bars"),
    rightUi = tagList(
      userOutput("user")
    )  
   ),
  sidebar = dashboardSidebar(
    sidebarUserPanel(
      name = h6("Welcome Onboard!"),
      image = "logo2.jpg"
    ),
    width = "300px",
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
          text = "Commodity Analysis",
          tabName = "commodity_analysis",
          icon = icon("chevron-right")
        ),
        menuSubItem(
          text = "Trade-Forecasting",
          tabName = "forecasting_trade",
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
  footer = dashboardFooter(
    left = "MacroTrade Insight",
    right = "2025"
  ),
  body = dashboardBody(
    tabItems(
      tabItem(
        tabName = "home",
        
        tags$head(
          tags$style(
            HTML(
              ".box-quote {
                 font-style: italic,
                 color: #2c3e50;
                 background-color: #f4f6f9;
                 border-left: 5px solid #001f3f;
                 padding: 15px;"
            )
          )
        ),
        
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
              type = 2
            ),
            status = "navy",
            "A Honours graduate from Victoria University of Wellington and University of Indonesia,
            majoring in Economics and Econometrics. Skill set: Master of procastination and a certified
            doomed scroller."
          ),
          
          box(
            collapsible = FALSE,
            title = "Favourite Quote",
            
            div(class = "box-quote",
                HTML("“In your actions, don’t procrastinate. In your conversations, don’t confuse. In your thoughts, don’t wander. In your soul, don’t be passive or aggressive. In your life, don’t be all about business.” <br><b>- Marcus Aurelius</b>")
            )
            
          )
        )
      ),
      tabItem(
        tabName = "macro_main_stats",
        fluidRow(
          div(
            style = "text-align: left; width: 100%; padding-bottom: 10px;",
            
            tags$head(
              tags$style(HTML("
      @keyframes slideFadeInText {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @keyframes slideInLine {
        from { width: 0; opacity: 0; }
        to { width: 100%; opacity: 1; }
      }

      .title-container {
        display: inline-block;
        animation: slideFadeInText 0.8s ease-out forwards;
      }

      .title-content {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 28px;
        font-weight: 700;
        color: #2c3e50;
        margin: 0;
      }

      .animated-hr {
        height: 4px;
        background: linear-gradient(to right, #2c3e50, #3498db);
        border: none;
        margin-top: 5px;
        animation: slideInLine 1s ease-out forwards;
        animation-delay: 0.4s;
        animation-fill-mode: both;
      }

      .header-icon {
        color: #3498db;
        font-size: 28px;
      }
    "))
            ),
            
            div(
              class = "title-container",
              tags$div(
                class = "title-content",
                tags$i(class = "fas fa-chart-line header-icon"),
                "Macroeconomic Statistics"
              ),
              tags$hr(class = "animated-hr")
            )
          ),
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
                style = "jelly",
                color = "primary"
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
              elevation = 4,
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
              elevation = 4,
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
              elevation = 4,
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
              elevation = 4,
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
                 elevation = 4,
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
        useBusyIndicators(),
        busyIndicatorOptions(spinner_type = "ring2",
                             spinner_size = "80px"),
        fluidRow(
          column(
            width = 4,
            virtualSelectInput(inputId = "trade_stats_country",
                               label = "Select Country",
                               choices = countrycode$iso.name.en,
                               multiple = FALSE,
                               width = "100%",
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
              width = "100%"
            )
          )
        ),
        fluidRow(
          column(
            width = 8,
            div(
              style = "display: inline-block; text-align: left;",
              tags$head(
                tags$style(HTML("
      @keyframes slideFadeInText {
        from {
          opacity: 0;
          transform: translateY(20px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }

      @keyframes slideInLine {
        from {
          width: 0;
          opacity: 0;
        }
        to {
          width: 100%;
          opacity: 1;
        }
      }

      .animated-h3 {
        animation: slideFadeInText 0.8s ease-out forwards;
        color: #2c3e50;
        font-weight: bold;
        font-size: 26px;
        margin-top: 20px;
      }

      .animated-hr {
        height: 3px;
        background: linear-gradient(to right, #2c3e50, #3498db);
        border: none;
        animation: slideInLine 1s ease-out forwards;
        animation-delay: 0.3s;
        animation-fill-mode: both;
      }
    "))
              ),
              tags$h3("📦 International Merchandise Trade Statistics (Goods Export and Import)",
                      class = "animated-h3"),
              tags$hr(class = "animated-hr")
            )
        ),
        column(
          width = 4,
           radioGroupButtons(
            inputId = "trade_flow_input",
            label = "Trade Flow Map",
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
          leafletOutput(outputId = "trade_map_leaflet", height = "400px")
        )
      ),
        fluidRow(
          column(
            width = 3,
            uiOutput("two_way")
          ),
            column(
              width = 3,
              uiOutput("trade_export")
            ),
          column(
            width = 3,
            uiOutput("trade_import")
          ),
          column(
            width = 3,
            uiOutput("trade_balance")
          )
        ),
      tags$br(),
      fluidRow(
        column(
          width = 12,
          uiOutput("export_import_line_chart")
        )
      ),
      fluidRow(
        column(
          width = 6,
          uiOutput("top_export")
        ), 
        column(
          width = 6,
          uiOutput("top_import")
        )
       ),
      fluidRow(
        column(
          width = 6,
          uiOutput("top_export_commod")
        ),
        column(
          width = 6,
          uiOutput("top_import_commod")
        )
      ),
      fluidRow(
        column(
          width = 12,
          uiOutput("sankey_chart")
        )
      ),
      # this bit below should be in a uioutput
      uiOutput("service_title"),
      fluidRow(
        column(width = 6,
               uiOutput("service_valuebox"),
               uiOutput("top_export_import_service")
        ),
        column(width = 6,
               uiOutput("service_line_chart")
        )
      )
     ),
     tabItem(
       tabName = "commodity_analysis",
       useBusyIndicators(),
       fluidRow(
         column(
           width = 4,
       div(
         style = "text-align: left; width: 100%; padding-bottom: 10px;",

         tags$head(
           tags$style(HTML("
      @keyframes slideFadeInText {
        from { opacity: 0; transform: translateY(20px); }
        to { opacity: 1; transform: translateY(0); }
      }

      @keyframes slideInLine {
        from { width: 0; opacity: 0; }
        to { width: 100%; opacity: 1; }
      }

      .title-container {
        display: inline-block;
        animation: slideFadeInText 0.8s ease-out forwards;
      }
      
      .animate-text{
        animation: slideFadeInText 0.8 ease forwards;
      }

      .title-content {
        display: flex;
        align-items: center;
        gap: 10px;
        font-size: 28px;
        font-weight: 700;
        color: #2c3e50;
        margin: 0;
      }

      .animated-hr {
        height: 4px;
        background: linear-gradient(to right, #2c3e50, #3498db);
        border: none;
        margin-top: 5px;
        animation: slideInLine 1s ease-out forwards;
        animation-delay: 0.4s;
        animation-fill-mode: both;
      }

      .header-icon {
        color: #3498db;
        font-size: 28px;
      }
    "))
         ),

         div(
           class = "title-container",
           tags$div(
             class = "title-content",
             tags$i(class = "fas fa-chart-bar header-icon"),
             "Commodity Analysis"
           ),
           tags$hr(class = "animated-hr")
         )
       )
      )
     ),
     fluidRow(
       tags$head(
         tags$style(HTML("
      .btn-light {background-color: #FAFDFF; 
                  border-color: #070707;}
                  
      .flex-container {
        display: flex;
        gap: 20px;
        flex-wrap: wrap;
        align-items: flex-start;
      }
      
    "))
       ),
       
       column(
         width = 12,
       
       div(class = "flex-container",
           div(style = "width: 250px;",
               pickerInput(
                 inputId = "select_country_commod",
                 label = "Select country",
                 choices = countrycode$iso.name.en,
                 multiple = FALSE
               )
           ),
           
           div(style = "width: 250px;",
               pickerInput(
                 inputId = "select_exp_imp_commod",
                 label = "Select Trade Type",
                 choices = c("Export", "Import"),
                 selected = "Export",
                 multiple = FALSE
               )
           )
       )
      )
     ),
     tags$head(
       tags$style(HTML("
    @keyframes slideFadeInText {
      from { opacity: 0; transform: translateY(20px); }
      to { opacity: 1; transform: translateY(0); }
    }

    @keyframes slideInLine {
      from { width: 0; opacity: 0; }
        to { width: 100%; opacity: 1; }
    }
    
    .commod-content-title {
        display: inline-block;
        margin: 0;
      }

    .animate-text {
      animation: slideFadeInText 0.8s ease forwards;
    }
    
    .animate-line {
      height: 3px;
      background-color: #007BFF;
      border: none;
      margin-top: 5px;
      animation: slideInLine 1s ease-out forwards;
      animation-delay: 0.4s;
      animation-fill-mode: both;
      width: fit-content;
      max-width: 100%;
      display: inline-block;
    }
  "))
     ),
     # help text
     tags$div(id = "commod_analysis_help",
              style = "background-color: #FFFFFF; padding: 20px; border: 1px solid #ccc; border-radius: 10px;",
              fluidRow(
                column(
                  width = 12,
                  div(class = "commod-content-title",
                  h3(class = "animate-text", "Commodity Analysis on Export/Import"),
                  tags$hr(class = "animate-line")
                  ),
                  br(),
                  h4(class = "animate-text","How to:"),
                  tags$ol(
                    class = "animate-text",
                    tags$li("Search for either HS code or Services code and the commodity names in the search or filter box available in each column."),
                    tags$li("Choose whether to show only the first 10 entries or the entire table."),
                    tags$li("By default, the report shows Export data. You can switch to Import using the top selector."),
                    tags$li("After selecting one or more rows, the report will automatically generate for the selected codes.")
                  )
                )
              )
     ),
     tags$br(),
     tags$div(id = "commod_concord_table",
     fluidRow(
       column(
         width = 12,
         # render HS codes and Services codes
         dataTableOutput("concordance_codes"))
     )
    ),
     # clear table selection
    tags$div(id = "clear_commod_table",
             fluidRow(
               column(
                 width = 12,
                 div(style = "width: 250px;",
                 actionBttn(inputId = "clear_hsserv_table",
                            label = "Clear Selection",
                            icon = icon("refresh"),
                            style = "jelly")
                 )
               )
             )
            ),
    # insert UI for first level of commodity analysis
    tags$br(),
    tags$div(id = "commod_value_line_table"),
    tags$br(),
    tags$div(id = "commod_value_growth_share"),
    tags$br(),
    tags$div(id = "table_summary_commod")
    ),
     tabItem(
       tabName = "forecasting_trade",
       tags$h1("Work In Progress...")
     ),
     tabItem(
       tabName = "macro_forecast",
       tags$h1("Work In Progress...")
     ),
     tabItem(
       tabName = "macro_forecast",
       tags$h1("Work In Progress...")
     )
    )
  )
)

server <- function(input, output, session) {
  
  #-----------------------------------------------------------------------------------#
  #--------------------------------# MACRO TAB SERVER #-------------------------------#
  #-----------------------------------------------------------------------------------#
  
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
  
  # generate user profile on the right side of the dashboard header
  output$user <- renderUser({
    dashboardUser(
      name = "MacroTrade Insight",
      image = "logo2.jpg",
      title = "Satrio Bagus Panuntun",
      subtitle = "Author"
    )
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
  
  
  
  
  #-----------------------------------------------------------------------------------#
  #--------------------------------# TRADE TAB SERVER #-------------------------------#
  #-----------------------------------------------------------------------------------#
  
  ###----------------------------------------------------------###
  ### -----------------Summary Statistic page -----------------###
  ###----------------------------------------------------------###
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
  
  #### call trade data ##### 
  
  # NOTE: I think storing all of the data needed up front in a list 
  # Would be the best option to simplify and refactor repeated codes.
  # data requirements:
  # 1. Trade map data
  # 2. Trade data for valueboxes
  # 3. dataset for top commodities
  # 4. 10 years trade data for line chart.
  
  # get data trade helper
  get_trade_data <- function(conn, country, start, end, trade_flow, type = "goods") {
    # call sql export function
    if (type == "goods") {
      
    sql_export_query(conn, country = country, start = start, end = end, trade_flow = trade_flow, type = type)  %>% 
      mutate(primary_value = as.numeric(primary_value),
             fobvalue = as.numeric(fobvalue),
             cifvalue = as.numeric(cifvalue))
      
    } else if (type == "services") {
      
      start_serv <- paste0(start, "01")
      end_serv <- paste0(end, "04")
      
      sql_service_query(conn, country, start_serv, end_serv, trade_flow) %>% 
        mutate(primary_value = as.numeric(primary_value))
      
    } else if (type == "monthly_goods") {
      
      start_month_goods <- paste0(start, "01")
      end_month_goods <- paste0(end, "12")
      
      sql_monthly_export_query(conn, country, start_month_goods, end_month_goods, trade_flow)
      
    }
  }
  
  # declare centralised data storage
  trade_data_rv <- reactiveValues(trade_map_data = NULL,
                                  trade_data_box_commod = NULL,
                                  trade_data_max = NULL,
                                  trade_service_data = NULL)
  
  observe({
    
    # ensure the input exist before pulling any data
    req(trade_input$country, trade_input$year, trade_input$trade_flow_select)
    
    # parameters
    current_year <- as.character(trade_input$year)
    prev_year <- as.character(as.numeric(current_year) - 1)
    
    # trade map data
    trade_data_rv$trade_map_data <- get_trade_data(conn = conn,
                                                   country = trade_input$country,
                                                   start = current_year,
                                                   end = current_year,
                                                   trade_flow = trade_input$trade_flow_select,
                                                   type = "goods")
    
    # trade for valueboxes and top commodities
    export_box_commod <- get_trade_data(conn = conn,
                                        country = trade_input$country,
                                        start = prev_year,
                                        end = current_year,
                                        trade_flow = "X",
                                        type = "goods")
    
    import_box_commod <- get_trade_data(conn = conn,
                                        country = trade_input$country,
                                        start = prev_year,
                                        end = current_year,
                                        trade_flow = "M",
                                        type = "goods")
    
    trade_data_rv$trade_data_box_commod <- do.call(rbind, list(export_box_commod, import_box_commod))
    
    # max year trade data for line chart and sankey
    export_max <- get_trade_data(conn = conn,
                                 country = trade_input$country,
                                 start = min_year_trade,
                                 end = max_year_trade,
                                 trade_flow = "X",
                                 type = "goods")
    
    import_max <- get_trade_data(conn = conn,
                                 country = trade_input$country,
                                 start = min_year_trade,
                                 end = max_year_trade,
                                 trade_flow = "M",
                                 type = "goods")
    
    trade_data_rv$trade_data_max <- do.call(rbind, list(export_max, import_max))
    
    # serivce data periods ranges from 2005Q1 up to 2024Q4, matched the date range with
    # the goods data.
    export_max_serv <- get_trade_data(conn = conn,
                                 country = trade_input$country,
                                 start = min_year_trade,
                                 end = max_year_trade,
                                 trade_flow = "X",
                                 type = "services")
    
    import_max_serv <- get_trade_data(conn = conn,
                                 country = trade_input$country,
                                 start = min_year_trade,
                                 end = max_year_trade,
                                 trade_flow = "M",
                                 type = "services")
    
    trade_data_rv$trade_service_data <- do.call(rbind, list(export_max_serv, import_max_serv))
    
  })
  
  trade_map_function <- function(world_map_data, trade_data, trade_flow, year_select) {
    
    # calculate totals for each partner countries
    total_data <- trade_data %>% 
      filter(reporter_iso != partner_iso) %>% 
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
    
    req(trade_data_rv$trade_map_data)
    
    trade_world_map <- trade_map_function(world_map_data = world_sf,
                                          trade_data = trade_data_rv$trade_map_data,
                                          trade_flow = trade_input$trade_flow_select,
                                          year_select = trade_input$year )
  })
  
  ## stats trade boxes

  trade_stats_box <- function(data, year = NULL, type, what) {
    
    if (what == "value" & !is.null(year)) {
      
      data <- data %>% 
        filter(reporter_iso != partner_iso & period == year) %>%
        group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc) %>%
        summarise(total = sum(primary_value)/1000000000) %>%
        ungroup() %>% select(-flow_code) %>%
        pivot_wider(names_from = flow_desc, values_from = total) %>%
        mutate(two_way_trade = Export + Import, trade_balance = Export - Import)
      
      
      trade_stats_value <- switch(type,
                                  # total two way trade
                                  "two_way" = {
                                    x <- paste0("$", round(data$two_way_trade, 2), " Billion")
                                  },
                                  # total export
                                  "export" = {
                                    x <- paste0("$", round(data$Export, 2), " Billion")
                                  },
                                  # total import
                                  "import" = {
                                    x <- paste0("$", round(data$Import, 2), " Billion")
                                  },
                                  # trade balance
                                  "trade_balance" = {
                                    x <- paste0("$", round(data$trade_balance, 2), " Billion")
                                  }
      )
      
      return(trade_stats_value)
      
    } else if (what == "percent" & is.null(year)) {
      
      data <- data %>% 
        filter(reporter_iso != partner_iso) %>%
        group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc) %>%
        summarise(total = sum(primary_value)/1000000000) %>%
        ungroup() %>% select(-flow_code) %>%
        pivot_wider(names_from = flow_desc, values_from = total) %>%
        mutate(two_way_trade = Export + Import, trade_balance = Export - Import) %>%
        pivot_longer(cols = Import:trade_balance,names_to = "var", values_to = "value") %>%
        arrange(var, period) %>%
        group_by(reporter_iso, reporter_desc, var) %>% 
        mutate(growth = (value - lag(value))/lag(value) * 100) %>% 
        filter(!is.na(growth)) %>% select(-c(period, value)) %>%
        pivot_wider(names_from = var, values_from = growth)
      
      trade_stats_percent <- switch(type,
                                  # total two way trade
                                  "two_way" = {
                                    x <- paste0(round(data$two_way_trade, 2), "%")
                                  },
                                  # total export
                                  "export" = {
                                    x <- paste0(round(data$Export, 2), "%")
                                  },
                                  # total import
                                  "import" = {
                                    x <- paste0(round(data$Import, 2), "%")
                                  },
                                  # trade balance
                                  "trade_balance" = {
                                    x <- paste0(round(data$trade_balance, 2), "%")
                                  }
      )
      return(trade_stats_percent)
    }
    
  }

  ## render text values for stats box
  # two way trade
  # Render the percentage and value outputs
  per_two_way <- reactive({
    str_remove(trade_stats_box( trade_data_rv$trade_data_box_commod, type = "two_way", what = "percent"), "/w")
  })
  
  val_two_way <- reactive({
    trade_stats_box( trade_data_rv$trade_data_box_commod, year = trade_input$year, type = "two_way", what = "value")
  })
  
  output$two_way <- renderUI({
    number_color <- ifelse(per_two_way() > 0, "success", "danger")
    number_icon <- ifelse(per_two_way() > 0, "caret-up", "caret-down")
    descriptionBlock(
      number = per_two_way(),
      numberColor = number_color,
      numberIcon = icon(number_icon),
      header = val_two_way(),
      text = "Total Two-Way Trade",
      marginBottom = FALSE
    )
  })

  # total export trade
  # Render the percentage and value outputs
  per_export <- reactive({
    trade_stats_box( trade_data_rv$trade_data_box_commod, type = "export", what = "percent")
  })
  
  val_export <- reactive({
    trade_stats_box( trade_data_rv$trade_data_box_commod, year = trade_input$year, type = "export", what = "value")
  })
  
  output$trade_export <- renderUI({
    number_color <- ifelse(per_export() > 0, "success", "danger")
    number_icon <- ifelse(per_export() > 0, "caret-up", "caret-down")
    descriptionBlock(
      number = per_export(),
      numberColor = number_color,
      numberIcon = icon(number_icon),
      header = val_export(),
      text = "Total export",
      marginBottom = FALSE
    )
  })
  
  # total import trade
  # Render the percentage and value outputs
  per_import <- reactive({
    trade_stats_box(trade_data_rv$trade_data_box_commod, type = "import", what = "percent")
  })
  
  val_import <- reactive({
    trade_stats_box( trade_data_rv$trade_data_box_commod, year = trade_input$year, type = "import", what = "value")
  })
  
  output$trade_import <- renderUI({
    number_color <- ifelse(per_import() > 0, "success", "danger")
    number_icon <- ifelse(per_import() > 0, "caret-up", "caret-down")
    descriptionBlock(
      number = per_import(),
      numberColor = number_color,
      numberIcon = icon(number_icon),
      header = val_import(),
      text = "Total Import",
      marginBottom = FALSE
    )
  })
  
  # total trade balance trade
  # Render the percentage and value outputs
  per_balance <- reactive({
    trade_stats_box( trade_data_rv$trade_data_box_commod, type = "trade_balance", what = "percent")
  })
  
  val_balance <- reactive({
    trade_stats_box( trade_data_rv$trade_data_box_commod, year = trade_input$year, type = "trade_balance", what = "value")
  })
  
  output$trade_balance <- renderUI({
    number_color <- ifelse(per_balance() > 0, "success", "danger")
    number_icon <- ifelse(per_balance() > 0, "caret-up", "caret-down")
    descriptionBlock(
      number = per_balance(),
      numberColor = number_color,
      numberIcon = icon(number_icon),
      header = val_balance(),
      text = "Trade Balance",
      marginBottom = FALSE
    )
  })
  
  # --------------------------------------------------------------#  
  #                                                               #
  ###-- Bar chart showing top 10 export and import partner  -----###
  #                                                               #
  #---------------------------------------------------------------# 
  
  ## Bar chart showing top 10 export and import partner (selected year)
  trade_bar_chart <- function(data, trade_flow, year) {
    
    data <- data %>%
      group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc, partner_iso) %>% 
      summarise(total_value = round(sum(primary_value)/1000000, 4)) %>% 
      arrange(period, flow_desc, desc(total_value)) %>% 
      mutate(ranking = row_number()) %>% 
      filter(ranking <= 10)
    
    # Filter and sort data
    filtered_data <- data %>%
      filter(flow_code == trade_flow, period == year) %>%
      arrange(desc(total_value)) %>%
      mutate(partner_iso = fct_reorder(partner_iso, total_value))
    
    if (trade_flow == "X") {
      
      # Choose a sequential color palette (e.g., Reds or Blues)
      color_palette <- colorRampPalette(brewer.pal(9, "Blues"))(nrow(filtered_data))
      bar_colors <- color_palette[rank(filtered_data$total_value)]
      
      # Plot
      bar_chart <- plot_ly(
        data = filtered_data,
        x = ~total_value,
        y = ~partner_iso,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = bar_colors,
          line = list(color = 'rgba(80, 80, 80, 0.3)', width = 0.3)
        ),
        text = ~paste0(partner_iso, "<br>Trade: $", format(round(total_value, 2), big.mark = ",")),
        hoverinfo = 'text',
        textposition = 'outside',
        name = ""
      ) %>%
        layout(
          title = list(
            text = "Top Export Partners<br><span style='font-size:14px;'></span>",
            x = 0,
            font = list(size = 14, family = "Arial", color = "#000")
          ),
          xaxis = list(
            title = list(
              text = "Total FOB Value (Million USD)",
              standoff = 30,  # <-- Adds space between axis line and title
              font = list(size = 14)
            ),
            tickformat = ",d",
            showgrid = TRUE,
            gridcolor = 'rgba(220, 220, 220, 0.5)',
            range = c(max(filtered_data$total_value) * 1.3, 0),  # Extended scale
            zeroline = FALSE,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            tickfont = list(size = 12)
          ),
          annotations = list(
            list(
              x = 0,
              y = -0.10,
              text = "Source: UN Comtrade Database",
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              font = list(size = 11, color = 'gray')
            )
          ),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          margin = list(l = 5, r = 10, t = 20, b = 50)
        )
      
    } else if (trade_flow == "M") {
      
      # Choose a sequential color palette (e.g., Reds or Blues)
      color_palette <- colorRampPalette(brewer.pal(9, "Reds"))(nrow(filtered_data))
      bar_colors <- color_palette[rank(filtered_data$total_value)]
      
      # Plot
      bar_chart <- plot_ly(
        data = filtered_data,
        x = ~total_value,
        y = ~partner_iso,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = bar_colors,
          line = list(color = 'rgba(80, 80, 80, 0.3)', width = 0.3)
        ),
        text = ~paste0(partner_iso, "<br>Trade: $", format(round(total_value, 2), big.mark = ",")),
        hoverinfo = 'text',
        textposition = 'outside',
        name = ""
      ) %>%
        layout(
          title = list(
            text = "Top Import Partners<br><span style='font-size:14px;'></span>",
            x = 0,
            font = list(size = 14, family = "Arial", color = "#000")
          ),
          xaxis = list(
            title = list(
              text = "Total CIF Value (Million USD)",
              standoff = 30,  # <-- Adds space between axis line and title
              font = list(size = 14)
            ),
            tickformat = ",d",
            showgrid = TRUE,
            gridcolor = 'rgba(220, 220, 220, 0.5)',
            range = c(0, max(filtered_data$total_value) * 1.3),  # Extended scale
            zeroline = FALSE,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            tickfont = list(size = 12), 
            side = "right"
          ),
          annotations = list(
            list(
              x = 0,
              y = -0.10,
              text = "Source: UN Comtrade Database",
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              font = list(size = 11, color = 'gray')
            )
          ),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          margin = list(l = 5, r = 10, t = 20, b = 50)
        )
      
    }
    return(bar_chart)
  }
  
  output$top_export <- renderUI({
    
    output$top_10_export_bar <- renderPlotly({trade_bar_chart(data =  trade_data_rv$trade_data_box_commod, 
                                                              trade_flow = "X",
                                                              year = trade_input$year)})
      box(
        width = 12,
        status = "primary",
        collapsible = TRUE,
        elevation = 4,
        maximizable = TRUE,
        title = HTML("<b>Top 10 Export Partners</b>"),
        plotlyOutput("top_10_export_bar")
      )
    
  })
  
  output$top_import <- renderUI({
    
    output$top_10_import_bar <- renderPlotly({trade_bar_chart(data =  trade_data_rv$trade_data_box_commod, 
                                                              trade_flow = "M",
                                                              year = trade_input$year)})
    
      box(
        width = 12,
        status = "danger",
        collapsible = TRUE,
        elevation = 4,
        maximizable = TRUE,
        title = HTML("<b>Top 10 Import Partners</b>"),
        plotlyOutput("top_10_import_bar")
        
      )
    
  })
  
  # --------------------------------------------------------------#  
  #                                                               #
  ###----------- top 10 export and import commodities ----------###
  #                                                               #
  #---------------------------------------------------------------#  
  # dataset for top commod
  top_commod_value <- reactive({
    # use trade_data_for_box
    commod_val <-  trade_data_rv$trade_data_box_commod %>%
      left_join(clean_hs_commod, by = join_by(cmd_code == id)) %>% 
      select(period, reporter_iso, reporter_desc, partner_iso, flow_code, flow_desc, cmd_code, text, primary_value) %>% 
      rename("hs_description" = text) %>% 
      left_join(all_countrycode, by = join_by(partner_iso == ISO3_CODE)) %>% 
      group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc, cmd_code, hs_description) %>% 
      summarise(total_value_commod = sum(primary_value)/1000000) %>% 
      arrange(hs_description, flow_desc) %>% 
      ungroup() %>% 
      group_by(reporter_iso, reporter_desc, flow_code, flow_desc, cmd_code, hs_description) %>% 
      mutate(growth_rate = (total_value_commod-lag(total_value_commod))/lag(total_value_commod) * 100)
    
    return(commod_val)
  })
  
  trade_top_commod_bar_chart <- function(data, trade_flow, year) {
    
    # filtered data 
    filtered_data <- data %>% 
      filter(flow_code == trade_flow, period == year) %>% 
      arrange(desc(total_value_commod)) %>% 
      ungroup() %>% 
      mutate(cmd_code = fct_reorder(cmd_code, total_value_commod),
             ranking = row_number()) %>% 
      filter(ranking <= 10)
    
    if (trade_flow == "X") {
      
      # Choose a sequential color palette (e.g., Reds or Blues)
      color_palette <- colorRampPalette(brewer.pal(9, "Greens"))(nrow(filtered_data))
      bar_colors <- color_palette[rank(filtered_data$total_value_commod)]
      
      # Plot
      bar_chart <- plot_ly(
        data = filtered_data,
        x = ~total_value_commod,
        y = ~cmd_code,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = bar_colors,
          line = list(color = 'rgba(80, 80, 80, 0.3)', width = 0.3)
        ),
        text = ~paste0("Trade: $", format(round(total_value_commod, 2), big.mark = ",")),
        textposition = "outside",
        hovertext = ~paste0("HS ", cmd_code, ":", hs_description, "<br>Trade: $", format(round(total_value_commod, 2), big.mark = ",")),
        hoverinfo = "text",
        name = ""
      ) %>%
        layout(
          title = list(
            text = "Top Export Commodities<br><span style='font-size:14px;'></span>",
            x = 0,
            font = list(size = 14, family = "Arial", color = "#000")
          ),
          xaxis = list(
            title = list(
              text = "Total FOB Value (Million USD)",
              standoff = 30,  # <-- Adds space between axis line and title
              font = list(size = 14)
            ),
            tickformat = ",d",
            showgrid = TRUE,
            gridcolor = 'rgba(220, 220, 220, 0.5)',
            range = c(max(filtered_data$total_value_commod) * 1.4, 0),  # Extended scale
            zeroline = FALSE,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            tickfont = list(size = 12)
          ),
          annotations = list(
            list(
              x = 0,
              y = -0.10,
              text = "Source: UN Comtrade Database",
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              font = list(size = 11, color = 'gray')
            )
          ),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          margin = list(l = 5, r = 10, t = 20, b = 50)
        )
      
    } else if (trade_flow == "M") {
      
      # Choose a sequential color palette (e.g., Reds or Blues)
      color_palette <- colorRampPalette(brewer.pal(9, "Oranges"))(nrow(filtered_data))
      bar_colors <- color_palette[rank(filtered_data$total_value_commod)]
      
      # Plot
      bar_chart <- plot_ly(
        data = filtered_data,
        x = ~total_value_commod,
        y = ~cmd_code,
        type = 'bar',
        orientation = 'h',
        marker = list(
          color = bar_colors,
          line = list(color = 'rgba(80, 80, 80, 0.3)', width = 0.3)
        ),
        text = ~paste0("Trade: $", format(round(total_value_commod, 2), big.mark = ",")),
        textposition = "outside",
        hovertext = ~paste0("HS ", cmd_code, ":", hs_description, "<br>Trade: $", format(round(total_value_commod, 2), big.mark = ",")),
        hoverinfo = "text",
        name = ""
      ) %>%
        layout(
          title = list(
            text = "Top Import Commodities<br><span style='font-size:14px;'></span>",
            x = 0,
            font = list(size = 14, family = "Arial", color = "#000")
          ),
          xaxis = list(
            title = list(
              text = "Total CIF Value (Million USD)",
              standoff = 30,  # <-- Adds space between axis line and title
              font = list(size = 14)
            ),
            tickformat = ",d",
            showgrid = TRUE,
            gridcolor = 'rgba(220, 220, 220, 0.5)',
            range = c(0, max(filtered_data$total_value_commod) * 1.4),  # Extended scale
            zeroline = FALSE,
            tickfont = list(size = 12)
          ),
          yaxis = list(
            title = "",
            showgrid = FALSE,
            tickfont = list(size = 12), 
            side = "right"
          ),
          annotations = list(
            list(
              x = 0,
              y = -0.10,
              text = "Source: UN Comtrade Database",
              showarrow = FALSE,
              xref = 'paper',
              yref = 'paper',
              font = list(size = 11, color = 'gray')
            )
          ),
          plot_bgcolor = "#FFFFFF",
          paper_bgcolor = "#FFFFFF",
          margin = list(l = 5, r = 10, t = 20, b = 50)
        )
      
    }
    
  }
  
  html_trade_info_total <- function(data) {
    
    ref_year <- unique(data$ref_year)
    m <- max(ref_year)
    m_y <- m
    m_5 <- m - 5
    m_10 <- m - 10
    
    ref_5 <- as.character(ref_year[ref_year %in% seq(m_5, m, 1)])
    ref_10 <- as.character(ref_year[ref_year %in% seq(m_10, m, 1)])
    
    data_for_total_growth <- data %>%
      nest(data = flow_code:primary_value) %>% 
      mutate(
        total = lapply(data, FUN = function(x) x %>% 
                         group_by(flow_code, flow_desc) %>% 
                         summarise(total = sum(primary_value)/ 1000000000)))%>% 
      select(-data) %>% 
      unnest(total)
    
    total_growth_export_import <- function(x, min_y, max_y, code, what) {
      years <- if (what == "five") ref_5 else ref_10
      lag_n <- if (what == "five") 5L else 10L
      
      x %>%
        filter(period %in% years, flow_code == code) %>%
        arrange(reporter_iso, flow_code, period) %>%
        group_by(reporter_iso, reporter_desc, flow_code, flow_desc) %>%
        mutate(growth = (total - lag(total, n = lag_n)) / lag(total, n = lag_n) * 100) %>%
        filter(period %in% as.character(c(min_y, max_y)))
    }
    
    list(
      tot_5_export  = total_growth_export_import(data_for_total_growth, m_5, m_y, "X", "five"),
      tot_5_import  = total_growth_export_import(data_for_total_growth, m_5, m_y, "M", "five"),
      tot_10_export = total_growth_export_import(data_for_total_growth, m_10, m_y, "X", "ten"),
      tot_10_import = total_growth_export_import(data_for_total_growth, m_10, m_y, "M", "ten")
    )
  }
  
  generate_html_text_total <- function(data, what) {
    
    list_data <- data
    type_word <- ifelse(what == "export", "exported", "imported")
    type_noun <- ifelse(what == "export", "exports", "imports")
    
    if (what == "export") {
      data_5 <- list_data[["tot_5_export"]]
      data_10 <- list_data[["tot_10_export"]]
    } else if (what == "import") {
      data_5 <- list_data[["tot_5_import"]]
      data_10 <- list_data[["tot_10_import"]]
    } else {
      stop("Invalid value for 'what'. Choose 'export' or 'import'.")
    }
    
    # Extract values
    tot_5_total_min <- data_5[1, "total"]
    tot_5_total_max <- data_5[2, "total"]
    tot_5_total_growth <- data_5[2, "growth"]
    tot_5_total_growth_text <- if_else(tot_5_total_growth > 0, "increased", "decreased")
    tot_5_year_min <- data_5[1, "period"]
    tot_5_year_max <- data_5[2, "period"]
    tot_5_country <- data_5[2, "reporter_desc"]
    
    tot_10_total_min <- data_10[1, "total"]
    tot_10_total_max <- data_10[2, "total"]
    tot_10_total_growth <- data_10[2, "growth"]
    tot_10_total_growth_text <- if_else(tot_10_total_growth > 0, "increased", "decreased")
    tot_10_year_min <- data_10[1, "period"]
    tot_10_year_max <- data_10[2, "period"]
    tot_10_country <- data_10[2, "reporter_desc"]
    
    # HTML output
    html_text <- HTML(glue(
      "<div style='font-family: Arial, sans-serif; font-size: 14px; line-height: 1.6; padding-left: 20px; padding-top: 20px; padding-right: 20px; padding-bottom: 20px;'>
       <p>
         <strong>In {tot_5_year_max}</strong>, <strong>{tot_5_country}</strong> {type_word} a total of 
         <strong>${round(tot_5_total_max, 2)} Billion</strong>.
       </p>
       <p>
         Over the past five years, {tot_5_country}'s {type_noun} 
         <strong>{tot_5_total_growth_text}</strong> by 
         <strong>{round(tot_5_total_growth, 0)}%</strong>, 
         from <strong>${round(tot_5_total_min, 2)} Billion</strong> in 
         <strong>{tot_5_year_min}</strong> to 
         <strong>${round(tot_5_total_max, 2)} Billion</strong> in 
         <strong>{tot_5_year_max}</strong>.
       </p>
       <p>
         Over the past ten years, {type_noun} have 
         <strong>{tot_10_total_growth_text}</strong> by 
         <strong>{round(tot_10_total_growth, 0)}%</strong>, from 
         <strong>${round(tot_10_total_min, 2)} Billion</strong> in 
         <strong>{tot_10_year_min}</strong> to 
         <strong>${round(tot_10_total_max, 2)} Billion</strong> in 
         <strong>{tot_10_year_max}</strong>."
    ))
    
    return(html_text)
  }
  
  generate_commodities_html <- function(data, trade_flow, year, description_col = "hs_description", value_col = "total_value_commod") {
    
    data <- data %>% 
      filter(flow_code == trade_flow, period == year) %>% 
      arrange(desc(total_value_commod)) %>% 
      ungroup() %>% 
      mutate(cmd_code = fct_reorder(cmd_code, total_value_commod),
             total_value_commod = round(total_value_commod/1000, 2),
             ranking = row_number()) %>% 
      filter(ranking <= 10)
    
    
    # Validate inputs
    if (!description_col %in% names(data) || !value_col %in% names(data)) {
      stop("Column names provided do not exist in the data.")
    }
    
    if (trade_flow == "X") {trade_noun <- "exports"} else {trade_noun <- "imports"} 
    
    initial_text <- paste0("<p>The most recent ", trade_noun," are led by ")
    end_text <- ".</p></div>"
    
    n <- nrow(data)
    text_vec <- vector("character", n)
    
    for (i in 1:n) {
      comma_or_dot <- if (i != n) "," else ""
      text_vec[i] <- sprintf("<strong>%s</strong> ($%sB)%s", 
                             data[[description_col]][i], 
                             data[[value_col]][i], 
                             comma_or_dot)
    }
    
    final_html <- HTML(paste0(initial_text, paste0(text_vec, collapse = " "), end_text))
    return(final_html)
  }
  
  text_total_list <- reactive({html_trade_info_total(trade_data_rv$trade_data_max)})
    
    output$top_export_commod <- renderUI({
      
      output$top_10_export_bar_commod <- renderPlotly({trade_top_commod_bar_chart(data = top_commod_value(), 
                                                                                  trade_flow = "X",
                                                                                  year = trade_input$year)})
      # text for export
      output$html_text_export <- renderText({paste0(generate_html_text_total(text_total_list(), what = "export"),
                                                    generate_commodities_html(top_commod_value(), trade_flow = "X", year = trade_input$year))})
      
      
      box(
        width = 12,
        status = "olive",
        collapsible = TRUE,
        elevation = 4,
        maximizable = TRUE,
        title = HTML("<b>Top 10 Export Commodities</b>"),
        plotlyOutput("top_10_export_bar_commod"),
        tags$hr(),
        htmlOutput("html_text_export")
      )
      
    })
    
    output$top_import_commod <- renderUI({
      
      output$top_10_import_bar_commod <- renderPlotly({trade_top_commod_bar_chart(data = top_commod_value(), 
                                                                                  trade_flow = "M",
                                                                                  year = trade_input$year)})
    
      # text for import
      output$html_text_import <- renderText({paste0(generate_html_text_total(text_total_list(), what = "import"),
                                                    generate_commodities_html(top_commod_value(), trade_flow = "M", year = trade_input$year))})
      
      box(
        width = 12,
        status = "orange",
        collapsible = TRUE,
        elevation = 4,
        maximizable = TRUE,
        title = HTML("<b>Top 10 Import Commodities</b>"),
        plotlyOutput("top_10_import_bar_commod"),
        tags$hr(),
        htmlOutput("html_text_import")
      )
      
    })
  
    
    # --------------------------------------------------------------#  
    #                                                               #
    ###------------------- Annual Aggregate series----------------###
    #                                                               #
    #---------------------------------------------------------------#  
  ## Line chart for export, import and trade balance in one charts split into export and import 
    
    output$export_import_line_chart <- renderUI({
      
      ts_total_trade <-  trade_data_rv$trade_data_max %>%
        left_join(clean_hs_commod, by = join_by(cmd_code == id)) %>% 
        select(period, reporter_iso, reporter_desc, partner_iso, flow_code, flow_desc, cmd_code, text, primary_value) %>% 
        rename("hs_description" = text) %>% 
        left_join(all_countrycode, by = join_by(partner_iso == ISO3_CODE)) %>% 
        group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc) %>% 
        summarise(total_value = sum(primary_value)/1e9) %>% 
        ungroup() %>%
        arrange(flow_desc) %>% 
        select(-flow_code) %>% 
        pivot_wider(names_from = flow_desc, values_from = total_value) %>% 
        mutate(`Two-Way Trade` = Export + Import,
               `Trade Balance` = Export - Import) %>% 
        pivot_longer(Export:`Trade Balance`, names_to = "flow_desc", values_to = "total_value")
      
      output$line_chart <-  renderHighchart({
        
        # Ensure period is converted to a Date format for proper time handling
        ts_total_trade$period <- as.Date(paste0(ts_total_trade$period, "-01","-01"))
        
        
        # Define color palette (muted, official tones)
        line_chart_palette <- c("#00529F", "#CD590A", "#007A33", "#6BAA75", "#8C4799", "#B58DB6")
        
        # Build chart
        hchart(ts_total_trade %>% filter(flow_desc != "Trade Balance"), type = "line", hcaes(x = period, y = total_value, group = flow_desc)) %>%
          hc_colors(line_chart_palette) %>%
          hc_title(
            text = "Total Trade Over Time",
            style = list(fontSize = "18px", fontWeight = "bold", fontFamily = "Helvetica Neue", color = "#333")
          ) %>%
          hc_xAxis(
            type = "datetime",
            title = list(text = "Period", style = list(fontSize = "14px", fontWeight = "normal")),
            lineColor = "#cccccc",
            tickColor = "#cccccc",
            labels = list(style = list(fontSize = "12px", color = "#333"))
          ) %>%
          hc_yAxis(
            title = list(text = "Total Trade Value (Billion USD)", style = list(fontSize = "14px", fontWeight = "normal")),
            gridLineColor = "#EAEAEA",
            labels = list(format = "{value}", style = list(fontSize = "12px", color = "#333"))
          ) %>%
          hc_plotOptions(
            series = list(
              marker = list(enabled = TRUE, radius = 5),
              lineWidth = 3,
              animation = list(duration = 1000)
            )
          ) %>%
          hc_tooltip(
            shared = TRUE,
            crosshairs = TRUE,
            valueDecimals = 2,
            valueSuffix = "B",
            valuePrefix = "$",
            headerFormat = '<b>{point.key}</b><br>',
            style = list(fontSize = "12px")
          ) %>%
          hc_legend(
            layout = "horizontal",
            align = "center",
            verticalAlign = "bottom",
            itemStyle = list(fontSize = "12px", fontWeight = "normal", color = "#333")
          ) %>%
          hc_chart(
            backgroundColor = "#FFFFFF",
            style = list(fontFamily = "Helvetica Neue")
          ) %>%
          hc_credits(
            enabled = TRUE,
            text = "Source: UN Comtrade",
            style = list(fontSize = "11px", color = "#666"))
        
      })
      
      
      output$bar_chart <-  renderHighchart({
        
        # Ensure period is converted to a Date format for proper time handling
        ts_total_trade$period <- as.Date(paste0(ts_total_trade$period, "-01","-01"))
        
        
        highchart() %>% 
          hc_add_series(data = ts_total_trade %>% filter(flow_desc == "Trade Balance"),
                        type = "l",
                        mapping = hcaes(x = period,))
        
        
        # Filter once
        trade_balance_data <- ts_total_trade %>% 
          filter(flow_desc == "Trade Balance")
        
        highchart() %>% 
          hc_chart(type = "column") %>%  # Set base type (bar is vertical column in Highcharts)
          
          # Add bar series
          hc_add_series(
            data = trade_balance_data,
            type = "column",
            name = "Trade Balance (Bar)",
            color = "#1f77b4",
            mapping = hcaes(x = period, y = total_value)
          ) %>% 
          
          # Add line series
          hc_add_series(
            data = trade_balance_data,
            type = "line",
            name = "Trade Balance (Line)",
            color = "#ff7f0e",
            mapping = hcaes(x = period, y = total_value)
          ) %>%
          
          # Shared tooltip
          hc_tooltip(shared = TRUE,
                     crosshairs = TRUE,
                     valueDecimals = 0, 
                     valuePrefix = "$",
                     valueSuffix = "B",
                     style = list(fontSize = "12px")) %>%
          
          hc_chart(
            backgroundColor = "#FFFFFF",
            style = list(fontFamily = "Helvetica Neue")
          ) %>% 
          
          # X-axis formatting
          hc_xAxis(
            type = "datetime",
            title = list(text = "Period"),
            labels = list(rotation = -45)
          ) %>%
          
          # Y-axis
          hc_yAxis(
            title = list(text = "Total Value ($ Billion)"),
            labels = list(format = "${value:,.0f}B")
          ) %>%
          
          hc_credits(
            enabled = TRUE,
            text = "Source: UN Comtrade",
            style = list(fontSize = "11px", color = "#666")
          ) %>% 
          
          # Chart title
          hc_title(text = "Trade Balance Over Time",
                   style = list(fontSize = "18px", fontWeight = "bold", fontFamily = "Helvetica Neue", color = "#333"))
        
        
      })
      
      tabBox(title = HTML("<b>Aggregate Time Series</b>"),
             type = "tabs",
             status = "info",
             width = 12,
             elevation = 4,
             id = 'tab_for_trade_series',
             maximizable = TRUE,
             collapsible = TRUE,
             tabPanel(title = HTML("<b>Total Annual Trade</b>"),
                      highchartOutput("line_chart", height = "400px")
                      ),
             tabPanel(title = HTML("<b>Annual Trade Balance</b>"),
                      highchartOutput("bar_chart", height = "400px"))
             )
    })
    
    # --------------------------------------------------------------#  
    #                                                               #
    ###------------------- Sankey Analysis page ------------------###
    #                                                               #
    #---------------------------------------------------------------#
    sankey_trade_chart <- function(data, country, what, year, hs_vector, commod, countrycode) {
      
      if(length(hs_vector) > 6) {
        stop("aborting, the maximum length is 6 or less")
      } else if (length(hs_vector) == 0) {
        stop("hs_vector argument is missing, with no default")
      }
      
      data_sankey_base <- data %>% 
        left_join(commod, by = join_by(cmd_code == id)) %>% 
        select(period, reporter_iso, reporter_desc, partner_iso, flow_code, flow_desc, cmd_code, text, primary_value) %>% 
        rename("hs_description" = text) %>% 
        left_join(countrycode, by = join_by(partner_iso == ISO3_CODE))
      
      sankey_chart_builder <- function(data, country, type, year, hs_commod) {
        
        if (type == "Export") {
          
          select_flow_code <- "X"
          trade_noun <- "Export"
          arrow <- "→"
          
        } else if (type == "Import"){
          
          select_flow_code <- "M"
          trade_noun <- "Import"
          arrow <- "←"
          
        }
        
        # Build the commodity→partner edges
        data_one <- data %>% 
          group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc, cmd_code, hs_description, partner_iso) %>% 
          summarise(total_value_commod = sum(primary_value)) %>% 
          filter(flow_code == select_flow_code & period == year) %>% 
          arrange(cmd_code) %>% 
          ungroup() %>% 
          select(hs_description, partner_iso, total_value_commod) %>% 
          group_by(hs_description) %>% 
          mutate(total = sum(total_value_commod),
                 weight = round(total_value_commod/1e6, 2),
                 q25_weight = quantile(weight, probs = c(0.25)),
                 q50_weight = quantile(weight, probs = c(0.50)),
                 q75_weight = quantile(weight, probs = c(0.75)),
                 q95_weight = quantile(weight, probs = c(0.95)),
                 partner_iso = if_else(weight < q95_weight, "ROW", partner_iso),
                 id = paste0(hs_description, partner_iso),
          ) %>% 
          select(-c(total_value_commod, total)) %>% 
          relocate(hs_description, partner_iso, weight, id) %>% 
          rename("from" = hs_description,
                 "to" = partner_iso) %>% 
          filter(from %in% hs_commod)
        
        # Build the reporter→commodity edges
        data_two <- data %>% 
          group_by(period, reporter_iso, reporter_desc, flow_code, flow_desc, cmd_code, hs_description) %>% 
          summarise(total_value = sum(primary_value)) %>% 
          filter(flow_code == select_flow_code  & period == year) %>% 
          group_by(hs_description) %>% 
          mutate(weight = round(total_value/1e6, 2),
                 id = paste0(reporter_desc, hs_description),
          ) %>% 
          select(reporter_desc, hs_description, weight, id) %>% 
          rename("from" = reporter_desc,
                 "to" =  hs_description) %>% 
          filter(to %in% hs_commod)
        
        data_final_sankey <- rbind(data_one, data_two)
        
        sankey_chart <- hchart(
          data_final_sankey, 
          type = "sankey", 
          name = paste0(trade_noun, " Profile")
        ) %>%
          
          hc_chart(
            backgroundColor = "#FFFFFF",
            style = list(fontFamily = "Helvetica Neue")
          ) %>% 
          
          hc_title(
            text = paste0(year," ",country," ",trade_noun," Profile"),
            style = list(fontSize = "18px", fontWeight = "bold", fontFamily = "Helvetica Neue", color = "#333")
          ) %>% 
          
          hc_credits(
            enabled = TRUE,
            text = "Source: UN Comtrade",
            style = list(fontSize = "11px", color = "#666")
          ) %>% 
          
          hc_tooltip(
            pointFormat = sprintf("<b>{point.fromNode.name}</b> %s <b>{point.toNode.name}</b>: ${point.weight} M USD", arrow)
          )
        
        return(sankey_chart)
      }
      
      sankey_chart_final <- sankey_chart_builder(data_sankey_base, country, what, year, hs_vector)
      
      return(sankey_chart_final)
    }
    
    output$sankey_chart <- renderUI({
      
      my_css <- "
                 .bs-select-all {
                   display: none;
                 }
                 .bs-deselect-all {
                   width: 100%;
                 }
              "
      
      box(
        width = 12,
        status = "olive",
        title = HTML("<b>International Trade Flow by Commodities</b>"),
        elevation = 4,
        fluidRow(
          column(
            width = 8,
            tags$head(tags$style(HTML(my_css))),
            pickerInput(
              inputId = "sankey_hs_input",
              label = "Select HS Commodities",
              choices = sort(clean_hs_commod$text),
              selected = sort(clean_hs_commod$text)[1:4],
              multiple = TRUE,
              options = pickerOptions(maxOptions = 6,
                                      maxOptionsText = "Sorry, picking further than this would affect the readability of the chart",
                                      actionsBox = TRUE,
                                      liveSearch = TRUE)
            )
          ),
          column(
            width = 4,
            radioGroupButtons(
              inputId = "sankey_what_input",
              label = "Select Trade Flow",
              selected = "Export",
              choices = c("Export", "Import"),
              justified = TRUE,
              checkIcon = list(
                yes = icon("ok", lib = "glyphicon")
              )
            )
          )
        ),
        highchartOutput(outputId = "sankey_trade_output", height = "650px"),
        maximizable = TRUE
      )
      
    })
    
    output$sankey_trade_output <- renderHighchart({
      req(input$sankey_hs_input)           # ← wait until it’s non‑null
      
      sankey_trade_chart(
        data        = trade_data_rv$trade_data_max,
        country     = trade_input$country,
        what        = input$sankey_what_input,
        year        = trade_input$year,
        hs_vector   = input$sankey_hs_input,
        commod      = clean_hs_commod,
        countrycode = all_countrycode
      )

    })
    
    ###----------------------------------------------------------###
    ### ----------------- Services Summary stats ----------------###
    ###----------------------------------------------------------###
    # title generation
    output$service_title <- renderUI({
      fluidRow(
        column(
          width = 6,
          div(
            tags$br(),
            tags$br(),
            tags$h3("🌍 International Trade in Services (Service Export and Import)",
                    class = "animated-h3"),
            tags$hr(class = "animated-hr"))
        )
      )
    })

    # data for service boxes
    service_data_for_box <- reactive({
      
      trade_data_rv$trade_service_data %>%
        group_by(ref_year, reporter_iso, reporter_desc,flow_code, flow_desc, cmd_code, cmd_desc) %>% 
        summarise(total_value = sum(primary_value)/1000) %>% 
        ungroup() %>% 
        filter(cmd_code == "S") %>% 
        select(-flow_code) %>% 
        pivot_wider(names_from = flow_desc, values_from = total_value) %>% 
        mutate(`Trade Balance` = round(Exports - Imports, 2),
               Exports = round(Exports, 2),
               Imports = round(Imports, 2))
    })
    
    
    # valueboxes 
    output$service_valuebox <- renderUI({
      val_tot_exp <- service_data_for_box()$Exports[service_data_for_box()$ref_year == trade_input$year]
      val_tot_imp <- service_data_for_box()$Imports[service_data_for_box()$ref_year == trade_input$year]
      val_tot_bal <- service_data_for_box()$`Trade Balance`[service_data_for_box()$ref_year == trade_input$year]
     
      fluidRow( 
      # 1. total services export
      column(width = 4,
      valueBox(
        width = 12,
        subtitle = HTML("<b>Service Export</b>"),
        value = h5(paste0("$", val_tot_exp, "B")),
        color = "primary",
        gradient = TRUE,
        icon = icon("paper-plane"),
        elevation = 4
      )),
    # 2. total services import
      column(width = 4,
      valueBox(
        width = 12,
        subtitle = HTML("<b>Service Import</b>"),
        value = h5(paste0("$", val_tot_imp, "B")),
        color = "olive",
        gradient = TRUE,
        icon = icon("envelope-open-text"),
        elevation = 4
      )),
    # 3. total services trade balance
      column(width = 4,
      valueBox(
        width = 12,
        subtitle = HTML("<b>Trade Balance</b>"),
        value = h5(paste0("$", val_tot_bal, "B")),
        color = if_else(val_tot_bal < 0, "danger", "success"),
        gradient = TRUE,
        icon = icon(if_else(val_tot_bal < 0, "arrow-down", "arrow-up")),
        elevation = 4
      ))
      )
    })
    
    # service breakdown treemap
    total_service_map <- reactive({
      
      trade_data_rv$trade_service_data %>% 
        group_by(ref_year, reporter_iso, reporter_desc,flow_code, flow_desc, cmd_code, cmd_desc) %>% 
        summarise(total_value = sum(primary_value)/1000) %>% 
        filter(!cmd_code %in% c("S", "SOX", "SPX1")) %>% 
        ungroup() %>% 
        group_by(ref_year, reporter_iso, reporter_desc,flow_code, flow_desc) %>% 
        mutate(total_year = sum(total_value),
               share_percent = (total_value/total_year) * 100)
      
    })
    
    output$top_export_import_service <- renderUI({
      
      box(
        title = tagList(
          div(
            style = "display: flex; justify-content: space-between; align-items: center; width: 100%;",
            
            # Left: Title
            span(HTML("<strong>Services Breakdown</strong>")),
            
            # Right: Radio Buttons
            div(
              style = "
                position: absolute;
                top: 7px;
                right: 80px;
                padding-top: 2px;
                padding-right: 10px;
              ",
              radioGroupButtons(
                inputId = "service_what_input",
                label = NULL,
                choices = c("Exports", "Imports"),
                selected = "Exports",
                size = "sm",
                checkIcon = list(yes = icon("check"))
              )
            )
          )
        ),
        width = 12,
        maximizable = TRUE,
        elevation = 4,
        status = "info",
        collapsible = TRUE,
        
        # Treemap output
         highchartOutput("treemap_service",height = "800px")
        
      )
      
    })
    
    output$treemap_service <- renderHighchart({
      req(trade_input$year, input$service_what_input)
      total_service_map() %>%
        filter(ref_year == trade_input$year & flow_desc == input$service_what_input) %>%
        ungroup() %>% 
        data_to_hierarchical(cmd_desc, total_value) %>%
        hchart(type = "treemap") %>% 
        hc_tooltip(pointFormat = "<b>{point.name}</b><br>Value: ${point.value:,.2f}B") %>% 
        hc_plotOptions(
          treemap = list(
            layoutAlgorithm = "squarified",
            allowPointSelect = TRUE,
            dataLabels = list(
              enabled = TRUE,
              format = '{point.name}',
              style = list(
                fontSize = '13px'
              )
            )
          )
        ) %>% 
        hc_credits(enabled = TRUE,
                   text = "Source: UN Comtrade",
                   style = list(fontSize = "12px", color = "#666"))
    })
    
    # data for service line chart
    # helper function
    service_data_summariser <- function(data, freq = c("annual", "quarterly"), what = c("total", "commodities")) {
      freq <- match.arg(freq)
      what <- match.arg(what)
      
      filtered_data <- data %>% 
        filter(!cmd_code %in% c("SOX", "SPX1")) %>% 
        mutate(primary_value = primary_value / 1000)  
      
      if (freq == "annual" && what == "total") {
        return_data <- filtered_data %>%
          filter(cmd_code == "S") %>% 
          group_by(ref_year, reporter_iso, reporter_desc, flow_code, flow_desc, cmd_code, cmd_desc) %>% 
          summarise(primary_value = sum(primary_value), .groups = "drop") %>% 
          mutate(period = as.character(ref_year)) %>% 
          select(-ref_year)
        
      } else if (freq == "annual" && what == "commodities") {
        return_data <- filtered_data %>%
          filter(cmd_code != "S") %>% 
          group_by(ref_year, reporter_iso, reporter_desc, flow_code, flow_desc, cmd_code, cmd_desc) %>% 
          summarise(primary_value = sum(primary_value), .groups = "drop") %>% 
          mutate(period = as.character(ref_year)) %>% 
          select(-ref_year)
        
      } else if (freq == "quarterly" && what == "total") {
        return_data <- filtered_data %>% 
          filter(cmd_code == "S") %>% 
          mutate(period = ref_period_id) %>% 
          select(-ref_period_id)
        
      } else if (freq == "quarterly" && what == "commodities") {
        return_data <- filtered_data %>% 
          filter(cmd_code != "S")%>% 
          mutate(period = ref_period_id) %>% 
          select(-ref_period_id)
        
      } else {
        stop("Invalid combination of freq and what.")
      }
      
      return(return_data)
    }
    
    serv_exp_imp_balance <- function(serv_data) {
      
      serv_data %>% 
        select(-c(flow_code)) %>% 
        relocate(period) %>% 
        pivot_wider(names_from = flow_desc, values_from = primary_value) %>% 
        relocate(Exports, Imports, .after = cmd_desc) %>% 
        mutate(`Trade Balance` = Exports-Imports) %>% 
        pivot_longer(cols = Exports:`Trade Balance`, names_to = "flow_desc", values_to = "primary_value")
      
    }
    
    ##line chart
    ## time series for services export, import and trade balance
    # render UI
    output$service_line_chart <- renderUI({
      
     box(
        id = "service_line_box",
        title = HTML("<strong>Service Aggregate Series</strong>"),
        width = 12, 
        elevation = 4,
        type = "tabs",
        collapsible = TRUE,
        maximizable = TRUE,
        status = "primary",
        # services export, import and trade balance
        fluidRow(
          column(
            width = 6,
            pickerInput(
              inputId = "select_serv_aggr",
              label = NULL,
              width = "150px",
              choices = c("Annual", "Quarterly"),
              selected = "Annual",
              options = list(`style` = "btn-default btn-sm")
            )
          )
        ),
        highchartOutput(outputId = "service_line_output", height = "550px"),
        dataTableOutput(outputId = "service_table", height = "250px")
     )
    })
    
    
    output$service_line_output <- renderHighchart({
      req(input$select_serv_aggr)
      # data for chart
      data <- service_data_summariser(trade_data_rv$trade_service_data, freq = tolower(input$select_serv_aggr))
      
      # calculate necessary columns
      serv_exp_imp_bal_data <- serv_exp_imp_balance(data)
      
      hchart(serv_exp_imp_bal_data, type = "line", 
             hcaes(x = period, y = primary_value, group = flow_desc),
             connectNulls = TRUE,
             visible = c(TRUE, TRUE, FALSE)
             ) %>% 
        hc_xAxis(
          title = list(text = "Period", style = list(fontSize = "14px")),
          labels = list(style = list(fontSize = "12px"))
        ) %>% 
        hc_yAxis(
          title = list(text = "Total Trade Value (Billion USD)", 
                       style = list(fontSize = "14px")),
          labels = list(format = "{value}", 
                        style = list(fontSize = "12px"))
        ) %>% 
        hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          valueDecimals = 2,
          valueSuffix = "B",
          valuePrefix = "$",
          headerFormat = '<b>{point.key}</b><br>',
          style = list(fontSize = "12px")
        ) %>% 
        hc_legend(
          layout = "vertical",
          align = "left",
          verticalAlign = "top",
          floating = TRUE,
          x = 80, 
          y = 0,
          itemStyle = list(fontSize = "12px")
        ) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: UN Comtrade",
          style = list(fontSize = "11px"))
    })
    
    # data table 
    output$service_table <- renderDataTable({
      
      data <- service_data_summariser(trade_data_rv$trade_service_data, freq = tolower(input$select_serv_aggr))
      
      # calculate necessary columns
      serv_exp_imp_bal_data <- serv_exp_imp_balance(data) %>%
        select(period, reporter_desc, flow_desc, primary_value) %>% 
        mutate(primary_value = paste0("$",round(primary_value, 2),"B")) %>% 
        pivot_wider(names_from = flow_desc, values_from = primary_value) %>% 
        rename("Period" = period, 
               "Country" = reporter_desc)
      
      datatable(
        serv_exp_imp_bal_data,
        rownames = FALSE,
        extensions = "Buttons",
        options = list(
          pageLength = 5,
          paging = TRUE,
          dom = "tp"
        ),
        caption = htmltools::tags$caption(
          style = 'caption-side: top; text-align: left;',
          'Export-Import Balance of Services'
        )
      )
    })
    
    ###----------------------------------------------------------###
    ### ----------------- Commodity Analysis page ---------------###
    ###----------------------------------------------------------###
    
    ## WHAT DO I NEED?
    
    ## PLAY AROUND WITH INSERT UI AND REMOVE UI 
    # plan is for the user to pick which country, designated market (country grouping), 
    # commodities and the year?
    # then when press "OK" it will insert a UI page filled with Export and import chart
    # and interesting key facts.
    
    # HS + services concordances table
    output$concordance_codes <-  renderDataTable({
      
      datatable(combined_concord ,
                 rownames = FALSE,
                 filter = c("top"),
                 extensions = 'Buttons',
                 options = list(dom = 'Bfltp', 
                                scrollX = TRUE,
                                buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                pageLength = 10,
                                lengthMenu = list(c(10, -1), list('10', 'All')),
                                searchHighlight = TRUE,
                                search = list(regex = TRUE, caseInsensitive = FALSE )
                   ),
                 colnames = c("HS + Services Code", 'Classification')
        )
      })
    concordance_code_proxy <- dataTableProxy("concordance_codes")
    
    # clear all selection
    observeEvent(input$clear_hsserv_table,{
      # add reset button(remove UI)
      concordance_code_proxy  %>% selectRows(NULL)
    })

    # build data -----------------
    # set reactivevalues to store inputs 
    rv_commod_analysis <- reactiveValues()
    
    observe({

      # ensure the input exist before pulling any data
      req(input$select_country_commod, input$select_exp_imp_commod)
      
      # storing reactive parameters
      rv_commod_analysis$country_commod <- input$select_country_commod # selected country
      rv_commod_analysis$exp_imp_commod <- input$select_exp_imp_commod # export or import
      rv_commod_analysis$commod_table_filter <- combined_concord[input$concordance_codes_rows_selected, ] # filtered hs+serv table
      
      
      # parameters (use maximum year available in the database)
      max_year <- max_year_trade
      min_year <- min_year_trade

      # annual trade data
      export_max <- get_trade_data(conn = conn,
                                   country = input$select_country_commod,
                                   start = min_year,
                                   end = max_year,
                                   trade_flow = "X",
                                   type = "goods")

      import_max <- get_trade_data(conn = conn,
                                   country = input$select_country_commod,
                                   start = min_year_trade,
                                   end = max_year_trade,
                                   trade_flow = "M",
                                   type = "goods")

      rv_commod_analysis$trade_commod_annual_goods_data <- do.call(rbind, list(export_max, import_max)) %>% mutate(trade_type = "goods")


      # monthly trade data
      monthly_export <- get_trade_data(conn = conn,
                                        country = input$select_country_commod,
                                        start = min_year_trade,
                                        end = max_year_trade,
                                        trade_flow = "X",
                                        type = "monthly_goods")
      
      monthly_import<- get_trade_data(conn = conn,
                                        country = input$select_country_commod,
                                        start = min_year_trade,
                                        end = max_year_trade,
                                        trade_flow = "M",
                                        type = "monthly_goods")
      
      rv_commod_analysis$trade_commod_monthly_data <- do.call(rbind, list(monthly_export, monthly_import)) %>%
        mutate(trade_type = "goods") %>% 
        # harmonising hs codes, due to different HS revision and versions 
        select(-cmd_desc) %>% 
        inner_join(combined_concord, by = join_by("cmd_code" == "id")) %>% 
        rename("cmd_desc" = text)
      
      
      # serivce data periods ranges from 2005Q1 up to 2024Q4, matched the date range with
      export_max_serv <- get_trade_data(conn = conn,
                                        country = input$select_country_commod,
                                        start = min_year_trade,
                                        end = max_year_trade,
                                        trade_flow = "X",
                                        type = "services")

      import_max_serv <- get_trade_data(conn = conn,
                                        country = input$select_country_commod,
                                        start = min_year_trade,
                                        end = max_year_trade,
                                        trade_flow = "M",
                                        type = "services")

      rv_commod_analysis$trade_service_commod_data <- do.call(rbind, list(export_max_serv, import_max_serv)) %>%
        mutate(trade_type = "services") %>% 
        filter(!cmd_code %in% c("S"))

    })
    
    # function to summarised based on the selection of frequency (monthly, quarterly, annual)
    monthly_trade_summariser_func <- function(data, freq, aggregate_by_code = TRUE) {
      
      # Preprocess trade data
      trade_data <- data %>% 
        mutate(
          month = substr(period, 5, 6),
          date_str = paste0(ref_year, "-", month, "-01"),
          quarter = paste0("Q", lubridate::quarter(lubridate::ymd(date_str)), " ", ref_year),
          ref_quarter = as.numeric(paste0(ref_year, lubridate::quarter(lubridate::ymd(date_str)))),
          period = paste0(ref_year, "-", month)
        )
      
      # Base grouping columns
      group_cols <- c("reporter_iso", "reporter_desc", "partner_iso", "flow_code", "flow_desc", "trade_type")
      if (aggregate_by_code) {
        group_cols <- c(group_cols, "cmd_code", "cmd_desc")
      }
      
      # Apply summary logic based on frequency
      trade_output_summarise <- switch(freq,
                                       "Monthly" = {
                                         trade_data %>%
                                           group_by(across(all_of(c("period", group_cols)))) %>%
                                           summarise(total_trade_value = sum(primary_value), .groups = "drop") %>% 
                                           relocate(period) %>% 
                                           mutate(period = trimws(period),
                                                  period = as.Date(paste0(period, "-01")))
                                       },
                                       "Quarterly" = {
                                         trade_data %>%
                                           group_by(across(all_of(c("quarter", "ref_quarter", group_cols)))) %>%
                                           summarise(total_trade_value = sum(primary_value), .groups = "drop") %>%
                                           mutate(period = quarter,
                                                  period = trimws(period)) %>% 
                                                  # ref_quarter = as.Date(paste0(ref_quarter, "01"))) %>%
                                           arrange(ref_quarter) %>%
                                           select(-quarter) %>% 
                                           relocate(period)
                                       },
                                       "Annual" = {
                                         trade_data %>%
                                           group_by(across(all_of(c("ref_year", group_cols)))) %>%
                                           summarise(total_trade_value = sum(primary_value), .groups = "drop") %>%
                                           arrange(ref_year) %>% 
                                           mutate(period = as.character(ref_year),
                                                  period = trimws(period)) %>%
                                           select(-ref_year) %>% 
                                           relocate(period)
                                       },
                                       stop("Invalid frequency. Choose from 'Monthly', 'Quarterly', or 'Annual'.")
      )
      
      return(trade_output_summarise)
    }
    

    service_trade_summariser_func <- function(data, freq, aggregate_by_code = FALSE) {
      
      # Base grouping columns
      group_cols <- c("reporter_iso", "reporter_desc", "partner_iso", "flow_code", "flow_desc", "trade_type")
      if (aggregate_by_code) {
        group_cols <- c(group_cols, "cmd_code", "cmd_desc")
      }
      
      trade_output_summarise <- switch(freq,
                                       # quarterly
                                       "Quarterly" = {
                                         data %>%
                                           group_by(!!!syms(c("ref_period_id", group_cols))) %>%
                                           summarise(total_trade_value = sum(primary_value), .groups = "drop") %>% 
                                           rename("period" = ref_period_id)  %>% 
                                           mutate(period = trimws(period),
                                                  ref_period = as.numeric(paste0(substr(period, 4, 7), substr(period, 2,2)))) %>% 
                                           arrange(ref_period) %>% 
                                           select(-ref_period)
                                         
                                       },
                                       # annual
                                       "Annual" = {
                                         data %>% 
                                           group_by(!!!syms(c("ref_year", group_cols))) %>% 
                                           summarise(total_trade_value = sum(primary_value)) %>% 
                                           ungroup() %>% 
                                           mutate(period = as.character(ref_year),
                                                  period = trimws(period)) %>% 
                                           relocate(period) %>% 
                                           select(-ref_year)
                                       })
      
      return(trade_output_summarise)
    }
    
    
    # build data set
    observe({
      req(
        rv_commod_analysis$commod_table_filter,
        length(rv_commod_analysis$commod_table_filter) > 0,
        rv_commod_analysis$trade_commod_monthly_data,
        rv_commod_analysis$trade_service_commod_data
      )
      
      rv_commod_analysis$bind_data <- rv_commod_analysis$trade_commod_monthly_data %>% 
        arrange(ref_year) %>% 
        mutate(ref_period_id = as.character(ref_period_id),
               ref_month = as.character(ref_month),
               period = as.character(period),
               primary_value = primary_value/1e6) %>% 
        bind_rows(rv_commod_analysis$trade_service_commod_data) %>% 
        mutate(flow_desc = str_replace(flow_desc, "s", ""))
      
      rv_commod_analysis$commod_data_set <-  rv_commod_analysis$bind_data %>%
        filter(flow_desc == rv_commod_analysis$exp_imp_commod, 
               cmd_code %in% c(rv_commod_analysis$commod_table_filter$id))
      
    })
    
    # check if services exist in the data
    observe({
      rv_commod_analysis$check_services_code <- unique(rv_commod_analysis$commod_data_set$trade_type)
      })
    
    observe({print(input$select_commod_val_agg)})
    
    # data processing for selected commodities
    data_for_commod_live_val_chart <- reactive({
      req(rv_commod_analysis$commod_data_set , input$select_commod_val_aggr, rv_commod_analysis$check_services_code)
      
      # data for chart 
      ########## FUNCTIONISED THIS BAD BOY 
      if (length(rv_commod_analysis$check_services_code) > 1) {
        data_goods <- rv_commod_analysis$commod_data_set  %>% filter(trade_type == "goods")
        data_serv <- rv_commod_analysis$commod_data_set  %>% filter(trade_type == "services")
        
        data_goods_final <- monthly_trade_summariser_func(data_goods, input$select_commod_val_aggr, aggregate_by_code = TRUE)
        data_serv_final <- service_trade_summariser_func(data_serv, input$select_commod_val_aggr, aggregate_by_code = TRUE)
        
        data_final <- data_goods_final %>% 
          bind_rows(data_serv_final) %>% 
          mutate(period = trimws(period))
        
      } else if (rv_commod_analysis$check_services_code %in% c("goods")) {

        data_final <- monthly_trade_summariser_func(rv_commod_analysis$commod_data_set, input$select_commod_val_aggr, aggregate_by_code = TRUE)
        
      } else {

        data_final <- service_trade_summariser_func(rv_commod_analysis$commod_data_set, input$select_commod_val_aggr, aggregate_by_code = TRUE)
        
      }
    })
    
    
    ## line chart for selected codes
    output$commod_line_val_chart <- renderHighchart({
      req(data_for_commod_live_val_chart(), input$select_commod_val_aggr)
      
      hchart(data_for_commod_live_val_chart(), type = "line", 
             hcaes(x = period, y = total_trade_value, group = cmd_desc),
             connectNulls = TRUE
      ) %>% 
        hc_xAxis(
          title = list(text = "Period", style = list(fontSize = "14px")),
          labels = list(style = list(fontSize = "12px"))
        ) %>% 
        hc_yAxis(
          title = list(text = "Total Trade Value (Million USD)", 
                       style = list(fontSize = "14px")),
          labels = list(format = "{value}", 
                        style = list(fontSize = "12px"))
        ) %>% 
        hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          valueDecimals = 2,
          valueSuffix = "M",
          valuePrefix = "$",
          headerFormat = '<b>{point.key}</b><br>',
          style = list(fontSize = "12px")
        ) %>% 
        hc_legend(
          layout = "vertical",
          align = "left",
          verticalAlign = "top",
          floating = TRUE,
          x = 80, 
          y = 0,
          itemStyle = list(fontSize = "12px")
        ) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: UN Comtrade",
          style = list(fontSize = "11px"))
    })
    
    # export name 
    output$title_commod_val <- renderText({
      req(rv_commod_analysis$exp_imp_commod)
      paste0(rv_commod_analysis$exp_imp_commod, " for selected commodities")
    })
    
    ## Insert UI
    insertUI(
      selector = "#commod_value_line_table",
      ui = 
        div(id = "commod_value_line_table_add",
            style = "background-color: #FFFFFF; padding: 20px; border: 1px solid #ccc; border-radius: 10px;",
            tags$br(),
            fluidRow(
              div(
                style = "text-align: center; 
                         font-size: 28px;
                         font-weight: 700;
                         color: #2c3e50;
                         padding: 5px;
                         margin-bottom: 15px;
                         margin-top: -20px;
                         margin-left: 5px",
                textOutput("title_commod_val")
              ),
              column(
                width = 12,
                div(
                  style = "padding: 3px; margin-left: 18px; margin-bottom: 3px;",
                  pickerInput(
                    inputId = "select_commod_val_aggr",
                    label = NULL,
                    width = "150px",
                    choices = c("Monthly","Quarterly", "Annual"),
                    selected = "Monthly",
                    options = list(`style` = "btn-default btn-sm")
                  )
              )
            ),
            highchartOutput("commod_line_val_chart")
        )
      )
    )
      
    calc_growth_share_commod <- function(select_input_commod, what, trade_data_commod, row_filter_vector){
      
      # find total export/import by frequency
      total_data_goods <- trade_data_commod %>%
        filter(trade_type == "goods" & flow_desc == what)
      
      total_data_services <-  trade_data_commod %>%
        filter(trade_type == "services" & flow_desc == what)
      
      data_goods_fil <-  trade_data_commod  %>%
        filter(trade_type == "goods" & flow_desc == what & cmd_code %in% c(row_filter_vector))
      
      data_services_fil <-  trade_data_commod  %>%
        filter(trade_type == "services" & flow_desc == what & cmd_code %in% c(row_filter_vector))
      
      total_data_goods_out <- monthly_trade_summariser_func(total_data_goods ,
                                                            freq = select_input_commod,
                                                            aggregate_by_code = FALSE) 
      
      total_data_services_out <- service_trade_summariser_func(total_data_services,
                                                               freq = select_input_commod,
                                                               aggregate_by_code = FALSE)
      
      total_data_goods_out_commod <- monthly_trade_summariser_func(data_goods_fil,
                                                                   freq = select_input_commod,
                                                                   aggregate_by_code = TRUE)
      
      total_data_services_out_commod <- service_trade_summariser_func(data_services_fil,
                                                                      freq = select_input_commod,
                                                                      aggregate_by_code = TRUE)
      
      data_final_total <- total_data_goods_out %>%
        bind_rows(total_data_services_out) %>%
        rename("total_trade_freq" = total_trade_value)
      
      data_final_commod <- total_data_goods_out_commod  %>%
        bind_rows(total_data_services_out_commod)
      
      join_by_group <- c("reporter_iso", "reporter_desc", "partner_iso", "flow_code", "flow_desc", "trade_type")
      
      if (select_input_commod == "Quarterly") {
        
        join_by_group <- c("period", "ref_quarter", join_by_group)
        
      } else {
        
        join_by_group <- c("period", join_by_group)
        
      }
      
      data_final_total_commod <- data_final_commod %>%
        inner_join(data_final_total, by = join_by(!!!syms(join_by_group))) %>% 
        mutate(share_percent = (total_trade_value/total_trade_freq)) %>% 
        ungroup() %>% 
        group_by(reporter_iso, reporter_desc, partner_iso, flow_code, flow_desc, trade_type, cmd_desc, cmd_code) %>% 
        mutate(growth_rate = ((total_trade_value - lag(total_trade_value))/lag(total_trade_value)))
      
      return(data_final_total_commod)
    }
    
    
    ## growth rate & share % of total line chart
    # growth rate data & # gather chart data 
    growth_commod_data <- reactive({
      req(input$select_commod_val_growth, input$select_exp_imp_commod, rv_commod_analysis$bind_data, rv_commod_analysis$commod_table_filter$id)
      calc_growth_share_commod(select_input_commod = input$select_commod_val_growth,
                               what = input$select_exp_imp_commod,
                               trade_data_commod = rv_commod_analysis$bind_data,
                               row_filter_vector = rv_commod_analysis$commod_table_filter$i)
    
    })
    
    share_commod_data <- reactive({
      req(input$select_commod_val_share, input$select_exp_imp_commod, rv_commod_analysis$bind_data, rv_commod_analysis$commod_table_filter$id)
      calc_growth_share_commod(select_input_commod = input$select_commod_val_share,
                               what = input$select_exp_imp_commod,
                               trade_data_commod = rv_commod_analysis$bind_data,
                               row_filter_vector = rv_commod_analysis$commod_table_filter$i)
    })
    
    # share of % against the total chart
    # growth rate chart
    output$commod_line_val_growth <- renderHighchart({
      req(growth_commod_data(),input$select_commod_val_growth)
      
      hchart(growth_commod_data(), type = "line", 
             hcaes(x = period, y = growth_rate, group = cmd_desc),
             connectNulls = TRUE
      ) %>% 
        hc_xAxis(
          title = list(text = "Period", style = list(fontSize = "14px")),
          labels = list(style = list(fontSize = "12px"))
        ) %>% 
        hc_yAxis(
          title = list(text = "% Percentage", 
                       style = list(fontSize = "14px")),
          labels = list(format = "{value}", 
                        style = list(fontSize = "12px"))
        ) %>% 
        hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          valueDecimals = 2,
          valueSuffix = "%",
          headerFormat = '<b>{point.key}</b><br>',
          style = list(fontSize = "12px")
        ) %>% 
        hc_legend(
          layout = "vertical",
          align = "left",
          verticalAlign = "top",
          floating = TRUE,
          x = 80, 
          y = 0,
          itemStyle = list(fontSize = "12px")
        ) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: UN Comtrade",
          style = list(fontSize = "11px"))
    })
    
    # growth rate chart
    output$commod_line_val_share <- renderHighchart({
      req(share_commod_data(),input$select_commod_val_share)
      
      hchart(share_commod_data(), type = "line", 
             hcaes(x = period, y = share_percent, group = cmd_desc),
             connectNulls = TRUE
      ) %>% 
        hc_xAxis(
          title = list(text = "Period", style = list(fontSize = "14px")),
          labels = list(style = list(fontSize = "12px"))
        ) %>% 
        hc_yAxis(
          title = list(text = "% Percentage", 
                       style = list(fontSize = "14px")),
          labels = list(format = "{value}", 
                        style = list(fontSize = "12px"))
        ) %>% 
        hc_tooltip(
          shared = TRUE,
          crosshairs = TRUE,
          valueDecimals = 2,
          valueSuffix = "%",
          headerFormat = '<b>{point.key}</b><br>',
          style = list(fontSize = "12px")
        ) %>% 
        hc_legend(
          layout = "vertical",
          align = "left",
          verticalAlign = "top",
          floating = TRUE,
          x = 80, 
          y = 0,
          itemStyle = list(fontSize = "12px")
        ) %>%
        hc_credits(
          enabled = TRUE,
          text = "Source: UN Comtrade",
          style = list(fontSize = "11px"))
    })
    
    # title for share rate
    output$title_commod_val_share <- renderText({
      req(rv_commod_analysis$exp_imp_commod)
      paste0("Percentage share of total ", rv_commod_analysis$exp_imp_commod)
    })
    
    # title for growth
    output$title_commod_val_growth <- renderText({
      req(rv_commod_analysis$exp_imp_commod)
      paste0(rv_commod_analysis$exp_imp_commod, " growth rate")
    })
    
    # insert UI
    insertUI(
      selector = "#commod_value_growth_share",
      ui = 
        div(id = "commod_value_growth_share_add",
            fluidRow(
              column(
                width = 6,
                div(
                  style = "background-color: #FFFFFF; padding: 20px; border: 1px solid #ccc; border-radius: 10px;",
                  div(
                    style = "text-align: center;
                         font-size: 22px;
                         font-weight: 700;
                         color: #2c3e50;
                         padding: 5px;
                         margin-bottom: 5px;
                         margin-top: -10px;",
                    textOutput("title_commod_val_growth")
                  ),
                  div(
                    style = "padding: 3px; margin-left: 18px; margin-bottom: 3px;",
                    pickerInput(
                      inputId = "select_commod_val_growth",
                      label = NULL,
                      width = "150px",
                      choices = c("Monthly", "Quarterly", "Annual"),
                      selected = "Monthly",
                      options = list(`style` = "btn-default btn-sm")
                    )
                  ),
                  highchartOutput("commod_line_val_growth")
                )
              ),
              
              column(
                width = 6,
                div(
                  style = "background-color: #FFFFFF; padding: 20px; border: 1px solid #ccc; border-radius: 10px;",
                  div(
                    style = "text-align: center;
                         font-size: 22px;
                         font-weight: 700;
                         color: #2c3e50;
                         padding: 5px;
                         margin-bottom: 5px;
                         margin-top: -10px;",
                    textOutput("title_commod_val_share")
                  ),
                  div(
                    style = "padding: 3px; margin-left: 18px; margin-bottom: 3px;",
                    pickerInput(
                      inputId = "select_commod_val_share",
                      label = NULL,
                      width = "150px",
                      choices = c("Monthly", "Quarterly", "Annual"),
                      selected = "Monthly",
                      options = list(`style` = "btn-default btn-sm")
                    )
                  ),
                  highchartOutput("commod_line_val_share")
                )
              )
            )
        )
    )
    
    ## summary statistics table and chart data table
    # compiling necessary dataset for stats summary
    table_commod_data <- reactive({
      req(input$select_commod_table_summary, input$select_exp_imp_commod, rv_commod_analysis$bind_data, rv_commod_analysis$commod_table_filter$id)
      calc_growth_share_commod(select_input_commod = input$select_commod_table_summary,
                               what = input$select_exp_imp_commod,
                               trade_data_commod = rv_commod_analysis$bind_data,
                               row_filter_vector = rv_commod_analysis$commod_table_filter$i)
    })
    
    # data for as of values
      as_of_stats_commod <- reactive({
        table_commod_data() %>% 
          arrange(period, cmd_code) %>% 
          ungroup() %>% 
          group_by(flow_desc, trade_type, cmd_code, cmd_desc) %>% 
          mutate(n = row_number()) %>% 
          filter(n == max(n)) %>% 
          ungroup() %>% 
          select(-c(reporter_iso, partner_iso, flow_code, n))
        
      })
      
      # summary statistics
      summary_stats_commod <-  reactive({
        table_commod_data() %>%
          group_by(flow_desc, trade_type, cmd_code, cmd_desc) %>%
          summarise(mean = mean(total_trade_value, na.rm = TRUE),
                    median = median(total_trade_value, na.rm = TRUE),
                    sd = sd(total_trade_value, na.rm = TRUE)) %>%
          ungroup() %>%
          select(-c("flow_desc", "trade_type"))
      })

      # joined both dataset
      combined_stats_commod <- reactive({
        as_of_stats_commod() %>% 
          select(-c("period", "reporter_desc", "flow_desc", "trade_type", "total_trade_freq")) %>%
          inner_join(summary_stats_commod(), by = join_by(cmd_code, cmd_desc))
      })
      
    observe({print(as_of_stats_commod())})
    observe({print(summary_stats_commod())})
    observe({print(combined_stats_commod())})
    # datatable for summary
    output$commod_table_summary <- renderDataTable({
      req(combined_stats_commod(), input$select_exp_imp_commod)
      data <- combined_stats_commod() %>% 
        mutate(across(c(total_trade_value, share_percent, growth_rate, mean, median, sd), as.numeric))
 
      datatable(data,
                rownames = FALSE,
                extensions = c("Buttons", "Responsive"),
                options = list(dom = "Btp",
                               buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                               pageLength = 5,
                               lengthMenu = list(c(5, 10, -1), c("5", "10", "All"))
                               ),
                colnames = c("Classification Code", "Classification", "Value ($M)", 
                             paste0("share of ",input$select_exp_imp_commod, " (%)"),
                             "Growth Rate (%)", "Average ($M)", "Median ($M)", "SD ($M)")
                ) %>% 
        formatStyle(c("growth_rate"),
                    color = JS("value < 0 ? 'darkred' : value > 0 ? 'darkgreen' : 'black'")) %>% 
        formatPercentage(c("share_percent", "growth_rate"),digits = 2) %>% 
        formatCurrency(c("total_trade_value", "mean", "median", "sd"), currency = "$")
      
      
      
      
    })
    
    # title for table
    output$title_commod_table_summary <- renderText({
      req(rv_commod_analysis$exp_imp_commod)
      paste0(rv_commod_analysis$exp_imp_commod, " summary statistics for selected commodities")
    })
    
    # Insert UI
    insertUI(
      selector = "#table_summary_commod",
      ui = div(id = "table_summary_commod_add",
               fluidRow(
                 column(
                   width = 12,
                   div(
                     style = "background-color: #FFFFFF; padding: 20px; border: 1px solid #ccc; border-radius: 10px;",
                     div( style = "text-align: center;
                         font-size: 22px;
                         font-weight: 700;
                         color: #2c3e50;
                         padding: 5px;
                         margin-bottom: 5px;
                         margin-top: -10px;",
                         textOutput("title_commod_table_summary")
                     ),
                     div(style = "padding: 3px; margin-left: 18px; margin-bottom: 3px;",
                         pickerInput(
                           inputId = "select_commod_table_summary",
                           label = NULL,
                           width = "150px",
                           choices = c("Monthly", "Quarterly", "Annual"),
                           selected = "Monthly",
                           options = list(`style` = "btn-default btn-sm")
                         )
                      ),
                     dataTableOutput(outputId = "commod_table_summary")
                   )
                 )
               )
             )
    )
    
    ## update choices 
    observe({
      req(rv_commod_analysis$check_services_code)
      
      check_commod_codes <- rv_commod_analysis$check_services_code
      
      if(length(check_commod_codes) > 1) { 
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_aggr",
                          choices = c("Quarterly", "Annual"))
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_growth",
                          choices = c("Quarterly", "Annual"))
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_share",
                          choices = c("Quarterly", "Annual"))
        
        
      } else if (check_commod_codes %in% c("goods")){
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_aggr",
                          choices = c("Monthly","Quarterly", "Annual"))
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_growth",
                          choices = c("Monthly","Quarterly", "Annual"))
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_share",
                          choices = c("Monthly","Quarterly", "Annual"))
        
      } else {
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_aggr",
                          choices = c("Quarterly", "Annual"))
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_growth",
                          choices = c("Quarterly", "Annual"))
        
        updatePickerInput(session = session, 
                          inputId = "select_commod_val_share",
                          choices = c("Quarterly", "Annual"))
        
      }
    })
    
    
    
    ## Implement the selectised input for further analysis
    
    # inser UI
    
    # trade data both services and goods
    # service choices
    serv_commod_choices <- reactive({
      choices <- unique(trade_data_rv$trade_service_data$cmd_desc)
      filtered_choices <- choices[!choices %in% c("Other services", "Memo item: Commercial services")]
      return(filtered_choices)
    })
    
    

    
    ###----------------------------------------------------------###
    ### ---------------- TRADE-FORECASTING page ------------_----###
    ###----------------------------------------------------------###
    
    
}

shinyApp(ui, server)
