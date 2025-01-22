library(dplyr)
library(httr2)
library(fredr)
library(jsonlite)

### List of important economic indicators
# GDP Nominal & real
# Inflation (CPI)
# Unemployment rate
# Current account balance
# Export & Import (covered by comtrade)
# Trade balance (covered by comtrade)
# īnterest rate
# 10 year govt bonds
# share prices

# List countries
# G20
# OECD
# these two for now. Will add more if necessary

## function to search up available series for each selected countries
# search function look up
api_search_request <- function(..., search_text, search_tag, limit = 100){
  params <- list(
    ...,
    search_text = search_text,
    tag_names = search_tag,
    limit = limit,
    file_type = "json"
  )
  
  req_json <- request("https://api.stlouisfed.org/fred/series") |> 
    req_url_path_append("search") |> 
    req_url_query(!!!params,`api_key` = Sys.getenv("FRED_API_KEY")) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  
  req_body <- tibble(req_json$seriess)
  
  return(req_body)
}

api_pull_data_request <- function(..., series, start, end, sort = "desc") {
  if (is.character(series)) {
    stop("Ensure the series name is in character")
  }
  
  params <- list(
    ...,
    series_id = series,
    observation_start = start,
    observation_end = end,
    sort_order = sort,
    file_type = "json"
  )
  
  req_json <- request("https://api.stlouisfed.org/fred/series") |> 
    req_url_path_append("observations") |> 
    req_url_query(!!!params, `api_key` = Sys.getenv("FRED_API_KEY")) |> 
    req_perform() |> 
    resp_body_json(simplifyVector = TRUE)
  
  req_body <- tibble(req_json)
  
}

test <- api_pull_data_request(series = "")


filter_ngdp <- function(x, freq) {
  
  if (!freq %in% c("Annual", "Quarterly")) {
    stop("Please supply the correct frequency")
  }
  
  if (is.data.frame(x)) {
    stop("object is not a dataframe")
  }
  
  data <- x |> 
    filter(frequency == freq) |> 
    select(id, frequency) |> 
    mutate(id_reduce = substr(id, 1, 4)) |> 
    filter(id_reduce == "NGDP")
  
  return(data)

}

# checking available series
check_available_series <- function(country) {
  if(!is.character(country)) {country <- as.character(country)}
  
  if (tolower(country) != "usa") {
    
    exchange_rate <- TRUE
    
  } else {
    
    exchange_rate <- FALSE
    
  }
  
  ## series to check
  # GDP nominal
  nominal_gdp <- api_search_request(search_text = "nominal gdp", search_tag = country, limit = 1000)

  nominal_gdp_series <- filter_ngdp(gdp_nominal, freq = "Quarterly")
  
  if (unique(nominal_gdp_series$frequency) != "Quarterly") {
    
    nominal_gdp_series <- filter_ngdp(gdp_nominal, freq = "Annual")
    
  } else if (nrow(nominal_gdp_series) == 0) {
    
    nominal_gdp_series <- data.frame(id = NA,
                                     frequency = NA,
                                     id_reduce = NA)
  }
  
  # GDP real
  real_gdp <- api_search_request(search_text = "real gdp", searc)
  
  # Inflation (CPI)
  
  # Unemployment rate
  
  # Current account balance
  
  # Interest rate
  
  # 10 year govt bonds
  
  # share prices
  
  # exchange rate

  
  
  
}

test <- api_search_request(search_text = "gross domestic product", search_tag = "russia", limit = 1000)


## pull data function

## convert data into digestible format (tibble)
fredr_series_search_text(search_text = "gdp",tag_names = "usa")
