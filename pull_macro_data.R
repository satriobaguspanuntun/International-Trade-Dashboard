library(dplyr)
library(httr2)
library(fredr)
library(WDI)
library(jsonlite)

## CHECK WDI PACKAGE
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

## pull data function
api_pull_data_request <- function(..., series, start, end, sort = "desc") {
  if (!is.character(series)) {
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
  
  return(tibble(req_json$observations))
  
}

test <- api_pull_data_request(series = "NGDPRNSAXDCUSQ", start = "2010-01-01", end = "2024-12-01", sort = "desc")


filter_var <- function(x, id_target = NULL, freq) {
  
  if (!freq %in% c("Annual", "Quarterly")) {
    stop("Please supply the correct frequency")
  }
  
  if (!is.character(id_target)) {
    
    id_target <- as.character(id_target)
    
  } else if (nchar(id_target) > 4) {
    
    stop("Please provide 4 digit series ID")
    
  } else if (is.null(id_target)) {
    
    id_target_bool <- FALSE 
    
  }
  
  if (!is.data.frame(x)) {
    
    stop("object is not a dataframe")
    
  }
  
  if (id_target_bool) {
    
    data <- x |> 
      filter(frequency == freq) |> 
      select(id, frequency) |> 
      mutate(id_reduce = substr(id, 1, 4)) |> 
      filter(id_reduce == id_target)
    
  } else {
    
    data <- x |> 
      filter(frequency == freq) |> 
      select(id, frequency) |> 
      mutate(id_reduce = substr(id, 1, 4))
    
  }
  
  return(data)
}

pull_available_series <- function(country) {
  if(!is.character(country)) {country <- as.character(country)}
  if(grepl("[A-Z]", country)) {country <- tolower(country)}
  if (tolower(country) != "usa") {
    
    exchange_rate_bool <- TRUE
    
  } else {
    
    exchange_rate_bool <- FALSE
    
  }
  
  # GDP
  gdp <- api_search_request(search_text = "gdp", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # Inflation (CPI)
  cpi <- api_search_request(search_text = "cpi", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # Unemployment rate
  unemp <- api_search_request(search_text = "unemployment", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # Current account balance
  current_acc <- api_search_request(search_text = "current account balance", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # Interest rate
  central_bank_rate <- api_search_request(search_text = "interest rate", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # 10 year govt bonds
  govt_bonds <- api_search_request(search_text = "government bonds", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # share prices
  share_prices <- api_search_request(search_text = "share prices", search_tag = country, limit = 1000)
  Sys.sleep(0.5)
  # exchange rate
  if (exchange_rate_bool) {
    
    exchange_rate <- api_search_request(search_text = "exchange rate", search_tag = country, limit = 1000)
    Sys.sleep(0.5)
  } else {
    
    exchange_rate <- data.frame(id = NA,
                                realtime_start = NA,
                                realtime_end = NA,
                                title = NA,
                                observation_start = NA,
                                observation_end = NA,
                                frequency = NA,
                                freuency_short = NA,
                                units = NA,
                                units_short = NA,
                                seasonal_adjustment = NA,
                                seasonal_adjustment_short = NA,
                                last_updated = NA,
                                popularity = NA,
                                group_popularity = NA,
                                notes = NA)
    Sys.sleep(0.5)
  }
  
  data_combine <- do.call(rbind, list(gdp = gdp, 
                                      cpi = cpi, 
                                      unemp = unemp, 
                                      current_acc = current_acc, 
                                      central_bank_rate = central_bank_rate, 
                                      govt_bonds = govt_bonds, 
                                      share_prices = share_prices, 
                                      exchange_rate = exchange_rate))
  
  rownames(data_combine) <- 1:nrow(data_combine)
  
  return(data_combine)
}


loop_available_series <- function(countries_list) {
  
  empty_list <- list()
  
  for (country in test_countries) {
    
    empty_list[[country]] <- pull_available_series(country = country)
    
  }
  
  return(empty_list)
}

################################################################################
#----------------------------- WDI package ------------------------------------#
################################################################################

## GDP Nominal & real
# GDP Nominal: NY.GDP.MKTP.CD
# GDP growth : NY.GDP.MKTP.KD.ZG
# GDP real : Nominal/deflator
# GDP per Capita: NY.GDP.PCAP.CD

## Inflation (CPI)
# Inflation consumer prices: FP.CPI.TOTL.ZG

## Unemployment rate
# Unemployment rate: SL.UEM.TOTL.ZS

## Current account balance
# Current account: BN.CAB.XOKA.CD
# Foreign direct Investment, net inflows : BX.KLT.DINV.CD.WD

# Export & Import (covered by comtrade)
# Trade balance (covered by comtrade)

## pull data function

wdi_dat_function <- function(countries, start, end) {
  
  container <- list()
  
  series_id <- c("NY.GDP.MKTP.CD", "NY.GDP.MKTP.KD.ZG",
                 "FP.CPI.TOTL.ZG", "SL.UEM.TOTL.ZS", "BN.CAB.XOKA.CD",
                 "BX.KLT.DINV.CD.WD")
  
  for (series in seq_along(series_id)) {
    
    print(paste0("Downloading Series type: ", series_id[series]))
    
    data <- WDI(country = countries, indicator = series_id[series], start = start, end = end)
    
    container[[series_id[series]]] <- data
    
    Sys.sleep(0.5)
  }
  
  # join all data in the container list
  final_data <- container |> purrr::reduce(inner_join, by = c("country", "iso2c", "iso3c", "year"))
  
  return(final_data)
}

countries <- c("MX", "AU")

test_data <- wdi_dat_function(countries = countries, start = 2000, end = 2023)


  
  