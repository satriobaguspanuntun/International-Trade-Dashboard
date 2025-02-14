# LOGIC SCRIPT FOR PULLING TRADE AND MACRO DATA FOR EACH SELECTED COUNTRIES.
# Use windows scheduler to run this script.
## the main idea behind this script is to reduce the amount of api call
## each day for the countries that we are interested in. Slicing the countries
## into batches (like 5 to 10 countries) and pull their data each day recursively
library(dplyr)
library(lubridate)
library(comtradr)
library(tidyr)
library(httr2)
library(fredr)
library(WDI)
library(jsonlite)
library(RSQLite)

source("~/international-Trade-Dashboard/pull_trade_data.R")
source("~/international-Trade-Dashboard/pull_macro_data.R")

options(scipen = 999)

list_concord <- concordance()

hs_concord <- list_concord$hs
serv_concord <- list_concord$serv

rm(list_concord)

hs2 <- hs_concord |> filter(aggrLevel == 2) |> select(id)
hs2 <- hs2$id

# function to split the countries into small batches, each batch consist of 5-10 countries
country_split <- function(n = 5) {
  
  country_code <- readRDS("~/international-Trade-Dashboard/data/countrycode.rds")
  
  colnames(country_code) <- tolower(colnames(country_code))
  
  list_batches  <- country_code |> group_by(row_number() %/% n) |> group_map(~ .x)
  
  return(list_batches)
}

country_batches <- country_split()

# function pull all data for each country
pull_all_countries_data <- function(country_batch, hs2, start_date, end_date) {
  
  # check year
  if (!year_check(start_date) & !year_check(end_date)) {
    stop("The start and end date has to be in 'YYYY' format.")
  }
  
  # check hs2 (only hs2 can pass the check)
  if (!any(grepl("\\d{2}", hs2))) {
    stop("The Harmonised System Classification can only be in HS2 level.")
  }
  
  ## parameters
  # trade date
  trade_start <- as.character(start_date)
  trade_end <- as.character(end_date)
  
  # WDI date
  wdi_start <- as.numeric(start_date)
  wdi_end <- as.numeric(end_date)
  
  # country code
  trade_iso3 <- unlist(country_batch[, 1])
  wdi_iso2 <- unlist(country_batch[, 3])
  
  # pull trade data
  trade_data <- pull_trade(trade_iso3,
                           partner = "all_countries",
                           direction = c("import", "re-import", "export", "re-export"),
                           start = start_date,
                           end = end_date,
                           freq = "A",
                           commod_code = hs2)
  
  # pull macro data
  macro_data <- wdi_dat_function(countries = wdi_iso2,
                                 start = wdi_start,
                                 end = wdi_end)

  # try bind them in a list
  return(list(trade = trade_data, macro = macro_data))
}

# function to loop over country batches
loop_across_countries <- function(batches, start, end, hs) {
  
  # dataframe to store
  # goods trade data
  final_trade_data <- data.frame()
  
  # service trade data
  final_service_data <- data.frame()
  
  # macro data
  final_macro_data <- data.frame()
  
  # information data frame
  info_data <- data.frame()
  
  for (i in seq_along(batches)) {
    
    batch <- batches[[i]]
    
    data <- pull_all_countries_data(country_batch = batch, start_date = start, end_date = end, hs2 = hs)
    
    # store goods data
    trade_data <- data[[1]][[1]]
    if (nrow(final_trade_data) == 0 & ncol(final_trade_data) == 0) {
      
      final_trade_data <- trade_data
      
    } else {
      
      final_trade_data <- rbind(final_trade_data, trade_data)
      
    }
    
    # store service data
    service_data <- data[[1]][[2]]
    if (nrow(final_service_data) == 0 & ncol(final_service_data) == 0) {
      
      final_service_data <- service_data
      
    } else {
      
      final_service_data <- rbind(final_service_data, service_data)
      
    }
    
    # store macro data
    macro_data <- data[[2]]
    if (nrow(final_macro_data) == 0 & ncol(final_macro_data) == 0) {
      
      final_macro_data <- macro_data
      
    } else {
      
      final_macro_data <- rbind(final_macro_data, macro_data)
      
    }
    
  }
  
  # check for missing data
  
  # retry to pull the missing data otherwise remove the rows
  
  # inject data to the final dataframe
  
  
  return(list(goods = final_trade_data, services = final_service_data, macro = final_macro_data))
}

check_missing <- function(x) {
  
  col_names <- colnames(x)
  
  if (!col_names %in% c("classification_code")) {
    
    missing_data <- tryCatch({
      data <-  x %>% 
        mutate(na = rowSums(is.na(.))) %>% 
        filter(na >= ncol(.) - 3) %>% 
        select_if(function(x) any(!is.na(x))) %>% 
        select(-na) 
      
    }, error = function(e){
      data <- data.frame(period = NA,
                         reporter_iso = NA)
      
    })
    
    if (all(is.na(missing_data))) {
      
      missing_data <- missing_data %>% mutate(retry = FALSE)
      
    } else {
      
      missing_data <- missing_data %>% mutate(retry = TRUE)
      
    }
  
  } else {
    
    # delete labels from the columns
    
    # detct NA's and flag them
    
    # mutate indicator column 
    
  }
  
  
  
  
}



# function for storing final dataframe into sqlite format


test <- loop_across_countries(country_batches[1:2], start = "2020", "2023", hs = hs2)
