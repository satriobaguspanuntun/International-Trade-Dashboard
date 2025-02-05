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

# function to loop over countries grouping
pull_all_countries_data <- function(country_batch, hs2 , start_date, end_date) {
  
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
  
  # check goods & service trade data for irregularities
  # check year range for each queried countries
  
  # year_queried_range <- seq.Date(from = as.Date(paste("2015", "01", "01", sep = "-")),
  #                                to = as.Date(paste("2023", "01", "01", sep = "-")),
  #                                by = "1 year")
  # 
  # year_queried_range <- substr(as.character(year_queried_range), 1, 4)
  # 
  # for (i in seq_along(trade_data)) {
  #   
  #   data_to_check <- trade_data[[i]]
  #   
  #   missing_data <- data_to_check |> 
  #     group_by(ref_year, reporter_iso) |> 
  #     mutate(mising_year = if_else(!ref_year %in% year_queried_range, 1, 0)) |> 
  #     filter(missing_year == 1)
  #   
  #   if (){}
  #   
  # }
  
  # if missing year was found, retry to fetch the data for that missing year


  # check macro data
  # check year range 
  
  

  
  # try bind them in a list
  return(list(trade = trade_data, macro = macro_data))
}



testing <- pull_all_countries_data(country_batch = country_batches[[4]], hs2 = hs2, start_date = "2015", end_date = "2023")


# test <- pull_trade(c("NZL", "CHN", "ARG", "IDN", "KOR"), 
#                    "all_countries", 
#                    direction = c("import", "re-import", "export", "re-export"),
#                    start = "2019", 
#                    end = "2024",
#                    freq = "A",
#                    commod_code = hs2)
# 
# countries <- c("NZ", "CH", "AR", "ID", "NZ")
# 
# test_data <- wdi_dat_function(countries = countries, start = 2000, end = 2024)
