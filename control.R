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

conn <- dbConnect(SQLite(), "master_db.db")

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
  # add id column
  # trade id
  final_trade_data <- final_trade_data %>% 
    mutate(id = paste0(ref_period_id, reporter_iso, flow_code, cmd_code, partner_iso),
           id = if_else(!grepl("^[0-9]", id), NA, id)) %>% 
    relocate(id)
  
  # service id
  final_service_data <- final_service_data %>% 
    mutate(id = paste0(ref_period_id, reporter_iso, flow_code, cmd_code, partner_iso),
           id = if_else(!grepl("^[0-9]", id), NA, id)) %>% 
    relocate(id)
  
  # macro id
  final_macro_data <- final_macro_data %>% 
    mutate(id = paste0(iso3c, year)) %>% 
    relocate(id)
  
  # check for missing data
  
  # retry to pull the missing data otherwise remove the rows
  
  # inject data to the final dataframe
  
  
  return(list(goods = final_trade_data, services = final_service_data, macro = final_macro_data))
}

# create sqlite database and update or save data on the first instance
sqlite_push <- function(data_list, overwrite = FALSE){
  
  # dataset names
  dataset_names <- names(data_list)
  
  if (file.exists("~/international-Trade-Dashboard/master_db.db")) {
    
    for (i in dataset_names) {
      
      data <- data_list[[paste0(i)]]
      
      if (i %in% c("goods", "services")) {
        
        # construct sql queries
        sql_query <- sprintf("SELECT DISTINCT ref_year, reporter_iso FROM %s", i)
        sql_data_for_join <- dbGetQuery(conn, sql_query)
        
        # new data summarised
        new_data_for_join <- data %>% group_by(ref_year, reporter_iso) %>%  summarise(n = n()) %>% select(-n)
        
        # anti join both dataframe (this will collect the remainder data rows that are not in the sql database)
        new_data_filter <- anti_join(new_data_for_join, sql_data_for_join, by = join_by(ref_year, reporter_iso))
        
        if (nrow(new_data_filter) > 0) {
          
          # filter new data
          new_data <- data %>% filter(ref_year %in% new_data_filter$ref_year, reporter_iso %in% new_data_filter$reporter_iso)
          
          # append new data into sqlite table
          dbAppendTable(conn, name = i, value = new_data)
          
          
        } else {
          
          cli::cli_h3(paste0("The new data dimesion and content are exactly matched with the one in the base"))
          
          overwrite_table <- rstudioapi::showQuestion(title = "Overwriting option", 
                                                      message = "Would you like to overwrite the table?",
                                                      ok = "Yes", cancel = "No")
          if (overwrite_table) {
            
            dbWriteTable(conn, name = i, value = data)
            
          }
          
        }
        
      } else { 
        
        
        
        
        
      }
      
      
      
    }
    
    # check whether the new data suppose to be overwrite or append 
    ## check dimension
    ## does the n rows and columns matched?
    ## anti join between tables 
    ## if anti join return 0 rows and any of n rows and columns return 0 as well
    ### skip or next
    ### else append the table
    
    
    
    # save 
    
  } else {
    
    # create new blank database (master_db)
    ## Table for Trade goods
    dbExecute(conn, "CREATE TABLE goods (
    
    id varchar(100),
    
    freq_code char(1),
    
    ref_period_id INT,
    
    ref_year INT,
    
    ref_month INT,
    
    period TEXT,
    
    reporter_iso TEXT,
    
    reporter_desc TEXT,
    
    flow_code TEXT,
    
    flow_desc TEXT,
    
    partner_iso TEXT,
    
    partner2desc TEXT,
    
    classification_code char(2),
    
    cmd_code varchar(10),
    
    cmd_desc varchar(100),
    
    aggr_level int,
    
    customs_code varchar(15),
    
    customs_desc varchar(100),
    
    cifvalue bigint,
    
    fobvalue bigint,
    
    primary_value bigint,
    
    PRIMARY KEY (id)
)")
    
    ## Table for service data
    dbExecute(conn, "CREATE TABLE service (
    
    id varchar(100),
    
    freq_code char(1),
    
    ref_period_id INT,
    
    ref_year INT,
    
    ref_month INT,
    
    period TEXT,
    
    reporter_iso TEXT,
    
    reporter_desc TEXT,
    
    flow_code TEXT,
    
    flow_desc TEXT,
    
    partner_iso TEXT,
    
    partner2desc TEXT,
    
    classification_code char(2),
    
    cmd_code varchar(10),
    
    cmd_desc varchar(100),
    
    aggr_level int,
    
    customs_code varchar(15),
    
    customs_desc varchar(100),
    
    cifvalue bigint,
    
    fobvalue bigint,
    
    primary_value bigint,
    
    PRIMARY KEY (id)
)")
    ## Table for Macroeconomic data
    dbExecute(conn, "CREATE TABLE macro (
              
              id varchar (100),
              
              country TEXT,
              
              iso2c TEXT,
              
              iso3c TEXT,
              
              year TEXT,
              
              gdp_nominal bigint,
              
              gdp_nominal_growth int,
              
              inflation int,
              
              unemployment int,
              
              current_account int,
              
              fdi int,
              
              PRIMARY KEY (id)
              
    )")
    
    # write table into the database
    # goods trade
    dbWriteTable(conn, name = "goods", trade_new, append = TRUE)
    
    # service trade
    dbWriteTable(conn, name = "services", test[[2]], append = TRUE)
    
    # macro trade
    dbWriteTable(conn, name = "macro", test[[3]], append = TRUE)
    
    
  }
  
  
  
  
  
}






# check_missing <- function(x) {
#   
#   col_names <- colnames(x)
#   
#   if (!col_names %in% c("classification_code")) {
#     
#     missing_data <- tryCatch({
#       data <-  x %>% 
#         mutate(na = rowSums(is.na(.))) %>% 
#         filter(na >= ncol(.) - 3) %>% 
#         select_if(function(x) any(!is.na(x))) %>% 
#         select(-na) 
#       
#     }, error = function(e){
#       data <- data.frame(period = NA,
#                          reporter_iso = NA)
#       
#     })
#     
#     if (all(is.na(missing_data))) {
#       
#       missing_data <- missing_data %>% mutate(retry = FALSE)
#       
#     } else {
#       
#       missing_data <- missing_data %>% mutate(retry = TRUE)
#       
#     }
#   
#   } else {
#     
#     # delete labels from the columns
#     col <- names(x)[5:ncol(x)]
#     
#     for (i in col) {
#       
#       indicator <- as.character(attr(x[, as.character(i)], "label"))
#       
#       if (indicator != "NULL") {
#         
#         attr(x[, as.character(i)], "label") <- NULL
#         
#       } else {
#         
#         next
#         
#       }
#     }
#     
#     # detect NA's and flag them
#     
#     # mutate indicator column 
#     
#   }
#   
#   
#   
#   
# }

# function for storing final dataframe into sqlite format

test <- loop_across_countries(country_batches[1], start = "2020", "2023", hs = hs2)
