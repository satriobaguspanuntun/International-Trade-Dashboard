## MASTER DATA PULL SCRIPT ##
## OBJECTIVE ##
# develop a function that can loop over the country batches and set a threshold
# where if the function is called more than n times then it will ceased pulling data
library(dplyr)
library(lubridate)
library(comtradr)
library(tidyr)
library(httr2)
library(fredr)
library(WDI)
library(jsonlite)
library(RSQLite)

source("~/international-Trade-Dashboard/R/pull_trade_data.R")
source("~/international-Trade-Dashboard/R/pull_macro_data.R")
source("~/international-Trade-Dashboard/R/pull_all_data.R")
options(scipen = 999)

#----------------------------- SET PARAMETERS --------------------------------#
## set start and end date

# start date
start_pull_date <- "2013"

# end date
end_pull_date <- "2024"

# overwrite all dataset in the sql tables?
overwrite <- FALSE # False by default but you can adjust it as you see fit.

# number of iteration (at this stage, it's set at 2)
iteration <- 2

## Connection specs
conn <- dbConnect(SQLite(), "master_db.db")

## puilling relevant concordances
list_concord <- concordance()
hs_concord <- list_concord$hs
serv_concord <- list_concord$serv
rm(list_concord)
hs2 <- hs_concord |> filter(aggrLevel == 2) |> select(id)
hs2 <- hs2$id

## country batches
country_batches <- country_split()

#-----------------------  PULL TRADE AND MACRO DATA ---------------------------#

# function to iterate over the country batches and stop if data fetch function is called more
# than 2 times to avoid reaching the maximum amount of calls that API can handle.

pull_master_data <- function(country_batches_to_run, start_date, end_date, hs_type, overwrite_all_table = overwrite, iteration_value = iteration) {
  
  ## Set parameters
  current_date <- Sys.time()
  length_list <- length(country_batches_to_run)  # Fix: use the correct variable
  increment_check <- c()
  log_file_path <- "~/international-Trade-Dashboard/data/log_data.rds"

  # Check if log file exists
  if (file.exists(log_file_path)) {
    log_data_old <- readRDS(log_file_path)
    
    # check previous loop counter
    check_loop <- log_data_old[nrow(log_data_old), ]$list_step + 1
    
  } else {
    log_data_old <- data.frame(time = character(), date = character(), list_step = numeric(), username = character())
  }

  # Check start and end year format
  if (year_check(start_date) & year_check(end_date)) {
    
    # Initialize loop counter
    if (exists("check_loop") & overwrite_all_table == FALSE) {
      i <- check_loop
    } else if (exists("check_loop") & overwrite_all_table == TRUE) {
      i <- 1
    } else {
      i <- 1
    }
    
    while (i <= length_list) {
      
      cli::cli_h1(paste0("Processing country batch ", i, " out of ", length_list))
      
      data_for_sql <- loop_across_countries(
        batches = country_batches_to_run[i], 
        start = start_date,
        end = end_date,
        hs = hs_type
      )
      
      # Store data in SQLite
      sqlite_push(data_for_sql)
      
      # Stop if the iterator reaches the threshold
      if (length(increment_check) >= iteration_value) {
        
        # Create log entry
        log_update_data <- data.frame(
          time = current_date,
          date = substr(current_date, 1, 10),
          list_step = i,
          username = Sys.info()[["user"]],  # Fix: Use a more reliable user detection method
          overwrite_all = overwrite_all_table
        )
        
        # Update and save log
        log_data_new <- rbind(log_data_old, log_update_data)
        saveRDS(log_data_new, log_file_path)
        
        cli::cli_h1("Log data has been updated or saved at ~/international-Trade-Dashboard/data folder path")
        
        stop("You have reached the maximum data pull. Please try again tomorrow.")
      }
      
      i <- i + 1  # Increment iterator
      increment_check[i] <- i
    }
  } else {
    stop("The year value is not in YYYY format.")
  }
}

pull_master_data(country_batches_to_run = country_batches, start_date = start_pull_date, end_date = end_pull_date, hs_type = hs2)

dbDisconnect(conn)
