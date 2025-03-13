## MASTER DATA PULL SCRIPT ##
## OBJECTIVE ##
# develop a function that can loop over the country batches and set a threshold
# where if the function is called more than 3 times then it will ceased pulling data
library(dplyr)
library(lubridate)
library(comtradr)
library(tidyr)
library(httr2)
library(fredr)
library(WDI)
library(jsonlite)
library(RSQLite)

source("~/international-Trade-Dashboard/data_pull_scripts/pull_trade_data.R")
source("~/international-Trade-Dashboard/data_pull_scripts/pull_macro_data.R")
source("~/international-Trade-Dashboard/data_pull_scripts/pull_all_data.R")
options(scipen = 999)

#----------------------------- SET PARAMETERS --------------------------------#
## set start and end date

# start date
start_pull_date <- "2015"

# end date
end_pull_date <- "2023"

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
# than 3 times to avoid reaching the maximum amount of calls that API can handle.

pull_master_data <- function(country_batches_to_run, start_date, end_date, hs_type) {
  
  current_date <- Sys.time()
  length_list <- length(country_batches)
  
  # check start and end year format
  if (year_check(start_date) & year_check(end_date)) {
    
    # while loop
    i <- 1 
    
    while (i <= length_list) {
      
      # print country batches
      cli::cli_h1(paste0("Processing country batches: ", i, " out of ", length_list))
      
      data_for_sql <- loop_across_countries(batches = country_batches_to_run[i], 
                                            start = start_date,
                                            end = end_date,
                                            hs = hs_type)
      
      # store to sqlite
      sqlite_push(data_for_sql)
      
      # if the iterator reach more than 3, stop the loop
      if (i >= 3) {
        
        stop("You have the reach the maximum data pull, please try again tomorrow.")
        
        # create a log file 
        log_data <- data.frame(time = current_date,
                               date = substr(current_date, 1, 10),
                               list_step = i,
                               username = Sys.getenv("USERNAME"))
        
        # save it in data folder
        saveRDS(log_data, "~/international-Trade-Dashboard/data/log_data.rds")
        
        cli::cli_h1("Log data has been updated or saved at ~/international-Trade-Dashboard/data folder path")
      }
      
      # print triple dot on the console
      
      
      i <- i + 1
      
    }
    
    
  }
  
}


test <- loop_across_countries(country_batches[3], start = "2014", end = "2023", hs = hs2)

sqlite_push(data_list = test)

dbDisconnect(conn)


pull_master_data(country_batches_to_run = country_batches, start_date = "2020", end_date = "2023", hs_type = hs2)

