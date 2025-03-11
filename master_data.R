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

### SET PARAMETERS ###
## set start and end date

# start date
start_pull_date <- "2015"

# end date
end_pull_date <- "2023"

# Connection specs
conn <- dbConnect(SQLite(), "master_db.db")

# puilling relevant concordances
list_concord <- concordance()
hs_concord <- list_concord$hs
serv_concord <- list_concord$serv
rm(list_concord)
hs2 <- hs_concord |> filter(aggrLevel == 2) |> select(id)
hs2 <- hs2$id

# country batches
country_batches <- country_split()

# function to iterate over the country batches and stop if data fetch function is called more
# than 3 times to avoid reaching the maximum amount of calls that API can handle.

pull_master_data <- function(batches, start_date, end_date) {
  
  # check start and end year format
  
  
  # loop
  
  # store to sqlite
  
  # done
  
}


test <- loop_across_countries(country_batches[5], start = "2014", end = "2023", hs = hs2)

sqlite_push(data_list = test)

dbDisconnect(conn)




