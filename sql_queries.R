### SQL Queries ----------------------
library(DBI)
library(tidyverse)
library(RSQLite)

## Connection specs
conn <- dbConnect(SQLite(), "~/international-Trade-Dashboard/master_db.db")

# 1. pull trade data -------------------
sql_export_query <- function(conn, country, start, end, trade_flow, type) {
  # incorporate error handling exception (IMPORTANT)
  
  start_numeric <- as.numeric(start)
  end_numeric <- as.numeric(end)
  
  date_sql_trade_range <- paste0(as.character(seq.int(start_numeric, end_numeric, by = 1)), collapse = ",")
  trade_flow <- paste0(trade_flow, collapse = ",")
  
  sql_query <- sprintf("SELECT * FROM '%s' 
                       WHERE period IN (%s) 
                       AND flow_code = '%s' 
                       AND reporter_desc = '%s'",
                       type,
                       date_sql_trade_range,
                       trade_flow,
                       country)
  
  data_trade <- dbGetQuery(conn, sql_query)
  return(data_trade)
}

# 3. pull macro data --------------------
sql_macro_query <-  function(conn, start, end) {
  
  start_numeric <- as.numeric(start)
  end_numeric <- as.numeric(end)
  
  date_sql_macro_range <- paste0(as.character(seq.int(start_numeric, end_numeric, by = 1)), collapse = ",")
  
  sql_query <- sprintf("SELECT country,
                           iso3c,
                           year,
                           gdp_nominal,
                           gdp_per_capita_current,
                           gdp_per_capita_growth,
                           export_gdp_percent,
                           inflation,
                           unemployment,
                           population,
                           net_migration,
                           current_account,
                           fdi,
                           gross_capital_formation 
                           
                        FROM macro
                        WHERE year IN (%s)",
                        date_sql_macro_range)
  
  data_sql_macro <- dbGetQuery(conn, sql_query)
  return(data_sql_macro)
}

# 4. joined export, import, and macro data with country concordances ------------------

# 5. create tables to store modelling and forecasting values -----------------------

# 6. storing values back to sqlite database -------------------------

# 7. max year and min year in database -------------------------
sql_year_range <- function(conn) {
  
  # goods & service year range
  sql_trade_year_range <- "SELECT DISTINCT goods.period AS goods_years, 
                                           services.period AS services_years
                           FROM goods 
                           INNER JOIN services 
                           ON goods.reporter_iso = services.reporter_iso 
                           AND goods.flow_code = services.flow_code
                           AND goods.period = services.period"
  
  sql <- "SELECT DISTINCT period AS goods_years FROM goods"
  
  trade_year_range <- dbGetQuery(conn, sql)
  
  # macro year range
  sql_macro_year_range <- "SELECT DISTINCT macro.year FROM macro"
  
  macro_year_range <- dbGetQuery(conn, sql_macro_year_range)
  
  return(list(macro_year = macro_year_range, trade_year = trade_year_range))
}
