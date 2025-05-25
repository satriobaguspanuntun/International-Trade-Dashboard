### SQL Queries ----------------------
library(DBI)
library(tidyverse)
library(RSQLite)

## Connection specs
conn <- dbConnect(SQLite(), "~/international-Trade-Dashboard/master_db.db")

# 1. pull annual trade data -------------------
sql_export_query <- function(conn, country, start, end, trade_flow, type) {
  # incorporate error handling exception (IMPORTANT)
  
  # Basic input validation
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
  
  data_trade <- dbGetQuery(conn, sql_query) %>% mutate(fobvalue = as.numeric(fobvalue),
                                                       primary_value = as.numeric(primary_value),
                                                       cifvalue = as.numeric(cifvalue))
  return(data_trade)
}

# 2. pull monthly trade data -----------------
sql_monthly_export_query <- function(conn, country, start, end, trade_flow, hs_concordance) {
  
  check_year_month <- function(x) {
    grepl("^\\d{4}\\d{2}$", x)
  }
  
  if (!check_year_month(start) & !check_year_month(end)){
    stop("Please supply the dates with YYYYMM format.")
  }
  
  # Basic input validation
  start_numeric <- as.numeric(start)
  end_numeric <- as.numeric(end)
  
  sql_query <- sprintf("SELECT * FROM (SELECT 
                                            freq_code,
                                            ref_period_id,
                                            ref_year,
                                            ref_month,
                                            CAST(period AS INT) AS period,
                                            reporter_iso,
                                            reporter_desc,
                                            flow_code,
                                            flow_desc,
                                            partner_iso,
                                            partner2desc,
                                            cmd_code,
                                            cmd_desc,
                                            primary_value
                                       FROM monthly_goods 
                                       WHERE reporter_desc = '%s')
                        WHERE period >= %s
                        AND period <= %s
                        AND flow_code = '%s'",
                       country,
                       start_numeric,
                       end_numeric,
                       trade_flow)
  
  data_trade_monthly <- dbGetQuery(conn, sql_query) %>% mutate(primary_value = as.numeric(primary_value))
  
  return(data_trade_monthly)
}

# 3. pull macro data --------------------
sql_macro_query <-  function(conn, start, end) {
  
  # Basic input validation
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

# 4. pull services data ------------------
sql_service_query <- function(conn, country, start, end, trade_flow) {
  
  
  check_year_month <- function(x) {
    grepl("^\\d{4}\\d{2}$", x)
  }
  
  if (!check_year_month(start) & !check_year_month(end)){
    stop("Please supply the dates with YYYYMM format.")
  }
  
  # Basic input validation
  start_numeric <- as.numeric(start)
  end_numeric <- as.numeric(end)
  
  sql_query <- sprintf("SELECT * FROM (SELECT 
                                            freq_code,
                                            ref_period_id,
                                            ref_year,
                                            ref_month,
                                            CAST(
                                                ref_year || SUBSTR(ref_month, 2, 3)
                                                AS INT
                                                ) AS new_ref_year_month,
                                            period,
                                            reporter_iso,
                                            reporter_desc,
                                            flow_code,
                                            flow_desc,
                                            partner_iso,
                                            partner2desc,
                                            cmd_code,
                                            cmd_desc,
                                            primary_value
                                       FROM services 
                                       WHERE reporter_desc = '%s')
                        WHERE new_ref_year_month >= %s
                        AND new_ref_year_month <= %s
                        AND flow_code = '%s'
                        AND cmd_code NOT IN ('SOX', 'SPX1')",
                       country, start_numeric, end_numeric, trade_flow)
  
  data_sql_services <- dbGetQuery(conn, sql_query) %>% select(-c(new_ref_year_month))
  
  return(data_sql_services)
}

# 5. pull services data ------------------
sql_service_concordance <- function(conn) {
  
  # query
  sql_query <- "SELECT DISTINCT cmd_code, cmd_desc FROM services WHERE cmd_code NOT IN ('SOX', 'SPX1', 'S')"
  
  # fetch data
  serv_concord_vector <- dbGetQuery(conn, sql_query) %>% 
    rename("id" = cmd_code,
           "text" = cmd_desc)
  
  return(serv_concord_vector)
}

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

# 8. available reporter country code in database -------------------------
sql_country_code <- function(conn) {
  
  # goods available country
  sql_goods_country_query <- "SELECT DISTINCT reporter_iso FROM goods"
  
  # service available country
  sql_service_country_query <- "SELECT DISTINCT reporter_iso FROM services"
  
  # fetch data
  sql_goods_country <- dbGetQuery(conn, sql_goods_country_query)
  sql_service_country <- dbGetQuery(conn, sql_service_country_query)
  
  anti_join_country <- anti_join(sql_goods_country, sql_service_country) %>% pull()
  
  if (length(anti_join_country) == 0) {
    
    goods_country <- sql_goods_country$reporter_iso
    services_country <- sql_service_country$reporter_iso
    
  } else {
    
    stop("error: country codes mismatch between goods and services table, please redownload all of the data")
    
  }
  
  return(list(goods_avail_country = goods_country, serv_avail_country = services_country ))
}

# 4. joined export, import, and macro data with country concordances ------------------

# 5. create tables to store modelling and forecasting values -----------------------

# 6. storing values back to sqlite database -------------------------
