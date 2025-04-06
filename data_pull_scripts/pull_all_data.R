# LOGIC SCRIPT FOR PULLING TRADE AND MACRO DATA FOR EACH SELECTED COUNTRIES.
# Use windows scheduler to run this script.
## the main idea behind this script is to reduce the amount of api call
## each day for the countries that we are interested in. Slicing the countries
## into batches (like 5 to 10 countries) and pull their data each day recursively

# function to split the countries into small batches, each batch consist of 5-10 countries
country_split <- function(n = 5) {
  
  country_code <- readRDS("~/international-Trade-Dashboard/data/countrycode.rds")
  
  colnames(country_code) <- tolower(colnames(country_code))
  
  list_batches  <- country_code |> group_by(row_number() %/% n) |> group_map(~ .x)
  
  return(list_batches)
}

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
  
  return(list(goods = final_trade_data, services = final_service_data, macro = final_macro_data))
}

# create sqlite database and update or save data on the first instance
sqlite_push <- function(data_list){
  
  # dataset names
  dataset_names <- names(data_list)
  
  # check if master_db does exist AND whether db has data in it
  if (file.exists("~/international-Trade-Dashboard/master_db.db") & length(dbListTables(conn)) != 0) {
    
    for (i in dataset_names) {
      
      if (i %in% c("goods", "services")) {
        
        data <- data_list[[paste0(i)]]
        
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
          
          cli::cli_h3(paste0("The new data dimension and content are exactly matched with the one in the base"))
          
          overwrite_table <- rstudioapi::showQuestion(title = "Overwriting option", 
                                                      message = "Would you like to overwrite the table?",
                                                      ok = "Yes", cancel = "No")
          if (overwrite_table) {
            
            dbWriteTable(conn, name = i, value = data, overwrite = TRUE)
            
          }
          
        }
        
      } else { 
        
        data <- data_list[[paste0(i)]]
        
        # macro side
        sql_macro_query <- sprintf("SELECT * FROM macro")
        macro_data_table <- dbGetQuery(conn, sql_macro_query) %>% 
          mutate(year = as.character(year),
                 current_account = as.numeric(current_account),
                 fdi = as.numeric(fdi))
        
        # pivot longer both dataset (new and old)
        # old
        pivot_macro_table <- pivot_longer(macro_data_table, 
                                          cols = gdp_nominal:fdi,
                                          names_to = "var",
                                          values_to = "value") %>% 
          arrange(desc(var))
        
        # new 
        pivot_new_macro_table <- pivot_longer(data,
                                              cols = gdp_nominal:fdi,
                                              names_to = "var",
                                              values_to = "value") %>% 
          mutate(year = as.character(year))
        
        # anti join to find the remainder
        macro_data_remainder <- anti_join(pivot_new_macro_table, pivot_macro_table,
                                          join_by(country, iso2c, iso3c, year, var))
        
        # append if new rows detected
        if (nrow(macro_data_remainder) > 0) {
          
          dbAppendTable(conn, name = i, value = data)
          
          
        } else {
          
          cli::cli_h3(paste0("The new data dimension and content are exactly matched with the one in the base"))
          
          overwrite_table <- rstudioapi::showQuestion(title = "Overwriting option", 
                                                      message = "Would you like to overwrite the table?",
                                                      ok = "Yes", cancel = "No")
          if (overwrite_table) {
            
            dbWriteTable(conn, name = i, value = data, overwrite = TRUE)
            
          }
          
        }
        
      }
      
    }
    
  } else {
    
    # create new blank database (master_db)
    ## Table for Trade goods
    dbExecute(conn, "CREATE TABLE goods (
    
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
    
    primary_value bigint
)")
    
    ## Table for service data
    dbExecute(conn, "CREATE TABLE services (
    
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
    
    primary_value bigint
)")
    ## Table for Macroeconomic data
    dbExecute(conn, "CREATE TABLE macro (
              
              country TEXT,
              
              iso2c TEXT,
              
              iso3c TEXT,
              
              year INT,
              
              gdp_nominal INT,
              
              gdp_per_capita_current INT,
              
              gdp_per_capita_growth INT,
              
              export_gdp_percent INT,
              
              inflation INT,
              
              unemployment INT,
              
              population INT,
              
              net_migration INT,
              
              current_account INT,
              
              fdi INT,
              
              gross_capital_formation INT
              
    )")
    
    # write table into the database
    # goods trade
    dbWriteTable(conn, name = "goods", data_list[[1]], append = TRUE)
    
    # service trade
    dbWriteTable(conn, name = "services", data_list[[2]], append = TRUE)
    
    # macro trade
    dbWriteTable(conn, name = "macro", data_list[[3]], append = TRUE)
    
  }
}


#test <- loop_across_countries(country_batches[2], start = "2020", "2023", hs = hs2)

#sqlite_push(data_list = test)

# db_data_test <- dbGetQuery(conn, "SELECT * FROM goods WHERE reporter_iso in ('ARG', 'AUS', 'AUT')")

# NICE IT WORKSS YEAYY
# Now create a new script so that you can automatically update the database on a given period of frequency
# maybe every week or what have you just ensure that everything goes smoothly without any issues (impossible but it's something worth to fight for)

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
