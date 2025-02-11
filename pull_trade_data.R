library(dplyr)
library(lubridate)
library(comtradr)
library(tidyr)

# set comtrade api key
set_primary_comtrade_key(Sys.getenv("COMTRADE_API_KEY"))

# check api key 
check_comtrade_api <- function(api_key) {
  
  indicator <- FALSE
  
  if (is.null(api_key)) {
    
    stop("Please ensure that you've sign up to Comtrade for the free comtrade -v1 API key.")
    
  } else  if (nchar(api_key) > 32) {
    
    stop("Recheck your API key, the generic API key for comtrade is 32 character.")
    
  } else {
    
    indicator <- TRUE
  }
  return(indicator)
}

# check year date function
year_check <- function(x) {
  grepl("^\\d{4}$", x)
}
# check year month combination function
year_month_check <- function(x) {
  grepl("^\\d{4}-\\d{2}$", x)
}

# create a function to pull one country export data
pull_trade <- function(reporter, partner, direction, commod_code, freq, start, end) {
  # sense check
  if (c("all_countries") %in% reporter & c("all_countries") %in% partner) {
    warning("this will reduce the amount of data you can pull, the hit call limit is 100K rows")
  }
  # Call API
  ## OMT Goods data
  ## create a for loop for omt data to be able to pull more data 
  if (freq == "A") {
    
    if (year_check(start) & year_check(end)) {
      
      range <- seq.Date(from = as.Date(paste(start, "01", "01", sep = "-")),
                        to = as.Date(paste(end, "01", "01", sep = "-")),
                        by = "1 year")
      
      range <- substr(as.character(range), 1, 4)
      
    } else {
      
      stop("please input the correct year format 'YYYY'")
      
    }
    
  } else if (freq == "M") {
    
    if (year_month_check(start) & year_month_check(end)) {
      
      range <- seq.Date(from = as.Date(paste(start, "01", sep = "-")),
                        to = as.Date(paste(end, "01", sep = "-")),
                        by = "1 month")
      
      range <- substr(as.character(range), 1, 7)
      
    } else {
      
      stop("please input the appropriate year format 'YYYY-MM'")
    }
    
  } else {
    
    stop("Frequency options are either 'A' Annual or 'M' Monthly")
    
  }
  
  # loop time
  output_goods_list <- list()
  
  for (i in reporter) {
    country_data <- list()  # To store data for all dates for this country
    cli::cli_h1(paste0("Downloading data for ",i))
    
    for (j in range) {
      goods_data <- tryCatch({
        cli::cli_bullets(paste0("Pulling data for the year: ", j))
        ct_get_data(
          type = "goods",
          frequency = "A",
          commodity_classification = "HS",
          commodity_code = hs2,
          flow_direction = c("import", "re-import", "export", "re-export"),
          reporter = i,
          partner = "all_countries",
          start_date = j,
          end_date = j
        ) |> select(freq_code, 
                    ref_period_id,
                    ref_year, 
                    ref_month,
                    period,
                    reporter_iso, 
                    reporter_desc, 
                    flow_code, 
                    flow_desc,
                    partner_iso, 
                    partner2desc, 
                    classification_code,
                    cmd_code, 
                    cmd_desc, 
                    aggr_level,
                    customs_code,
                    customs_desc,
                    cifvalue,
                    fobvalue,
                    primary_value)
      }, error = function(e) {
        message("Error for country: ", i, ", date: ", j, ": ", e)
        missing_data <- data.frame(freq_code = NA, 
                                   ref_period_id = NA,
                                   ref_year = NA, 
                                   ref_month = NA,
                                   period = j,
                                   reporter_iso = i, 
                                   reporter_desc = NA, 
                                   flow_code = NA, 
                                   flow_desc = NA,
                                   partner_iso = NA, 
                                   partner2desc = NA, 
                                   classification_code = NA,
                                   cmd_code = NA, 
                                   cmd_desc = NA, 
                                   aggr_level = NA,
                                   customs_code = NA,
                                   customs_desc = NA,
                                   cifvalue = NA,
                                   fobvalue = NA,
                                   primary_value = NA)
        return(missing_data)
      })
      
      # if (identical(nrow(goods_data), ncol(goods_data))) {
      #   next
      # } else {}
        country_data[[j]] <- goods_data
      
      
      Sys.sleep(0.5)  # Avoid API rate limit issues
    }
    
    # Combine all data for the country
    if (length(country_data) > 0) {
      output_goods_list[[i]] <- do.call(rbind, country_data)
    }
    
  }
  
  goods_output <- as.data.frame(do.call(rbind, output_goods_list))
  
  rownames(goods_output) <- 1:nrow(goods_output)
  
  ## services data
  service_output_list <- list()
  
  for (i in reporter) {
    
    cli::cli_h1("Pulling Services Data for Reporter: {i}")
    
    service_data <- tryCatch({
      data <- ct_get_data(type = "services",
                          frequency = "A",
                          commodity_classification = "EB",
                          commodity_code = c(200),
                          flow_direction = c("import", "re-import", "export", "re-export"),
                          reporter = i,
                          partner = "World",
                          start_date = start,
                          end_date = end) 
      
      if (identical(nrow(data), ncol(data))) {
        data <- data.frame(freq_code = NA, 
                           ref_period_id = NA,
                           ref_year = NA, 
                           ref_month = NA,
                           period = NA,
                           reporter_iso = i, 
                           reporter_desc = NA, 
                           flow_code = NA, 
                           flow_desc = NA,
                           partner_iso = NA, 
                           partner2desc = NA, 
                           classification_code = NA,
                           cmd_code = NA, 
                           cmd_desc = NA, 
                           aggr_level = NA,
                           customs_code = NA,
                           customs_desc = NA,
                           cifvalue = NA,
                           fobvalue = NA,
                           primary_value = NA)
      } else {
        data <- data %>% 
          select(freq_code, 
                 ref_period_id,
                 ref_year, 
                 ref_month,
                 period,
                 reporter_iso, 
                 reporter_desc, 
                 flow_code, 
                 flow_desc,
                 partner_iso, 
                 partner2desc, 
                 classification_code,
                 cmd_code, 
                 cmd_desc, 
                 aggr_level,
                 customs_code,
                 customs_desc,
                 cifvalue,
                 fobvalue,
                 primary_value)
      }
    }, error = function(e){
      data <- data.frame(freq_code = NA, 
                         ref_period_id = NA,
                         ref_year = NA, 
                         ref_month = NA,
                         period = NA,
                         reporter_iso = i, 
                         reporter_desc = NA, 
                         flow_code = NA, 
                         flow_desc = NA,
                         partner_iso = NA, 
                         partner2desc = NA, 
                         classification_code = NA,
                         cmd_code = NA, 
                         cmd_desc = NA, 
                         aggr_level = NA,
                         customs_code = NA,
                         customs_desc = NA,
                         cifvalue = NA,
                         fobvalue = NA,
                         primary_value = NA)
    })
    
    service_output_list[[i]] <- service_data
    
  }
  
  service_output <- as.data.frame(do.call(rbind, service_output_list))
  
  rownames(service_output) <- 1:nrow(service_output)
  
  # return list
  output_list <- list(goods = goods_output, services = service_output)
  return(output_list)
}

pull_hs_concord <- function () {
  cli::cli_h1("Downloading HS level concordance table")
  
  hs_concord <- ct_get_ref_table("HS")
  
  saveRDS(hs_concord, "~/international-Trade-Dashboard/data/hs_concord.rds")
  
  cli::cli_alert_success(paste0("HS concordance table is stored in"," ~/international-Trade-Dashboard/data/hs_concord.rds"))
}


## function to call EB10 IMTS 10 concordance
pull_serv_concord <- function() {
  cli::cli_h1("Downloading EBOPS 10 concordance table")
  
  serv_concord <- ct_get_ref_table("EB")
  
  saveRDS(serv_concord, "~/international-Trade-Dashboard/data/serv_concord.rds")
  
  cli::cli_alert_success(paste0("EBOPS 2010 concordance table is store in ~/international-Trade-Dashboard/data/serv_concord.rds"))
}

## function to call HS2 concordance
concordance <-  function (){
  
  path <- "~/international-Trade-Dashboard/data"
  
  files <- list.files(path)
  
  files_count <- length(files)
  
  if (files_count == 0) {
    
    pull_hs_concord()
    pull_serv_concord()
    
    hs_concord <- readRDS("~/international-Trade-Dashboard/data/hs_concord.rds")
    serv_concord <- readRDS("~/international-Trade-Dashboard/data/serv_concord.rds")
    list_concord <- list(hs = hs_concord, serv = serv_concord)
    
  }  else {
    
    cli::cli_h1("HS level concordance table")
    
    hs_concord <- readRDS("~/international-Trade-Dashboard/data/hs_concord.rds")
    
    cli::cli_alert_success("Sucessfully loaded!")
    
    cli::cli_h1("EBOPS 2010 concordance table")
    
    serv_concord <- readRDS("~/international-Trade-Dashboard/data/serv_concord.rds")
    
    cli::cli_alert_success("Sucessfully loaded!")
    list_concord <- list(hs = hs_concord, serv = serv_concord)
  }
  return(list_concord)
}

