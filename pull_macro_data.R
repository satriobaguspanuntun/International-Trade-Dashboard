library(dplyr)
library(httr2)
library(fredr)

### List of important economic indicators
# GDP Nominal & real
# Inflation (CPI)
# Unemployment rate
# Current account balance
# Export & Import (covered by comtrade)
# Trade balance (covered by comtrade)
# īnterest rate
# 10 year govt bonds
# share prices

# List countries
# G20
# OECD
# these two for now. Will add more if necessary

## function to search up available series for each selected countries
api_search_request <- function(..., search_text, search_tag, api_key){
  params <- list(
    ...,
    search_text = search_text,
    search_tag = search_tag,
    api_key = api_key
  )
  
  if (!is.c) {}
  
} 

## pull data function

## convert data into digestible format (tibble)
