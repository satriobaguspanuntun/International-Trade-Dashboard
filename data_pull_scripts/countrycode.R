library(dplyr)
library(giscoR)

# EU countries vector
eu_iso3_code <- gisco_countrycode |> filter(eu == TRUE)
eu_iso3_code_vec <- eu_iso3_code$ISO3_CODE

# countries vector G20
g20_iso3_code <- gisco_countrycode |> 
  filter(cldr.short.en %in% c("Argentina", 
                              "Australia", 
                              "Brazil",
                              "Canada",
                              "China",
                              "France",
                              "Germany",
                              "India",
                              "Indonesia",
                              "Italy",
                              "Japan",
                              "Mexico",
                              "Russia",
                              "Saudi Arabia",
                              "South Africa",
                              "South Korea",
                              "Turkey",
                              "United Kingdom",
                              "United States"))
g20_iso3_code_vec <- g20_iso3_code$ISO3_CODE

# countries vector OECD
oecd_iso3_code <- gisco_countrycode |> 
  filter(cldr.short.en %in% c("Australia",
                              "Austria",
                              "Belgium",
                              "Canada",
                              "Chile",
                              "Colombia",
                              "Costa Rica",
                              "Czechia",
                              "Denmark",
                              "Estonia",
                              "Finland",
                              "France",
                              "Germany",
                              "Greece",
                              "Hungary",
                              "Iceland",
                              "Ireland",
                              "Israel",
                              "Italy",
                              "Japan",
                              "South Korea",
                              "Latvia",
                              "Lithuania",
                              "Luxembourg",
                              "Mexico",
                              "Netherlands",
                              "New Zealand",
                              "Norway",
                              "Poland",
                              "Portugal",
                              "Slovakia",
                              "Slovenia",
                              "Spain",
                              "Sweden",
                              "Switzerland",
                              "Turkey",
                              "UK",
                              "US"))
oecd_country_vec <- oecd_iso3_code$ISO3_CODE

# all countries dataframe
all_countrycode <- gisco_countrycode |> 
  select(-eu) |> 
  mutate(eu = if_else(ISO3_CODE %in% eu_iso3_code_vec, 1, 0),
         g20 = if_else(ISO3_CODE %in% g20_iso3_code_vec, 1, 0),
         oecd = if_else(ISO3_CODE %in% oecd_country_vec, 1, 0)) |> 
  filter(eu == 1 | g20 == 1 | oecd == 1)

# save countrycode dataframe 
saveRDS(all_countrycode, "~/international-Trade-Dashboard/data/countrycode.rds")

# create a new table in sqlite and save country meta data





