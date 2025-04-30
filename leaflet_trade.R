library(sf)
library(leaflet)
library(raster)

world_sf <- read_sf("~/international-Trade-Dashboard/data/world-administrative-boundaries.geojson")

trade_map_function <- function(world_map_data, trade_data, reporter_iso_select, trade_flow, year_select) {
  
  # calculate totals for each partner countries
  total_data <- trade_data %>% 
    filter(reporter_iso == reporter_iso_select & 
             period == year_select &
             flow_code %in% c("M", "X") &
             partner_iso != reporter_iso_select) %>% 
    group_by(period, reporter_iso, reporter_desc, partner_iso, flow_code, flow_desc) %>% 
    summarise(total_value_country = sum(primary_value)/1000000000) %>% 
    ungroup() %>% 
    filter(flow_code == trade_flow)
    
  # joined with world map sf data
  joined_data_sf <- left_join(world_map_data, total_data, join_by("iso3" == "partner_iso"))
  joined_data_sf <- st_as_sf(joined_data_sf)
  
  # bins & and color
  mybins <- quantile(joined_data_sf$total_value_country, probs = c(0, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99, 1), na.rm = TRUE)

  if (trade_flow == "M") {

    mypalette <- colorBin(
      palette = "YlOrBr", domain = joined_data_sf$total_value_country,
      na.color = "transparent", bins = mybins
    )

  } else if (trade_flow == "X") {

    mypalette <- colorBin(
      palette = "YlGnBu", domain = joined_data_sf$total_value_country,
      na.color = "transparent", bins = mybins
    )
  }

  text_label <- paste("Country: ", joined_data_sf$name, "<br/>",
                      "Area: ", joined_data_sf$continent, "<br/>",
                      "Trade Flow: ", joined_data_sf$flow_desc, "<br/>",
                      "Value: ", paste0("$",round(joined_data_sf$total_value_country, 4), "B")) %>%
    lapply(htmltools::HTML)

  # leaflet map
  trade_map <- leaflet(joined_data_sf) %>%
    addTiles() %>%
    setView(10, 0, zoom = 2) %>% 
    addPolygons(
      stroke = FALSE,
      fillOpacity = 1,
      smoothFactor = 0.5,
      color = ~ mypalette(total_value_country),
      label = text_label,
      labelOptions = labelOptions(
        textsize = "13px",
        direction = "auto"
      ),
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.5,
        bringToFront = TRUE)
    ) %>%
    addLegend(
      pal = mypalette,
      values = ~total_value_country, 
      title = ifelse(trade_flow=="M", "Import Value", "Export Value"),
      opacity = 1,
      labFormat = labelFormat(prefix = "$", suffix = "B"),
      position = "bottomright"
    )

  return(trade_map)
}

test_map <- trade_map_function(world_map_data = world_sf,
                               trade_data = all_data,
                               reporter_iso_select = "CAN",
                               trade_flow = "X",
                               year_select = "2022")
test_map


total_aus <- aus_data %>%
  filter(flow_code %in% c("M", "X") & partner_iso != "AUS") %>% 
  group_by(period, reporter_iso, reporter_desc, partner_iso,flow_code, flow_desc) %>% 
  summarise(total_value_country = sum(primary_value)/1000000000)

total_aus2 <- total_aus %>% 
  filter(!partner_iso %in% c("_X ")) %>%
  ungroup() %>% 
  group_by(period, reporter_iso, flow_code, flow_desc) %>% 
  summarise(total_aus2= sum(total_value_country)/1000000000) 

joined_aus_sf <- left_join(total_aus %>% filter(world_sf, total_aus %>% period == "2023" & flow_code == "X", join_by("iso3" == "partner_iso"))

joined_aus_sf <- st_as_sf(joined_aus_sf)

mybins <- c(0, 1, 5, 10, 30, 50, 70, 100, Inf)
mypalette <- colorBin(
  palette = "YlOrBr", domain = joined_aus_sf$total_value_country,
  na.color = "transparent", bins = mybins
)

mypal <- colorQuantile(palette = "YlOrBr", domain = joined_aus_sf$total_value_country, n = 2, reverse = FALSE)

mypal <- colorNumeric(palette = "YlOrBr", domain = joined_aus_sf$total_value_country)
  
leaflet(joined_aus_sf) %>%
  addTiles() %>% 
  addPolygons(
    stroke = FALSE,
    fillOpacity = 0.9,
    smoothFactor = 0.5,
    color = ~ mypalette(total_value_country),
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      fillOpacity = 0.5,
      bringToFront = TRUE)
  )
  

t <- world_sf %>% inner_join(countrycode, join_by("iso3" == "ISO3_CODE")) %>% filter(status == "Member State")
