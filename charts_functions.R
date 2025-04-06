library(echarts4r)
library(plotly)

data_macro <- sql_macro_query(conn, start = 2015, end = 2023)

## graphing function for all of the series

# current account (usually bar chart)
# echarts
data_macro %>% 
  filter(iso3c == "ARG") %>% 
  mutate(year = as.character(year),
         current_account = current_account/1000000,
         curr_acc_label = if_else(current_account < 0, 1, 0)) %>% 
  arrange(year) %>% 
  e_chart(year) %>% 
  e_bar(current_account) %>% 
  e_y_axis(formatter = e_axis_formatter("currency"))

# plotly
bar_plotly <- plot_ly(data_macro %>% 
                        filter(iso3c == "ARG") %>% 
                        mutate(year = as.character(year),
                               current_account = current_account/1000000,
                               curr_acc_label = if_else(current_account < 0, "1", "0")),
                      x = ~year, 
                      y = ~current_account, 
                      name = ~if_else(curr_acc_label == "1", "Deficit", "Surplus"),
                      type = 'bar', 
                      color = ~curr_acc_label,
                      colors = c("#28a745", "#dc3545")) %>% 
  layout(title = 'Argentina Current Account',
         xaxis = list(
           title = ""
           ),
         yaxis = list(
           title = "USD (Millions)"
         ))
bar_plotly

curr_account_chart_macro <- function(data, country) {
  
  # sense check
  if (!is.data.frame(data)) {
    stop("Please supply the function with macroeconomic data.")
  }
  
  if (nchar(country) < 3 | nchar(country) > 3) {
    stop("Select the country code in ISO3 format e.g 'ARG'.")
  } 
  
  country_name <- unique(unlist(data[data$iso3c == country,]$country))
  
  # Error handling if country not found
  if (length(country_name) == 0) {
    stop("Country not found in the dataset.")
  }
  
  # Create the plot
  curr_acc_plotly <- plot_ly(data %>% 
                          filter(iso3c == as.character(country)) %>% 
                          mutate(year = as.character(year),
                                 current_account = current_account / 1000000,  # Convert to millions
                                 curr_acc_label = factor(if_else(current_account < 0, "Deficit", "Surplus"), levels = c("Surplus", "Deficit"))),
                        x = ~year, 
                        y = ~current_account, 
                        name = ~curr_acc_label,
                        type = 'bar',
                        color = ~curr_acc_label, 
                        colors = c("#28a745", "#dc3545")) %>% 
    layout(
      title = paste("Current Account for", country_name),
      xaxis = list(
        title = "Year",
        title_font = list(size = 14),
        tickangle = -45,  # Angle the x-axis labels for better readability
        tickfont = list(size = 12),
        zeroline = FALSE
      ),
      yaxis = list(
        title = "Current Account (USD Millions)",
        title_font = list(size = 14),
        tickfont = list(size = 12),
        zeroline = TRUE,
        tickformat = ".2s"  # Format axis in millions/billions notation
      ),
      legend = list(
        orientation = 'h',  # Horizontal legend
        x = 0.5,
        xanchor = 'center',
        y = -0.2,  # Adjust legend position
        yanchor = 'bottom'
      ),
      hoverlabel = list(bgcolor = "white", font = list(size = 12, color = "black")),
      showlegend = TRUE
    )
  return(curr_acc_plotly)
}

# line graphs function







