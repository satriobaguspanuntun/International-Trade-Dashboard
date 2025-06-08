## Purpose of the Dashboard

The purpose of this dashboard is threefold:

1. **Data Compilation & Database Building**  
   To gather and compile relevant macroeconomic and trade datasets from various sources—primarily via APIs—and store them in a structured database. This not only ensures ease of access but also gives me hands-on experience with SQL. I opted for SQLite because it's lightweight, free, and integrates well with R.

2. **Data Visualization & Analysis**  
   To build an interactive dashboard that displays key facts, statistics, and visualizations derived from both macroeconomic and trade datasets. It also includes time series modelling, forecasting, and drill-down analysis of trade flows.

3. **Why Not?**  
   Honestly, why not? Doing cool, data-driven projects while job hunting is a great way to stay sharp. Instead of waiting on chance or praying to the "probability gods," I figured I might as well learn, build, and document something useful.

---

## Structure of the Dashboard App

### 1. Data Gathering & Building the Local Database (SQLite)

There are three main datasets used in this dashboard:

- **UN Comtrade API**  
  Provides goods export and import data for nearly every country, with detail levels ranging from HS2 to HS6 (higher HS codes = more granular). However, it lacks comprehensive coverage of services data, especially at the bilateral level.

- **World Development Indicators (WDI) API**  
  Offers a broad range of macroeconomic and social indicators. This source is used to fetch up-to-date macroeconomic statistics for countries featured in the dashboard.

- **UNCTAD Statistics**  
  Used to fill the service trade data gap left by UN Comtrade. UNCTAD provides consistent and comprehensive country-to-world services data.

> *Note: Most APIs require registration to access keys. Users must fill out a form or sign up before they can download data programmatically.*

Once the API keys are acquired and the supplementary data downloaded, running the R script `master_data.R` will fetch and compile all datasets according to adjustable parameters. The data is then saved to a local SQLite database, which acts as persistent storage for the app.

**⚠️ Important:** The dashboard depends entirely on this SQLite database. The app executes various SQL queries against it, and the visualizations, forecasts, and statistics rely on these query results. Without the database, the dashboard will not function.

### 2. UI and Server Components

The app is structured using a single file: `app.R`.

- **UI Section (Top of `app.R`)**  
  Defines the layout, menus, and visual structure of the dashboard. It uses Shiny’s UI components to render plots, tables, and input controls.

- **Server Section (Majority of `app.R`)**  
  This is where the magic happens. It contains most of the logic, including:
  - Data retrieval via SQL queries
  - Data manipulation and wrangling
  - Statistical modelling and forecasting
  - Dynamic rendering of visuals based on user inputs
  
## 3. Libraries

If you fork or clone the dashboard project, please ensure you have the latest version of the following libraries installed:

### The App Libraries

```r
library(shiny)
library(shinyWidgets)
library(tidyverse)
library(bs4Dash)
library(RSQLite)
library(highcharter)
library(plotly)
library(DT)
library(RColorBrewer)
library(shinycssloaders)
library(sf)
library(leaflet)
library(glue)
library(waiter)
library(seasonal)
library(dygraphs)
library(shinyjs)
```

### Data Fetch & Database Construction

```r
library(dplyr)
library(lubridate)
library(comtradr)
library(tidyr)
library(httr2)
library(fredr)
library(WDI)
library(jsonlite)
```

### Installation Script (Run in Console)

To automatically install any missing packages, run this script in your R console:

```r
required_packages <- c(
  'shiny', 'shinyWidgets', 'tidyverse', 'bs4Dash', 'RSQLite', 'highcharter',
  'plotly', 'DT', 'RColorBrewer', 'shinycssloaders', 'sf', 'leaflet', 'glue',
  'waiter', 'seasonal', 'dygraphs', 'shinyjs', 'dplyr', 'lubridate', 'comtradr',
  'tidyr', 'httr2', 'fredr', 'WDI', 'jsonlite'
)

new_packages <- setdiff(required_packages, rownames(installed.packages()))
if (length(new_packages)) install.packages(new_packages)
```  