# Interactive Weather Analytics Dashboard

An interactive weather analytics application built in **R** with **Shiny** and **shinydashboard**. The dashboard retrieves live and historical weather data from the **Open-Meteo API**, transforms the data for analysis, computes summary statistics and moving averages, and presents the results through interactive dashboard views and `ggplot2` visualizations.

## Features

### Current Weather

Search for a city and view current conditions including:

- Temperature
- Feels-like temperature
- Humidity
- Wind speed
- Cloud cover
- Surface pressure
- Precipitation
- UV index

The dashboard geocodes the city name to latitude/longitude coordinates before requesting weather data.

### Detailed Forecast

The forecast view includes:

- 7-day high/low temperature range
- Hourly temperature trends
- 6-hour moving average
- Humidity over time
- Precipitation probability
- Summary statistics for temperature, humidity, precipitation, and wind

### Historical Comparison

Compare the same month across two different years for a selected city.

The dashboard displays:

- Daily average temperature comparison
- Monthly average, maximum, and minimum temperatures
- Total precipitation
- Maximum wind speed
- Precipitation comparison by day

## Data Pipeline

```text
City Search
    |
    v
Open-Meteo Geocoding API
    |
    v
Latitude / Longitude
    |
    +--------------------------+
    |                          |
    v                          v
Forecast API              Historical Archive API
    |                          |
    v                          v
Data Frames               Monthly Historical Data
    |                          |
    +------------+-------------+
                 |
                 v
        Cleaning & Transformation
                 |
                 v
       Statistical Analysis
                 |
                 v
        Shiny Dashboard + ggplot2
```

## Technologies

- **R**
- **Shiny**
- **shinydashboard**
- **shinyWidgets**
- **ggplot2**
- **dplyr**
- **httr**
- **jsonlite**
- **lubridate**
- **zoo**
- **Open-Meteo REST APIs**

## Project Structure

```text
.
├── app.R       # Shiny UI, server logic, reactive workflows, and dashboard outputs
├── api.R       # Geocoding, forecast, and historical Open-Meteo API requests
├── wrangle.R   # Data cleaning and transformation functions
├── stats.R     # Summary statistics, moving averages, and analytical plots
└── plots.R     # Reusable ggplot2 visualization functions
```

## How It Works

### 1. Data Retrieval

`api.R` handles communication with Open-Meteo.

A city name is first converted into geographic coordinates using the Open-Meteo Geocoding API. Those coordinates are then used to retrieve current, hourly, and 7-day forecast data.

The application also uses the Open-Meteo Historical Weather API to retrieve full-month daily data for year-over-year comparisons.

### 2. Data Wrangling

`wrangle.R` prepares the API output for analysis by:

- Selecting relevant weather variables
- Ordering hourly observations chronologically
- Calculating daily average temperature from daily high and low values
- Structuring current weather data into analysis-friendly formats

### 3. Statistical Analysis

`stats.R` calculates descriptive weather statistics including:

- Mean, minimum, and maximum temperature
- Temperature range
- Temperature standard deviation
- Mean, minimum, and maximum humidity
- Humidity trend
- Total and maximum precipitation
- Number of rainy days
- Maximum and average wind speed

A **6-hour rolling moving average** is calculated for hourly temperature and humidity using `zoo::rollmean()`.

### 4. Visualization

The application uses `ggplot2` to visualize:

- Daily average temperature
- 7-day temperature ranges
- Hourly temperature with a moving-average overlay
- Humidity over time
- Precipitation probability
- Historical temperature comparisons
- Historical precipitation comparisons

### 5. Interactive Dashboard

`app.R` combines the API, transformation, statistical, and visualization modules into an interactive Shiny application.

Reactive workflows update the dashboard when a user searches for a city or requests a historical comparison.

## Installation

Clone the repository:

```bash
git clone https://github.com/justFreds/Interactive-Weather-Analytics-Dashboard.git
cd Interactive-Weather-Analytics-Dashboard
```

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "shinyWidgets",
  "ggplot2",
  "dplyr",
  "httr",
  "jsonlite",
  "lubridate",
  "zoo"
))
```

## Running the Dashboard

From R or RStudio, set the working directory to the repository folder and run:

```r
shiny::runApp()
```

The dashboard will launch locally in your web browser.

## Example Workflow

1. Enter a city such as `Los Angeles`.
2. Click **Search**.
3. Review current conditions on the **Current Weather** tab.
4. Open **Detailed Forecast** to explore forecast statistics and time-series visualizations.
5. Open **Historical Comparison**.
6. Select a month and two years.
7. Click **Compare Years** to analyze historical temperature and precipitation differences.

## Concepts Demonstrated

This project demonstrates:

- REST API integration
- JSON parsing and data ingestion
- Data cleaning and transformation
- Exploratory data analysis
- Descriptive statistics
- Rolling-window analysis
- Time-series visualization
- Reactive programming with Shiny
- Modular R application design
- Interactive dashboard development

## Data Source

Weather and geocoding data are provided by **Open-Meteo**.

- Forecast API: current, hourly, and daily forecast data
- Geocoding API: city-to-coordinate lookup
- Historical Weather API: archived daily weather data

No API key is required for the endpoints used by this project.

## Possible Improvements

- Add unit selection for Celsius/Fahrenheit and km/h/mph
- Add downloadable CSV reports
- Add map-based location selection
- Add caching to reduce repeated API requests
- Add additional statistical comparisons across historical periods
- Deploy the application publicly with shinyapps.io or another hosting platform
