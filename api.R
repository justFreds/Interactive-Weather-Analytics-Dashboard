library(httr)
library(jsonlite)
library(lubridate)

# ----------------------------------------------------------------------------
# Function: geocode_city
# Purpose: converts city into lat/lon coordinates for api
# Input: city_name
# Output: List with lat, lon, and formatted name OR NULL if not found
# ----------------------------------------------------------------------------
geocode_city <- function(city_name) {
  tryCatch({
    #  geocode api url
    url <- paste0(
      "https://geocoding-api.open-meteo.com/v1/search?",
      "name=", URLencode(city_name),
      "&count=1&language=en&format=json"
    )
    
    # api request
    response <- GET(url)
    
    # check if request was successful
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      # Check if results exist
      if (!is.null(data$results) && length(data$results) > 0) {
        return(list(
          lat = data$results$latitude[1],
          lon = data$results$longitude[1],
          name = paste0(data$results$name[1], 
                        if (!is.null(data$results$admin1[1])) 
                          paste0(", ", data$results$admin1[1]) 
                        else "")
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Geocoding error: ", e$message)
    return(NULL)
  })
}

# ----------------------------------------------------------------------------
# Function: fetch_weather
# Purpose: uses lat/lon coordinates to fetch weather data from api
# Input: lat (numeric), lon (numeric)
# Output: List containing weather data OR NULL if failed
# ----------------------------------------------------------------------------
fetch_weather <- function(lat, lon) {
  tryCatch({
    # weather api url with all parameters
    url <- paste0(
      "https://api.open-meteo.com/v1/forecast?",
      "latitude=", lat,
      "&longitude=", lon,
      "&current=temperature_2m,relative_humidity_2m,apparent_temperature,",
      "precipitation,weather_code,wind_speed_10m,wind_direction_10m,",
      "surface_pressure,cloud_cover,uv_index",
      "&hourly=temperature_2m,relative_humidity_2m,precipitation_probability,weather_code,wind_speed_10m",
      "&daily=temperature_2m_max,temperature_2m_min,weather_code,precipitation_sum,",
      "relative_humidity_2m_mean,uv_index_max,wind_speed_10m_max",
      "&timezone=auto"
    )
    
    # api req
    response <- GET(url)
    
    # Check if request was successful
    if (status_code(response) == 200) {
      return(fromJSON(content(response, "text")))
    }
    return(NULL)
  }, error = function(e) {
    message("Weather fetch error: ", e$message)
    return(NULL)
  })
}

# ----------------------------------------------------------------------------
# Function: get_weather_description
# Purpose: converts weather code to english description
# Input: code (numeric) - API weather code
# Output: String description of weather condition
# ----------------------------------------------------------------------------
get_weather_description <- function(code) {
  descriptions <- list(
    "0" = "Clear sky", "1" = "Mainly clear", "2" = "Partly cloudy",
    "3" = "Overcast", "45" = "Foggy", "48" = "Foggy",
    "51" = "Light drizzle", "53" = "Moderate drizzle", "55" = "Dense drizzle",
    "61" = "Slight rain", "63" = "Moderate rain", "65" = "Heavy rain",
    "71" = "Slight snow", "73" = "Moderate snow", "75" = "Heavy snow",
    "80" = "Rain showers", "81" = "Rain showers", "82" = "Heavy showers",
    "95" = "Thunderstorm", "96" = "Thunderstorm", "99" = "Severe thunderstorm"
  )
  return(descriptions[[as.character(code)]] %||% "Unknown")
}

# ----------------------------------------------------------------------------
# Function: fetch_forecast (MAIN FUNCTION)
# Purpose: Main function to fetch complete weather forecast for a city
# Input: city (string) - Name of the city to get weather for
# Output: List containing:
#         - current: Current weather conditions
#         - hourly: Data frame with hourly forecast
#         - daily: Daily forecast data
#         - location: Full location name
# Error: Stops execution if city not found or API fails
# ----------------------------------------------------------------------------
fetch_forecast <- function(city) {
  # 1. get lat and lon from city name
  message("Looking up coordinates for: ", city)
  coords <- geocode_city(city)
  
  if (is.null(coords)) {
    stop("ERROR: City '", city, "' not found. Please check spelling and try again.")
  }
  
  message("Found: ", coords$name, " (", coords$lat, ", ", coords$lon, ")")
  
  # 2. fetch weather data using coordinates
  message("Fetching weather data...")
  weather <- fetch_weather(coords$lat, coords$lon)
  
  if (is.null(weather)) {
    stop("ERROR: Failed to fetch weather data. Please try again.")
  }
  
  # 3. create hourly data frame (next 48 hours)
  hourly_df <- data.frame(
    datetime = ymd_hm(weather$hourly$time),
    temp = weather$hourly$temperature_2m,
    humidity = weather$hourly$relative_humidity_2m,
    description = sapply(weather$hourly$weather_code, get_weather_description),
    precipitation_prob = weather$hourly$precipitation_probability,
    wind_speed = weather$hourly$wind_speed_10m,
    stringsAsFactors = FALSE
  )
  
  # 4. create daily data frame (next 7 days)
  daily_df <- data.frame(
    date = as.Date(weather$daily$time),
    temp_max = weather$daily$temperature_2m_max,
    temp_min = weather$daily$temperature_2m_min,
    humidity_mean = weather$daily$relative_humidity_2m_mean,
    description = sapply(weather$daily$weather_code, get_weather_description),
    precipitation_sum = weather$daily$precipitation_sum,
    uv_index_max = weather$daily$uv_index_max,
    wind_speed_max = weather$daily$wind_speed_10m_max,
    stringsAsFactors = FALSE
  )
  
  # 5. return data
  message("Weather data fetched successfully!")
  return(list(
    current = list(
      temperature = weather$current$temperature_2m,
      humidity = weather$current$relative_humidity_2m,
      feels_like = weather$current$apparent_temperature,
      precipitation = weather$current$precipitation,
      wind_speed = weather$current$wind_speed_10m,
      wind_direction = weather$current$wind_direction_10m,
      pressure = weather$current$surface_pressure,
      cloud_cover = weather$current$cloud_cover,
      uv_index = weather$current$uv_index,
      weather_code = weather$current$weather_code,
      description = get_weather_description(weather$current$weather_code)
    ),
    hourly = hourly_df,
    daily = daily_df,
    location = coords$name,
    coordinates = list(lat = coords$lat, lon = coords$lon)
  ))
}

# ----------------------------------------------------------------------------
# Function: fetch_historical_weather
# Purpose: Fetch historical weather data for a specific year and month
# Input: lat, lon, year (numeric), month (numeric 1-12)
# Output: Data frame with daily historical data for the entire month
# ----------------------------------------------------------------------------
fetch_historical_weather <- function(lat, lon, year, month) {
  tryCatch({
    # Build start and end dates for the full month
    start_date <- as.Date(paste0(year, "-", sprintf("%02d", month), "-01"))
    # Get last day of month
    end_date <- ceiling_date(start_date, "month") - days(1)
    
    # Historical weather API
    url <- paste0(
      "https://archive-api.open-meteo.com/v1/archive?",
      "latitude=", lat,
      "&longitude=", lon,
      "&start_date=", start_date,
      "&end_date=", end_date,
      "&daily=temperature_2m_max,temperature_2m_min,precipitation_sum,",
      "wind_speed_10m_max",
      "&timezone=auto"
    )
    
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, "text"))
      
      if (!is.null(data$daily)) {
        return(data.frame(
          date = as.Date(data$daily$time),
          temp_max = data$daily$temperature_2m_max,
          temp_min = data$daily$temperature_2m_min,
          avg_temp = (data$daily$temperature_2m_max + data$daily$temperature_2m_min) / 2,
          precipitation_sum = data$daily$precipitation_sum,
          wind_speed_max = data$daily$wind_speed_10m_max,
          stringsAsFactors = FALSE
        ))
      }
    }
    return(NULL)
  }, error = function(e) {
    message("Historical fetch error: ", e$message)
    return(NULL)
  })
}

# ----------------------------------------------------------------------------
# Function: fetch_historical_comparison
# Purpose: Fetch historical data for two years for comparison (same month)
# Input: city (string), year1 (numeric), year2 (numeric), month (numeric 1-12)
# Output: List with data frames for both years
# ----------------------------------------------------------------------------
fetch_historical_comparison <- function(city, year1, year2, month) {
  coords <- geocode_city(city)
  
  if (is.null(coords)) {
    stop("ERROR: City '", city, "' not found.")
  }
  
  message("Fetching historical data for ", month.name[month], " ", year1, " and ", year2, "...")
  
  data1 <- fetch_historical_weather(coords$lat, coords$lon, year1, month)
  data2 <- fetch_historical_weather(coords$lat, coords$lon, year2, month)
  
  if (is.null(data1) || is.null(data2)) {
    stop("ERROR: Could not fetch historical data. Try different years.")
  }
  
  # Add year column and day number for alignment
  data1$year <- year1
  data1$day_num <- 1:nrow(data1)
  
  data2$year <- year2
  data2$day_num <- 1:nrow(data2)
  
  return(list(
    year1_data = data1,
    year2_data = data2,
    location = coords$name,
    month_name = month.name[month]
  ))
}
