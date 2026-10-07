# ----------------------------------------------------------------------------
# wrangle.R
# Purpose: Data cleaning and preparation functions for weather API output
# ----------------------------------------------------------------------------


# Example: function to prepare hourly temperature timeseries
prepare_temp_timeseries <- function(hourly_df) {
  # hourly_df: expects the 'hourly' data frame from fetch_forecast() output
  cleaned <- hourly_df
  
  # datetime is already POSIXct in api.r, so no need to convert again
  
  cleaned <- cleaned[, c("datetime", "temp", "humidity", "description",
                         "precipitation_prob", "wind_speed")]
  
  # ensure data is in order by time
  cleaned <- cleaned[order(cleaned$datetime), ]
  
  return(cleaned)
}

# ----------------------------------------------------------------------------
# Example: function to prepare daily averages
prepare_daily_averages <- function(daily_df) {
  # daily_df: expects the 'daily' data frame from fetch_forecast() output
  cleaned_daily <- daily_df
  
  cleaned_daily$avg_temp <- (cleaned_daily$temp_max + cleaned_daily$temp_min) / 2
  
  cleaned_daily <- cleaned_daily[, c("date", "temp_max", "temp_min", "avg_temp", "description", "precipitation_sum", "uv_index_max", "wind_speed_max")]
  
  return(cleaned_daily)
}

# ----------------------------------------------------------------------------
# Example: function to prepare humidity or other current weather info
prepare_humidity <- function(current_list) {
  # current_list: expects the 'current' list from fetch_forecast() output
  
  cleaned_humidity <- data.frame(
    temperature = current_list$temperature,
    humidity = current_list$humidity,
    feels_like = current_list$feels_like,
    precipitation = current_list$precipitation,
    wind_speed = current_list$wind_speed,
    uv_index = current_list$uv_index,
    description = current_list$description
  )  
  
  return(cleaned_humidity)
}

# ----------------------------------------------------------------------------
# Example usage
# ----------------------------------------------------------------------------

# result <- fetch_forecast("Los Angeles")
# hourly_clean <- prepare_temp_timeseries(result$hourly)
# daily_clean <- prepare_daily_averages(result$daily)
# humidity_clean <- prepare_humidity(result$current)

# print(head(hourly_clean))
# print(daily_clean))
# print(humidity_clean)
