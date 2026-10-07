# ----------------------------------------------------------------------------
# stats.R
# Purpose: Summary statistics, moving averages, and additional features
# ----------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(zoo)

# ----------------------------------------------------------------------------
# Function: compute_summary_stats
# Purpose: Calculate summary statistics for temperature and humidity
# Input: hourly_df - cleaned hourly data from prepare_temp_timeseries()
#        daily_df - cleaned daily data from prepare_daily_averages()
# Output: List containing summary statistics
# ----------------------------------------------------------------------------
compute_summary_stats <- function(hourly_df, daily_df) {
  
  # Temperature statistics (hourly)
  temp_stats <- list(
    mean_temp = round(mean(hourly_df$temp, na.rm = TRUE), 1),
    min_temp = round(min(hourly_df$temp, na.rm = TRUE), 1),
    max_temp = round(max(hourly_df$temp, na.rm = TRUE), 1),
    temp_range = round(max(hourly_df$temp, na.rm = TRUE) - min(hourly_df$temp, na.rm = TRUE), 1),
    std_dev_temp = round(sd(hourly_df$temp, na.rm = TRUE), 2)
  )
  
  # Humidity statistics (hourly)
  humidity_stats <- list(
    mean_humidity = round(mean(hourly_df$humidity, na.rm = TRUE), 1),
    min_humidity = round(min(hourly_df$humidity, na.rm = TRUE), 1),
    max_humidity = round(max(hourly_df$humidity, na.rm = TRUE), 1),
    humidity_trend = ifelse(
      tail(hourly_df$humidity, 12) %>% mean(na.rm = TRUE) > 
        head(hourly_df$humidity, 12) %>% mean(na.rm = TRUE),
      "Increasing", "Decreasing"
    )
  )
  
  # Precipitation statistics (daily)
  precip_stats <- list(
    total_precip = round(sum(daily_df$precipitation_sum, na.rm = TRUE), 2),
    max_daily_precip = round(max(daily_df$precipitation_sum, na.rm = TRUE), 2),
    rainy_days = sum(daily_df$precipitation_sum > 0, na.rm = TRUE)
  )
  
  # Wind statistics (daily)
  wind_stats <- list(
    max_wind = round(max(daily_df$wind_speed_max, na.rm = TRUE), 1),
    avg_wind = round(mean(daily_df$wind_speed_max, na.rm = TRUE), 1)
  )
  
  return(list(
    temperature = temp_stats,
    humidity = humidity_stats,
    precipitation = precip_stats,
    wind = wind_stats
  ))
}

# ----------------------------------------------------------------------------
# Function: format_stats_table
# Purpose: Format summary stats into a display-friendly data frame
# Input: stats - output from compute_summary_stats()
# Output: Data frame suitable for table display
# Note: All stats are based on the 7-day forecast period
# ----------------------------------------------------------------------------
format_stats_table <- function(stats) {
  data.frame(
    Metric = c(
      "Mean Temp (7-day forecast)",
      "Min Temp (7-day forecast)", 
      "Max Temp (7-day forecast)",
      "Temp Range (°C)",
      "Mean Humidity (%)",
      "Humidity Trend",
      "Total Precipitation (mm)",
      "Rainy Days",
      "Max Wind Speed (km/h)"
    ),
    Value = c(
      paste0(stats$temperature$mean_temp, "°C"),
      paste0(stats$temperature$min_temp, "°C"),
      paste0(stats$temperature$max_temp, "°C"),
      paste0(stats$temperature$temp_range, "°C"),
      paste0(stats$humidity$mean_humidity, "%"),
      stats$humidity$humidity_trend,
      paste0(stats$precipitation$total_precip, " mm"),
      stats$precipitation$rainy_days,
      paste0(stats$wind$max_wind, " km/h")
    ),
    stringsAsFactors = FALSE
  )
}

# ----------------------------------------------------------------------------
# Function: add_moving_average
# Purpose: Add moving average columns to hourly data
# Input: hourly_df - cleaned hourly data
#        window - number of hours for moving average (default 6)
# Output: Data frame with additional moving average columns
# ----------------------------------------------------------------------------
add_moving_average <- function(hourly_df, window = 6) {
  hourly_df <- hourly_df %>%
    mutate(
      temp_ma = zoo::rollmean(temp, k = window, fill = NA, align = "right"),
      humidity_ma = zoo::rollmean(humidity, k = window, fill = NA, align = "right")
    )
  return(hourly_df)
}

# ----------------------------------------------------------------------------
# Function: plot_temp_with_ma
# Purpose: Plot temperature with moving average overlay
# Input: hourly_df - hourly data (will add MA if not present)
# Output: ggplot object
# ----------------------------------------------------------------------------
plot_temp_with_ma <- function(hourly_df) {
  if (!"temp_ma" %in% names(hourly_df)) {
    hourly_df <- add_moving_average(hourly_df)
  }
  
  ggplot(hourly_df, aes(x = datetime)) +
    geom_line(aes(y = temp, color = "Actual"), linewidth = 0.8, alpha = 0.7) +
    geom_line(aes(y = temp_ma, color = "6-Hour Moving Avg"), 
              linewidth = 1.3, na.rm = TRUE) +
    scale_color_manual(
      values = c("Actual" = "#90CAF9", "6-Hour Moving Avg" = "#1565C0"),
      name = ""
    ) +
    labs(
      title = "Hourly Temperature with Trend",
      x = "Time",
      y = "Temperature (°C)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}

# ----------------------------------------------------------------------------
# Function: plot_precipitation_forecast
# Purpose: Plot precipitation probability over time
# Input: hourly_df - hourly data with precipitation_prob
# Output: ggplot object
# ----------------------------------------------------------------------------
plot_precipitation_forecast <- function(hourly_df) {
  ggplot(hourly_df, aes(x = datetime, y = precipitation_prob)) +
    geom_area(fill = "#64B5F6", alpha = 0.6) +
    geom_line(color = "#1565C0", linewidth = 0.8) +
    labs(
      title = "Precipitation Probability",
      x = "Time",
      y = "Probability (%)"
    ) +
    theme_minimal() +
    scale_y_continuous(limits = c(0, 100))
}

# ----------------------------------------------------------------------------
# Function: plot_daily_temp_range
# Purpose: Show daily high/low temperature range
# Input: daily_df - cleaned daily data
# Output: ggplot object
# ----------------------------------------------------------------------------
plot_daily_temp_range <- function(daily_df) {
  ggplot(daily_df, aes(x = date)) +
    geom_ribbon(aes(ymin = temp_min, ymax = temp_max), 
                fill = "#90CAF9", alpha = 0.5) +
    geom_line(aes(y = temp_max, color = "High"), linewidth = 1) +
    geom_line(aes(y = temp_min, color = "Low"), linewidth = 1) +
    geom_point(aes(y = temp_max), color = "#D32F2F", size = 2) +
    geom_point(aes(y = temp_min), color = "#1976D2", size = 2) +
    scale_color_manual(
      values = c("High" = "#D32F2F", "Low" = "#1976D2"),
      name = ""
    ) +
    labs(
      title = "7-Day Temperature Range",
      x = "Date",
      y = "Temperature (°C)"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}