library(ggplot2)

#Temp time-series plot

plot_temp_timeseries <- function(hourly_clean) {
  ggplot(hourly_clean, aes(x = datetime, y = temp)) +
    geom_line() +
    labs(
      title = "Temperature Over Time",
      x = "Time",
      y = "Temperature (°C)"
    ) +
    theme_minimal()
}

#Humidity plot

plot_humidity <- function(hourly_clean) {
  ggplot(hourly_clean, aes(x = datetime, y = humidity)) +
    geom_line() +
    labs(
      title = "Humidity Over Time",
      x = "Time",
      y = "Relative Humidity (%)"
    ) +
    theme_minimal()
}


#daily averages plot

plot_daily_averages <- function(daily_clean) {
  ggplot(daily_clean, aes(x = date, y = avg_temp)) +
    geom_col() +
    labs(
      title = "Daily Average Temperature",
      x = "Date",
      y = "Average Temperature (°C)"
    ) +
    theme_minimal()
}

# result <- fetch_forecast("Los Angeles")

# hourly_clean <- prepare_temp_timeseries(result$hourly)
# daily_clean <- prepare_daily_averages(result$daily)

# p1 <- plot_temp_timeseries(hourly_clean)
# print(p1)

# p2 <- plot_humidity(hourly_clean)
# print(p2)

# p3 <- plot_daily_averages(daily_clean)
# print(p3)
