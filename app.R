library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(dplyr)

source("api.R")
source("wrangle.R")
source("plots.R")
source("stats.R")

# ----------------------------------------------------------------------------
# UI
# ----------------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Weather Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      textInput("city", "Enter a city:", "Los Angeles"),
      actionButton("go", "Search", icon = icon("search"), 
                   style = "width: 88%; margin-left: 6%;"),
      hr(),
      menuItem("Current Weather", tabName = "current", icon = icon("sun")),
      menuItem("Detailed Forecast", tabName = "forecast", icon = icon("cloud")),
      menuItem("Historical Comparison", tabName = "historical", icon = icon("chart-line")),
      hr(),
      # Historical comparison controls (only show when on historical tab)
      conditionalPanel(
        condition = "input.tabs == 'historical'",
        selectInput("hist_month", "Select Month:",
                    choices = setNames(1:12, month.name), selected = 1),
        selectInput("year1", "Select Year 1:", 
                    choices = seq(2024, 2010, -1), selected = 2024),
        selectInput("year2", "Compare with Year 2:", 
                    choices = seq(2023, 2010, -1), selected = 2023),
        actionButton("compare", "Compare Years", icon = icon("balance-scale"),
                     style = "width: 88%; margin-left: 6%;")
      )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style.css"),
      tags$style(HTML("
        .info-box-icon { height: 90px; line-height: 90px; }
        .info-box-content { padding-top: 15px; padding-bottom: 15px; }
        .small-box { min-height: 110px; }
        .content-wrapper { background-color: #f4f6f9; }
      "))
    ),
    
    tabItems(
      # ====================================================================
      # TAB 1: Current Weather
      # ====================================================================
      tabItem(
        tabName = "current",
        
        # Location header
        fluidRow(
          box(width = 12, 
              h3(textOutput("locationName"), style = "margin: 0; text-align: center;"),
              background = "light-blue")
        ),
        
        # Current conditions: temp, feels like, condition, humidity, windspeed
        fluidRow(
          valueBoxOutput("tempBox", width = 3),
          valueBoxOutput("feelsLikeBox", width = 3),
          valueBoxOutput("humidityBox", width = 3),
          valueBoxOutput("windBox", width = 3)
        ),
        
        # Weather details row
        fluidRow(
          box(width = 6, title = "Weather Details", status = "primary", solidHeader = TRUE,
              fluidRow(
                column(6, 
                       tags$div(style = "padding: 10px;",
                         tags$h4(icon("cloud"), " Cloud Cover"),
                         tags$h3(textOutput("cloudCover"))
                       )
                ),
                column(6,
                       tags$div(style = "padding: 10px;",
                         tags$h4(icon("gauge-high"), " Pressure"),
                         tags$h3(textOutput("pressure"))
                       )
                )
              ),
              fluidRow(
                column(6,
                       tags$div(style = "padding: 10px;",
                         tags$h4(icon("droplet"), " Precipitation"),
                         tags$h3(textOutput("precipitation"))
                       )
                ),
                column(6,
                       tags$div(style = "padding: 10px;",
                         tags$h4(icon("sun"), " UV Index"),
                         tags$h3(textOutput("uvIndex"))
                       )
                )
              )
          ),
          
          # Daily Average Temperature plot
          box(width = 6, title = "Daily Average Temperature", status = "primary", solidHeader = TRUE,
              plotOutput("dailyAvgPlot", height = "250px"))
        )
      ),
      
      # ====================================================================
      # TAB 2: Detailed Forecast
      # ====================================================================
      tabItem(
        tabName = "forecast",
        
        # 7-day forecast + Summary Statistics
        fluidRow(
          box(width = 8, title = "7-Day Temperature Forecast", status = "primary", solidHeader = TRUE,
              plotOutput("tempRangePlot", height = "280px")),
          box(width = 4, title = "Summary Statistics", status = "primary", solidHeader = TRUE,
              tableOutput("summaryStats"))
        ),
        
        # Temperature with MA + Humidity
        fluidRow(
          box(width = 6, title = "Temperature Over Time (6-Hour Moving Average)", status = "primary",
              plotOutput("tempMAPlot", height = "280px")),
          box(width = 6, title = "Humidity Over Time",
              plotOutput("humidityPlot", height = "280px"))
        ),
        
        # Precipitation probability
        fluidRow(
          box(width = 12, title = "Precipitation Probability", status = "primary",
              plotOutput("precipPlot", height = "250px"))
        )
      ),
      
      # ====================================================================
      # TAB 3: Historical Comparison
      # ====================================================================
      tabItem(
        tabName = "historical",
        
        fluidRow(
          box(width = 12,
              h4("Compare monthly temperature data from different years", 
                 style = "text-align: center; color: #666;"),
              h5("Select a month and two years, then click 'Compare Years'", 
                 style = "text-align: center; color: #888;")
          )
        ),
        
        # Historical comparison plot
        fluidRow(
          box(width = 12, title = "Temperature Comparison", status = "warning", solidHeader = TRUE,
              plotOutput("historicalTempPlot", height = "350px"))
        ),
        
        # Comparison stats
        fluidRow(
          box(width = 6, title = textOutput("year1Title"), status = "primary", solidHeader = TRUE,
              tableOutput("year1Stats")),
          box(width = 6, title = textOutput("year2Title"), status = "danger", solidHeader = TRUE,
              tableOutput("year2Stats"))
        ),
        
        # Precipitation comparison
        fluidRow(
          box(width = 12, title = "Precipitation Comparison", status = "warning", solidHeader = TRUE,
              plotOutput("historicalPrecipPlot", height = "280px"))
        )
      )
    )
  )
)

# ----------------------------------------------------------------------------
# Server
# ----------------------------------------------------------------------------
server <- function(input, output, session) {
  
  # Reactive: Fetch current/forecast weather data
  weather_data <- eventReactive(input$go, {
    req(input$city)
    result <- fetch_forecast(input$city)
    list(
      hourly = prepare_temp_timeseries(result$hourly),
      current = result$current,
      daily = prepare_daily_averages(result$daily),
      location = result$location,
      coords = result$coordinates
    )
  })
  
  # Reactive: Hourly data with fixed 6-hour moving average
  hourly_with_ma <- reactive({
    req(weather_data())
    add_moving_average(weather_data()$hourly, window = 6)
  })
  
  # Reactive: Summary statistics
  summary_stats <- reactive({
    req(weather_data())
    compute_summary_stats(weather_data()$hourly, weather_data()$daily)
  })
  
  # Reactive: Historical comparison data
  historical_data <- eventReactive(input$compare, {
    req(input$city, input$year1, input$year2, input$hist_month)
    fetch_historical_comparison(
      input$city, 
      as.numeric(input$year1), 
      as.numeric(input$year2),
      as.numeric(input$hist_month)
    )
  })
  
  # ====================================================================
  # TAB 1 OUTPUTS: Current Weather
  # ====================================================================
  
  output$locationName <- renderText({
    req(weather_data())
    paste(weather_data()$location)
  })
  
  output$tempBox <- renderValueBox({
    req(weather_data())
    valueBox(
      paste0(weather_data()$current$temperature, "°C"),
      "Temperature",
      icon = icon("temperature-high"),
      color = "red"
    )
  })
  
  output$feelsLikeBox <- renderValueBox({
    req(weather_data())
    valueBox(
      paste0(weather_data()$current$feels_like, "°C"),
      "Feels Like",
      icon = icon("thermometer-half"),
      color = "orange"
    )
  })
  
  output$humidityBox <- renderValueBox({
    req(weather_data())
    valueBox(
      paste0(weather_data()$current$humidity, "%"),
      "Humidity",
      icon = icon("droplet"),
      color = "aqua"
    )
  })
  
  output$windBox <- renderValueBox({
    req(weather_data())
    valueBox(
      paste0(weather_data()$current$wind_speed, " km/h"),
      "Wind Speed",
      icon = icon("wind"),
      color = "teal"
    )
  })
  
  # Weather details
  output$cloudCover <- renderText({
    req(weather_data())
    paste0(weather_data()$current$cloud_cover, "%")
  })
  
  output$pressure <- renderText({
    req(weather_data())
    paste0(weather_data()$current$pressure, " hPa")
  })
  
  output$precipitation <- renderText({
    req(weather_data())
    paste0(weather_data()$current$precipitation, " mm")
  })
  
  output$uvIndex <- renderText({
    req(weather_data())
    weather_data()$current$uv_index
  })
  
  # Daily Average Temperature plot
  output$dailyAvgPlot <- renderPlot({
    req(weather_data())
    plot_daily_averages(weather_data()$daily)
  })

  # ====================================================================
  # TAB 2 OUTPUTS: Detailed Forecast
  # ====================================================================
  
  # Summary statistics table
  output$summaryStats <- renderTable({
    req(summary_stats())
    format_stats_table(summary_stats())
  })
  
  output$tempRangePlot <- renderPlot({
    req(weather_data())
    plot_daily_temp_range(weather_data()$daily)
  })
  
  output$tempMAPlot <- renderPlot({
    req(hourly_with_ma())
    plot_temp_with_ma(hourly_with_ma())
  })
  
  output$humidityPlot <- renderPlot({
    req(weather_data())
    plot_humidity(weather_data()$hourly)
  })
  
  output$precipPlot <- renderPlot({
    req(weather_data())
    plot_precipitation_forecast(weather_data()$hourly)
  })
  
  # ====================================================================
  # TAB 3 OUTPUTS: Historical Comparison
  # ====================================================================
  
  output$year1Title <- renderText({
    req(historical_data())
    paste(historical_data()$month_name, input$year1, "Statistics")
  })
  
  output$year2Title <- renderText({
    req(historical_data())
    paste(historical_data()$month_name, input$year2, "Statistics")
  })
  
  # Historical temperature comparison plot
  output$historicalTempPlot <- renderPlot({
    req(historical_data())
    
    # Combine data for plotting
    data1 <- historical_data()$year1_data
    data2 <- historical_data()$year2_data
    month_name <- historical_data()$month_name
    
    combined <- rbind(
      data.frame(day_num = data1$day_num, avg_temp = data1$avg_temp, 
                 temp_max = data1$temp_max, temp_min = data1$temp_min,
                 year = as.character(data1$year[1])),
      data.frame(day_num = data2$day_num, avg_temp = data2$avg_temp,
                 temp_max = data2$temp_max, temp_min = data2$temp_min,
                 year = as.character(data2$year[1]))
    )
    
    ggplot(combined, aes(x = day_num, color = year, fill = year)) +
      geom_line(aes(y = avg_temp), linewidth = 1.2) +
      geom_point(aes(y = avg_temp), size = 2, alpha = 0.7) +
      scale_color_manual(values = c("#E53935", "#1E88E5"), name = "Year") +
      scale_fill_manual(values = c("#E53935", "#1E88E5"), name = "Year") +
      labs(
        title = paste(month_name, "Temperature Comparison:", input$year1, "vs", input$year2),
        subtitle = historical_data()$location,
        x = paste("Month of", month_name),
        y = "Temperature (°C)"
      ) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12, color = "gray50")
      )
  })
  
  # Year 1 statistics table
  output$year1Stats <- renderTable({
    req(historical_data())
    data <- historical_data()$year1_data
    data.frame(
      Metric = c("Average Temp (°C)", "Max Temp (°C)", "Min Temp (°C)", 
                 "Total Precipitation (mm)", "Max Wind (km/h)"),
      Value = c(
        round(mean(data$avg_temp, na.rm = TRUE), 1),
        round(max(data$temp_max, na.rm = TRUE), 1),
        round(min(data$temp_min, na.rm = TRUE), 1),
        round(sum(data$precipitation_sum, na.rm = TRUE), 1),
        round(max(data$wind_speed_max, na.rm = TRUE), 1)
      )
    )
  })
  
  # Year 2 statistics table
  output$year2Stats <- renderTable({
    req(historical_data())
    data <- historical_data()$year2_data
    data.frame(
      Metric = c("Average Temp (°C)", "Max Temp (°C)", "Min Temp (°C)", 
                 "Total Precipitation (mm)", "Max Wind (km/h)"),
      Value = c(
        round(mean(data$avg_temp, na.rm = TRUE), 1),
        round(max(data$temp_max, na.rm = TRUE), 1),
        round(min(data$temp_min, na.rm = TRUE), 1),
        round(sum(data$precipitation_sum, na.rm = TRUE), 1),
        round(max(data$wind_speed_max, na.rm = TRUE), 1)
      )
    )
  })
  
  # Historical precipitation comparison
  output$historicalPrecipPlot <- renderPlot({
    req(historical_data())
    
    data1 <- historical_data()$year1_data
    data2 <- historical_data()$year2_data
    month_name <- historical_data()$month_name
    
    combined <- rbind(
      data.frame(day_num = data1$day_num, precip = data1$precipitation_sum, 
                 year = as.character(data1$year[1])),
      data.frame(day_num = data2$day_num, precip = data2$precipitation_sum,
                 year = as.character(data2$year[1]))
    )
    
    ggplot(combined, aes(x = day_num, y = precip, fill = year)) +
      geom_col(position = position_dodge(width = 0.8), width = 0.7, alpha = 0.8) +
      scale_fill_manual(values = c("#E53935", "#1E88E5"), name = "Year") +
      labs(
        title = paste(month_name, "Precipitation Comparison"),
        x = paste("Month of", month_name),
        y = "Precipitation (mm)"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
  })
}

# Run the application
shinyApp(ui = ui, server = server)