# Load libraries
library(shiny)
library(shinydashboard)
library(plotly)
library(leaflet)
library(jsonlite)
library(readr)
library(dplyr)
library(httr)
library(ggplot2)
library(janitor)
library(lubridate)
library(viridis)
library(viridisLite)

# pat<-Sys.getenv("GITHUB_PAT", "")
# if (identical(pat,"")) {
#   stop("Please set your GITHUB PAT in the environmental variable.")
# }
# 
# auth_readRDS <- function(raw_url) {
#   res <- GET(
#     raw_url, 
#     add_headers(Authorization = paste("token",pat))
#   )
#   if (res$status_code !=200) {
#     stop("Failed to GET", raw_url, ":HTTP", res$status_code)
#   }
#   readRDS(rawConnection(content(res, "raw")))
# }

# Master location list with both weather and river metadata
location_meta <- list(
  "Grande Ronde (Clear Creek)" = list(
    lat = 45.186076, lon = -118.390811, station = "13317850"
  ),
  "Catherine Creek near Union" = list(
    lat = 45.190765, lon = -117.828088, station = "13320000"
  ),
  "Lostine River (Baker Rd)" = list(
    lat = 45.531282, lon = -117.468560, station = "13330300"
  ),
  "Meadow Creek (below Dark Canyon)" = list(
    lat = 45.265651, lon = -118.521599, station = "13318210"
  ),
  "Minam" = list(
    lat = 45.622032, lon = -117.721522, station = "13331500"
  )
)
#Open-Meteo weather code descriptions
weather_code_desc <- function(code) {
  codes <- c(
    "0" = "Clear sky", "1" = "Mainly clear", "2" = "Partly cloudy", "3" = "Overcast",
    "45" = "Fog", "48" = "Depositing rime fog",
    "51" = "Light drizzle", "53" = "Moderate drizzle", "55" = "Dense drizzle",
    "56" = "Light freezing drizzle", "57" = "Dense freezing drizzle",
    "61" = "Slight rain", "63" = "Moderate rain", "65" = "Heavy rain",
    "66" = "Light freezing rain", "67" = "Heavy freezing rain",
    "71" = "Slight snow fall", "73" = "Moderate snow fall", "75" = "Heavy snow fall",
    "77" = "Snow grains",
    "80" = "Slight rain showers", "81" = "Moderate rain showers", "82" = "Violent rain showers",
    "85" = "Slight snow showers", "86" = "Heavy snow showers",
    "95" = "Thunderstorm", "96" = "Thunderstorm with slight hail", "99" = "Thunderstorm with heavy hail"
  )
  return(codes[as.character(code)])
}

# Helper to fetch forecast from NWS API with error handling
get_openmeteo <- function(lat, lon) {
  url <- sprintf(
    paste0(
      "https://api.open-meteo.com/v1/forecast?latitude=%.4f&longitude=%.4f",
      "&daily=temperature_2m_max,temperature_2m_min,precipitation_probability_max,weathercode,wind_speed_10m_max",
      "&past_days=3&current_weather=true&timezone=auto"
    ), lat, lon
  )
  resp <- GET(url)
  if (status_code(resp) != 200) return(NULL)
  dat <- fromJSON(content(resp, "text", encoding = "UTF-8"), flatten = TRUE)
  # Convert Celsius to Fahrenheit and km/h to mph, then round
  c_to_f     <- function(c) round(c * 9/5 + 32, 1)
  kmh_to_mph <- function(kmh) round(kmh * 0.621371, 1)
  
  daily <- data.frame(
    date = as.Date(dat$daily$time),
    tmax = c_to_f(dat$daily$temperature_2m_max),
    tmin = c_to_f(dat$daily$temperature_2m_min),
    wind = kmh_to_mph(dat$daily$wind_speed_10m_max),
    precip = dat$daily$precipitation_probability_max,
    wcode = dat$daily$weathercode,
    stringsAsFactors = FALSE
  )
  current <- data.frame(
    date = Sys.Date(),
    current_temp = c_to_f(dat$current_weather$temperature),
    current_wind = kmh_to_mph(dat$current_weather$windspeed),
    current_wcode = dat$current_weather$weathercode,
    current_wdesc = weather_code_desc(dat$current_weather$weathercode),
    stringsAsFactors = FALSE
  )
  list(daily = daily, current = current)
}

#Pit Antenna site data
antenna_plot <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/antenna_plot.rds"))
timer_tags <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/uptime_data.rds"))

# Fetch OWRD text data using standard headers
los_baker_flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/los_baker_rd_flow.rds")) %>%
  mutate(Station = "Lostine_Baker_Rd")
#los_ranch_flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/los_ranch_flow.rds")) %>%
#  mutate(Station = "Lostine_Ranch")
#lostine_flow = bind_rows(los_baker_flow, los_ranch_flow)
lostine_temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/los_ranch_temp.rds"))
minam_flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/minam_flow.rds"))
minam_temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/minam_temp.rds"))
catherine_flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/cc_flow.rds"))
catherine_temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/cc_temp.rds"))
meadow_flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/mdc_flow.rds"))
meadow_temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/mdc_temp.rds"))
ugr_flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/ugr_flow.rds"))
ugr_temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/ugr_temp.rds"))

 # river_data_list <- list(
 #   "Lostine River" = list(flow = bind_rows(los_baker_flow, los_ranch_flow),
 #                          temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/los_ranch_temp.rds"))),
 #   "Minam River" = list(flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/minam_flow.rds")),
 #                        temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/minam_temp.rds"))),
 #   "Catherine Creek" = list(flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/cc_flow.rds")),
 #                            temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/cc_temp.rds"))),
 #   "Meadow Creek" = list(flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/mdc_flow.rds")),
 #                         temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/mdc_temp.rds"))),
 #   "Upper Grande Ronde River" = list(flow <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/ugr_flow.rds")),
 #                                     temp <- readRDS(url("https://raw.githubusercontent.com/fdras510/ERFR-Dashboard/main/data/ugr_temp.rds")))
 # )


# UI definition
ui <- dashboardPage(
  dashboardHeader(title = "Fisheries Dashboard"),
  dashboardSidebar(
    selectInput("location", "Select Location:", choices = names(location_meta), selected = names(location_meta)[1]),
    sidebarMenu(
      menuItem("Dashboard",        tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("River Data",       tabName = "river",     icon = icon("tint")),
      menuItem("Weather",          tabName = "weather",   icon = icon("cloud-sun")),
      menuItem("Screw Trap",       tabName = "screwtrap", icon = icon("fish")),
      menuItem("Meadow Creek",     tabName = "meadow",    icon = icon("leaf")),
      menuItem("PIT Antennas",     tabName = "pit",       icon = icon("rss"))
    )
  ),
  dashboardBody(
    tabItems(
      # Dashboard Overview
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("vb_flow"),
                valueBoxOutput("vb_temp"),
                valueBoxOutput("vb_current_weather"),
              )
      ),
      tabItem(tabName = "river",
              fluidRow(
                # Lostine River
                box(
                  title = "Lostine River",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("Lostine_River", height = "250px"),
                  plotOutput("Lostine_Temperature", height = "250px")
                ),
                # Grande Ronde River
                box(
                  title = "Grande Ronde River",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12, 
                  plotOutput("Grande_Ronde_flow", height = "250px"),
                  plotOutput("Grande_Ronde_Temperature", height = "250px")
                ),
                # Meadow Creek
                box(
                  title = "Meadow Creek",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("Meadow_Creek_flow", height = "250px"),
                  plotOutput("Meadow_Creek_Temperature", height = "250px")
                ),
                # Catherine Creek
                box(
                  title = "Catherine Creek",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("Catherine_Creek_flow", height = "250px"),
                  plotOutput("Catherine_Creek_Temperature", height = "250px")
                ),
                # Minam
                box(
                  title = "Minam River",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("Minam_flow", height = "300px"),
                  plotOutput("Minam_Temperature", height = "300px")
                )
              )
      ),
      tabItem(tabName = "weather",
              fluidRow(
                box(width=12, title = "7-Day Forecast (°F)", status = "info", plotOutput("plot_weather")),
                box(width=12, title = "Max Wind Speed (km/h)", status = "info", plotOutput("plot_wind")),
                box(width=12, title = "Precipitation Probability (%)", status = "info", plotOutput("plot_precip"))
              )
      ),
      # Placeholders
      tabItem(tabName = "screwtrap", h3("Screw Trap Data Coming Soon")),
      tabItem(tabName = "meadow",    h3("Juvenile Sampling Coming Soon")),
      tabItem(tabName = "pit",
              box(width = 12, title = "Daily Detections (Last 30 Days)", status = "primary",
                  "This chart shows the number of unique tags detected per site each day.",
                  plotlyOutput("antenna_plotly", height = "900px")
              ),
              box(width = 12, title = "Daily Uptime Proportion", status = "primary",
                  "This table shows the proportion of each day that each antenna was operational(1.00 represents 100% uptime). ",
                  tableOutput("uptime_table")
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  meta <- reactive(location_meta[[input$location]])
  river_data <- reactive(get_owrd_data(meta()$station))
  weather_all <- reactive(get_openmeteo(meta()$lat, meta()$lon))
  
  # Dashboard river boxes
  output$vb_flow <- renderValueBox({
    df <- river_data(); if (is.null(df)) return(valueBox("N/A","Flow",icon=icon("tint"),color="blue"))
    valueBox(paste0(tail(df$flow,1)," cfs"),"Flow",icon=icon("tint"),color="blue")
  })
  output$vb_temp <- renderValueBox({
    df <- river_data(); if (is.null(df)||!"temp"%in%names(df)) return(valueBox("N/A","Water Temp",icon=icon("thermometer-half"),color="teal"))
    valueBox(paste0(tail(df$temp,1)," °C"),"Water Temp",icon=icon("thermometer-half"),color="teal")
  })
  
  # Dashboard weather box (current)
  output$vb_current_weather <- renderValueBox({
    wa <- weather_all()
    if (is.null(wa)) {
      return(valueBox(
        HTML("N/A<br/>N/A km/h<br/>No data"),
        "Current Weather",
        icon = icon("cloud"),
        color = "yellow"
      ))
    }
    tmp  <- wa$current$current_temp
    wnd  <- wa$current$current_wind
    desc <- wa$current$current_wdesc
    
    label_html <- HTML(paste0(
      tmp, " °F", "<br/>",
      wnd, "mph", "<br/>",
      desc
    ))
    
    valueBox(
      label_html,
      "Current Weather",
      icon = icon("cloud"),
      color = "yellow"
    )
  })
  
  # Plot river data
  output$Lostine_River <- renderPlot({
    ggplot(los_baker_flow, aes(x=Date,y=CFS, color = Station)) +
      geom_line(color = "blue") + labs(title = "Lostine River at Baker Road", y = "CFS")
    })
  output$Lostine_Temperature <-renderPlot({
    ggplot(lostine_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
      labs(title = "Lostine River Temperature", y = "Temperature (°C)") +
      theme_minimal()
  })
  output$Grande_Ronde_flow <- renderPlot({
    ggplot(ugr_flow, aes(x=Date,y=CFS)) +
    geom_line(color = "blue") + labs(title = "Grande Ronde River at Clear Creek Flow", y = "CFS")
  })
  output$Grande_Ronde_Temperature <- renderPlot({
    ggplot(ugr_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
      labs(title = "Grande Ronde River at Clear Creek Temperature", y = "Temperature (°C)") +
      theme_minimal()
  })
  output$Meadow_Creek_flow <- renderPlot({
    ggplot(meadow_flow, aes(x=Date,y=CFS)) +
      geom_line(color = "blue") + labs(title = "Meadow Creek below Dark Canyon Flow", y = "CFS")
  })
  output$Meadow_Creek_Temperature <- renderPlot({
    ggplot(meadow_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
      labs(title = "Meadow Creek below Dark Canyon Temperature", y = "Temperature (°C)") +
    theme_minimal()
  })
  output$Catherine_Creek_flow <- renderPlot({
    ggplot(catherine_flow, aes(x=Date,y=CFS)) +
      geom_line(color = "blue") + labs(title = "Catherine Creek near Union Flow", y = "CFS")
  })
  output$Catherine_Creek_Temperature <- renderPlot({
    ggplot(catherine_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
      labs(title = "Catherine Creek near Union Temperature", y = "Temperature (°C)") +
    theme_minimal()
  })
  output$Minam_flow <- renderPlot({
    ggplot(minam_flow, aes(x=Date,y=CFS)) +
      geom_line(color = "blue") + labs(title = "Minam River Flow", y = "CFS")
  })
  output$Minam_Temperature <- renderPlot({
    ggplot(minam_temp, aes(x=Date, y=Temp_C, color = Temp_C)) +
      geom_line(linewidth = 1) +
      scale_color_viridis_c(option = "H", name = "Temperature (°C)") +
      labs(title = "Minam River Temperature", y = "Temperature (°C)") +
    theme_minimal()
  })
  
  # Plot weather forecast
  output$plot_weather <- renderPlot({
    wa <- weather_all(); if(is.null(wa)) return()
    df <- wa$daily
    ggplot(df, aes(date)) +
      geom_line(aes(y = tmax), linetype = "dashed", color = "red") +
      geom_line(aes(y = tmin), color = "blue") +
      labs(title = "7-Day Forecast", y = "Temp (°F)")
  })
  output$plot_wind <- renderPlot({
    wa <- weather_all(); if(is.null(wa)) return()
    df <- wa$daily
    ggplot(df, aes(date, wind)) + geom_line(color = "darkgreen") + labs(y = "Wind Speed (mph)")
  })
  output$plot_precip <- renderPlot({
    wa <- weather_all(); if(is.null(wa)) return()
    df <- wa$daily
    ggplot(df, aes(date, precip)) + geom_col(fill = "skyblue") + labs(y = "Precip Probability (%)")
  })
  # PIT Detections plot
   output$antenna_plotly <- renderPlotly({
     p <- ggplot(antenna_plot,aes(x = date, y = total_tags, fill = species)) +
       geom_bar(position = "stack", stat = "identity") + 
       labs(x = "Date", y = "Total tags", fill = "Species") +
       scale_fill_viridis_d(direction = -1) +
       facet_wrap(~site)
     ggplotly(p)
   })

  # PIT Uptime table
   output$uptime_table <- renderTable({
     timer_tags}, rownames = FALSE)
 }

shinyApp(ui, server)

