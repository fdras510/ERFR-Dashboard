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

#Interrogation site data
interrogation_df <- {
  tmp <- tempfile(fileext = ".csv")
  GET(
    'https://api.ptagis.org/reporting/reports/fglas25/file/erfr_interrogation_summary.csv',
    write_disk(tmp, overwrite = TRUE)
  )
  read_csv(tmp, locale = locale(encoding = "UTF-16")) %>%
    clean_names() %>%
    mutate(first_time = as.Date(mdy_hms(first_time))) %>%
    filter(first_time >= Sys.Date() - 30) %>%
    distinct(site, date = first_time, tag) %>%
    group_by(site, date) %>%
    summarise(total_tags = n(), .groups = "drop")
}

uptime_df <- {
  tmp <- tempfile(fileext = ".csv")
  GET(
    'https://api.ptagis.org/reporting/reports/fglas25/file/erfr_timer_tag.csv',
    write_disk(tmp, overwrite = TRUE)
  )
  read_csv(tmp, locale = locale(encoding = "UTF-16")) %>%
    clean_names() %>%
    rename(date = metrics) %>%
    mutate(
      date = as.Date(date, "%m/%d/%Y"),
      across(starts_with("timer_tag_count"), as.numeric)
    ) %>%
    group_by(site_code, antenna, date) %>%
    summarise(hourly_counts = sum(rowSums(across(starts_with("timer_tag_count")))), .groups = "drop") %>%
    mutate(prop_up = pmin(hourly_counts / 24, 1))
  #Assign sites, antennas, and antenna group
  antenna_map <- tribble(
    ~site_code, ~antenna, ~array,
    "CC5", "01", "Upstream Array",
    "CC5", "02", "Downstream Array",
    
    "CC4", "01", "Upstream Array",
    "CC4", "02", "Upstream Array",
    "CC4", "03", "Downstream Array",
    
    "CCW", "01", "Top Ladder Antenna",
    "CCW", "02", "Bottom Laddder Antenna",
    
    "CCU", "01", "Upstream Array",
    "CCU", "02", "Upstream Array",
    "CCU", "03", "Downstream Array",
    "CCU", "04", "Downstream Array",
    
    "UG4", "01", "Upstream Array",
    "UG4", "02", "Downstream Array",
    
    "UG3", "01", "Upstream Array",
    "UG3", "02", "Downstream Array",
    
    "UGS", "02", "Upstream Array",
    "UGS", "03", "Upstream Array",
    "UGS", "04", "Middle Array",
    "UGS", "05", "Middle Array",
    "UGS", "06", "Middle Array",
    "UGS", "07", "Downstream Array",
    "UGS", "08", "Downstream Array",
    
    "MDC", "01", "Upstream Array",
    "MDC", "02", "Middle Array",
    "MDC", "03", "Downstream Array",
    
    "IR4", "01", "Upstream Array",
    "IR4", "02", "Upstream Array",
    "IR4", "03", "Upstream Array",
    "IR4", "04", "Downstream Array",
    "IR4", "05", "Downstream Array",
    "IR4", "06", "Downstream Array",
    
    "IR5", "01", "Upstream Array",
    "IR5", "02", "Upstream Array",
    "IR5", "03", "Upstream Array",
    "IR5", "04", "Upstream Array",
    "IR5", "05", "Downstream Array",
    "IR5", "06", "Downstream Array",
    "IR5", "07", "Downstream Array",
    "IR5", "08", "Downstream Array",
    
    "WEN", "01", "Upstream Array",
    "WEN", "02", "Upstream Array",
    "WEN", "03", "Downstream Array",
    "WEN", "04", "Downstream Array"
  )
  #Join Timer tag data and antenna map
  timers3 <- timers3 %>%
    left_join(antenna_map, by = c("site_code","antenna")) %>%
    relocate(array, .after = antenna)
  uptime <- timers3 |>
    select(site_code,antenna,array,date,prop_up)
  uptime <- uptime |>
    mutate(prop_up = round(prop_up,2))
  uptime <- uptime |>
    pivot_wider(
      names_from = date,
      values_from = prop_up,
      values_fill = 0,
      names_expand = TRUE)
}


# Fetch OWRD text data using standard headers
get_owrd_data <- function(station_id) {
  base_url <- "https://apps.wrd.state.or.us/apps/sw/hydrographs/data.aspx"
  query <- list(station_nbr = station_id)  # remove format param to fetch raw text
  resp <- try(GET(base_url, query = query), silent = TRUE)
  if (inherits(resp, "try-error") || status_code(resp) != 200) return(NULL)
  
  # Read raw text and split into lines
  raw <- content(resp, as = "text", encoding = "UTF-8")
  lines <- strsplit(raw, "
?
")[[1]]
  # Identify lines starting with station number (digits), comprising the data rows including header
  data_start <- grep("^station_nbr", lines, ignore.case = TRUE)
  if (length(data_start) == 0) return(NULL)
  data_lines <- lines[data_start:length(lines)]
  
  # Read data into a data.frame
  df <- read.csv(text = data_lines, stringsAsFactors = FALSE)
  names(df) <- tolower(gsub("[° ]", "", names(df)))
  
  # Select and rename only relevant columns
  df <- df %>%
    select(station_nbr, record_date,
           flow = matches("instan.*flow_cfs", ignore.case = TRUE),
           temp = matches("instantaneous_water_temp_c", ignore.case = TRUE),
           stage = matches("instan.*stage_ft", ignore.case = TRUE),
           mean_flow = matches("mean_daily_flow_cfs", ignore.case = TRUE)) %>%
    rename(datetime = record_date)
  
  df$datetime <- as.POSIXct(df$datetime, format = "%Y-%m-%d %H:%M:%S")
  df <- df[order(df$datetime), ]
  return(df)
}

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
              plotOutput("plot_flow"),
              plotOutput("plot_temp"),
              plotOutput("plot_stage")
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
                  plotOutput("plot_detections")
              ),
              box(width = 12, title = "Daily Uptime Proportion", status = "primary",
                  "This table shows the proportion of each day that each antenna was operational.",
                  tableOutput("table_uptime")
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
  
  # Plot river
  output$plot_flow <- renderPlot({ df <- river_data(); if(is.null(df)) return(); ggplot(df,aes(datetime,flow))+geom_line()+labs(title="Flow (cfs)") })
  output$plot_temp <- renderPlot({ df <- river_data(); if(is.null(df)||!"temp"%in%names(df)) return(); ggplot(df,aes(datetime,temp))+geom_line()+labs(title="Water Temp (°C)") })
  output$plot_stage <- renderPlot({ df <- river_data(); if(is.null(df)||!"stage"%in%names(df)) return(); ggplot(df,aes(datetime,stage))+geom_line()+labs(title="Stage (ft)") })
  
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
  output$plot_detections <- renderPlot({
    ggplot(interrogation_df, aes(date, total_tags, fill = site)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~site, scales = "free_y") +
      labs(title = "Daily PIT Tag Detections", x = "Date", y = "Unique Tags") +
      theme_minimal()
  })
  
  # PIT Uptime table
  output$table_uptime <- renderTable({
    uptime_df %>% select(site_code, antenna, date, prop_up)
  }, rownames = FALSE)
}

shinyApp(ui, server)
