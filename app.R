library(shiny)
library(shinyWidgets)
library(leaflet)
library(DT)
library(httr2)
library(tidyverse)
library(lubridate)
library(terra)

# -----------------------------
# Settings
# -----------------------------

SYNOPTIC_TOKEN <- Sys.getenv("SYNOPTIC_TOKEN")
source("scripts/update_forecast_cache.R")

# -----------------------------
# Load cached forecast
# -----------------------------

forecast_cache <- readRDS("cache/forecast_cache.rds")
forecast_qpf <- terra::rast(forecast_cache$qpf_file)

GARDEN_NAME <- forecast_cache$garden$name
GARDEN_LAT  <- forecast_cache$garden$lat
GARDEN_LON  <- forecast_cache$garden$lon
GARDEN_TZ   <- forecast_cache$garden$timezone

RAINFALL_RADIUS_MILES <- forecast_cache$rainfall_radius_miles

# -----------------------------
# Icons and colors
# -----------------------------

garden_icon <- awesomeIcons(
  icon = "heart",
  iconColor = "white",
  library = "fa",
  markerColor = "red"
)

rain_bins <- c(-Inf, 0, 0.01, 0.10, 0.25, 0.50, 1, Inf)

rain_colors <- c(
  "#e0e0e0",
  "#deebf7",
  "#9ecae1",
  "#6baed6",
  "#3182bd",
  "#08519c",
  "#08306b"
)

rain_labels <- c(
  "0 in",
  ">0–0.01 in",
  "0.01–0.10 in",
  "0.10–0.25 in",
  "0.25–0.50 in",
  "0.50–1.00 in",
  ">1.00 in"
)

forecast_choices <- forecast_cache$table$slider_label

card_style <- "
  background:#f8f9fa;
  padding:15px;
  border-radius:10px;
  text-align:center;
  min-height:100px;
  box-shadow:0 1px 3px rgba(0,0,0,0.12);
"

# -----------------------------
# Helpers
# -----------------------------

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

format_rain_window <- function(hours) {
  if (hours == 1) "1-hr" else paste0(hours, "-hr")
}

get_precip_value <- function(obs) {
  
  if (is.null(obs)) return(NA_real_)
  
  flat <- unlist(obs, recursive = TRUE, use.names = TRUE)
  
  if (length(flat) == 0) return(NA_real_)
  
  precip_candidates <- flat[
    grepl("precip|rain|total", names(flat), ignore.case = TRUE)
  ]
  
  nums <- suppressWarnings(as.numeric(precip_candidates))
  nums <- nums[!is.na(nums)]
  
  if (length(nums) == 0) return(NA_real_)
  
  nums[1]
}

get_garden_rainfall <- function(hours = 24) {
  
  if (SYNOPTIC_TOKEN == "") {
    stop("SYNOPTIC_TOKEN is empty. Check .Renviron and restart RStudio.")
  }
  
  end_time <- with_tz(Sys.time(), "UTC")
  start_time <- end_time - lubridate::hours(hours)
  
  start_txt <- format(start_time, "%Y%m%d%H%M")
  end_txt   <- format(end_time, "%Y%m%d%H%M")
  
  resp <- request("https://api.synopticdata.com/v2/stations/precip") |>
    req_url_query(
      token = SYNOPTIC_TOKEN,
      radius = paste(GARDEN_LAT, GARDEN_LON, RAINFALL_RADIUS_MILES, sep = ","),
      start = start_txt,
      end = end_txt,
      pmode = "totals",
      units = "precip|in",
      output = "json"
    ) |>
    req_perform()
  
  dat <- resp_body_json(resp, simplifyVector = FALSE)
  
  if (!is.null(dat$SUMMARY$RESPONSE_CODE) &&
      dat$SUMMARY$RESPONSE_CODE != 1) {
    stop(dat$SUMMARY$RESPONSE_MESSAGE %||%
           "Synoptic API request failed.")
  }
  
  if (is.null(dat$STATION) || length(dat$STATION) == 0) {
    return(tibble())
  }
  
  purrr::map_dfr(dat$STATION, function(x) {
    
    tibble(
      station = x$NAME %||% NA_character_,
      stid = x$STID %||% NA_character_,
      lat = suppressWarnings(as.numeric(x$LATITUDE %||% NA_real_)),
      lon = suppressWarnings(as.numeric(x$LONGITUDE %||% NA_real_)),
      distance_mi = suppressWarnings(as.numeric(x$DISTANCE %||% NA_real_)),
      rainfall_in = get_precip_value(x$OBSERVATIONS)
    )
  }) |>
    arrange(distance_mi)
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Garden Rainfall"),
  
  tabsetPanel(
    
    tabPanel(
      "Measured Rainfall",
      
      br(),
      
      fluidRow(
        column(
          3,
          div(
            style = card_style,
            h5("Max Rainfall"),
            h3(textOutput("max_rain", inline = TRUE))
          )
        ),
        
        column(
          3,
          div(
            style = card_style,
            h5("Nearest Station"),
            h4(textOutput("nearest_station", inline = TRUE))
          )
        ),
        
        column(
          3,
          div(
            style = card_style,
            h5("Stations"),
            h3(textOutput("station_count", inline = TRUE))
          )
        ),
        
        column(
          3,
          div(
            style = card_style,
            h5("Updated"),
            h4(textOutput("last_updated", inline = TRUE))
          )
        )
      ),
      
      br(),
      
      fluidRow(
        column(
          8,
          leafletOutput("measured_map", height = 500)
        ),
        
        column(
          4,
          
          h4("Garden Summary"),
          
          selectInput(
            "rain_window",
            "Rainfall Period",
            choices = c(
              "1 hour" = 1,
              "3 hours" = 3,
              "6 hours" = 6,
              "12 hours" = 12,
              "24 hours" = 24,
              "48 hours" = 48,
              "72 hours" = 72
            ),
            selected = 24
          ),
          
          tableOutput("garden_summary")
        )
      ),
      
      br(),
      
      DTOutput("station_table")
    ),
    
    tabPanel(
      "Forecast Rainfall",
      
      br(),
      
      h4("NDFD Forecast Rainfall"),
      
      div(
        style = "
          background:#f8f9fa;
          padding:15px;
          border-radius:10px;
          margin-bottom:12px;
          box-shadow:0 1px 3px rgba(0,0,0,0.12);
        ",
        
        div(
          style = "
            text-align:center;
            font-size:18px;
            font-weight:600;
            margin-bottom:10px;
          ",
          textOutput("forecast_time_label")
        ),
        
        fluidRow(
          
          column(
            2,
            actionButton(
              "forecast_prev",
              "\u25C0 Previous",
              width = "100%"
            )
          ),
          
          column(
            8,
            sliderTextInput(
              inputId = "forecast_period",
              label = NULL,
              choices = forecast_choices,
              selected = forecast_choices[1],
              grid = FALSE,
              width = "100%"
            )
          ),
          
          column(
            2,
            actionButton(
              "forecast_next",
              "Next \u25B6",
              width = "100%"
            )
          )
        )
      ),
      
      leafletOutput("forecast_map", height = 650)
    )
  )
)

# -----------------------------
# Server
# -----------------------------

server <- function(input, output, session) {
  
  selected_hours <- reactive({
    as.numeric(input$rain_window)
  })
  
  selected_window_label <- reactive({
    format_rain_window(selected_hours())
  })
  
  rainfall_data <- reactive({
    get_garden_rainfall(hours = selected_hours())
  })
  
  output$max_rain <- renderText({
    
    df <- rainfall_data()
    
    if (nrow(df) == 0 ||
        all(is.na(df$rainfall_in))) {
      return("NA")
    }
    
    paste0(
      round(max(df$rainfall_in, na.rm = TRUE), 2),
      " in"
    )
  })
  
  output$nearest_station <- renderText({
    
    df <- rainfall_data()
    
    if (nrow(df) == 0) return("None")
    
    df$station[1]
  })
  
  output$station_count <- renderText({
    nrow(rainfall_data())
  })
  
  output$last_updated <- renderText({
    format(
      with_tz(Sys.time(), GARDEN_TZ),
      "%H:%M"
    )
  })
  
  output$measured_map <- renderLeaflet({
    
    df <- rainfall_data()
    window_label <- selected_window_label()
    
    pal <- colorBin(
      palette = rain_colors,
      bins = rain_bins,
      domain = rain_bins,
      na.color = "#000000",
      right = TRUE
    )
    
    m <- leaflet() |>
      
      addProviderTiles(providers$CartoDB.Voyager) |>
      
      addAwesomeMarkers(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        icon = garden_icon,
        label = GARDEN_NAME
      ) |>
      
      addCircles(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        radius = RAINFALL_RADIUS_MILES * 1609.34,
        color = "forestgreen",
        fillOpacity = 0,
        weight = 2,
        dashArray = "5,5"
      ) |>
      
      addLegend(
        position = "bottomright",
        colors = c(rain_colors, "#000000"),
        labels = c(rain_labels, "No value"),
        title = paste(window_label, "rainfall"),
        opacity = 0.8
      ) |>
      
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 12
      )
    
    if (nrow(df) > 0) {
      
      m <- m |>
        
        addCircleMarkers(
          data = df,
          lng = ~lon,
          lat = ~lat,
          radius = 7,
          color = "black",
          weight = 1,
          
          fillColor = ~ifelse(
            is.na(rainfall_in),
            "#000000",
            pal(rainfall_in)
          ),
          
          fillOpacity = 0.85,
          
          popup = ~paste0(
            "<strong>", station, "</strong><br>",
            "Rainfall: ",
            round(rainfall_in, 2),
            " in"
          )
        )
    }
    
    m
  })
  
  output$garden_summary <- renderTable({
    
    data.frame(
      Metric = c(
        "Garden",
        "Latitude",
        "Longitude",
        "Search Radius",
        "Rainfall Period"
      ),
      
      Value = c(
        GARDEN_NAME,
        GARDEN_LAT,
        GARDEN_LON,
        paste(RAINFALL_RADIUS_MILES, "miles"),
        paste(selected_hours(), "hours")
      )
    )
  })
  
  output$station_table <- renderDT({
    
    df <- rainfall_data()
    
    rain_col_name <- paste0(
      selected_window_label(),
      " Rainfall (in)"
    )
    
    if (nrow(df) == 0) {
      
      return(
        datatable(
          data.frame(
            Message = "No rainfall stations returned."
          ),
          rownames = FALSE,
          options = list(dom = "t")
        )
      )
    }
    
    out <- df |>
      
      transmute(
        Station = station,
        ID = stid,
        `Distance (mi)` = round(distance_mi, 1),
        Rainfall = round(rainfall_in, 2)
      )
    
    names(out)[names(out) == "Rainfall"] <- rain_col_name
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
  
  observeEvent(input$forecast_prev, {
    
    current_index <- match(
      input$forecast_period,
      forecast_choices
    )
    
    new_index <- max(1, current_index - 1)
    
    updateSliderTextInput(
      session,
      "forecast_period",
      selected = forecast_choices[new_index]
    )
  })
  
  observeEvent(input$forecast_next, {
    
    current_index <- match(
      input$forecast_period,
      forecast_choices
    )
    
    new_index <- min(
      length(forecast_choices),
      current_index + 1
    )
    
    updateSliderTextInput(
      session,
      "forecast_period",
      selected = forecast_choices[new_index]
    )
  })
  
  selected_forecast_row <- reactive({
    
    req(input$forecast_period)
    
    forecast_cache$table[
      forecast_cache$table$slider_label ==
        input$forecast_period,
    ]
  })
  
  output$forecast_time_label <- renderText({
    
    row <- selected_forecast_row()
    
    paste0(
      "Forecast time: ",
      row$slider_label,
      "   |   Forecast rainfall: ",
      round(row$rainfall_in, 2),
      " in   |   Cumulative: ",
      round(row$cumulative_rainfall_in, 2),
      " in"
    )
  })
  
  output$forecast_map <- renderLeaflet({
    
    row <- selected_forecast_row()
    
    lyr <- row$layer
    
    qpf_layer <- forecast_qpf[[lyr]]
    
    rain_pal <- colorBin(
      palette = rain_colors,
      bins = rain_bins,
      domain = rain_bins,
      na.color = "transparent",
      right = TRUE
    )
    
    leaflet() |>
      
      addProviderTiles(providers$CartoDB.Voyager) |>
      
      addRasterImage(
        qpf_layer,
        colors = rain_pal,
        opacity = 0.7,
        project = FALSE
      ) |>
      
      addAwesomeMarkers(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        icon = garden_icon,
        label = GARDEN_NAME
      ) |>
      
      addCircles(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        radius = RAINFALL_RADIUS_MILES * 1609.34,
        color = "forestgreen",
        fillOpacity = 0,
        weight = 2,
        dashArray = "5,5"
      ) |>
      
      addLegend(
        position = "bottomright",
        colors = rain_colors,
        labels = rain_labels,
        title = "6-hr Forecast Rainfall",
        opacity = 0.8
      ) |>
      
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 11
      )
  })
}

shinyApp(ui, server)