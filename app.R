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

GARDEN_NAME <- "Garden"
GARDEN_LAT  <- 35.611622
GARDEN_LON  <- -82.371369
GARDEN_TZ   <- "America/New_York"

NDFD_QPF_URL <- "https://tgftp.nws.noaa.gov/SL.us008001/ST.opnl/DF.gr2/DC.ndfd/AR.conus/VP.001-003/ds.qpf.bin"

RAINFALL_RADIUS_MILES <- 5
FORECAST_MAP_RADIUS_MULTIPLIER <- 2

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
  
  print(dat$SUMMARY)
  
  if (!is.null(dat$SUMMARY$RESPONSE_CODE) && dat$SUMMARY$RESPONSE_CODE != 1) {
    stop(dat$SUMMARY$RESPONSE_MESSAGE %||% "Synoptic API request failed.")
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

get_ndfd_qpf <- function() {
  
  tmp <- tempfile(fileext = ".bin")
  download.file(NDFD_QPF_URL, tmp, mode = "wb", quiet = TRUE)
  
  qpf <- terra::rast(tmp)
  
  garden_pt <- terra::vect(
    data.frame(
      lon = GARDEN_LON,
      lat = GARDEN_LAT
    ),
    geom = c("lon", "lat"),
    crs = "EPSG:4326"
  )
  
  vals <- terra::extract(qpf, garden_pt)
  qpf_values <- as.numeric(vals[1, -1])
  
  valid_times <- terra::time(qpf)
  
  if (is.null(valid_times) || all(is.na(valid_times))) {
    valid_times <- seq(
      from = floor_date(with_tz(Sys.time(), "UTC"), "6 hours"),
      by = "6 hours",
      length.out = terra::nlyr(qpf)
    )
  }
  
  qpf_table <- tibble(
    period = seq_along(qpf_values),
    slider_value = as.character(seq_along(qpf_values)),
    valid_time_utc = valid_times,
    valid_time_local = with_tz(valid_times, GARDEN_TZ),
    rainfall_in = qpf_values,
    cumulative_rainfall_in = cumsum(replace_na(qpf_values, 0))
  )
  
  list(
    qpf = qpf,
    table = qpf_table,
    valid_times = valid_times
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  
  titlePanel("Garden Rainfall"),
  
  tabsetPanel(
    id = "main_tabs",
    
    tabPanel(
      "Measured Rainfall",
      
      br(),
      
      fluidRow(
        column(3, div(style = card_style, h5("Max Rainfall"), h3(textOutput("max_rain", inline = TRUE)))),
        column(3, div(style = card_style, h5("Nearest Station"), h4(textOutput("nearest_station", inline = TRUE)))),
        column(3, div(style = card_style, h5("Stations"), h3(textOutput("station_count", inline = TRUE)))),
        column(3, div(style = card_style, h5("Updated"), h4(textOutput("last_updated", inline = TRUE))))
      ),
      
      br(),
      
      fluidRow(
        column(8, leafletOutput("measured_map", height = 500)),
        
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
      
      leafletOutput("forecast_map", height = 600),
      
      br(),
      
      sliderTextInput(
        inputId = "forecast_period",
        label = "Forecast Period",
        choices = "Loading...",
        selected = "Loading...",
        animate = animationOptions(
          interval = 1200,
          loop = TRUE
        )
      ),
      
      textOutput("forecast_time_label")
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
    if (nrow(df) == 0 || all(is.na(df$rainfall_in))) return("NA")
    paste0(round(max(df$rainfall_in, na.rm = TRUE), 2), " in")
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
    format(with_tz(Sys.time(), GARDEN_TZ), "%H:%M")
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
        label = GARDEN_NAME,
        popup = paste0(
          "<strong>", GARDEN_NAME, "</strong><br>",
          "Lat: ", GARDEN_LAT, "<br>",
          "Lon: ", GARDEN_LON
        )
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
          label = ~paste0(
            station, ": ",
            ifelse(is.na(rainfall_in), "No rainfall value", paste0(round(rainfall_in, 2), " in"))
          ),
          popup = ~paste0(
            "<strong>", station, "</strong><br>",
            "Station: ", stid, "<br>",
            "Distance: ", round(distance_mi, 1), " mi<br>",
            window_label, " rainfall: ",
            ifelse(is.na(rainfall_in), "No value", paste0(round(rainfall_in, 2), " in"))
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
    rain_col_name <- paste0(selected_window_label(), " Rainfall (in)")
    
    if (nrow(df) == 0) {
      return(datatable(
        data.frame(Message = "No rainfall stations returned."),
        rownames = FALSE,
        options = list(dom = "t")
      ))
    }
    
    out <- df |>
      transmute(
        Station = station,
        ID = stid,
        `Distance (mi)` = round(distance_mi, 1),
        Rainfall = round(rainfall_in, 2),
        Latitude = round(lat, 4),
        Longitude = round(lon, 4)
      )
    
    names(out)[names(out) == "Rainfall"] <- rain_col_name
    
    datatable(
      out,
      rownames = FALSE,
      options = list(pageLength = 10)
    )
  })
  
  forecast_data <- reactive({
    req(input$main_tabs == "Forecast Rainfall")
    get_ndfd_qpf()
  })
  
  observeEvent(forecast_data(), {
    
    df <- forecast_data()$table
    
    slider_choices <- setNames(
      df$slider_value,
      format(df$valid_time_local, "%b %d %H:%M")
    )
    
    updateSliderTextInput(
      session,
      "forecast_period",
      choices = slider_choices,
      selected = df$slider_value[1]
    )
  })
  
  output$forecast_time_label <- renderText({
    
    req(input$forecast_period)
    req(input$forecast_period != "Loading...")
    
    df <- forecast_data()$table
    row <- df[df$slider_value == input$forecast_period, ]
    
    paste0(
      "Forecast valid: ",
      format(row$valid_time_local, "%b %d %H:%M"),
      " | 6-hr rainfall at garden: ",
      round(row$rainfall_in, 2),
      " in | Cumulative: ",
      round(row$cumulative_rainfall_in, 2),
      " in"
    )
  })
  
  output$forecast_map <- renderLeaflet({
    
    req(input$forecast_period)
    req(input$forecast_period != "Loading...")
    
    dat <- forecast_data()
    qpf <- dat$qpf
    df <- dat$table
    
    lyr <- as.numeric(input$forecast_period)
    qpf_layer <- qpf[[lyr]]
    
    row <- df[df$slider_value == input$forecast_period, ]
    
    garden_pt <- terra::vect(
      data.frame(
        lon = GARDEN_LON,
        lat = GARDEN_LAT
      ),
      geom = c("lon", "lat"),
      crs = "EPSG:4326"
    )
    
    garden_buffer <- terra::buffer(
      terra::project(garden_pt, "EPSG:3857"),
      width = RAINFALL_RADIUS_MILES * FORECAST_MAP_RADIUS_MULTIPLIER * 1609.34
    ) |>
      terra::project(terra::crs(qpf_layer))
    
    qpf_crop <- terra::crop(qpf_layer, garden_buffer)
    qpf_mask <- terra::mask(qpf_crop, garden_buffer)
    
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
        qpf_mask,
        colors = rain_pal,
        opacity = 0.7,
        project = TRUE
      ) |>
      addAwesomeMarkers(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        icon = garden_icon,
        label = GARDEN_NAME,
        popup = paste0(
          "<strong>", GARDEN_NAME, "</strong><br>",
          "6-hr QPF: ", round(row$rainfall_in, 2), " in<br>",
          "Cumulative: ", round(row$cumulative_rainfall_in, 2), " in"
        )
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
        title = "6-hr QPF",
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