library(shiny)
library(shinyWidgets)
library(leaflet)
library(httr2)
library(tidyverse)
library(lubridate)
library(terra)

# -----------------------------
# Settings
# -----------------------------

if (file.exists(".secrets/synoptic_token.R")) {
  source(".secrets/synoptic_token.R")
}

SYNOPTIC_TOKEN <- Sys.getenv("SYNOPTIC_TOKEN")

# -----------------------------
# Remote forecast cache
# -----------------------------

GITHUB_OWNER <- "jeremyash"
GITHUB_REPO  <- "garden_rainfall"
CACHE_BRANCH <- "cache-data"

REMOTE_CACHE_RDS <- paste0(
  "https://raw.githubusercontent.com/",
  GITHUB_OWNER, "/", GITHUB_REPO, "/",
  CACHE_BRANCH, "/cache/forecast_cache.rds"
)

REMOTE_CACHE_TIF <- paste0(
  "https://raw.githubusercontent.com/",
  GITHUB_OWNER, "/", GITHUB_REPO, "/",
  CACHE_BRANCH, "/cache/forecast_qpf_ll.tif"
)

download_remote_cache <- function() {
  
  rds_tmp <- tempfile(fileext = ".rds")
  tif_tmp <- tempfile(fileext = ".tif")
  
  download.file(REMOTE_CACHE_RDS, rds_tmp, mode = "wb", quiet = TRUE)
  download.file(REMOTE_CACHE_TIF, tif_tmp, mode = "wb", quiet = TRUE)
  
  list(
    cache = readRDS(rds_tmp),
    qpf = terra::rast(tif_tmp)
  )
}

remote_forecast <- download_remote_cache()

forecast_cache <- remote_forecast$cache
forecast_qpf <- remote_forecast$qpf

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
  
  token <- Sys.getenv("SYNOPTIC_TOKEN")
  
  if (token == "") {
    warning("SYNOPTIC_TOKEN is empty. Measured rainfall will not load.")
    return(tibble())
  }
  
  end_time <- with_tz(Sys.time(), "UTC")
  start_time <- end_time - lubridate::hours(hours)
  
  dat <- request("https://api.synopticdata.com/v2/stations/precip") |>
    req_url_query(
      token = token,
      radius = paste(GARDEN_LAT, GARDEN_LON, RAINFALL_RADIUS_MILES, sep = ","),
      start = format(start_time, "%Y%m%d%H%M"),
      end = format(end_time, "%Y%m%d%H%M"),
      pmode = "totals",
      units = "precip|in",
      output = "json"
    ) |>
    req_perform() |>
    resp_body_json(simplifyVector = FALSE)
  
  if (!is.null(dat$SUMMARY$RESPONSE_CODE) &&
      dat$SUMMARY$RESPONSE_CODE != 1) {
    warning(dat$SUMMARY$RESPONSE_MESSAGE %||% "Synoptic API request failed.")
    return(tibble())
  }
  
  if (is.null(dat$STATION) || length(dat$STATION) == 0) {
    return(tibble())
  }
  
  purrr::map_dfr(dat$STATION, \(x) {
    tibble(
      station = x$NAME %||% "Station",
      lat = suppressWarnings(as.numeric(x$LATITUDE %||% NA_real_)),
      lon = suppressWarnings(as.numeric(x$LONGITUDE %||% NA_real_)),
      distance_mi = suppressWarnings(as.numeric(x$DISTANCE %||% NA_real_)),
      rainfall_in = get_precip_value(x$OBSERVATIONS)
    )
  }) |>
    filter(!is.na(lat), !is.na(lon)) |>
    arrange(distance_mi)
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  
  div(
    class = "garden-header",
    
    div(
      class = "garden-title",
      HTML("&#10084; Garden Rainfall")
    ),
    
    div(
      class = "garden-subtitle",
      "Measured rainfall observations and forecast precipitation"
    )
  ),
  
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$style(HTML("
      @media (max-width: 768px) {
        .container-fluid {
          padding-left: 8px;
          padding-right: 8px;
        }

        h2 {
          font-size: 24px;
        }

        h4 {
          font-size: 18px;
        }

        .leaflet-container {
          height: 430px !important;
        }

        .btn {
          font-size: 14px;
          padding: 8px 10px;
        }

        .garden-map-control {
          width: 155px !important;
          padding: 8px !important;
        }

        .garden-map-legend {
          max-width: 210px !important;
          font-size: 11px !important;
        }

        .garden-map-control label {
          font-size: 12px;
        }
      }
      
      body {
      background:#eef2f5;
      font-family:-apple-system,BlinkMacSystemFont,'Segoe UI',Roboto,sans-serif;
    }
    
    .navbar,
    .nav-tabs {
      font-weight:600;
    }
    
    .tab-content {
      padding-top:12px;
    }
    
    .well,
    .card,
    .garden-card {
      border:none;
    }
    
    .leaflet-container {
      border-radius:14px;
      overflow:hidden;
      box-shadow:0 3px 12px rgba(0,0,0,0.15);
    }
    
    .garden-title {
      font-size:34px;
      font-weight:700;
      color:#1f2d3d;
      margin-bottom:4px;
    }
    
    .garden-subtitle {
      color:#5c6b7a;
      font-size:15px;
      margin-bottom:18px;
    }
    
    .garden-card {
      background:white;
      padding:16px;
      border-radius:14px;
      text-align:center;
      min-height:105px;
      box-shadow:0 2px 10px rgba(0,0,0,0.08);
      transition:all 0.2s ease;
    }
    
    .garden-card:hover {
      transform:translateY(-2px);
      box-shadow:0 5px 18px rgba(0,0,0,0.12);
    }
    
    .garden-card h5 {
      font-size:14px;
      font-weight:600;
      color:#5c6b7a;
      margin-bottom:8px;
      text-transform:uppercase;
      letter-spacing:0.4px;
    }
    
    .garden-card h3,
    .garden-card h4 {
      margin:0;
      font-weight:700;
      color:#1f2d3d;
    }
    
    .garden-map-control,
    .garden-map-legend {
      backdrop-filter:blur(6px);
      background:rgba(255,255,255,0.92) !important;
    }
    
    .btn-default,
    .btn {
      border-radius:10px !important;
      border:none !important;
      box-shadow:0 1px 4px rgba(0,0,0,0.15);
    }
    
    .irs-bar,
    .irs-single {
      background:#3182bd !important;
      border-color:#3182bd !important;
    }
    
    .selectize-input,
    .form-control {
      border-radius:10px !important;
      border:1px solid #d0d7de !important;
      box-shadow:none !important;
    }
    
    .control-label {
      font-weight:600;
      color:#334155;
    }
    
    .nav-tabs > li > a {
      border-radius:10px 10px 0 0;
      font-weight:600;
      color:#334155;
    }
    
    .nav-tabs > li.active > a {
      background:white;
      border-bottom:2px solid #3182bd !important;
    }
    
    @media (max-width:768px) {
    
      .garden-title {
        font-size:28px;
      }
    
      .garden-card {
        margin-bottom:10px;
      }
    
      .garden-map-control {
        width:150px !important;
      }
    
      .garden-map-legend {
        max-width:220px !important;
        font-size:11px !important;
      }
    }
    "))
  ),
  
  tabsetPanel(
    
    tabPanel(
      "Measured Rainfall",
      
      br(),
      
      fluidRow(
        column(3, div(class = "garden-card", h5("Max Rainfall"), h3(textOutput("max_rain", inline = TRUE)))),
        column(3, div(class = "garden-card", h5("Nearest Station"), h4(textOutput("nearest_station", inline = TRUE)))),
        column(3, div(class = "garden-card", h5("Stations"), h3(textOutput("station_count", inline = TRUE)))),
        column(3, div(class = "garden-card", h5("Updated"), h4(textOutput("last_updated", inline = TRUE))))
      ),
      
      br(),
      
      fluidRow(
        column(
          12,
          
          div(
            style = "position:relative;",
            
            leafletOutput("measured_map", height = 500),
            
            div(
              class = "garden-map-control",
              style = "
                position:absolute;
                top:80px;
                left:10px;
                z-index:500;
                background:white;
                padding:10px;
                border-radius:8px;
                box-shadow:0 1px 5px rgba(0,0,0,0.35);
                width:170px;
              ",
              
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
                selected = 24,
                selectize = FALSE,
                width = "100%"
              )
            ),
            
            div(
              class = "garden-map-legend",
              style = "
    position:absolute;
    top:10px;
    right:10px;
    z-index:500;
    background:white;
    padding:8px 10px;
    border-radius:8px;
    box-shadow:0 1px 5px rgba(0,0,0,0.35);
    font-size:12px;
    line-height:1.35;
  ",
              
              tags$strong(textOutput("legend_title", inline = TRUE)),
              
              lapply(seq_along(rain_labels), function(i) {
                div(
                  style = "
        display:flex;
        align-items:center;
        gap:5px;
        margin-top:4px;
      ",
                  div(
                    style = paste0(
                      "
          width:14px;
          height:14px;
          border:1px solid #666;
          background:",
                      rain_colors[i],
                      ";
          "
                    )
                  ),
                  span(rain_labels[i])
                )
              }),
              
              div(
                style = "
      display:flex;
      align-items:center;
      gap:5px;
      margin-top:4px;
    ",
                div(
                  style = "
        width:14px;
        height:14px;
        border:1px solid #666;
        background:#000000;
      "
                ),
                span("No value")
              )
            )
          )
        )
      )
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
          column(2, actionButton("forecast_prev", "\u25C0", width = "100%")),
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
          column(2, actionButton("forecast_next", "\u25B6", width = "100%"))
        )
      ),
      
      leafletOutput("forecast_map", height = 500)
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
    invalidateLater(5 * 60 * 1000, session)
    get_garden_rainfall(hours = selected_hours())
  })
  
  output$legend_title <- renderText({
    paste(selected_window_label(), "rainfall")
  })
  
  output$max_rain <- renderText({
    
    df <- rainfall_data()
    
    if (nrow(df) == 0 || all(is.na(df$rainfall_in))) {
      return("NA")
    }
    
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
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 11
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
            ifelse(is.na(rainfall_in), "No value", paste0(round(rainfall_in, 2), " in"))
          )
        )
    }
    
    m
  })
  
  observeEvent(input$forecast_prev, {
    
    current_index <- match(input$forecast_period, forecast_choices)
    new_index <- max(1, current_index - 1)
    
    updateSliderTextInput(
      session,
      "forecast_period",
      selected = forecast_choices[new_index]
    )
  })
  
  observeEvent(input$forecast_next, {
    
    current_index <- match(input$forecast_period, forecast_choices)
    new_index <- min(length(forecast_choices), current_index + 1)
    
    updateSliderTextInput(
      session,
      "forecast_period",
      selected = forecast_choices[new_index]
    )
  })
  
  selected_forecast_row <- reactive({
    req(input$forecast_period)
    
    forecast_cache$table[
      forecast_cache$table$slider_label == input$forecast_period,
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
      addControl(
        html = paste0(
          "
    <div style='
      background:white;
      padding:8px 10px;
      border-radius:8px;
      box-shadow:0 1px 5px rgba(0,0,0,0.35);
      font-size:12px;
      line-height:1.35;
    '>
    
    <strong>6-hr Forecast Rainfall</strong>
    ",
          
          paste0(
            lapply(seq_along(rain_labels), function(i) {
              paste0(
                "
          <div style='
            display:flex;
            align-items:center;
            gap:5px;
            margin-top:4px;
          '>
          
          <div style='
            width:14px;
            height:14px;
            border:1px solid #666;
            background:",
                rain_colors[i],
                ";
          '></div>
          
          <span>",
                rain_labels[i],
                "</span>
          
          </div>
          "
              )
            }),
            collapse = ""
          ),
          
          "
    </div>
    "
        ),
        
        position = "topright"
      ) |>
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 11
      )
  })
}

shinyApp(ui, server)