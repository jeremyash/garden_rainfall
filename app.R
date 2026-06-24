library(shiny)
library(shinyWidgets)
library(leaflet)
library(tidyverse)
library(lubridate)
library(terra)


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

REMOTE_OBSERVED_RDS <- paste0(
  "https://raw.githubusercontent.com/",
  GITHUB_OWNER, "/", GITHUB_REPO, "/",
  CACHE_BRANCH, "/cache/observed_cache.rds"
)

REMOTE_OBSERVED_TIF <- paste0(
  "https://raw.githubusercontent.com/",
  GITHUB_OWNER, "/", GITHUB_REPO, "/",
  CACHE_BRANCH, "/cache/observed_qpe_24h_ll.tif"
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

download_observed_cache <- function() {
  
  local_rds <- "cache/observed_cache.rds"
  local_tif <- "cache/observed_qpe_24h_ll.tif"
  
  if (file.exists(local_rds) && file.exists(local_tif)) {
    message("Using local observed rainfall cache.")
    
    return(
      list(
        cache = readRDS(local_rds),
        qpe = terra::rast(local_tif)
      )
    )
  }
  
  rds_tmp <- tempfile(fileext = ".rds")
  tif_tmp <- tempfile(fileext = ".tif")
  
  download.file(REMOTE_OBSERVED_RDS, rds_tmp, mode = "wb", quiet = TRUE)
  download.file(REMOTE_OBSERVED_TIF, tif_tmp, mode = "wb", quiet = TRUE)
  
  list(
    cache = readRDS(rds_tmp),
    qpe = terra::rast(tif_tmp)
  )
}

# remote_observed <- download_observed_cache()
# 
# observed_cache <- remote_observed$cache
# observed_qpe <- remote_observed$qpe

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

if (!"slider_value" %in% names(forecast_cache$table)) {
  forecast_cache$table$slider_value <- as.character(forecast_cache$table$layer)
}

forecast_choices <- setNames(
  forecast_cache$table$slider_value,
  forecast_cache$table$slider_label
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

forecast_legend_html <- function() {
  
  paste0(
    "
    <div style='
      background:white;
      padding:8px 10px;
      border-radius:8px;
      box-shadow:0 1px 5px rgba(0,0,0,0.35);
      font-size:12px;
      line-height:1.35;
    '>
    <strong>Forecast Rainfall</strong>
    ",
    
    paste0(
      lapply(seq_along(rain_labels), function(i) {
        paste0(
          "
          <div style='display:flex;align-items:center;gap:5px;margin-top:4px;'>
            <div style='
              width:14px;
              height:14px;
              border:1px solid #666;
              background:", rain_colors[i], ";
            '></div>
            <span>", rain_labels[i], "</span>
          </div>
          "
        )
      }),
      collapse = ""
    ),
    
    "</div>"
  )
}

# -----------------------------
# UI
# -----------------------------

ui <- fluidPage(
  title = "Garden Rainfall",
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
    # Force mobile viewport
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-title", content = "Garden Rainfall"),
    tags$meta(name = "mobile-web-app-capable", content = "yes"),
    tags$meta(name = "theme-color", content = "#d62828"),
    
    # iOS home-screen behavior
    tags$meta(name = "apple-mobile-web-app-capable", content = "yes"),
    tags$meta(name = "apple-mobile-web-app-title", content = "Garden Rainfall"),
    tags$meta(name = "apple-mobile-web-app-status-bar-style", content = "default"),
    
    # Force iOS home-screen icon.
    # The version query helps beat old Shiny/browser icon cache.
    tags$link(
      rel = "apple-touch-icon",
      sizes = "180x180",
      href = "apple-touch-icon.png?v=20260520"
    ),
    tags$link(
      rel = "apple-touch-icon-precomposed",
      sizes = "180x180",
      href = "apple-touch-icon.png?v=20260520"
    ),
    tags$link(
      rel = "apple-touch-icon",
      sizes = "167x167",
      href = "apple-touch-icon-167x167.png?v=20260520"
    ),
    tags$link(
      rel = "apple-touch-icon",
      sizes = "152x152",
      href = "apple-touch-icon-152x152.png?v=20260520"
    ),
    
    # Browser favicons
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "32x32",
      href = "favicon-32x32.png?v=20260520"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "16x16",
      href = "favicon-16x16.png?v=20260520"
    ),
    
    # Larger icons for Chrome/Android/PWA-style shortcuts
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "192x192",
      href = "favicon-192x192.png?v=20260520"
    ),
    tags$link(
      rel = "icon",
      type = "image/png",
      sizes = "512x512",
      href = "favicon-512x512.png?v=20260520"
    ),
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
    
    .forecast-control-card {
      background:rgba(255,255,255,0.95);
      padding:14px;
      border-radius:14px;
      margin-bottom:12px;
      box-shadow:0 2px 10px rgba(0,0,0,0.12);
    }
    
    .forecast-time-label {
      text-align:center;
      font-size:17px;
      font-weight:700;
      color:#1f2d3d;
      margin-bottom:12px;
      line-height:1.35;
    }
    
    .forecast-step-row {
      display:flex;
      align-items:center;
      gap:10px;
    }
    
    .forecast-step-button {
      width:80px;
      min-height:44px;
      font-size:18px !important;
      font-weight:700 !important;
      border-radius:12px !important;
    }
    
    .forecast-slider-wrap {
      flex:1;
      min-width:0;
    }
    
    .forecast-slider-wrap .form-group {
      margin-bottom:0;
    }
    
    .forecast-slider-wrap .irs {
      margin-top:0;
    }
    
    @media (max-width:768px) {
      .forecast-control-card {
        padding:10px;
      }
    
      .forecast-time-label {
        font-size:14px;
        margin-bottom:8px;
      }
    
      .forecast-step-row {
        display:grid;
        grid-template-columns:1fr 1fr;
        gap:8px;
      }
    
      .forecast-slider-wrap {
        grid-column:1 / -1;
        order:1;
      }
    
      .forecast-prev-wrap {
        order:2;
      }
    
      .forecast-next-wrap {
        order:3;
      }
    
      .forecast-step-button {
        width:100%;
        min-height:46px;
        font-size:20px !important;
      }
    }
    "))
  ),
  
  tabsetPanel(
    
    tabPanel(
      "Observed Rainfall",
      
      br(),
      
      fluidRow(
        column(
          6,
          div(
            class = "garden-card",
            h5("Garden Rainfall"),
            h3(textOutput("observed_rainfall", inline = TRUE))
          )
        ),
        
        column(
          6,
          div(
            class = "garden-card",
            h5("Observation Time"),
            h4(textOutput("last_updated", inline = TRUE))
          )
        )
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
                top:10px;
                right:10px;
                z-index:500;
                background:white;
                padding:10px;
                border-radius:8px;
                box-shadow:0 1px 5px rgba(0,0,0,0.35);
                width:170px;
              ",
              
              div(
                style = "
                  font-weight:600;
                  text-align:center;
                ",
                "Observed Rainfall (MRMS 24-hour)"
              )
            ),
            
            div(
              class = "garden-map-legend",
              style = "
                position:absolute;
                bottom:18px;
                left:10px;
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
      
      h4("Forecast Rainfall"),
      
      div(
        class = "forecast-control-card",
        
        div(
          class = "forecast-time-label",
          textOutput("forecast_time_label")
        ),
        
        div(
          class = "forecast-step-row",
          
          div(
            class = "forecast-prev-wrap",
            actionButton("forecast_prev", "\u25C0", class = "forecast-step-button")
          ),
          
          div(
            class = "forecast-slider-wrap",
            sliderInput(
              inputId = "forecast_period",
              label = NULL,
              min = 1,
              max = nrow(forecast_cache$table),
              value = 1,
              step = 1,
              ticks = FALSE,
              width = "100%"
            )
          ),
          
          div(
            class = "forecast-next-wrap",
            actionButton("forecast_next", "\u25B6", class = "forecast-step-button")
          )
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
  
  observed_data <- reactive({
    
    invalidateLater(60 * 60 * 1000, session) # hourly
    
    download_observed_cache()
  })
  
  observed_info <- reactive({
    observed_data()$cache$table
  })
  
  observed_raster <- reactive({
    observed_data()$qpe
  })
  
  observed_info <- reactive({
    observed_cache$table
  })
  
  output$legend_title <- renderText({
    "24-hr observed rainfall"
  })
  
  output$observed_rainfall <- renderText({
    paste0(round(observed_info()$rainfall_in, 2), " in")
  })
  
  output$last_updated <- renderText({
    paste0(
      format(
        observed_info()$valid_time_local,
        "%b %d %H:%M"
      ),
      " ET"
    )
  })
  
  output$measured_map <- renderLeaflet({
    
    pal <- colorBin(
      palette = rain_colors,
      bins = rain_bins,
      domain = rain_bins,
      na.color = "transparent"
    )
    
    leaflet() |>
      
      addProviderTiles(
        providers$CartoDB.Voyager
      ) |>
      
      addRasterImage(
        observed_raster(),
        colors = pal,
        opacity = 0.75,
        project = FALSE
      ) |>
      
      addAwesomeMarkers(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        icon = garden_icon,
        label = GARDEN_NAME,
        popup = paste0(
          "<b>", GARDEN_NAME, "</b><br>",
          round(observed_info()$rainfall_in, 2),
          " inches in past 24 hours<br>",
          "Updated: ",
          observed_info()$label
        )
      ) |>
      
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 11
      )
  })
  
  forecast_values <- reactive({
    unname(forecast_choices)
  })
  
  observeEvent(input$forecast_prev, {
    updateSliderInput(
      session,
      "forecast_period",
      value = max(1, input$forecast_period - 1)
    )
  })
  
  observeEvent(input$forecast_next, {
    updateSliderInput(
      session,
      "forecast_period",
      value = min(nrow(forecast_cache$table), input$forecast_period + 1)
    )
  })
  
  selected_forecast_row <- reactive({
    req(input$forecast_period)
    forecast_cache$table[input$forecast_period, ]
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
    
    leaflet() |>
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
      addControl(
        html = forecast_legend_html(),
        position = "bottomleft"
      ) |>
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 11
      )
  })
  
  observeEvent(input$forecast_period, {
    
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
    
    leafletProxy("forecast_map") |>
      clearImages() |>
      addRasterImage(
        qpf_layer,
        colors = rain_pal,
        opacity = 0.7,
        project = FALSE
      )
  })
}

shinyApp(ui, server)