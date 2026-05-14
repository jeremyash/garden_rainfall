library(shiny)
library(leaflet)
library(DT)

GARDEN_NAME <- "Garden"
GARDEN_LAT <- 35.611622
GARDEN_LON <- -82.371369
GARDEN_TZ   <- "America/New_York"

ui <- fluidPage(
  
  titlePanel("Garden Rainfall"),
  
  tabsetPanel(
    
    tabPanel(
      "Measured Rainfall",
      
      br(),
      
      fluidRow(
        column(
          8,
          leafletOutput("measured_map", height = 500)
        ),
        
        column(
          4,
          h4("Garden Summary"),
          tableOutput("garden_summary")
        )
      ),
      
      br(),
      
      DTOutput("station_table")
    ),
    
    tabPanel(
      "Forecast Rainfall",
      
      br(),
      
      h4("Forecast rainfall coming soon")
    )
  )
)

server <- function(input, output, session) {
  
  output$measured_map <- renderLeaflet({
    
    leaflet() |>
      addProviderTiles(providers$CartoDB.Voyager) |>
      
      addCircleMarkers(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        radius = 10,
        color = "black",
        weight = 2,
        fillColor = "forestgreen",
        fillOpacity = 1,
        label = GARDEN_NAME
      ) |>
      
      addCircles(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        radius = 25 * 1609.34,
        color = "forestgreen",
        fillOpacity = 0,
        weight = 2,
        dashArray = "5,5"
      ) |>
      
      setView(
        lng = GARDEN_LON,
        lat = GARDEN_LAT,
        zoom = 10
      )
  })
  
  output$garden_summary <- renderTable({
    
    data.frame(
      Metric = c(
        "Garden",
        "Latitude",
        "Longitude",
        "Search Radius"
      ),
      
      Value = c(
        GARDEN_NAME,
        GARDEN_LAT,
        GARDEN_LON,
        "25 miles"
      )
    )
  })
  
  output$station_table <- renderDT({
    
    datatable(
      data.frame(
        Station = character(),
        Rainfall = numeric()
      ),
      options = list(dom = "t")
    )
  })
}

shinyApp(ui, server)