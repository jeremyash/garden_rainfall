library(shiny)
library(leaflet)
library(DT)

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
      setView(
        lng = -82.55,
        lat = 35.60,
        zoom = 10
      )
  })
  
  output$garden_summary <- renderTable({
    
    data.frame(
      Metric = c(
        "Site Name",
        "Latitude",
        "Longitude"
      ),
      
      Value = c(
        "Garden",
        35.60,
        -82.55
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