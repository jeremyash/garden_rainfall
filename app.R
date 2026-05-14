library(shiny)

ui <- fluidPage(
  titlePanel("Garden Rainfall")
)

server <- function(input, output, session) {
}

shinyApp(ui, server)