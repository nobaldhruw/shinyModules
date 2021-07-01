histogramUI <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(
      title = "Input", 
      status = "primary", 
      width = 3, 
      solidHeader = TRUE,
      sliderInput(ns("num"),"Choose a number", min=10, max=100, value = 50, step = 5)
    ),
    box(
      title = "Output", 
      status = "primary", 
      width = 9, 
      solidHeader = TRUE,
      plotOutput(ns("plot_histogram"))
    )
  )
}

histogramServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      data <- reactive({ rnorm(input$num)})
      
      output$plot_histogram <- renderPlot({
        hist(data())
      })
      
    }
  )
}
