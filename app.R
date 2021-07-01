library(shiny)
library(shinydashboard)

header <- dashboardHeader(title="Shiny modules")
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Histogram", tabName = "tab_histogram")
    )
)
body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "tab_histogram",
            fluidRow(
                box(
                    title = "Input", 
                    status = "primary", 
                    width = 3, 
                    solidHeader = TRUE,
                    sliderInput("num","Choose a number", min=10, max=100, value = 50, step = 5)
                ),
                box(
                    title = "Output", 
                    status = "primary", 
                    width = 9, 
                    solidHeader = TRUE,
                    plotOutput("plot_histogram")
                )
            )
        )
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output){
    data <- reactive({ rnorm(input$num)})
    output$plot_histogram <- renderPlot({
        hist(data())
    })
}

shinyApp(ui, server)