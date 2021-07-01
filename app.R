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
            histogramUI("histogram")
        )
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output){
    histogramServer("histogram")
}

shinyApp(ui, server)