options(shiny.maxRequestSize=1024*1024^2) 

library(shiny)
library(shinydashboard)
library(shinycssloaders)

header <- dashboardHeader(title="Shiny modules")
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Import data", tabName = "tab_data_upload")
    )
)
body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "tab_data_upload",
            dataUploadUI("data_upload")
        )
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output){
    dataUploadServer("data_upload")
}

shinyApp(ui, server)