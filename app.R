options(shiny.maxRequestSize=1024*1024^2) 

library(shiny)
library(shinydashboard)
library(shinycssloaders)

header <- dashboardHeader(title="Shiny modules")
sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Import data", tabName = "tab_data_upload"),
        menuItem("PCA", tabName = "tab_pca"),
        menuItem("Differential expression", tabName = "tab_diffexp"),
        menuItem("EnrichR", tabName = "tab_enrichr")
    )
)
body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "tab_data_upload",
            dataUploadUI("data_upload")
        ),
        tabItem(
            tabName = "tab_pca",
            pcaUI("pca")
        ),
        tabItem(
            tabName = "tab_diffexp",
            diffexpUI("diffexp")
        ),
        tabItem(
            tabName = "tab_enrichr",
            enrichrUI("enrichr")
        )
    )
)

ui <- dashboardPage(
    header = header,
    sidebar = sidebar,
    body = body
)

server <- function(input, output){
    data <- dataUploadServer("data_upload")
    pcaServer("pca", data)
    de_result <- diffexpServer("diffexp", data)
    enrichrServer("enrichr", de_result)
}

shinyApp(ui, server)