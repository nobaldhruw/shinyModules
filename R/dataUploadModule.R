library(reactable)
library(cmapR)

dataUploadUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(solidHeader = TRUE, fileInput(ns("file"),"Upload a GCT file", accept = ".gct"))
    ),
    fluidRow(
      box(
        title = "Expression data",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        withSpinner(reactableOutput(ns("exp")))
      )
    ),
    fluidRow(
      box(
        title = "Sample metadata",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        withSpinner(reactableOutput(ns("colData")))
      )
    )
  )
}

dataUploadServer <- function(id){
  moduleServer(
    id,
    function(input, output, server){
      
      rv <- reactiveValues(exp=NULL, colData=NULL)
      
      observeEvent(input$file, {
        gctObj <- parse_gctx(input$file$datapath)
        rv$exp <- gctObj@mat
        rv$colData <- gctObj@cdesc
      })
      
      output$exp <- renderReactable({
        req(rv$exp)
        reactable(rv$exp, defaultPageSize=5)
      })
      
      output$colData <- renderReactable({
        req(rv$colData)
        reactable(rv$colData, defaultPageSize=5)
      })
      
      return(rv)
    }
  )
}