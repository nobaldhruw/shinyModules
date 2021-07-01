pcaUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Input",
        solidHeader = TRUE,
        status = "primary",
        width = 3,
        selectInput(ns("pc_x"),"PC on x axis", choices = NULL),
        selectInput(ns("pc_y"),"PC on y axis", choices = NULL),
        numericInput(ns("top_variable_genes"), "No. of top variables features to use", min=50, value=500),
        selectInput(ns("color_col"), "Color samples by", choices = NULL),
        actionButton(ns("go"), "Go!")
      ),
      box(
        title = "PCA scores plot",
        solidHeader = TRUE,
        status = "primary",
        width = 9,
        plotOutput(ns("scores_plot"))
      )
    )
  )
}

pcaServer <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      rv <- reactiveValues(pca_scores=NULL)
      
      observeEvent(input$go, {
        rv$pca_scores <- compute_pca(data$exp, data$colData, input$top_variable_genes)
      })
      
      observeEvent(data$colData, {
        npc = min(dim(data$exp))
        updateSelectInput(session, "pc_x", choices = 1:npc, selected = 1)
        updateSelectInput(session, "pc_y", choices = 1:npc, selected = 2)
        updateSelectInput(session, "color_col", choices = colnames(data$colData))
      })
      
      output$scores_plot <- renderPlot({
        req(rv$pca_scores, input$color_col, input$pc_x, input$pc_y)
        pca_plot(rv$pca_scores, input$color_col, input$pc_x, input$pc_y)
      })
    }
  )
}

#####################################################################
#
#  Compute PCA
#
#####################################################################
compute_pca <- function(mat, metadata, num_highly_variable_genes = NULL){
  print("Compute PCA called")
  # Drop rows with NA values
  mat <- as.data.frame(mat[complete.cases(mat), ])
  
  # Drop rows with zero variance
  row_variance <- apply(mat, 1, var)
  mat <- mat[row_variance > 0, ]
  
  if(!is.null(num_highly_variable_genes)){
    mat$mad <- apply(mat, 1, mad)
    mat <- mat[order(mat$mad, decreasing = TRUE), ]
    mat <- mat[1:num_highly_variable_genes, ]
    mat$mad <- NULL
  }
  
  ## compute PCA
  pca_result <- prcomp(t(mat), center = T, scale = T)
  PCAScores <- data.frame(pca_result$x, metadata)
  
  return (PCAScores) 
}
#####################################################################
#
#  Plot PCA scores
#
#####################################################################
pca_plot <- function(PCAScores, cohort, PCx = 1, PCy = 2){
  require(ggplot2)
  p <- ggplot(PCAScores, aes_string(x = paste0('PC', PCx),
                                    y = paste0('PC', PCy),
                                    fill = cohort)) + 
    geom_point(shape = 21, size = 7, alpha = 0.7) + 
    labs(x = paste("PC",PCx),
         y = paste("PC",PCy), fill = "cohort") + 
    theme(legend.position = "right", legend.direction = "vertical",
          axis.line = element_line(size=1, colour = "black"), 
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          panel.border = element_blank(), panel.background = element_blank(), 
          axis.title = element_text(colour="black", size = 15, face = "bold"), 
          axis.text.x = element_text(colour="black", size = 10, margin=unit(c(0.5,0.5,0.1,0.1), "cm"), face = "bold"), 
          axis.text.y = element_text(colour="black", size = 10, margin=unit(c(0.5,0.5,0.1,0.1), "cm"), face = "bold"), 
          legend.text = element_text(size = 10, face = "bold"),
          legend.title = element_text(colour="black", size=12, face="bold"),
          axis.ticks.length = unit(-0.25, "cm"))
  p
}