diffexpUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = "Differential expression inputs",
        solidHeader = TRUE,
        status = "primary",
        width = 6,
        selectInput(ns("cohort_col"), "Choose cohort column", choices = NULL),
        selectInput(ns("cohort_A"), "Choose cohort A", choices = NULL, multiple = TRUE),
        selectInput(ns("cohort_B"), "Choose cohort B", choices = NULL, multiple = TRUE),
        selectInput(ns("pval_correction"), "P.Value correction", choices=c("BH","Bonferroni")),
        actionButton(ns("go"), "Go!")
      ),
      box(
        title = "Volcano plot inputs",
        solidHeader = TRUE,
        status = "warning",
        width = 6,
        sliderInput(ns("abs_logFC_cutoff"),"Absolute log2FC cutoff", min=0, max=4, step=0.05, value=1.0),
        numericInput(ns("fdr_cutoff"),"FDR cutoff", min=0, max=1, step=0.05, value=0.05),
        numericInput(ns("ngenes_to_label"),"No. of top genes to label", min=0, max=20, step=1, value=10)
      )
    ),
    fluidRow(
      box(
        title = "Volcano plot",
        solidHeader = TRUE,
        status = "primary",
        width = 12,
        plotOutput(ns("volcano_plot"))
      )
    ),
    fluidRow(
      box(
        title = "Differential expression table",
        status = "primary",
        solidHeader = TRUE,
        width = 12,
        reactableOutput(ns("diff_exp_table"))
      )
    )
  )
}

diffexpServer <- function(id, data){
  moduleServer(
    id,
    function(input, output, session){
      rv <- reactiveValues(de_result=NULL)
      
      observeEvent(data$exp, {
        updateSelectInput(session, "cohort_col", choices = colnames(data$colData))
      })
      
      observeEvent(input$cohort_col, {
        cohort_col_values <- unique(data$colData[, input$cohort_col])
        updateSelectInput(session, "cohort_A", choices = cohort_col_values)
        updateSelectInput(session, "cohort_B", choices = cohort_col_values)
      })
      
      observeEvent(input$go, {
        rv$de_result <- diff_exp_limma(data$exp, 
                                       data$colData, 
                                       input$cohort_col, 
                                       input$cohort_A,
                                       input$cohort_B,
                                       input$pval_correction)
      })
      
      output$volcano_plot <- renderPlot({
        req(rv$de_result)
        volcano_plot(rv$de_result, 
                     log2fc_cutoff=input$abs_logFC_cutoff,
                     p_val_cutoff=input$fdr_cutoff, 
                     ngenes_to_label = input$ngenes_to_label)
      })
      
      output$diff_exp_table <- renderReactable({
        req(rv$de_result)
        reactable(rv$de_result, defaultPageSize = 5, searchable = TRUE)
      })
    }
  )
}

#####################################################################
#
#  Limma's differential expression
#
#####################################################################
diff_exp_limma <- function(data_matrix, metadata, cohortCol, cohort_a, cohort_b, p_val_correct_methods='BH') {
  
  # Subset the metadata dataframe to have samples from the provided cohorts only
  metadata <- metadata[metadata[, cohortCol] %in% c(cohort_a, cohort_b),]
  # Create a comparison column
  metadata[, "comparison"] <- NA
  metadata[metadata[, cohortCol] %in% cohort_a, "comparison"] <- "A"
  metadata[metadata[, cohortCol] %in% cohort_b, "comparison"] <- "B"
  
  condition <- metadata[, "comparison"]
  ### Creating design matrix of comparisons required (without batch)
  design <- model.matrix(~ condition + 0)
  colnames(design) <- gsub("condition", "", colnames(design))
  contrast_matrix <- limma::makeContrasts(contrasts = c(paste("B", "A", sep = "-")), levels = design)
  
  data_matrix <- data_matrix[, rownames(metadata)]
  data_matrix <- data_matrix[!apply(data_matrix, 1, anyNA), ]
  
  ### Fitting linear model to log2 normalised expression data
  fit <- limma::lmFit(data_matrix, design)
  fit <- limma::contrasts.fit(fit, contrast_matrix)
  fit <- limma::eBayes(fit)
  limma_results_df <- limma::topTable(fit, coef = paste("B", "A", sep = "-"), number = nrow(data_matrix))
  
  ### p value correction
  for (p_val_correct in p_val_correct_methods) {
    if (p_val_correct == "Bonferroni") {
      p_val_correct_method <- "bonferroni"
    }else {
      p_val_correct_method <- "BH"
    }    
    limma_results_df[, p_val_correct] <- p.adjust(limma_results_df$P.Value, method = p_val_correct_method)
  }
  return(limma_results_df)
}
#####################################################################
#
#  Volcano plot
#
#####################################################################
volcano_plot <- function(de_result, log2fc_cutoff=0, p_val_cutoff=0.05, ngenes_to_label = 10) {
  require(ggplot2)
  require(ggrepel)
  # Add logical vector as a column (significant) to the res_tableOE
  de_result$significance <- "not significant"
  de_result[(abs(de_result$logFC) > log2fc_cutoff) & (de_result$P.Value <= p_val_cutoff), "significance"] <- "significant"
  
  # Select top N genes for labelling 
  ## Sort by ordered padj
  de_result <- de_result[order(de_result$adj.P.Val), ] 
  
  ## Create a column to indicate which genes to label
  de_result$genelabels <- ""
  de_result$genelabels[1:ngenes_to_label] <- rownames(de_result)[1:ngenes_to_label]
  
  ## Volcano plot
  p <- ggplot(de_result, aes(x = logFC, y = -log10(adj.P.Val), fill = significance,label=genelabels)) +
    geom_point(shape = 21, size = 3, alpha = 1.0) +
    geom_text_repel(size = 5) + 
    #geom_text(aes(label = ifelse(genelabels !=0, as.character(genelabels), "")),hjust = 0, nudge_x = 0.05, size=5, colour = "black") +
    xlab("log2FC") + 
    ylab("-log10 (padj)") +
    theme(legend.position = "top", legend.direction = "horizontal", # legend positioned at the bottom, horizantal direction,
          axis.line = element_line(size=1, colour = "black"),	# axis line of size 1 inch in black color
          panel.grid.major = element_blank(),	# major grids included
          panel.grid.minor = element_blank(),	# no minor grids
          panel.border = element_blank(), panel.background = element_blank(), # no borders and background color
          axis.title = element_text(colour="black", size = 25, face = "bold"), # axis title 
          axis.text.x = element_text(colour="black", size = 20, margin=unit(c(0.5,0.5,0.1,0.1), "cm"), face = "bold"), # x-axis text in fontsize 10
          axis.text.y = element_text(colour="black", size = 20, margin=unit(c(0.5,0.5,0.1,0.1), "cm"), face = "bold"), # y-axis text in fontsize 10
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_blank(),
          axis.ticks.length = unit(-0.25, "cm")) # ticks facing inward with 0.25cm length
  p
}