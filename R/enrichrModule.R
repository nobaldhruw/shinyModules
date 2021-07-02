enrichrUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
      box(
        title = 'Input paramters',
        status = 'primary',
        width = 3,
        solidHeader = TRUE,
        radioButtons(ns("enrichr_gene_direction"), "Gene direction", choices=c("up", "down", "both"), selected = "both"),
        sliderInput(ns("enrichr_abs_logFC_cutoff"), "Absolute logFC cutoff", min = 0, max = 10, value = 1),
        numericInput(ns("enrichr_fdr_cutoff"), "FDR cutoff", min = 0, max = 1, value = 0.05),
        selectInput(ns("enrichr_db"), "Choose DB", choices=NULL),
        numericInput(ns("enrichr_ngs"), "No. of gene sets", min=1, max=20, value=10)
      ),
      box(
        title = 'Enriched Gene sets',
        status = 'primary',
        width = 9,
        solidHeader = TRUE,
        plotOutput(ns("enrichr_dotplot"))
      )
    )
  )
}

enrichrServer <- function(id, de_result){
  moduleServer(
    id,
    function(input, output, session){
      rv <- reactiveValues(egs=NULL, enrichr_dbs=get_enrichr_dblist())
      
      observeEvent(rv$de_table, {
        updateSelectInput(session, inputId = "enrichr_db", choices = rv$enrichr_dbs, selected = rv$enrichr_dbs[1])
        print(input$enrichr_db)
        print(head(rv$de_table))
        rv$egs <- get_enriched_gene_sets(rv$de_table, 
                                         gene_direction  = input$enrichr_gene_direction,
                                         log2fc_cutoff   = input$enrichr_abs_logFC_cutoff, 
                                         fdr_cutoff      = input$enrichr_fdr_cutoff,
                                         db              = input$enrichr_db)
        
      })
      
      output$enrichr_dotplot <- renderPlot({
        req(rv$egs)
        plot_enriched_gene_sets(rv$egs, input$enrichr_ngs)
      })
    }
  )
}

#####################################################################
#
#  Fetch dbList from enrichR
#
#####################################################################
get_enrichr_dblist <- function(){
  return(enrichR::listEnrichrDbs()[,'libraryName'])
}
#####################################################################
#
#  Identify enriched gene sets
#
#####################################################################
get_enriched_gene_sets <- function(de_result, 
                                   gene_direction = 'both', 
                                   log2fc_cutoff  = 0, 
                                   fdr_cutoff     = 0.05,
                                   db             = 'KEGG_2019_Human')
{
  require(enrichR)
  gene_set <- row.names(de_result[(de_result[,'adj.P.Val'] < fdr_cutoff) & (abs(de_result$logFC) > log2fc_cutoff), ])
  if(gene_direction == 'up'){
    gene_set <- row.names(de_result[(de_result[,'adj.P.Val'] < fdr_cutoff) & (de_result$logFC > log2fc_cutoff), ])
  }else if(gene_direction == 'down'){
    gene_set <- row.names(de_result[(de_result[,'adj.P.Val'] < fdr_cutoff) & (de_result$logFC < -log2fc_cutoff), ])
  }
  enriched <- enrichr(gene_set, db)
  enriched[[db]]$geneRatio <- sapply(enriched[[db]]$Overlap, function(x){
    num <- as.numeric(strsplit(x, '/')[[1]][1])
    den <- as.numeric(strsplit(x, '/')[[1]][2])
    return (num/den)
  })
  egs <- enriched[[db]]
  egs <- egs[order(egs$Combined.Score, decreasing = T), ]
  return(egs)
}
#####################################################################
#
#  Visualise enriched gene sets
#
#####################################################################
options(repr.plot.width = 12, repr.plot.height = 6)

plot_enriched_gene_sets <- function(egs, nsets = 10){
  p <- ggplot(egs[1:nsets, ], aes(x = Combined.Score, y = Term, fill = Adjusted.P.value)) +
    geom_point(shape = 21, alpha = 1.0, aes(size = geneRatio)) +
    scale_y_discrete(limits = rev(egs[1:nsets,'Term'])) +
    scale_fill_continuous(low="brown", high="grey", limits=c(0,0.05)) +
    scale_size(range = c(2, 10)) +
    xlab("Combined Score") + 
    ylab("Gene set") +
    theme(legend.position = "right", legend.direction = "vertical", # legend positioned at the bottom, horizantal direction,
          axis.line = element_line(size=1, colour = "black"),	# axis line of size 1 inch in black color
          panel.background = element_rect(fill = "white",
                                          colour = "black",
                                          linetype = "solid", size = 1),
          panel.grid.major = element_line(linetype = 'solid', colour = "grey"),
          axis.title = element_text(colour="black", size = 20, face = "bold"), # axis title 
          axis.text.x = element_text(colour="black", size = 15, margin=unit(c(0.5,0.5,0.1,0.1), "cm"), face = "bold"), # x-axis text in fontsize 10
          axis.text.y = element_text(colour="black", size = 15, margin=unit(c(0.5,0.5,0.1,0.1), "cm"), face = "bold"), # y-axis text in fontsize 10
          legend.text = element_text(size = 15, face = "bold"),
          legend.title = element_text(colour = "black", size = 15, face = "bold"),
          axis.ticks.length = unit(-0.25, "cm")) # ticks facing inward with 0.25cm length
  p
}