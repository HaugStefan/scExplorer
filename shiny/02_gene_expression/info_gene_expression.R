##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##


gene_search_info <- list(
  title = "Visualization of gene expression",
  text = p(
    "Please select/enter one or more gene names.",
    br(),
    "Gene expression for selected genes is displayed by cluster in a:",
    tags$ul(
      tags$li("violin-plot"),
      tags$li("heatmap"),
      tags$li("dimensional reduction plot", br(), "(several dimensional reductions may be available depending on dataset)"),
      
    ),
    strong("The violin and the dimensional-reduction plot are not displayed if more than one gene name is selected."),
    br(),
    "The plots are interactive (drag and zoom) but depending on the computer of the user and the number of cells displayed it can become slow."
  )
)

violin_info <- list(
  title = "Violin Plots of Log-Normalized Gene Expression",
  text = p(
    "Please select clusters to be displayed.",
    br(),
    "If possible, clusters were first ordered along the anatomical structure of a nephron.
     Then cell clusters not belonging to the nephron were listed."
  )
)

heatmap_info <- list(
  title = "Heatmap of Average-Log-Normalized Gene Expression",
  text = p(
    "Please configure the graph with the following options:",
    tags$ul(
      tags$li("Cluster selection"),
      tags$li("Rescaling: With 'no rescaling' the Log-Normalized Average Expression is displayed for each cluster. 
                When choosing 'rescale per gene' the Log-Normalized Average Expression values are 
                further normalized across all clusters for each of the genes to a range between 0 and 1."),
      tags$li("Color scale (scheme)"),
      
    ),
    
    br(),
    
    "If possible, clusters were first ordered along the anatomical structure of a nephron.
     Then cell clusters not belonging to the nephron itself were listed."
  )
)


dimreduc_info <- list(
  title = "Dimensional Reduction Plots",
  text = p(
    "Please configure the graph with the following options:",
    tags$ul(
      tags$li("Projection: Select from the dimensional reductions available in the dataset"),
      tags$li("Cluster selection"),
      tags$li("Plotting order (random or cells with highest expression on top)"),
      tags$li("Point size"),
      tags$li("Color scale (scheme)")
      
    ),
    
    br(),
    
    "If possible, clusters were first ordered along the anatomical structure of a nephron.
     Then cell clusters not belonging to the nephron itself were listed."
  )
)