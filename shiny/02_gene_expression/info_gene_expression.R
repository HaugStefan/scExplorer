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
