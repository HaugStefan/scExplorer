##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##

tab_gene_expression <- tabItem(
  tabName = "gene_expression",
  tagList(
      fluidRow(
        cerebroBox(
          title = tagList("Gene Search",
                          actionButton(
                            inputId = "expression_info",
                            label = "info",
                            icon = NULL,
                            class = "btn-xs",
                            title = "Show additional information for this panel.",
                            style = "margin-right: 5px; margin-left: 10px;"
                          )
          ),
          tagList(
            # gene list input
            column(width = 9, offset = 0, style = "padding: 0px;", 
                uiOutput("gene_selection_UI")
            ),
            # reset gene list
            column(width = 1, offset = 0, style = "padding-left: 10px;",
                   actionLink("reset_genes", NULL, icon = icon("fas", "fa-times-circle"), class = "btn_leftAlign topbar-element", style = "padding-top:5px;"),
                   # shinyBS::bsTooltip(
                   #   "reset_gene",
                   #   title = "Reset gene selection",
                   #   options = list(container = "body")
                   # )
            ),
            column(width = 2, offset = 0, style = "padding: 0px;",
        
            )
          )
        )
      ),

    fluidRow(
      uiOutput("expression_violin_plot_UI")
    ),
    
    fluidRow(
     uiOutput("expression_heatmap_UI")
    ),
    fluidRow(
     uiOutput("expression_dim_reduction_UI")
      
    ),
  )
)
