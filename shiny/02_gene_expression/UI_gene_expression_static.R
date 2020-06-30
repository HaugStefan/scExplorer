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
      #uiOutput("expression_heatmap_UI")
      cerebroBox(
        title = tagList(
          boxTitle("Average gene expression within clusters"),
          #cerebroInfoButton("expression_violin_plot_info")
        ),
        tagList(
          column(width = 9, offset = 0, style = "padding: 0px;",
                 plotly::plotlyOutput("expression_heatmap", height = "400px")
          )
          ,
          column(width = 3, offset = 0, style = "padding-left: 20px;",
                 #uiOutput("expression_heatmap_options"),
                 #uiOutput("expression_heatmap_color_scale_range")
                 tagList(
                   shinyWidgets::pickerInput(
                     "expression_heatmap_cluster_select",
                     label = "Cluster selection",
                     choices = sample_data()$cluster_names,
                     selected = sample_data()$cluster_names,  # selcected specifies initially selected values
                     options = list("actions-box" = TRUE),
                     multiple = TRUE
                   ),
                   selectInput(
                     "expression_heatmap_rescaling_select",
                     label = "Rescaling",
                     choices = c("No rescaling", "Rescale per gene"),
                     selected = "Random"
                   ),
                   selectInput(
                     "expression_heatmap_color_scale",
                     label = "Color scale",
                     choices = c("Cividis","YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","Viridis"),
                     selected = "Cividis"
                   )
                 )
          )
        )
      )
      
      
    ),
    fluidRow(
      uiOutput("expression_dim_reduction_UI")
      
    ),
  )
)
