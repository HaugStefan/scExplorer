##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##

tab_gene_expression <- tabItem(
  tabName = "gene_expression",
  tagList(
    # Gene Selection
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
                 #uiOutput("gene_selection_UI")
                 selectizeInput(
                   'expression_genes_input',
                   label = NULL,
                   choices = c(""), #gene_names(),  #rownames(sample_data()$expression),
                   options = list(create = TRUE, placeholder = "Choose or enter gene name(s) here"),
                   multiple = TRUE
                 )
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
    # Violin Plots
    fluidRow(
      #uiOutput("expression_violin_plot_UI")
      # wrap with conditional panel ==> if more than 1 gene is selected, violin plots are hidden.
      conditionalPanel(
        condition = "output.number_genes_selected <= 1",
        cerebroBox(
          #id = "expression_violin_plot_box",
          title = tagList(
            boxTitle("Expression levels by cluster"),
            #cerebroInfoButton("expression_violin_plot_info")
          ),
          tagList(
            column(width = 9, offset = 0, style = "padding: 0px;",
                   #h1("expression by cluster")
                   plotly::plotlyOutput("expression_violin_plot")
            ),
            column(width = 3, offset = 0, style = "padding-left: 20px;",
                   #uiOutput("expression_violin_plot_options")
                   shinyWidgets::pickerInput(
                     "expression_violin_plot_cluster_select",
                     label = "Cluster selection",
                     choices = c(""),  # updated, when dataset is selected
                     selected = c(""), # selcected specifies initially selected values
                     options = list("actions-box" = TRUE),
                     multiple = TRUE
                   )
            )
          )
        )
    )
    ),
    # Expression Heatmap
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
                     choices = c(""),  # updated, when dataset is selected
                     selected = c(""), # selcected specifies initially selected values
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
                   ),
                   # sliderInput(
                   #   "expression_heatmap_color_range_slider",
                   #   label = "Range of color scale",
                   #   min = 0,
                   #   max = 1,
                   #   value = c(0, 1)
                   # )
                 )
          )
        )
      )
      
      
    ),
    # Dim Reduction Plot
    fluidRow(
      #uiOutput("expression_dim_reduction_UI")
      # wrap with conditional panel ==> if more than 1 gene is selected, violin plots are hidden.
      conditionalPanel(
        condition = "output.number_genes_selected <= 1",
        cerebroBox(
          title = tagList(
            boxTitle("Dimensional reduction"),
            # actionButton(
            #   inputId = "expression_dim_reduction_plot_info",
            #   label = "info",
            #   icon = NULL,
            #   class = "btn-xs",
            #   title = "Show additional information for this panel.",
            #   style = "margin-right: 5px"
            # )
          ),
          tagList(
            column(width = 9, offset = 0, style = "padding: 0px;",
                   plotly::plotlyOutput(
                     "dim_reduction_plot",
                     width = "auto",
                     height = 500 #"85vh"
                   ),
                   tags$br(),
            ),
            column(width = 3, offset = 0, style = "padding-left: 20px;",
                   #uiOutput("expression_dim_reduction_plot_options"),
                   tagList(
                     selectInput(
                       "expression_dim_reduction_plot_projection_select",
                       label = "Projection",
                       choices = c("")
                     ),
                     shinyWidgets::pickerInput(
                       "expression_dim_reduction_plot_cluster_select",
                       label = "Cluster selection",
                       choices = c(""),
                       selected = c(""),
                       options = list("actions-box" = TRUE),
                       multiple = TRUE
                     ),
                     selectInput(
                       "expression_dim_reduction_plot_plotting_order",
                       label = "Plotting order",
                       choices = c("Random", "Highest expression on top"),
                       selected = "Highest expression on top"
                     ),
                     sliderInput(
                       "expression_dim_reduction_plot_dot_size",
                       label = "Point size",
                       min = 1,
                       max = 20,
                       step = 1,
                       value = 5
                     ),
                     selectInput(
                       "expression_dim_reduction_plot_color_scale",
                       label = "Color scale",
                       choices = c("Cividis","YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","Viridis"),
                       selected = "Cividis"
                     )
                   )#,
                   #uiOutput("expression_expression_dim_reduction_plot_color_scale_range")
            )
          )
        )
      )
    ),
  )
)
