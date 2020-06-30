##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##


# Here boxes are rendered by the server function because user actions can 
# collapse the boxes.

# Trace massages to browser:
options(shiny.trace = TRUE)



# Update controls when sample data has changed
observeEvent(sample_data(),{
  
  # updateSelectInput(session, "expression_violin_plot_cluster_select",
  #                      choices = sample_data()$cluster_names,
  #                      selected = sample_data()$cluster_names)
  
  # updateSelectInput(session, "expression_heatmap_cluster_select",
  #                   choices = sample_data()$cluster_names,
  #                   selected = sample_data()$cluster_names)
  
  # show loading screen
  w$show()
  
  
  # Update gene list 
  updateSelectizeInput(session, "expression_genes_input",
                       choices =  rownames(sample_data()$expression),
                       server = TRUE)

  # Cluster Selection Expression Heatmap
  shinyWidgets::updatePickerInput(session, "expression_heatmap_cluster_select",
                                  choices = sample_data()$cluster_names,
                                  selected = sample_data()$cluster_names)
  
  shinyWidgets::updatePickerInput(session, "expression_violin_plot_cluster_select",
                                  choices = sample_data()$cluster_names,
                                  selected = sample_data()$cluster_names)
  
  updateSelectInput(session, "expression_dim_reduction_plot_cluster_select",
                       choices = sample_data()$cluster_names,
                       selected = sample_data()$cluster_names)

  updateSelectInput(session, "expression_dim_reduction_plot_projection_select",
                    choices = sample_data()$projections,
                    selected = sample_data()$projections)

  w$hide()
})




##----------------------------------------------------------------------------##
## UI: GENE SELECTION
##----------------------------------------------------------------------------##

# output[["gene_selection_UI"]] <- renderUI ({
#   if(is.null(sample_data())) {
#     p("No dataset loaded.")
#   } else {
#     tagList(
#       selectizeInput(
#         'expression_genes_input',
#         label = NULL,
#         choices = gene_names(),  #rownames(sample_data()$expression),
#         options = list(create = TRUE, placeholder = "Choose or enter gene name(s) here"),
#         multiple = TRUE
#       )
#     )
#   }
# })


## Observe reset Button
 observeEvent(input$reset_genes, {
#   shinyjs::reset("gene_selection_UI")
   reset("expression_genes_input")
   # updateSelectizeInput(session, "expression_genes_input",
   #                      selected = NULL)
 }) 




## Make gene names reactive and add loading animation
# Here the sample expression data is loaded for the first time which takes 
# the longest loading time.
w <- Waiter$new(html = tagList(spin_wave(), h4( "Loading Dataset ..."),
                               p("Depending on connection and hardware, loading may require up to 60 seconds.")   ),
                color = "rgb(0, 0, 0, .8)")

gene_names <- reactive({
  w$show()
  gene_names <- rownames(sample_data()$expression)
  w$hide()
  gene_names
})



##----------------------------------------------------------------------------##
# Gene Search Info Box

observeEvent(input[["expression_info"]], {
  showModal(
    modalDialog(
      gene_search_info$text,
      title = gene_search_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})


##----------------------------------------------------------------------------##
## DATA: GENES TO PLOT
##----------------------------------------------------------------------------##

# cannot use req() because it delays initialization and plot is updated only 
# with button press so plot doesn't initialize at all
genesToPlot <- reactive({
  
  genesToPlot <- list()
  if ( is.null(input[["expression_genes_input"]]) ) {
    genesToPlot[["genes_to_display"]] <- character()
  } else {
    genesToPlot[["genes_to_display"]] <- input[["expression_genes_input"]] %>%
      strsplit(",| |;|\n") %>%
      unlist() %>%
      gsub(pattern = " ", replacement = "", fixed = TRUE) %>%
      unique() %>%
      .[. != ""]
  }
  genesToPlot[["genes_to_display_here"]] <- rownames(sample_data()$expression)[ match(tolower(genesToPlot[["genes_to_display"]]), tolower(rownames(sample_data()$expression))) ]
  genesToPlot[["genes_to_display_present"]] <- na.omit(genesToPlot[["genes_to_display_here"]])
  genesToPlot[["genes_to_display_missing"]] <- genesToPlot[["genes_to_display"]][ which(is.na(genesToPlot[["genes_to_display_here"]])) ]
  
  return(genesToPlot)
})


##----------------------------------------------------------------------------##
## UI: DIMENSIONAL REDUCTION (tsne, umap)
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
# Dimensional reduction box

output[["expression_dim_reduction_UI"]] <- renderUI({
  if ( length(input[["expression_genes_input"]]) < 2) {
    tagList(
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
                 #div(style = "padding-left: 10px;",
                 uiOutput("expression_dim_reduction_plot_options"),
                 uiOutput("expression_expression_dim_reduction_plot_color_scale_range")
          )
        )
      )
    )
  } else {
      
  }
})


##----------------------------------------------------------------------------##
# Dimensional reduction user interfaces

output[["expression_dim_reduction_plot_options"]] <- renderUI({
  
  proj <- sample_data()$projections
  #browser()
  tagList(
    selectInput(
      "expression_dim_reduction_plot_projection_select",
      label = "Projection",
      choices = names(sample_data()$projections)
    ),
    shinyWidgets::pickerInput(
      "expression_dim_reduction_plot_cluster_select",
      label = "Cluster selection",
      choices = sample_data()$cluster_names,
      selected = sample_data()$cluster_names,
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
      min = scatter_plot_dot_size[["min"]],
      max = scatter_plot_dot_size[["max"]],
      step = scatter_plot_dot_size[["step"]],
      value = scatter_plot_dot_size[["default"]]
    ),
    selectInput(
      "expression_dim_reduction_plot_color_scale",
      label = "Color scale",
      choices = c("Cividis","YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","Viridis"),
      selected = "Cividis"
    )
  )
})

## color scale range
output[["expression_expression_dim_reduction_plot_color_scale_range"]] <- renderUI({
  req(
    input[["expression_dim_reduction_plot_cluster_select"]]
  )
  range <- c(0,0)
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
    # display clusters if no genes were entered
    clusters_to_display <- input[["expression_dim_reduction_plot_cluster_select"]]
    cluster_number <- length(unique(clusters_to_display))
    range<- c(0, cluster_number)
  } else if (length(genesToPlot()$genes_to_display_present) == 1) {
    range <- range(expression_dim_reduction_plot_data()$level)
  }
  
  if ( range[1] == 0 & range[2] == 0 ) {
    range[2] = 1
  } else {
    range[1] <- range[1] %>% round(digits = 2)
    range[2] <- range[2] %>% round(digits = 2)
  }
  tagList(
    sliderInput(
      "expression_dim_reduction_plot_color_scale_range",
      label = "Range of color scale",
      min = range[1],
      max = range[2],
      value = c(range[1], range[2])
    )
  )
})

# Observe if sample_data has changed and update select_input
# Observers use eagerevaluation (reactive lazy evaluation)!
# observe({
#   updateSelectInput(session, "expression_dim_reduction_plot_cluster_select",
#                     choices = sample_data()$cluster_names,
#                     selected = sample_data()$cluster_names)
# })

# observe({
#   updateSelectInput(session, "expression_dim_reduction_plot_projection_select",
#                     choices = sample_data()$projections,
#                     selected = sample_data()$projections)
# })


##----------------------------------------------------------------------------##
# Dimensional reduction Plot

output[["dim_reduction_plot"]] <- plotly::renderPlotly({
  
  req(
    input[["expression_dim_reduction_plot_projection_select"]],
    input[["expression_dim_reduction_plot_dot_size"]],
    input[["expression_dim_reduction_plot_color_scale"]],
    input[["expression_dim_reduction_plot_color_scale_range"]],
    input[["expression_dim_reduction_plot_cluster_select"]]
  )
  
  # plot only drawn for none or one gene (otherwise not shown)
  if (length(genesToPlot()$genes_to_display_present) < 2 ) {
  
    projections_available <- names(sample_data()$projections)
    projection_to_display <- input[["expression_dim_reduction_plot_projection_select"]]
    
    # Test if selected projection is available (in case datasets have been changed)
    # Somehow this was neccessary...
    if(!(projection_to_display %in% projections_available)) {
      projection_to_display <- projections_available[1]
    }
    
    range_x_min <- sample_data()$projections[[ projection_to_display ]][,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
    range_x_max <- sample_data()$projections[[ projection_to_display ]][,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
    range_y_min <- sample_data()$projections[[ projection_to_display ]][,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
    range_y_max <- sample_data()$projections[[ projection_to_display ]][,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
    
    color_scale <- input[["expression_dim_reduction_plot_color_scale"]]
    
    
    # check if genes were entered
    if (length(genesToPlot()$genes_to_display_present) == 0 ) {
      # display clusters in different colors if no genes were entered
      color_levels <- ~unclass(factor(cluster)) #expression_dim_reduction_plot_data()$cluster #
      show_scale <- FALSE
    } else {
      # show expression if gene name was entered
      color_levels <- ~level #expression_dim_reduction_plot_data()$level
      show_scale <- TRUE
    }
   
    if ( ncol(sample_data()$projections[[ projection_to_display ]]) == 3 ) {
      plotly::plot_ly(
        expression_dim_reduction_plot_data(),
        x = expression_dim_reduction_plot_data()[,1],
        y = expression_dim_reduction_plot_data()[,2],
        z = expression_dim_reduction_plot_data()[,3],
        type = "scatter3d",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = "Expression"
          ),
          color = ~level,
          colorscale = color_scale,
          cauto = FALSE,
          cmin = input[["expression_dim_reduction_plot_color_scale_range"]][1],
          cmax = input[["expression_dim_reduction_plot_color_scale_range"]][2],
          reversescale = TRUE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["expression_dim_reduction_plot_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~paste(
          "<b>Cell</b>: ", expression_dim_reduction_plot_data()$cell_barcode, "<br>",
          "<b>Cluster</b>: ", expression_dim_reduction_plot_data()$cluster, "<br>",
          "<b>Transcripts</b>: ", formatC(expression_dim_reduction_plot_data()$nUMI, format = "f", big.mark = ",", digits = 0), "<br>",
          "<b>Expressed genes</b>: ", formatC(expression_dim_reduction_plot_data()$nGene, format = "f", big.mark = ",", digits = 0), "<br>",
          "<b>Expression level</b>: ", formatC(expression_dim_reduction_plot_data()$level, format = "f", big.mark = ",", digits = 3)
        ),
        source = "dim_reduction_plot"
      ) %>%
        plotly::layout(
          scene = list(
            xaxis = list(
              title = colnames(expression_dim_reduction_plot_data())[1],
              mirror = TRUE,
              showline = TRUE,
              zeroline = FALSE
            ),
            yaxis = list(
              title = colnames(expression_dim_reduction_plot_data())[2],
              mirror = TRUE,
              showline = TRUE,
              zeroline = FALSE
            ),
            zaxis = list(
              title = colnames(expression_dim_reduction_plot_data())[3],
              mirror = TRUE,
              showline = TRUE,
              zeroline = FALSE
            )
          ),
          hoverlabel = list(
            font = list(
              size = 11,
              color = "black"
            ),
            bgcolor = "lightgrey"
          )
        )
    } else {
      plot <- plotly::plot_ly(
        expression_dim_reduction_plot_data(),
        x = expression_dim_reduction_plot_data()[,1],
        y = expression_dim_reduction_plot_data()[,2],
        type = "scatter",
        mode = "markers",
        marker = list(
          colorbar = list(
            title = "Expression"
          ),
          showscale = show_scale,
          color = color_levels,
          colorscale = color_scale,
          cauto = FALSE,
          cmin = input[["expression_dim_reduction_plot_color_scale_range"]][1],
          cmax = input[["expression_dim_reduction_plot_color_scale_range"]][2],
          reversescale = TRUE,
          line = list(
            color = "rgb(196,196,196)",
            width = 1
          ),
          size = input[["expression_dim_reduction_plot_dot_size"]]
        ),
        hoverinfo = "text",
        text = ~paste(
          "<b>Cell</b>: ", expression_dim_reduction_plot_data()$cell_barcode, "<br>",
          "<b>Cluster</b>: ", expression_dim_reduction_plot_data()$cluster, "<br>",
          "<b>Transcripts</b>: ", formatC(expression_dim_reduction_plot_data()$nUMI, format = "f", big.mark = ",", digits = 0), "<br>",
          "<b>Expressed genes</b>: ", formatC(expression_dim_reduction_plot_data()$nGene, format = "f", big.mark = ",", digits = 0), "<br>",
          "<b>Expression level</b>: ", formatC(expression_dim_reduction_plot_data()$level, format = "f", big.mark = ",", digits = 3)
        ),
        source = "dim_reduction_plot"
      ) %>%
        plotly::layout(
          xaxis = list(
            title = NULL, #colnames(expression_dim_reduction_plot_data())[1],
            mirror = TRUE,
            showgrid = TRUE,
            showline = FALSE,
            ticks = "",
            showticklabels = FALSE,
            zeroline = FALSE,
            visible = TRUE,
            range = c(range_x_min, range_x_max)
          ),
          yaxis = list(
            title = NULL, #colnames(expression_dim_reduction_plot_data())[2],
            mirror = TRUE,
            showgrid = TRUE,
            showline = FALSE,
            ticks = "",
            showticklabels = FALSE,
            zeroline = FALSE,
            visible = TRUE,
            range = c(range_y_min, range_y_max)
          ),
          margin = list(
            l = 5,
            r = 2,
            t = 2,
            b = 2
          ),
          dragmode = "pan",
          hoverlabel = list(
            font = list(
              size = 11,
              color = "black"
            ),
            bgcolor = "lightgrey"
          )
        )
      if ( preferences$use_webgl == TRUE ) {
        plot %>% plotly::toWebGL()
      } else {
        plot
      }
    }
  }
})


##----------------------------------------------------------------------------##
## DATA: DIMENSION REDUCTION PLOT
##----------------------------------------------------------------------------##

expression_dim_reduction_plot_data <- reactive({
  req(
    input[["expression_dim_reduction_plot_projection_select"]],
    input[["expression_dim_reduction_plot_cluster_select"]],
    input[["expression_dim_reduction_plot_plotting_order"]]
  )
  
  projection_to_display <- input[["expression_dim_reduction_plot_projection_select"]]
  clusters_to_display <- input[["expression_dim_reduction_plot_cluster_select"]]
  plot_order <- input[["expression_dim_reduction_plot_plotting_order"]]
  
  
  # in case wrong projection names are loaded in control:
  projections_available <- names(sample_data()$projections)
   if(!(projection_to_display %in% projections_available)) {
     projection_to_display <- projections_available[1]
   }

  cells_to_display <- which(
      sample_data()$cells$cluster %in% clusters_to_display)
  
  # in case wrong cluster names are loaded in control (then cell-list is empty):
  if (is.null(cells_to_display)) {
    clusters_to_display <- sample_data()$cluster_names
    cells_to_display <- which(
      sample_data()$cells$cluster %in% clusters_to_display)
  }
  
  plot <- cbind(
      sample_data()$projections[[ projection_to_display ]][ cells_to_display , ],
      sample_data()$cells[ cells_to_display , ]
    )
  

  if ( length(genesToPlot()$genes_to_display_present) == 1 ) {
    plot$level <- genesToPlot()$genes_to_display_present %>%
      sample_data()$expression[ . , cells_to_display ]
    
    if ( plot_order == "Random" ) {
      plot <- sample(1:nrow(plot), nrow(plot)) %>%
        plot[ . , ]
    } else if ( plot_order == "Highest expression on top" ) {
      plot <- plot[ order(plot$level, decreasing = FALSE) , ]
    }
    
  } 
  
  return(plot)
  
})



##----------------------------------------------------------------------------##
## UI: EXPRESSION VIOLIN PLOT
##----------------------------------------------------------------------------##



##----------------------------------------------------------------------------##
# Expression violin plot box


# output[["expression_violin_plot_UI"]] <- renderUI({
#     if ( length(input[["expression_genes_input"]]) < 2) {
#       tagList(
#         cerebroBox(
#           title = tagList(
#             boxTitle("Expression levels by cluster"),
#             #cerebroInfoButton("expression_violin_plot_info")
#           ),
#           tagList(
#             column(width = 9, offset = 0, style = "padding: 0px;",
#                    #h1("expression by cluster")
#                    plotly::plotlyOutput("expression_violin_plot")
#             ),
#             column(width = 3, offset = 0, style = "padding-left: 20px;",
#                    uiOutput("expression_violin_plot_options")
#             )
#           )
#         )
#       ) 
#     } 
# })
  

# output[["expression_violin_plot_options"]] <- renderUI({
#   
#   tagList(
#     shinyWidgets::pickerInput(
#       "expression_violin_plot_cluster_select",
#       label = "Cluster selection",
#       choices = sample_data()$cluster_names,
#       selected = sample_data()$cluster_names,
#       options = list("actions-box" = TRUE),
#       multiple = TRUE
#     )
#   )
# })

#Observe if sample_data has changed and update select_input
#Observers use eagerevaluation (reactive lazy evaluation)!
# observeEvent(sample_data(),{
#   shinyWidgets::updatePickerInput(session, "expression_violin_plot_cluster_select",
#                     choices = sample_data()$cluster_names,
#                     selected = sample_data()$cluster_names)
# })

##----------------------------------------------------------------------------##
# Expression Violin Plot

output[["expression_violin_plot"]] <- plotly::renderPlotly({
  
  req(
    input[["expression_violin_plot_cluster_select"]]
  )
  
  # plot only drawn for none or one gene (otherwise not shown)
  if (length(genesToPlot()$genes_to_display_present) < 2 ) {
    
    selected_clusters <- input[["expression_violin_plot_cluster_select"]]
    # check if genes were entered
    if (length(genesToPlot()$genes_to_display_present) == 0 ) {
      y_level <- 0
      y_range <- c(0,1)
    } else {
      # show expression if gene name was entered
      y_level <- ~level
      y_range <- c(0, max(expression_violin_plot_data()$level) * 1.2)
    }
  
    plotly::plot_ly(
      expression_violin_plot_data(),
      x = ~cluster,
      y = y_level,
      type = "violin",
      box = list(
        visible = TRUE
      ),
      meanline = list(
        visible = TRUE
      ),
      color = ~cluster,
      colors = reactive_colors()$clusters,
      source = "subset",
      showlegend = FALSE,
      hoverinfo = "y",
      marker = list(
        size = 5
      )
    ) %>%
    plotly::layout(
      title = "",
      xaxis = list(
        title = "",
        mirror = TRUE,
        showline = TRUE,
        type = "category",
        autorange = TRUE,
        tickmode = "array",
        ticktext =~cluster
      ),
      yaxis = list(
        title = "Expression level",
        range = y_range,
        hoverformat = ".2f",
        mirror = TRUE,
        showline = TRUE
      ),
      dragmode =  "select",
      hovermode = "compare"
    )
  }
})

# info box
observeEvent(input[["expression_violin_plot_info"]], {
  showModal(
    modalDialog(
      title = expression_violin_plot_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## DATA: EXPRESSION BY CLUSTER
##----------------------------------------------------------------------------##

expression_violin_plot_data <- reactive({
  req(
    input[["expression_violin_plot_cluster_select"]]
  )
  
  clusters_to_display <- input[["expression_violin_plot_cluster_select"]]
  
  cells_to_display <- which(
    sample_data()$cells$cluster %in% clusters_to_display)
  
  # in case wrong projection names are loaded in control:
  if (is.null(cells_to_display)) {
    clusters_to_display <- sample_data()$cluster_names
    cells_to_display <- which(
      sample_data()$cells$cluster %in% clusters_to_display)
  }
    
  plot <- cbind(sample_data()$cells[ cells_to_display , ])
  
  # add experssion values if gene name is provided
  if ( length(genesToPlot()$genes_to_display_present) == 1 ) {
    plot$level <- genesToPlot()$genes_to_display_present %>%
      sample_data()$expression[ . , cells_to_display ]
  }

  # Calculate Means:
  # plot$level <- genesToPlot()$genes_to_display_present %>%
  #   sample_data()$expression[ . , cells_to_display ] %>%
  #   Matrix::colMeans()

  return(plot)
  
})




##----------------------------------------------------------------------------##
## UPDATE HEATMAP CONTROLS
##----------------------------------------------------------------------------##

##----------------------------------------------------------------------------##
# Heatmap Box

# output[["expression_heatmap_UI"]] <- renderUI({
#   
#   plot_height <- "400px"
#   
#   tagList(
#     cerebroBox(
#       title = tagList(
#         boxTitle("Average gene expression within clusters"),
#         #cerebroInfoButton("expression_violin_plot_info")
#       ),
#       tagList(
#         column(width = 9, offset = 0, style = "padding: 0px;",
#                plotly::plotlyOutput("expression_heatmap", height = plot_height)
#         )
#         ,
#         column(width = 3, offset = 0, style = "padding-left: 20px;",
#                uiOutput("expression_heatmap_options"),
#                uiOutput("expression_heatmap_color_scale_range")
#         )
#       )
#     )
#   )
# })



# heatmap controls
# output[["expression_heatmap_options"]] <- renderUI({
#   print("heatmap options")
#   tagList(
#     shinyWidgets::pickerInput(
#       "expression_heatmap_cluster_select",
#       label = "Cluster selection",
#       choices = sample_data()$cluster_names,
#       selected = sample_data()$cluster_names,  # selcected specifies initially selected values
#       options = list("actions-box" = TRUE),
#       multiple = TRUE
#     ),
#     selectInput(
#       "expression_heatmap_rescaling_select",
#       label = "Rescaling",
#       choices = c("No rescaling", "Rescale per gene"),
#       selected = "Random"
#     ),
#     selectInput(
#       "expression_heatmap_color_scale",
#       label = "Color scale",
#       choices = c("Cividis","YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","Viridis"),
#       selected = "Cividis"
#     )
#   )
# })


# Observe if sample_data has changed and update select_input
# Observers use eagerevaluation (reactive lazy evaluation)!

# update controls for expression heatmap when sample data changes
observeEvent(sample_data(),{
  
  # Cluster Selection
  # shinyWidgets::updatePickerInput(session, "expression_heatmap_cluster_select",
  #                   choices = sample_data()$cluster_names,
  #                   selected = sample_data()$cluster_names)
  
  # Color Range
  # updateSliderInput(session, "expression_heatmap_color_range_slider",
  #                   min = range[1],
  #                   max = range[2],
  #                   value = c(range[1], range[2])
  # )
  
  
})



## color scale range
# output[["expression_heatmap_color_scale_range"]] <- renderUI({
#   
#   if (!is.null((expression_heatmap_data()$av_expression)) && length(expression_heatmap_data()$av_expression) > 0)
#        {
#    
#     range<- c(min(expression_heatmap_data()$av_expression), 
#               max(expression_heatmap_data()$av_expression))
#     
#   } else {
#     range <- c(0,1)
#   }
#   
#   if ( range[1] == 0 & range[2] == 0 ) {
#     range[2] = 1
#   } else {
#     range[1] <- range[1] %>% round(digits = 2)
#     range[2] <- range[2] %>% round(digits = 2)
#   }
#   tagList(
#     sliderInput(
#       "expression_heatmap_color_range_slider",
#       label = "Range of color scale",
#       min = range[1],
#       max = range[2],
#       value = c(range[1], range[2])
#     )
#   )
# })
##----------------------------------------------------------------------------##
# Heatmap Plot

# Plot height (adapted to clusternumber and genenumber)

expression_heatmap_height <- reactive({
  gene_number <- length(expression_heatmap_data()$genes)
  cluster_number <- length(expression_heatmap_data()$clusters)
  # max height 400px, min row height 50 px (if max plot height permits this, otherwise less)
  height <- min(400, floor(150 + gene_number * max(450 / cluster_number, 50)))
  
  return(height)
  
})


# Plot
output[["expression_heatmap"]] <- plotly::renderPlotly({
  req(
    input[["expression_heatmap_cluster_select"]],
    input[["expression_heatmap_rescaling_select"]],
    input[["expression_heatmap_color_scale"]]    #,
   # input[["expression_heatmap_color_range_slider"]]
  )
  print("heatmap graph")
  print(expression_heatmap_data()$av_expression)
  color_scale <- input[["expression_heatmap_color_scale"]]
  #colorscale_min <- input[["expression_heatmap_color_range_slider"]][1]
  #colorscale_max <- input[["expression_heatmap_color_range_slider"]][2]
    
  plotly::plot_ly(
    x = expression_heatmap_data()$clusters,
    y = expression_heatmap_data()$genes,
    z = expression_heatmap_data()$av_expression,
    
    #height = expression_heatmap_height(),
    xgap = 4,
    ygap = 4,
    type = "heatmap",
    colorscale= color_scale,
    zauto = TRUE,
    #zmin = colorscale_min,
    #zmax = colorscale_max,
    reversescale = FALSE,
    colorbar = list(
      title = "Expression",
      lenmode = "pixels",
      len = 120
    )
  )%>%
    plotly::layout(
      title = "",
     
      
      xaxis = list(
        title = "",
        mirror = TRUE,
        #autorange = TRUE,
        showline = FALSE,
        type = "category"
      ),
      yaxis = list(
        #title = "Gene(s)",
        autorange = TRUE,
        hoverformat = ".2f",
        mirror = TRUE,
        showline = FALSE
      ),
      dragmode =  "select",
      hovermode = "compare"
    )
  
})

##----------------------------------------------------------------------------##
## DATA: HEATMAP
##----------------------------------------------------------------------------##


# returns list with 3 elements
# 1) $clusters: cluster names (vector)
# 2) $genes: gene names (vector)
# 3) $av_expr: average gene expression for each gene in each cluster (matrix)

expression_heatmap_data <- reactive({
  req(
    input[["expression_heatmap_cluster_select"]],
    input[["expression_heatmap_rescaling_select"]]
  )
  
  clusters_to_display <- input[["expression_heatmap_cluster_select"]]
  rescale <- input[["expression_heatmap_rescaling_select"]]
  
  cells_to_display <- which(
    sample_data()$cells$cluster %in% clusters_to_display
  )
  
  # in case wrong cluster names are loaded in control:
  if (is.null(cells_to_display)) {
    clusters_to_display <- sample_data()$cluster_names
    cells_to_display <- which(
      sample_data()$cells$cluster %in% clusters_to_display)
  }
  
  cluster_matrix <- sample_data()$cells[cells_to_display, c("cluster", "cell_barcode")]
  genes <- genesToPlot()$genes_to_display_present
  expression_matrix <- t(sample_data()$expression[genes, cells_to_display, drop=FALSE]) # drop false ==> keep dcgMatrix format also for one col
  # convert from sparse format
  expression_matrix <- as.matrix(expression_matrix)
  hmap_dataframe <- cbind(cluster_matrix, expression_matrix)
  hmap_dataframe[2] <- NULL  # remove cell barcodes
  
  
  if( ncol(hmap_dataframe) > 1) {
    # calculate average expression per cluster
    hmap_dataframe <- hmap_dataframe %>% group_by(cluster) %>% summarise_all(funs(mean))
    
    # remove cluster column
    hmap_matrix <- t(as.matrix(hmap_dataframe[-1]))
    
    #normalize if selected and more than one cluster selected
    if(rescale == "Rescale per gene" && length(clusters_to_display) > 1) {
      hmap_matrix <- t(apply(hmap_matrix, 1, function(x){(x-min(x))/(max(x)-min(x))}))
    }
    
  } else {
    hmap_matrix <- NULL
  }

  
  # generate ouput data
  hmap_data <- list()
  hmap_data[["clusters"]] <- as.vector(hmap_dataframe$cluster)
  #hmap_data[1] <- NULL
  hmap_data[["genes"]] <- as.vector(colnames(hmap_dataframe))
  hmap_data[["genes"]] <- hmap_data[["genes"]][-1]    # remove "cluster"
  hmap_data[["av_expression"]] <- hmap_matrix
  #browser()
  return(hmap_data)
})































