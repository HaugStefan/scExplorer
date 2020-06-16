##----------------------------------------------------------------------------##
## Tab: Gene expression
##----------------------------------------------------------------------------##


# Here boxes are rendered by the server function because user actions can 
# collapse the boxes.


##----------------------------------------------------------------------------##
## UI GENE SELECTION
##----------------------------------------------------------------------------##

output[["gene_selection_UI"]] <- renderUI ({
  tagList(
    cerebroBox(
      title = "Gene Search",
      tagList(
        # gene list input
        column(width = 9, offset = 0, style = "padding: 0px;",
               selectizeInput(
                 'expression_genes_input',
                 label = NULL,
                 choices = rownames(sample_data()$expression),
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
               #uiOutput("gene_selection_UI")),
        )
      )
    )
  )
})


observeEvent(input$reset_genes, {
  shinyjs::reset("gene_selection_UI")
}) 

##----------------------------------------------------------------------------##
## UI DIMENSIONAL REDUCTION (tsne, umap)
##----------------------------------------------------------------------------##

# box structure
output[["expression_projection_UI"]] <- renderUI({
  tagList(
    cerebroBox(
      title = tagList(
        boxTitle("Dimensional reduction"),
        # actionButton(
        #   inputId = "expression_projection_info",
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
                 "expression_projection",
                 width = "auto",
                 height = 500 #"85vh"
               ),
               tags$br(),
               htmlOutput("expression_genes_displayed")
        ),
        column(width = 3, offset = 0, style = "padding-left: 20px;",
               #div(style = "padding-left: 10px;",
               uiOutput("expression_UI"),
               uiOutput("expression_color_scale_range"),
               uiOutput("expression_scales")
               #) 
        )
      )
    )
  )
})

# plot




# user interfaces
output[["expression_UI"]] <- renderUI({
  tagList(
    selectInput(
      "expression_projection_to_display",
      label = "Projection",
      choices = names(sample_data()$projections)
    ),
    shinyWidgets::pickerInput(
      "expression_clusters_to_display",
      label = "Clusters to display",
      choices = sample_data()$cluster_names,
      selected = sample_data()$cluster_names,
      options = list("actions-box" = TRUE),
      multiple = TRUE
    ),
    selectInput(
      "expression_projection_plotting_order",
      label = "Plotting order",
      choices = c("Random", "Highest expression on top"),
      selected = "Random"
    ),
    sliderInput(
      "expression_projection_dot_size",
      label = "Point size",
      min = scatter_plot_dot_size[["min"]],
      max = scatter_plot_dot_size[["max"]],
      step = scatter_plot_dot_size[["step"]],
      value = scatter_plot_dot_size[["default"]]
    ),
    selectInput(
      "expression_projection_color_scale",
      label = "Color scale",
      choices = c("YlGnBu", "YlOrRd","Blues","Greens","Reds","RdBu","viridis"),
      selected = "YlGnBu"
    )
  )
})

##----------------------------------------------------------------------------##
## UI element for color scale range in projection.
##----------------------------------------------------------------------------##
output[["expression_color_scale_range"]] <- renderUI({
  
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
    # display clusters if no genes were entered
    cluster_number <- length(unique(gene_expression_plot_data()$cluster))
    range<- c(0, cluster_number)
  } else {
    range <- range(gene_expression_plot_data()$level)
  }
  
  if ( range[1] == 0 & range[2] == 0 ) {
    range[2] = 1
  } else {
    range[1] <- range[1] %>% round(digits = 2)
    range[2] <- range[2] %>% round(digits = 2)
  }
  tagList(
    sliderInput(
      "expression_projection_color_scale_range",
      label = "Range of color scale",
      min = range[1],
      max = range[2],
      value = c(range[1], range[2])
    )
  )
})

##----------------------------------------------------------------------------##
## UI element for X and Y scales in projection.
##----------------------------------------------------------------------------##
# output[["expression_scales"]] <- renderUI({
#   req(input[["expression_projection_to_display"]])
#   projection_to_display <- input[["expression_projection_to_display"]]
#   range_x_min <- sample_data()$projections[[ projection_to_display ]][,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
#   range_x_max <- sample_data()$projections[[ projection_to_display ]][,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
#   range_y_min <- sample_data()$projections[[ projection_to_display ]][,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
#   range_y_max <- sample_data()$projections[[ projection_to_display ]][,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
#   tagList(
#     sliderInput(
#       "expression_projection_scale_x_manual_range",
#       label = "Range of X axis",
#       min = range_x_min,
#       max = range_x_max,
#       value = c(range_x_min, range_x_max)
#     ),
#     sliderInput(
#       "expression_projection_scale_y_manual_range",
#       label = "Range of Y axis",
#       min = range_y_min,
#       max = range_y_max,
#       value = c(range_y_min, range_y_max)
#     )
#   )
# })

##----------------------------------------------------------------------------##
## Reactive data: Genes from user.
##----------------------------------------------------------------------------##

# cannot use req() because it delays initialization and plot is updated only with button press so plot doesn't initialize at all
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

# select genes to be displayed
output[["expression_genes_displayed"]] <- renderText({
  paste0(
    "<b>Showing expression for ",
    length(genesToPlot()[["genes_to_display_present"]]), " gene(s):</b><br>",
    paste0(genesToPlot()[["genes_to_display_present"]], collapse = ", "),
    "<br><b>",
    length(genesToPlot()[["genes_to_display_missing"]]),
    " gene(s) are not in data set: </b><br>",
    paste0(genesToPlot()[["genes_to_display_missing"]], collapse = ", ")
  )
})

##----------------------------------------------------------------------------##
## Data tables for plots.
##----------------------------------------------------------------------------##
#w <- Waiter$new(id = "expression_projection", "expression_by_cluster", "expression_compare_genes_heatmap")

# data to plot for projection and violin plots
gene_expression_plot_data <- reactive({
  req(
    input[["expression_projection_to_display"]],
    input[["expression_clusters_to_display"]],
    input[["expression_projection_plotting_order"]]
    #input[["expression_samples_to_display"]],
    #input[["expression_percentage_cells_to_show"]],
    
  )
  
  #w$show()
  
  projection_to_display <- input[["expression_projection_to_display"]]
  clusters_to_display <- input[["expression_clusters_to_display"]]
  plot_order <- input[["expression_projection_plotting_order"]]
  
  
 
  
  #samples_to_display <- input[["expression_samples_to_display"]]
  
  #percentage_cells_show <- input[["expression_percentage_cells_to_show"]]
  
  
  # check which cells to display
  # cells_to_display <- which(
  #     (sample_data()$cells$sample %in% samples_to_display) &
  #     (sample_data()$cells$cluster %in% clusters_to_display)
  #   )
  cells_to_display <- which(
      sample_data()$cells$cluster %in% clusters_to_display
  )
  # randomly remove cells
  # if ( percentage_cells_show < 100 ) {
  #   number_of_cells_to_plot <- ceiling(
  #     percentage_cells_show / 100 * length(cells_to_display)
  #   )
  #   cells_to_display <- cells_to_display[ sample(1:length(cells_to_display), number_of_cells_to_plot) ]
  # }
  plot <- cbind(
      sample_data()$projections[[ projection_to_display ]][ cells_to_display , ],
      sample_data()$cells[ cells_to_display , ]
    )
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
    plot$level <-  0
  } else if ( length(genesToPlot()$genes_to_display_present) == 1 ) {
    plot$level <- genesToPlot()$genes_to_display_present %>%
      sample_data()$expression[ . , cells_to_display ]
  } else {
    plot$level <- genesToPlot()$genes_to_display_present %>%
      sample_data()$expression[ . , cells_to_display ] %>%
      Matrix::colMeans()
  }
  if ( plot_order == "Random" ) {
    plot <- sample(1:nrow(plot), nrow(plot)) %>%
      plot[ . , ]
  } else if ( plot_order == "Highest expression on top" ) {
    plot <- plot[ order(plot$level, decreasing = FALSE) , ]
  }
  
  
  return(plot)
})

##----------------------------------------------------------------------------##
## Projection.
##----------------------------------------------------------------------------##

output[["expression_projection"]] <- plotly::renderPlotly({
  req(
    input[["expression_projection_to_display"]],
    input[["expression_projection_dot_size"]],
    #input[["expression_projection_dot_opacity"]],
    input[["expression_projection_color_scale"]],
    input[["expression_projection_color_scale_range"]],
    #input[["expression_projection_scale_x_manual_range"]],
    #input[["expression_projection_scale_y_manual_range"]]
  )
  projection_to_display <- input[["expression_projection_to_display"]]
  range_x_min <- sample_data()$projections[[ projection_to_display ]][,1] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_x_max <- sample_data()$projections[[ projection_to_display ]][,1] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  range_y_min <- sample_data()$projections[[ projection_to_display ]][,2] %>% min() %>% "*"(ifelse(.<0, 1.1, 0.9)) %>% round()
  range_y_max <- sample_data()$projections[[ projection_to_display ]][,2] %>% max() %>% "*"(ifelse(.<0, 0.9, 1.1)) %>% round()
  
  if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
    color_scale <- 'Viridis'
  } else {
    color_scale <- input[["expression_projection_color_scale"]]
  }
  
  # check if genes were entered
  if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
    # display clusters in different colors if no genes were entered
    color_levels <- ~unclass(factor(cluster))
    show_scale <- FALSE
  } else {
    # show expression if gene name was entered
    color_levels <- ~level
    show_scale <- TRUE
  }
  
  
  # Progress bar
  # progress <- Progress$new(session, min=1, max=15)
  # on.exit(progress$close())
  # 
  # progress$set(message = 'Calculation in progress',
  #              detail = 'This may take a while...')
  # 
  # for (i in 1:15) {
  #   progress$set(value = i)
  #   Sys.sleep(0.5)
  # }
  
  
  
  if ( ncol(sample_data()$projections[[ input[["expression_projection_to_display"]] ]]) == 3 ) {
    plotly::plot_ly(
      gene_expression_plot_data(),
      x = gene_expression_plot_data()[,1],
      y = gene_expression_plot_data()[,2],
      z = gene_expression_plot_data()[,3],
      type = "scatter3d",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        color = ~level,
        #opacity = input[["expression_projection_dot_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_scale_range"]][1],
        cmax = input[["expression_projection_color_scale_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_dot_size"]]
      ),
      hoverinfo = "text",
      text = ~paste(
        "<b>Cell</b>: ", gene_expression_plot_data()$cell_barcode, "<br>",
        #"<b>Sample</b>: ", gene_expression_plot_data()$sample, "<br>",
        "<b>Cluster</b>: ", gene_expression_plot_data()$cluster, "<br>",
        "<b>Transcripts</b>: ", formatC(gene_expression_plot_data()$nUMI, format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expressed genes</b>: ", formatC(gene_expression_plot_data()$nGene, format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expression level</b>: ", formatC(gene_expression_plot_data()$level, format = "f", big.mark = ",", digits = 3)
      ),
      source = "expression_projection"
    ) %>%
    plotly::layout(
      scene = list(
        xaxis = list(
          title = colnames(gene_expression_plot_data())[1],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        yaxis = list(
          title = colnames(gene_expression_plot_data())[2],
          mirror = TRUE,
          showline = TRUE,
          zeroline = FALSE
        ),
        zaxis = list(
          title = colnames(gene_expression_plot_data())[3],
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
      gene_expression_plot_data(),
      x = gene_expression_plot_data()[,1],
      y = gene_expression_plot_data()[,2],
      type = "scatter",
      mode = "markers",
      marker = list(
        colorbar = list(
          title = "Expression"
        ),
        showscale = show_scale,
        color = color_levels,
        #opacity = input[["expression_projection_dot_opacity"]],
        colorscale = color_scale,
        cauto = FALSE,
        cmin = input[["expression_projection_color_scale_range"]][1],
        cmax = input[["expression_projection_color_scale_range"]][2],
        reversescale = TRUE,
        line = list(
          color = "rgb(196,196,196)",
          width = 1
        ),
        size = input[["expression_projection_dot_size"]]
      ),
      hoverinfo = "text",
      text = ~paste(
        "<b>Cell</b>: ", gene_expression_plot_data()$cell_barcode, "<br>",
        #"<b>Sample</b>: ", gene_expression_plot_data()$sample, "<br>",
        "<b>Cluster</b>: ", gene_expression_plot_data()$cluster, "<br>",
        "<b>Transcripts</b>: ", formatC(gene_expression_plot_data()$nUMI, format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expressed genes</b>: ", formatC(gene_expression_plot_data()$nGene, format = "f", big.mark = ",", digits = 0), "<br>",
        "<b>Expression level</b>: ", formatC(gene_expression_plot_data()$level, format = "f", big.mark = ",", digits = 3)
      ),
      source = "expression_projection"
    ) %>%
    plotly::layout(
      xaxis = list(
        title = NULL, #colnames(gene_expression_plot_data())[1],
        mirror = TRUE,
        showgrid = TRUE,
        showline = FALSE,
        ticks = "",
        showticklabels = FALSE,
        zeroline = FALSE,
        visible = TRUE,
        range = c(range_x_min, range_x_max)
        #   input[["expression_projection_scale_x_manual_range"]][1],
        #   input[["expression_projection_scale_x_manual_range"]][2]
        # )
      ),
      yaxis = list(
        title = NULL, #colnames(gene_expression_plot_data())[2],
        mirror = TRUE,
        showgrid = TRUE,
        showline = FALSE,
        ticks = "",
        showticklabels = FALSE,
        zeroline = FALSE,
        visible = TRUE,
        range = c(range_y_min, range_y_max)
        #   input[["expression_projection_scale_y_manual_range"]][1],
        #   input[["expression_projection_scale_y_manual_range"]][2]
        # )
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
})

##----------------------------------------------------------------------------##
## Projection Info box.
##----------------------------------------------------------------------------##
observeEvent(input[["expression_projection_info"]], {
  showModal(
    modalDialog(
      expression_projection_info$text,
      title = expression_projection_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})

##----------------------------------------------------------------------------##
## Export function. ==> currently not used, button in UI also commented out
##----------------------------------------------------------------------------##
# observeEvent(input[["expression_projection_export"]], {
#   req(
#     input[["expression_projection_to_display"]],
#     input[["expression_projection_plotting_order"]],
#     input[["expression_projection_dot_size"]],
#     input[["expression_projection_dot_opacity"]],
#     input[["expression_projection_color_scale"]],
#     input[["expression_projection_color_scale_range"]],
#     input[["expression_projection_scale_x_manual_range"]],
#     input[["expression_projection_scale_y_manual_range"]]
#   )
#   library("ggplot2")
#   if ( exists("plot_export_path") ) {
#     xlim <- c(
#       input[["expression_projection_scale_x_manual_range"]][1],
#       input[["expression_projection_scale_x_manual_range"]][2]
#     )
#     ylim <- c(
#       input[["expression_projection_scale_y_manual_range"]][1],
#       input[["expression_projection_scale_y_manual_range"]][2]
#     )
#     if ( length(genesToPlot()$genes_to_display_present) == 0 ) {
#       out_filename <- paste0(
#         plot_export_path, "Cerebro_",
#         sample_data()$experiment$experiment_name, "_gene_expression_none"
#       )
#     } else if ( length(genesToPlot()$genes_to_display_present) == 1 ) {
#       out_filename <- paste0(
#         plot_export_path, "Cerebro_",
#         sample_data()$experiment$experiment_name, "_gene_expression_",
#         genesToPlot()$genes_to_display_present, "_",
#         input[["expression_projection_to_display"]]
#       )
#     } else {
#       out_filename <- paste0(
#         plot_export_path, "Cerebro_",
#         sample_data()$experiment$experiment_name, "_gene_expression_",
#         genesToPlot()$genes_to_display_present[1],
#         "_and_others_", input[["expression_projection_to_display"]]
#       )
#     }
# 
#     if ( input[["expression_projection_plotting_order"]] == "Random" ) {
#       out_filename <- paste0(out_filename, "_random_order.pdf")
#     } else if ( input[["expression_projection_plotting_order"]] == "Highest expression on top" ) {
#       out_filename <- paste0(out_filename, "_highest_expression_on_top.pdf")
#     }
# 
#     if ( ncol(sample_data()$projections[[ input[["expression_projection_to_display"]] ]]) == 3 ) {
#       shinyWidgets::sendSweetAlert(
#         session = session,
#         title = "Sorry!",
#         text = "It's currently not possible to create PDF plots from 3D dimensional reductions. Please use the PNG export button in the panel or a 2D dimensional reduction instead.",
#         type = "error"
#       )
#     } else {
#       p <- ggplot(
#           gene_expression_plot_data(),
#           aes_q(
#             x = as.name(colnames(gene_expression_plot_data())[1]),
#             y = as.name(colnames(gene_expression_plot_data())[2]),
#             fill = as.name("level")
#           )
#         ) +
#         geom_point(
#           shape = 21,
#           size = input[["expression_projection_dot_size"]]/3,
#           stroke = 0.2,
#           color = "#c4c4c4",
#           alpha = input[["expression_projection_dot_opacity"]]
#         ) +
#         lims(x = xlim, y = ylim) +
#         theme_bw()
# 
#         if ( input[["expression_projection_color_scale"]] == 'viridis' ) {
#           p <- p + viridis::scale_fill_viridis(
#             option = "viridis",
#             limits = input[["expression_projection_color_scale_range"]],
#             oob = scales::squish,
#             direction = -1,
#             name = "Log-normalised\nexpression",
#             guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
#           )
#         } else {
#           p <- p + scale_fill_distiller(
#             palette = input[["expression_projection_color_scale"]],
#             limits = input[["expression_projection_color_scale_range"]],
#             oob = scales::squish,
#             direction = 1,
#             name = "Log-normalised\nexpression",
#             guide = guide_colorbar(frame.colour = "black", ticks.colour = "black")
#           )
#         }
# 
#       pdf(NULL)
#       ggsave(out_filename, p, height = 8, width = 11)
# 
#       if ( file.exists(out_filename) ) {
#         shinyWidgets::sendSweetAlert(
#           session = session,
#           title = "Success!",
#           text = paste0("Plot saved successfully as: ", out_filename),
#           type = "success"
#         )
#       } else {
#         shinyWidgets::sendSweetAlert(
#           session = session,
#           title = "Error!",
#           text = "Sorry, it seems something went wrong...",
#           type = "error"
#         )
#       }
#     }
#   } else {
#     shinyWidgets::sendSweetAlert(
#       session = session,
#       title = "Error!",
#       text = "Sorry, we couldn't find a place to store the figure. Please submit an issue on GitHub @ https://github.com/romanhaa/cerebroApp",
#       type = "error"
#     )
#   }
# })




##----------------------------------------------------------------------------##
## Expression by cluster.
##----------------------------------------------------------------------------##

# box plot
output[["expression_by_cluster"]] <- plotly::renderPlotly({
  plotly::plot_ly(
    gene_expression_plot_data(),
    x = ~cluster,
    y = ~level,
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
      showline = TRUE
    ),
    yaxis = list(
      title = "Expression level",
      range = c(0, max(gene_expression_plot_data()$level) * 1.2),
      hoverformat = ".2f",
      mirror = TRUE,
      showline = TRUE
    ),
    dragmode =  "select",
    hovermode = "compare"
  )
})

# info box
observeEvent(input[["expression_by_cluster_info"]], {
  showModal(
    modalDialog(
      expression_by_cluster_info$text,
      title = expression_by_cluster_info$title,
      easyClose = TRUE,
      footer = NULL
    )
  )
})


##----------------------------------------------------------------------------##
## Heatmap, calculate average expression table.
##----------------------------------------------------------------------------##

# returns list with 3 elements
# 1) $clusters: cluster names (vector)
# 2) $genes: gene names (vector)
# 3) $av_expr: average gene expression for each gene in each cluster (matrix)

gene_expression_heatmap_data <- reactive({
  req(
      input[["expression_clusters_to_display"]]
     )
  
  clusters_to_display <- input[["expression_clusters_to_display"]]
  cells_to_display <- which(
     sample_data()$cells$cluster %in% clusters_to_display
    )
  
  cluster_matrix <- sample_data()$cells[cells_to_display, c("cluster", "cell_barcode")]
  genes <- genesToPlot()$genes_to_display_present
  expression_matrix <- t(sample_data()$expression[genes, cells_to_display, drop=FALSE]) # drop false ==> keep dcgMatrix format also for one col
  # convert from sparse format
  expression_matrix <- as.matrix(expression_matrix)
  hmap_dataframe <- cbind(cluster_matrix, expression_matrix)
  hmap_dataframe[2] <- NULL  # remove cell barcodes
  hmap_dataframe <- hmap_dataframe %>% group_by(cluster) %>% summarise_all(funs(mean))
  
  hmap_datalist <- list()
  hmap_datalist[["clusters"]] <- as.vector(hmap_dataframe$cluster)
  hmap_dataframe[1] <- NULL
  hmap_datalist[["genes"]] <- as.vector(colnames(hmap_dataframe))
  hmap_datalist[["av_expression"]] <- t(as.matrix(hmap_dataframe))

  return(hmap_datalist)
})




##----------------------------------------------------------------------------##
## Heatmap, compare gene Expression.
##----------------------------------------------------------------------------##


output[["expression_compare_genes_heatmap"]] <- plotly::renderPlotly({
  
  # gene_number <- length(gene_expression_heatmap_data()$genes)
  # plot_height <- paste0(gene_number * 20,"px")
  plotly::plot_ly(
    x = gene_expression_heatmap_data()$clusters,
    y = gene_expression_heatmap_data()$genes,
    z = gene_expression_heatmap_data()$av_expression,
    
    xgap = 5,
    ygap = 5,
    type = "heatmap"
  )%>%
    plotly::layout(
      title = "",
      #height = plot_height,
      xaxis = list(
        title = "",
        mirror = TRUE,
        autorange = TRUE,
        showline = FALSE
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

# output[["expression_by_cluster"]] <- plotly::renderPlotly({
#   plotly::plot_ly(
#     gene_expression_plot_data(),
#     x = ~cluster,
#     y = ~level,
#     type = "violin",
#     box = list(
#       visible = TRUE
#     ),
#     meanline = list(
#       visible = TRUE
#     ),
#     color = ~cluster,
#     colors = reactive_colors()$clusters,
#     source = "subset",
#     showlegend = FALSE,
#     hoverinfo = "y",
#     marker = list(
#       size = 5
#     )
#   ) %>%
#     plotly::layout(
#       title = "",
#       xaxis = list(
#         title = "",
#         mirror = TRUE,
#         showline = TRUE
#       ),
#       yaxis = list(
#         title = "Expression level",
#         range = c(0, max(gene_expression_plot_data()$level) * 1.2),
#         hoverformat = ".2f",
#         mirror = TRUE,
#         showline = TRUE
#       ),
#       dragmode =  "select",
#       hovermode = "compare"
#     )
# })
# 
# # info box
# observeEvent(input[["expression_by_cluster_info"]], {
#   showModal(
#     modalDialog(
#       expression_by_cluster_info$text,
#       title = expression_by_cluster_info$title,
#       easyClose = TRUE,
#       footer = NULL
#     )
#   )
# })