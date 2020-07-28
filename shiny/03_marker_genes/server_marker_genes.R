##----------------------------------------------------------------------------##
## Tab: Marker genes.
##----------------------------------------------------------------------------##

# Currently selected dataset (displayed in title)
output$selected_dataset_short_markergenes <- renderText({
  if (!is.null(input$table_scDatasets_rows_selected)) {
    
    paste0("(selected dataset: ",
           as.character(DStable[input$table_scDatasets_rows_selected, "Dataset"]),
           ")")
    
  } else {
    "(no dataset selected)"
  }
})

# UI element
output[["marker_genes_by_cluster_UI"]] <- renderUI({
  if ( !is.null(sample_data()$marker_genes) && is.data.frame(sample_data()$marker_genes)) {
      fluidRow(
        column(12,
               selectInput(
                 "marker_genes_by_cluster_input",
                 label = NULL,
                 choices =  c("Select Cluster Here" = "", "All Clusters (for Download)" = "all", as.vector(sample_data()$cluster_names)) 
               ),
               DT::dataTableOutput("marker_genes_by_cluster_table_present")
        )
      )
  } else {
    p("No dataset loaded or marker genes missing/not generated.")
    #textOutput("marker_genes_by_cluster_table_missing")
  }
})

# table
output[["marker_genes_by_cluster_table_present"]] <- DT::renderDataTable(server = FALSE, {
  req(
    input[["marker_genes_by_cluster_input"]]
    )
  
    if (input[["marker_genes_by_cluster_input"]] == "all") {
      m_table <- sample_data()$marker_genes
      
    } else {
      
      m_table <- sample_data()$marker_genes[ which(sample_data()$marker_genes$Cluster == input[["marker_genes_by_cluster_input"]]) ,]
    } 
  
    # number formating
    m_table$"log Fold Change" <- round(m_table$"log Fold Change", digits=3)
    m_table$"% Cells in Cluster Expressing Gene" <- formattable::percent(m_table$"% Cells in Cluster Expressing Gene")
    m_table$"% cells in All Other Clusters Expressing Gene" <- formattable::percent(m_table$"% cells in All Other Clusters Expressing Gene")
    m_table <- m_table %>% mutate("p Value"=formatC(m_table$"p Value", format="e", digits=3))
    m_table <- m_table %>% mutate("p Value adjusted"=formatC(m_table$"p Value adjusted", format="e", digits=3))
    
    # format datatable
    formattable::as.datatable(
      formattable(m_table) ,     # needs to be converted to formattable-table first.
      #filter = "top",             Don't understand the filter yet.
      #selection = "none",
      rownames = FALSE,
      #autoHideNavigation = TRUE,
      escape = FALSE,
      extensions = c("Buttons"),
      class = "stripe hover cell-border",
      options = list(
        dom = '<<B><"spacer_markergenetable"l><frtip>>',                               #"Blfrtip" ==> "l" is for length of page.
        lengthMenu = c(25, 50, 100),
        pageLength = 25,
        order = list(2, "asc"),
        columnDefs = list(
          list(width = '15%', targets = list(0,1,2,3,4,5)), 
          list(targets=1:6, class="dt-right")
          ),
        buttons = list(
          "colvis",
          list(
            extend = "collection",
            text = "Download",
            buttons = list(
              list(
                extend = "csv",
                filename = "marker_genes_by_cluster",
                title = "Marker genes by cluster"
              ),
              list(
                extend = "excel",
                filename = "marker_genes_by_cluster",
                title = "Marker genes by cluster"
              )
            )
          )
        )
       )
      )
})

# alternative text
output[["marker_genes_by_cluster_table_no_markers_found"]] <- renderText({
  "No marker genes identified for any of the clusters."
})

# alternative text
output[["marker_genes_by_cluster_table_missing"]] <- renderText({
  "Data not available. Possible reasons: Only 1 cluster in this data set or data not generated."
})

# info box
observeEvent(input[["marker_genes_by_cluster_info"]], {
  showModal(
    modalDialog(
      marker_genes_by_cluster_info[["text"]],
      title = marker_genes_by_cluster_info[["title"]],
      easyClose = TRUE,
      footer = NULL
    )
  )
})