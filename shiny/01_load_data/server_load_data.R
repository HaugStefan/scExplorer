##----------------------------------------------------------------------------##
## Tab: Load data.
##----------------------------------------------------------------------------##


##--------------------------------------------------------------------------##
# Load or update sample data.
##--------------------------------------------------------------------------##
sample_data <- reactive({
 
  if (is.null(input$table_scDatasets_rows_selected) || is.na(input$table_scDatasets_rows_selected) ) {
    sample_data <- NULL #readRDS("data/example.rds")
  } else {
    req(input$table_scDatasets_rows_selected)
    if(DStable[input$table_scDatasets_rows_selected, "Access"]=="public") {
        sample_data <- readRDS(paste0("data/", DStable[input$table_scDatasets_rows_selected, "FileName"]))
    } else {
        sample_data <- NULL
    }
  }

  sample_data
  
})


##--------------------------------------------------------------------------##
# dataset table
##--------------------------------------------------------------------------##
output$table_scDatasets <- DT::renderDataTable(
  # format datatable
  formattable::as.datatable(
    formattable(DStable) ,     # needs to be converted to formattable-table first.
    selection = 'single',
    class = "stripe hover select-checkbox",
    rownames = FALSE,
    options = list(
      dom = 't',                               #"Blfrtip" ==> "l" is for length of page.
      pageLength = 25,
      order = list(0, "asc"),
      columnDefs = list(
        list(width = "7%", class="dt-left", targets = list(0)),
        list(width = "7%", class="dt-left", targets = list(1,2,3)),
        list(visible=FALSE, targets= list(4,5,6))
      )
    )
  ),
  server = FALSE,
)

##--------------------------------------------------------------------------##
# display of selected dataset
##--------------------------------------------------------------------------##
# Dataset
output$selected_dataset <- renderUI({
  if (!is.null(input$table_scDatasets_rows_selected)) {
    
    if(DStable[input$table_scDatasets_rows_selected, "Access"]=="public") {
    
        tagList(
          strong(h3(as.character(DStable[input$table_scDatasets_rows_selected, "Dataset"]))),
          p(as.character(DStable[input$table_scDatasets_rows_selected, "Organism"]), 
            as.character(DStable[input$table_scDatasets_rows_selected, "Organ"]),
            ", ",
            as.character(DStable[input$table_scDatasets_rows_selected, "CellNumber"]),
            " ",
            as.character(DStable[input$table_scDatasets_rows_selected, "CellsNuclei"])
            ),
          p(strong(as.character(DStable[input$table_scDatasets_rows_selected, "Clusters"]),
                   " Clusters:"),
            br(),
            paste(sample_data()$cluster_names, sep = ", ", collapse = ", ")),
          p(strong(" Comments:"),
            br(),
            as.character(DStable[input$table_scDatasets_rows_selected, "Comment"]))
        )
    } else if (DStable[input$table_scDatasets_rows_selected, "Access"]=="CRC 1453 members") {
          tagList(
           strong(h3(as.character(DStable[input$table_scDatasets_rows_selected, "Dataset"]))),
            p("This dataset is accessible for CRC members only.", 
              "Please ", tags$a(href="http://nephgen-intern.imbi.uni-freiburg.de", "log in.")
              
            )
            )

}
  } else {
    h3("No dataset selected.")
  }
})



