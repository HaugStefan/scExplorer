##----------------------------------------------------------------------------##
## Tab: Load data.
##----------------------------------------------------------------------------##



##--------------------------------------------------------------------------##
# Global variable.
##--------------------------------------------------------------------------##




##--------------------------------------------------------------------------##
# Load or update sample data.
##--------------------------------------------------------------------------##


sample_data <- reactive({
 
  if (is.null(input$table_scDatasets_rows_selected) || is.na(input$table_scDatasets_rows_selected) ) {
    sample_data <- NULL #readRDS("data/example.rds")
  } else {
    req(input$table_scDatasets_rows_selected)
    sample_data <- readRDS(paste0("data/", DStable[input$table_scDatasets_rows_selected, "FileName"]))
  }
  #get list of sample names (remove later)
  # if ( is.factor(sample_data$cells$sample) ) {
  #   sample_data$sample_names <- levels(sample_data$cells$sample)
  # } else {
  #   sample_data$sample_names <- sample_data$cells$sample %>% unique()
  # }
  #get list of cluster names (remove later)
  # if ( is.factor(sample_data$cells$cluster) ) {
  #   sample_data$cluster_names <- levels(sample_data$cells$cluster)
  # } else {
  #   sample_data$cluster_names <- sample_data$cells$cluster %>% unique() %>% sort()
  # }
  
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

#

# Dataset
output$selected_dataset <- renderUI({
  if (!is.null(input$table_scDatasets_rows_selected)) {
    
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
  } else {
    h3("No dataset selected.")
  }
})




