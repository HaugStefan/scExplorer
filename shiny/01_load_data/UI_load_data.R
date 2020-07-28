##----------------------------------------------------------------------------##
# Tab: Load data.
##----------------------------------------------------------------------------##

tab_load_data <- tabItem(
  tabName = "load_data",
  
  tagList(
    fluidRow(
      column(
        width = 6,
        style = "padding:0px",
        box(
          title = "Select Dataset:",
          solidHeader = FALSE,
          status = "primary",
          width = 12,
          p("Please click on row to select."),
          DTOutput("table_scDatasets"),
          br(),
          br(),
          p("The list of datasets will be regularly updated. Last update: 28.07.2020")
        )
        
      ),
      column(
        width = 6,
        style = "padding:0px",
        # use div to move second box a little bit to the left
        div(style = "margin-left: -15px",
          box(
            title = "Selection:",
            solidHeader = FALSE,
            status = "primary",
            width = 12,
            tagList(
              uiOutput("selected_dataset")
            )
          )
        ) 
      )
    )
  )
)
  
  
  
