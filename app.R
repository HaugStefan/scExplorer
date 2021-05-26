# Stefan Haug 2021
# Institute of Genetic Epidemiology
# University Hospital Freiburg
# stefan.haug@uniklinik-freiburg.de
# Include: Copyright (c) 2020 Roman Hillje

##----------------------------------------------------------------------------##
# load libraries
##----------------------------------------------------------------------------##
library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyBS)
library(plotly)
library(dplyr)
library(Matrix)
library(DT)
library(formattable)
library(waiter)
##----------------------------------------------------------------------------##
## Custom functions.
##----------------------------------------------------------------------------##
cerebroBox <- function(title, content) {
  box(
    title = title,
    status = "primary",
    solidHeader = FALSE,
    width = 12,
    collapsible = TRUE,
    content
  )
}

cerebroInfoButton <- function(id) {
  actionButton(
    inputId = id,
    label = "info",
    icon = NULL,
    class = "btn-xs",
    title = "Show additional information for this panel."
  )
}

boxTitle <- function(title) {
  p(title, style = "padding-right: 5px; display: inline")
}

##----------------------------------------------------------------------------##
# load source code
##----------------------------------------------------------------------------##
# tab content
#source(system.file("shiny/01_load_data/UI.R", package = "cerebroApp"), local = TRUE)
source("shiny/01_load_data/UI_load_data.R", local=TRUE)
source("shiny/02_gene_expression/UI_gene_expression.R", local=TRUE)
source("shiny/03_marker_genes/UI_marker_genes.R", local=TRUE)
source("shiny/04_about/UI_about.R", local=TRUE)



##----------------------------------------------------------------------------##
# User Interface function
##----------------------------------------------------------------------------##

ui <- dashboardPage(
  title="NehpGen | scExplorer",
  skin = "black",
  dashboardHeader(
    title = span(
      tags$img(src='logo.svg',height='40'),
      tags$link(rel = "stylesheet", type = "text/css", href = "scExplorer_style.css")
      )
    ),
  
  dashboardSidebar(
    disable = FALSE,
    tags$head(tags$style(HTML(".content-wrapper {overflow-x: scroll;}"))),
    #sidebar menu rendered in server function
    sidebarMenu(
      sidebarMenuOutput("sidebar_menu")
    )
  ),
  
  dashboardBody(
    #tab items defined in seperate source files
    useShinyjs(), # used for resetting input fields
    use_waiter(), # include dependencies for loading animation
    tabItems(
      tab_load_data,
      tab_gene_expression,
      tab_marker_genes,
      tab_about
      )
  )
  
)




##----------------------------------------------------------------------------##
# Sever function
##----------------------------------------------------------------------------##

server <- function(input, output, session) {
  
  ##--------------------------------------------------------------------------##
  ## Color management.
  ##--------------------------------------------------------------------------##
  # Dutch palette from flatuicolors.com
  colorset_dutch <- c(
    "#FFC312","#C4E538","#12CBC4","#FDA7DF","#ED4C67",
    "#F79F1F","#A3CB38","#1289A7","#D980FA","#B53471",
    "#EE5A24","#009432","#0652DD","#9980FA","#833471",
    "#EA2027","#006266","#1B1464","#5758BB","#6F1E51"
  )
  
  # Spanish palette from flatuicolors.com
  colorset_spanish <- c(
    "#40407a","#706fd3","#f7f1e3","#34ace0","#33d9b2",
    "#2c2c54","#474787","#aaa69d","#227093","#218c74",
    "#ff5252","#ff793f","#d1ccc0","#ffb142","#ffda79",
    "#b33939","#cd6133","#84817a","#cc8e35","#ccae62"
  )
  
  default_colorset <- c(colorset_dutch, colorset_spanish)
  
  cell_cycle_colorset <- setNames(
    c("#45aaf2", "#f1c40f", "#e74c3c", "#7f8c8d"),
    c("G1",      "S",       "G2M",     "-")
  )
  
  ##--------------------------------------------------------------------------##
  ## Colors for samples and clusters.
  ##--------------------------------------------------------------------------##
  reactive_colors <- reactive({
    colors <- list()
    
    # grab sample-specific colors from input file if it exists, otherwise take
    # default colorset
    if ( !is.null(sample_data()$samples$colors) ) {
      colors$samples <- sample_data()$samples$colors
    } else {
      colors$samples <- default_colorset[1:length(sample_data()$sample_names)]
      names(colors$samples) <- sample_data()$sample_names
    }
    # assign respective color picker in "Color management" tab to each sample
    if ( !is.null(input[[paste0('color_sample_', levels(sample_data()$cells$sample)[1])]]) ) {
      for ( i in 1:length(colors$samples) ) {
        colors$samples[i] <- input[[paste0('color_sample_', levels(sample_data()$cells$sample)[i])]]
      }
    }
    
    # grab cluster-specific colors from input file if it exists, otherwise take
    # default colorset
    if ( !is.null(sample_data()$clusters$colors) ) {
      colors$clusters <- sample_data()$clusters$colors
    } else {
      colors$clusters <- default_colorset[1:length(sample_data()$cluster_names)]
      names(colors$clusters) <- sample_data()$cluster_names
    }
    # assign respective color picker in "Color management" tab to each cluster
    if ( !is.null(input[[paste0('color_cluster_', levels(sample_data()$cells$cluster)[1])]]) ) {
      for ( i in 1:length(colors$clusters) ) {
        colors$clusters[i] <- input[[paste0('color_cluster_', levels(sample_data()$cells$cluster)[i])]]
      }
    }
    
    return(colors)
  })
  
  ##--------------------------------------------------------------------------##
  ## Central parameters.
  ##--------------------------------------------------------------------------##
  
  # Still to add:
  # data/ folder
  
  scatter_plot_dot_size <- list(
    min = 1,
    max = 20,
    step = 1,
    default = 5
  )
  
  scatter_plot_dot_opacity <- list(
    min = 0.1,
    max = 1.0,
    step = 0.1,
    default = 1.0
  )
  
  scatter_plot_percentage_cells_to_show <- list(
    min = 10,
    max = 100,
    step = 10,
    default = 100
  )
  
  preferences <- reactiveValues(use_webgl = TRUE)
  
  
  
  ##--------------------------------------------------------------------------##
  ## Sidebar menu.
  ##--------------------------------------------------------------------------##
  output[["sidebar_menu"]] <- renderMenu({
    sidebarMenu(id = "sidebar",
                menuItem(
                  "Load dataset", tabName = "load_data",
                  icon = icon("spinner"), selected = TRUE
                ),
                menuItem(
                  "Gene expression", tabName = "gene_expression",
                  icon = icon("signal")
                ),
                menuItem(
                  "Marker Genes", tabName = "marker_genes",
                  icon = icon("list")
                ),
                menuItem(
                  "About", tabName = "about",
                  icon = icon("at")
                )
    )
  })
  
  
  ##----------------------------------------------------------------------------##
  # load table with datasets
  ##----------------------------------------------------------------------------##
  DStable <- readRDS("data/scExplorer_datasets.RDS")
  
  ##----------------------------------------------------------------------------##
  # load tab - specific sever code
  ##----------------------------------------------------------------------------##
  source("shiny/01_load_data/server_load_data.R", local=TRUE)
  source("shiny/02_gene_expression/server_gene_expression.R", local=TRUE)
  source("shiny/02_gene_expression/info_gene_expression.R", local=TRUE)
  source("shiny/03_marker_genes/server_marker_genes.R", local=TRUE)
  source("shiny/04_about/server_about.R", local=TRUE)
  
}



##----------------------------------------------------------------------------##
# Start app
##----------------------------------------------------------------------------##
shinyApp(ui, server)