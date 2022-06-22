#' CEDEN Format check box
#' 
#' @param id Namespace ID for scoping
#' @param label Label for dropdown field
#' @return Namespaced UI element
select_ceden_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    checkboxInput(
      inputId = ns("choice"),
      label = label,
      value = TRUE
    )
  )
}

#' Regression checkbox module server
#' 
#' @param id Namespace ID for scoping
#' @return Reactive user choice input
select_ceden_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    return(
      reactive({
        input$choice
      })
    )
  })
}

#' Dropdown (select input) element
#' 
#' @param id Namespace ID for scoping
#' @param label Label for dropdown field
#' @return Namespaced UI element
upload_format_choice_ui <- function(id, label) {
  ns <- NS(id)
  tagList(
    selectInput(
      ns("choice"), 
      label = label, 
      choices = c(
        "CEDEN Direct Download",
        "CEDEN Modified Download",
        "CEDEN Submission Template",
        "General Template")
      # choices = c("CEDEN Download","General Template")
    )
  )
}

#' Dropdown server logic
#' 
#' @param id Namespace ID for scoping
#' @param object Dropdown which triggers select input choice changes
#' @param choices Reactive character array containing selection choices
#' @return Reactive user choice input
upload_format_choice_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    return(
      # reactive({
        input$choice
      # })
    )
  })
}

#' Summary table UI module layout
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_inventory_ui <- function(id){
  ns <- NS(id)
  tagList(
    #### DATA TABLE ####
    dataTableOutput(ns("summary"))
  ) 
}

#' Inventory server logic. Requires some data handling to display
#' important columns only. We also manage the "first_run" variable
#' which allows us to lock out analysis tabs if not enough data selected.
#' 
#' @param id Namespace ID for scoping
#' @param dataset Dataframe containing dataset
#' @return Module server object
mod_inventory_server <- function(id, dataset, options){
  moduleServer(id, function(input, output, session) {

    #### DATA TABLE ####
    output$summary <- renderDataTable({
      data <- dataset()

      # Supported columns
      supported_cols <- c(
        "Watershed",
        "StationCode",
        "Parameter",
        "Units",
        "Date",
        "Result",
        "Fraction"
      )
      
      # Set aside vector for available columns
      summary_cols <- c()

      # Load available columns for summary display
      for (col in supported_cols){
        if (col %in% colnames(data)) {
          summary_cols <- append(summary_cols, col)
        }
      }

      # Create dataframe
      df <- data.frame(matrix(ncol = 0, nrow = nrow(data)))
      for (col in summary_cols){
        df[[col]] <- data[[col]]
      }

      # Use "<<-" to update global variable outside of current scope
      if (run_cnt > 0){
        run_cnt <<- run_cnt-1
      } else {
        first_run <<- FALSE
      }

      summary(df)
    })
  })
}