#' Dropdown (select input) element
#' 
#' @param id Namespace ID for scoping
#' @param label Label for dropdown field
#' @return Namespaced UI element
choice_ui <- function(id, label) {
  ns <- NS(id)
  tagList(
    selectInput(ns("choice"), label = label, choices = c())
  )
}

#' Dropdown server logic
#' 
#' @param id Namespace ID for scoping
#' @param object Dropdown which triggers select input choice changes
#' @param choices Reactive character array containing selection choices
#' @return Reactive user choice input
choice_server <- function(id, object, choices) {
  moduleServer( id, function(input, output, session) {
    observeEvent(
      eventExpr = object(),
      handlerExpr = {
        updateSelectInput(
          session = session, 
          inputId = "choice", 
          choices = choices()
        )
      }
    )
    return(
      reactive({
        input$choice
      })
    )
  })
}

#' Regression checkbox element for displaying a regression 
#' line in scatter plot
#' 
#' @param id Namespace ID for scoping
#' @param label Label for dropdown field
#' @return Namespaced UI element
select_regression_ui <- function(id, label){
  ns <- NS(id)
  tagList(
    checkboxInput(
      inputId = ns("choice"),
      label = label,
      value = FALSE
    )
  )
}

#' Regression checkbox module server
#' 
#' @param id Namespace ID for scoping
#' @return Reactive user choice input
select_regression_server <- function(id) {
  moduleServer( id, function(input, output, session) {
    return(
      reactive({
        input$choice
      })
    )
  })
}

#' Regression method select input, lets users choose
#' a regression method.
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI element
select_regression_method <- function(id){
  ns <- NS(id)
  tagList(
    selectInput(
      inputId = ns("choice"), 
      label = "Select smooth method", 
      choices = c("lm - Linear Model","loess - Polynomial Fit"))
  )
}

#' Regression select module server
#' 
#' @param id Namespace ID for scoping
#' @return Reactive user choice input
select_regression_method_server <- function(id) {
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
scatter_plot_axis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("scale"), label = NULL, choices = c("Linear","Logarithmic"))
  )
}

#' Dropdown server logic
#' 
#' @param id Namespace ID for scoping
#' @param object Dropdown which triggers select input choice changes
#' @param choices Reactive character array containing selection choices
#' @return Reactive user choice input
scatter_plot_axis_server <- function(id, object, choices) {
  moduleServer( id, function(input, output, session) {
    return(
      reactive({
        input$scale
      })
    )
  })
}

#' Basic plot UI element
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(color="#0dc5c1", plotOutput(ns("plot"), click = "plot_click"))
  ) 
}

#' Trends plot title UI
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_results_trends_title_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmlOutput(ns("trends_title"))
  )
}

#' Trends plot title server
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_results_trends_title_server <- function(id, options) {
  moduleServer( id, function(input, output, session){    
    output$trends_title <- renderUI({
      parameter <- options$parameter()
      HTML(paste0("<h3>Trends in ", parameter, "</h3>"))
    })
  })
}

#' Plotly plot UI element
#' 
#' @param id Namespace ID for scoping
#' @return Namespaced UI elements
mod_results_plotly_ui <- function(id){
  ns <- NS(id)
  tagList(
    withSpinner(color="#0dc5c1", plotlyOutput(ns("plot")))
  ) 
}

#' Core server logic for displaying results page plotly plot.
#' 
#' @param id Namespace ID for scoping
#' @param dataset dataframe containing uploaded data
#' @param options Reactive values for user dropdown choices
#' @return Plot object for rendering in shiny application
mod_results_plotly_server <- function(id, dataset, options){

    moduleServer(id, function(input, output, session) {
    output$plot <- renderPlotly({

      # Grab parameters
      watershed <- options$watershed()
      station <- options$station()
      parameter <- options$parameter()
      unit <- options$unit()
      add_regression_line <- options$add_regression_line()
      regression_method <- options$regression_method()

      # Update dataset
      df <- dataset()

      # Populate vectors for plotting
      df$dates <- as.POSIXct(as.character(df$Date), tryFormats = options$date_formats())
      df$dates_as_numeric <- as.numeric(df$dates)
      df$vals <- as.numeric(df$Result)

      # Generate plot
      p <- ggplot(df, aes( x = dates, y = vals))
      if ( exists(watershed) ) {
        p <- p + geom_point(
          aes(
            text = sprintf("Watershed: %s<br>Station: %s", watershed, station)
          )
        )
      } else {
        p <- p + geom_point(
          aes(
            text = sprintf("Station: %s", station)
          )
        )
      }

      # Add axes.
      p <- p + xlab("Date") +
        ylab(sprintf("%s (%s)", parameter, unit))

      # Return appropriate GGplot depending on regression method
      if (add_regression_line) {
        if (regression_method == "lm - Linear Model") { #method = lm
          p <- p + geom_smooth(method = lm)
        } else if (regression_method == "loess - Polynomial Fit") {
          p <- p + geom_smooth(method = loess)
        }
      }

      if (options$scale() == "Logarithmic") {
        p <- p  + scale_y_log10()
      }

      ggplotly(p)
    })
  })
}