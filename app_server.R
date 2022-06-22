#' Defines Shiny server 
#'
#' @param input Object for referencing UI elements
#' @param output Object for referencing data obtained by UI
#' @param session Session information
#' @return Shiny server function
app_server <- function(input, output, session) {

  ########### DATA SET ###########

  # Create parameter for holding important values
  data <- reactiveValues()

  # Options object for holding reactive filter values
  options <- reactiveValues()

  # Validation messages
  validation_messages <- c(
    "too_few" = "Sorry, you do not have enough data selected to display this plot.",
    "station" = "Cannot choose more than one station.",
    "no_variance" = "You can greatly reduce your sampling effort and still detect a trend.",
    "no_opts" = "Computations show you cannot detect a 100% change with double your effort.",
    "blank" = " ")

  # Date formats
  options$date_formats <- reactive({ return(c("%Y-%m-%d","%m/%d/%Y","%d/%b/%Y")) })

  # Minimum acceptable N
  options$min_n <- reactive({ return(10) })

  # Minimum acceptable variance
  options$min_variance <- reactive({ return(0) })

  # Track first run, user counter since menus render a few times
  first_run <<- TRUE
  run_cnt <<- 2
  # simulation_complete <<- FALSE

  # Load uploaded dataset if present; otherwise, use dropdown selected dataset
  data$dataset_path <- reactive({
    # input_file <- input$file_upload
    if (is.null(input$file_upload)){
      return(file.path("./data",input$dataset))
    } else {
      return(input$file_upload$datapath)
    }
  })

  # Reset filters and dataset button 
  # observeEvent(input$reset, {
  #   data$dataset_path <- reactive({
  #     return(file.path("./data",input$dataset))
  #   })
  # })

  ########### Data Loading Functions ###########
  # These functions are used to load data using formats listed
  # within the upload data dropdown.

  # Example data sets
  read_csv <- function(data_path, as_is) {
    return(
      read.table(
        data_path,
        header = TRUE,
        sep = ",",
        as.is = as_is
      )
    ) 
  }

  # CEDEN Direct Download
  read_ceden_dd <- function(data_path, as_is) {
    return(
      read.csv(
        data_path,
        skip = 2,
        header = TRUE,
        sep = "\t",
        as.is = as_is
      )
    )
  }

  # CEDEN Modified Download
  read_ceden_md <- function(data_path, as_is) {
    return(
      read.csv(
        data_path,
        skip = 1,
        header = TRUE,
        sep = "\t",
        as.is = as_is
      )
    )
  }

  # CEDEN Submission Template
  read_ceden_excel <- function(data_path) {
    return(
      read_excel(data_path, sheet = "ChemResults")
    )
  }

  # General Template
  read_general_template <- function(data_path) {
    return(
      read_excel(data_path, sheet = 1)
    )
  }

  ########### Dataset for populating select dropdowns. ###########
  # Load two datasets, one for use throught the application for calculations
  # and visualizations, another for viewing in the summary table. We then update
  # column names using YAML files corresponding to appropriate upload format.

  # Load data frame for data to use throughout application
  data$dataset <- reactive({

    # Load proper config file, depending on selected upload format
    config_file <- "./config/parameters.yml"
    format <- isolate(upload_format_choice_server("upload_format"))
    if (!is.null(input$file_upload)) {
      if (format == "CEDEN Direct Download") {
        config_file <- "./config/ceden_download.yml"
      } else if (format == "CEDEN Modified Download") {
        config_file <- "./config/ceden_download.yml"
      } else if (format == "CEDEN Submission Template"){
        config_file <- "./config/ceden_sub_template.yml"
      }
    }
    config <- yaml.load_file(config_file)

    # Read data, update column names
    if (!is.null(input$file_upload)) {

      is_ceden <- FALSE
      if (format == "CEDEN Direct Download") {
        upload <- read_ceden_dd(data$dataset_path(), as_is = TRUE)
        is_ceden <- TRUE
      } else if (format == "CEDEN Modified Download") {
        upload <- read_ceden_md(data$dataset_path(), as_is = TRUE)
        is_ceden <- TRUE
      } else if (format == "CEDEN Submission Template"){
        upload <- read_ceden_excel(data$dataset_path())
        is_ceden <- TRUE
      } else if (format == "General Template") {
        upload <- read_general_template(data$dataset_path())
      } else {
        upload <- read_csv(data$dataset_path(), as_is = TRUE)
      }

      # update column names if loading CEDEN format
      if (is_ceden){
        colnames(upload)[which(names(upload) == config$watersheds)] <- "Watershed"
        colnames(upload)[which(names(upload) == config$stations)] <- "StationCode"
        colnames(upload)[which(names(upload) == config$parameters)] <- "Parameter"
        colnames(upload)[which(names(upload) == config$units)] <- "Units"
        colnames(upload)[which(names(upload) == config$dates)] <- "Date"
        colnames(upload)[which(names(upload) == config$results)] <- "Result"
        colnames(upload)[which(names(upload) == config$fraction)] <- "Fraction"
      }      
      
    } else {
      upload <- read_csv(data$dataset_path(), as_is = TRUE)
    }

    upload <- upload[!is.na(upload$Result),]
    upload <- upload[!is.na(upload$Date),]
    
    return(upload)
  })

  # Load dataset for use with the data summary table.
  data$dataset_for_summary <- reactive({

    # Load proper config file, depending on selected upload format
    config_file <- "./config/parameters.yml"
    format <- isolate(upload_format_choice_server("upload_format"))
    if (!is.null(input$file_upload)) {
      if (format == "CEDEN Direct Download") {
        config_file <- "./config/ceden_download.yml"
      } else if (format == "CEDEN Modified Download") {
        config_file <- "./config/ceden_download.yml"
      } else if (format == "CEDEN Submission Template"){
        config_file <- "./config/ceden_sub_template.yml"
      }
    }
    config <- yaml.load_file(config_file)

    # Read data, update column names
    if (!is.null(input$file_upload)) {

      is_ceden <- FALSE
      if (format == "CEDEN Direct Download") {
        upload <- read_ceden_dd(data$dataset_path(), as_is = FALSE)
        is_ceden <- TRUE
      } else if (format == "CEDEN Modified Download") {
        upload <- read_ceden_md(data$dataset_path(), as_is = FALSE)
        is_ceden <- TRUE
      } else if (format == "CEDEN Submission Template"){
        upload <- read_ceden_excel(data$dataset_path())
        is_ceden <- TRUE
      } else if (format == "General Template") {
        upload <- read_general_template(data$dataset_path())
      } else {
        upload <- read_csv(data$dataset_path(), as_is = FALSE)
      }

      # update column names if loading CEDEN format
      if (is_ceden){
        colnames(upload)[which(names(upload) == config$watersheds)] <- "Watershed"
        colnames(upload)[which(names(upload) == config$stations)] <- "StationCode"
        colnames(upload)[which(names(upload) == config$parameters)] <- "Parameter"
        colnames(upload)[which(names(upload) == config$units)] <- "Units"
        colnames(upload)[which(names(upload) == config$dates)] <- "Date"
        colnames(upload)[which(names(upload) == config$results)] <- "Result"
        colnames(upload)[which(names(upload) == config$fraction)] <- "Fraction"
      }

      # Convert appropriate columns to factors for summary table.
      if (format == "CEDEN Submission Template" || format == "General Template"){
        if (length(upload$Result) == length(upload$Watershed)){
          upload$Watershed <- as.factor(upload$Watershed)
        }

        if (length(upload$Result) == length(upload$StationCode)){
          upload$StationCode <- as.factor(upload$StationCode)
        }

        if (length(upload$Result) == length(upload$Units)){
          upload$Units <- as.factor(upload$Units)
        }

        if (length(upload$Result) == length(upload$Fraction)){
          upload$Fraction <- as.factor(upload$Fraction)
        }
        
        upload$Parameter <- as.factor(upload$Parameter)
      }
      
    } else {
      upload <- read_csv(data$dataset_path(), as_is = FALSE)
    }

    # Remove rows where Result or Date are null
    upload <- upload[!is.na(upload$Result),]
    upload <- upload[!is.na(upload$Date),]

    # upload %>% 
    #   filter(! is.na(Result) | ! is.na(Date))
    
    return(upload)
  })  

  ########### DATASET FILTER OPTIONS ###########

  # We incrementally populate filters by filtering the dataset
  # as dropdown selections are made. Since choices are reactive
  # objects any interdependent components (e.g. plots, other dropdowns)
  # are updated as needed.

  # Watershed
  options$watershed <- choice_server(
    "choice_ui_watershed", 
    object = data$dataset,
    choices = get_field(data$dataset,"Watershed")
  ) 
  filtered_by_watershed <- filter_by_watershed(
    dataset = data$dataset,
    value = options$watershed
  )
  filtered_by_watershed_summary <- filter_by_watershed(
    dataset = data$dataset_for_summary,
    value = options$watershed
  )

  #Station
  options$station <- choice_server(
    "choice_ui_station", 
    object = filtered_by_watershed,
    choices = get_field(filtered_by_watershed,"StationCode")
  )
  filtered_by_station <- filter_by_station(
    dataset = filtered_by_watershed,
    value = options$station
  )
  filtered_by_station_summary <- filter_by_station(
    dataset = filtered_by_watershed_summary,
    value = options$station
  )
  # data$filtered_by_station <- filtered_by_station

  # Parameter
  options$parameter <- choice_server(
    "choice_ui_parameter", 
    object = filtered_by_station,
    choices = get_field(filtered_by_station,"Parameter")
  )
  filtered_by_parameter <- filter_by_parameter(
    dataset = filtered_by_station,
    value = options$parameter
  )
  filtered_by_parameter_summary <- filter_by_parameter(
    dataset = filtered_by_station_summary,
    value = options$parameter
  )
  # data$filtered_by_parameter <- filtered_by_parameter

  # Unit
  options$unit <- choice_server(
    "choice_ui_unit", 
    object = filtered_by_parameter,
    choices = get_field(filtered_by_parameter,"Units")
  )
  filtered_by_unit <- filter_by_unit(
    dataset = filtered_by_parameter,
    value = options$unit
  )
  filtered_by_unit_summary <- filter_by_unit(
    dataset = filtered_by_parameter_summary,
    value = options$unit
  )
  # data$filtered_by_unit <- filter_by_unit

  # Fraction
  options$fraction <- choice_server(
    "choice_ui_fraction", 
    object = filtered_by_unit,
    choices = get_field(filtered_by_unit,"Fraction")
  )
  data$filtered_by_fraction <- filter_by_fraction(
    dataset = filtered_by_unit,
    value = options$fraction
  )
  data$filtered_by_fraction_summary <- filter_by_fraction(
    dataset = filtered_by_unit_summary,
    value = options$fraction
  )

  ########### MENU LOGIC FOR NO DATA SELECTED STATE ###########
  # Menus have 3 states:
  #   1. First visit, show "Home" tab.
  #   2. Not first visit, data selection filters in initial state.
  #   3. Not first visit, data selection filters have some values selected
  # 
  # We chec for hte first run using the "first_run" variable. This gets
  # updated in mod_inventory_server (mod_inventory.R). When the summary table
  # runs a few times we know we've completed the first page load.

  output$menu <- renderMenu({
    if (grepl("All ",options$parameter()) || grepl("Select ",options$parameter())){
      if (first_run) { # State 1, first run
        sidebarMenu(id = "tabs",
          menuItem("Home", tabName = "Welcome", icon = icon("home"), selected = TRUE),
          menuItem("Select my data", tabName = "Data", icon = icon("check-circle")),
          menuItem("View data results", tabName = "Results_no_selection", icon = icon("chart-bar")),
          menuItem("How is my sampling frequency?", tabName = "Analysis_no_selection", icon = icon("tachometer"))
        )
      } else { # State 2, not first run, no filters
        sidebarMenu(id = "tabs",
          menuItem("Home", tabName = "Welcome", icon = icon("home")),
          menuItem("Select my data", tabName = "Data", icon = icon("check-circle"), selected = TRUE),
          menuItem("View data results", tabName = "Results_no_selection", icon = icon("chart-bar")),
          menuItem("How is my sampling frequency?", tabName = "Analysis_no_selection", icon = icon("tachometer"))
        )
      }  
    } else { # State 3, not first run, filters selected
      sidebarMenu(id = "tabs",
        menuItem("Welcome", tabName = "Welcome", icon = icon("home")),
        menuItem("Select my data", tabName = "Data", icon = icon("check-circle"), selected = TRUE),
        menuItem("View data results", tabName = "Results", icon = icon("chart-bar")),
        menuItem("How is my sampling frequency?", tabName = "Analysis", icon = icon("tachometer"))
      )
    }
  })

  
  ########### DATA PREP ###########
  # Run and store common calculations and conversions used 
  # throughout the application. Most occurances of these conversions
  # and calculations have been leveraged throught the application; however,
  # there might still be some places where we're doing redundant work.

  # Dates as POSIX, helps for plotting and trends.
  data$dates <- reactive({
    return( 
      as.POSIXct( as.character( data$filtered_by_fraction()$Date ), tryFormats = options$date_formats() ) 
    )
  })

  # Date column formatted as Date objects
  data$dates_as_dates <- reactive({
    return(
      as.Date( as.character( data$filtered_by_fraction()$Date ), tryFormats = options$date_formats() )
    )
  })

  # Numeric dates can be used for trends and analysis.
  data$numeric_dates <- reactive({
    return(
      as.numeric( data$dates() )
    )
  })

  # Cast results as numeric.
  data$vals <- reactive({
    return( as.numeric( data$filtered_by_fraction()$Result ) )
  })

  # Detrend data
  data$detrended_vals <- reactive({
    return( detrend( data$vals(), 'linear' ) )
  })

  # Number of observations in selected dataset
  data$n_vals <- reactive({
    return(length(data$vals()))
  })

  # Construct linear model
  data$lm <- reactive({

    # Double current N for N max
    n_max <- data$n_vals() * 2

    # Calculate model
    model <- lm( formula = data$detrended_vals() ~ data$dates() )

    return(model)
  })

  # Minimum Date
  data$date_min <- reactive({
    return( min( data$numeric_dates() ) )
  })

  # Maximum Date
  data$date_max <- reactive({
    return( max( data$numeric_dates() ) )
  })


  ########### INVENTORY TAB ###########
  # Server used for inventory screen.

  # Shows summary table for selected dataset
  mod_inventory_server(
    "mod_inventory_ui_1",
    dataset = data$filtered_by_fraction_summary,
    options
  )

  # ########### RESULTS TAB ###########
  # Server elements used in the trend analysis section.

  # Regression Line Options
  options$add_regression_line <- select_regression_server("choice_ui_regression")
  options$regression_method <- select_regression_method_server("choice_ui_regression_method")

  # Linear model summary
  mod_lm_summary_server(
    "model_residuals",
    dataset = data$filtered_by_fraction,
    options = options
  )

  # Generates title for trends plot
  mod_results_trends_title_server("mod_results_trends_title", options)

  # Scatter plot server
  mod_results_plotly_server(
    "mod_results_ui_4",
    dataset = data$filtered_by_fraction,
    options = options
  )

  # Plot Axis Scale Server
  options$scale <- scatter_plot_axis_server("mod_results_scale")

  # ########### Effort TAB ###########
  # Server elements used for effect size section.

  # Effort Selection Slider
  options$n_per_year <- mod_analysis_n_slider_server(
    "mod_analysis_gauge_effort_n_input",
    data = data
  )

  #Effort Chart
  data$effort <- get_effort_data(
    dataset = data, 
    session = session, 
    options = options
  )

  # Gauge Chart for selected data
  mod_analysis_gauge_effort_server(
    id = "mod_analysis_gauge_effort",
    data = data,
    is_user_n = FALSE,
    breaks = c(0,15,35,100),
    options = options,
    validation_messages = validation_messages
  )

  # Gauge chart for user selected sample size
  mod_analysis_gauge_effort_server(
    id = "mod_analysis_gauge_effort_n",
    # pos = 52,
    data = data,
    is_user_n = TRUE,
    breaks=c(0,15,35,100),
    options = options,
    validation_messages = validation_messages
  )

  # Number of years
  mod_analysis_num_years_server(
    "mod_analysis_num_years",
    data = data,
    options = options,
    validation_messages = validation_messages
  )

  # Effor Title
  mod_analysis_gauge_effort_title_server(
    "mod_analysis_gauge_effort_title",
    options = options
  )

  # Number of years for user entered N
  mod_analysis_num_years_n_server(
    "mod_analysis_num_years_n",
    data = data,
    options = options
  )
  # Number of years for user entered N
  mod_analysis_num_years_n_per_year_server(
    "mod_analysis_num_years_n_per_year",
    data = data,
    options = options,
    validation_messages = validation_messages
  )
  
  mod_effort_analysis_chart_server(
    "mod_effort_analysis_chart",
    dataset = data$filtered_by_fraction,
    data = data,
    options = options,
    validation_messages = validation_messages
  )
}