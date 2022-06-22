#' Defines Shiny Application UI 
#'
#' @return Shiny UI function

app_ui <- function() {
  dashboardPage(
    dashboardHeader(title = "Adaptive Management"),
    dashboardSidebar(
      sidebarMenuOutput("menu")
    ),
    dashboardBody(

      ########### WELCOME TAB ###########
      tabItems(
        tabItem(
          tabName = "Welcome",
          h1("Welcome to the Trends App ", align = 'center'),
          br(),
          box(
            status = "primary",
            width = 12,
            fluidRow(
              column(
                width = 12,
                h3("Adaptive Management App for Storm Managers", align = "center"),
                p("This application allows watershed managers to visualize trends in their time series monitoring data, then use the automated power analysis to determine how much change managers can detect with their current monitoring design. The app can also be used to help watershed managers optimize their sampling frequency and level of monitoring effort moving forward."),
                p("This trends app supports time series monitoring designs for many indicators including chemicals, bacteria, trash, biology, or volume. The app is limited to data sets with more than 10 samples at each site. 
This web app has three sections (starting with the navigation pane on the left): 1) Select my data to upload monitoring data, 2) View data results to visualize time series plots and determine if a trend currently exists, and 3) Optimize sampling frequency to visualize how much, if any, change your monitoring design can detect.  
The best part of this app is you get to use your own data! Upload your data using the app’s downloadable data template or uploading directly from CEDEN using the embedded link. Feel free to use the default data sets as a tutorial.
"),
                p("Version 2, updated: 3/22/22 ")
              ) # /column
            ) # /fluidRow
          ), # /box
          box(
            status = "primary",
            width = 12,
            h3("Contributors", align = "center"),
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/ken-schiff/", 'Ken Schiff'),", Southern California Coastal Water Research Project"),
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/elizabeth-fassman-beck/", 'Dr. Elizabeth Fassman-Beck'),", Southern California Coastal Water Research Project"),
            p(align = "center", a(href = "https://www.sccwrp.org/about/staff/emily-darin/", 'Emily Darin'),", Southern California Coastal Water Research Project"),
            p(align = "center", a(href = "https://www.sfei.org/users/lorenzo-flores", 'Lorenzo Flores'),", San Francisco Estuary Institute"),
            p(align = "center", a(href = "https://www.sfei.org/users/gemma-shusterman", 'Gemma Shusterman'),", San Francisco Estuary Institute"),
            tags$img(src="sccwrp.png", width = "20%", height = "20%", style = 'display: block; margin-left: auto; margin-right: auto;'),
            tags$img(src="sfei.png", width = "20%", height = "20%", style = 'display: block; margin-left: auto; margin-right: auto;')
          ) # /box
        ), # /tabItem

        ########### DATA TAB ###########
        tabItem(
          tabName = "Data",
          h1("Select Data to Review", align = 'center'),
          box(
            status = "primary",
            width = 12,
            p("Upload your data or choose an existing dataset to explore the app. You must use the 'Filter Data' selection tool to choose a subset of your data including selecting a watershed, station, &/or parameter before navigating to the results or sampling frequency tabs. The summary table on the right shows an inventory of the selected data set.  Tabs at the bottom filter the selected data set.")
          ), # /box
          fluidRow(
            column(4,
              fluidRow(
                width = 12,
                column(12,
                  tabBox(
                    width = 12,
                    tabPanel(
                      "Upload Data",
                      upload_format_choice_ui("upload_format","Choose upload format:"),
                      fileInput("file_upload", "Choose File",
                        multiple = FALSE,
                        accept = c(
                          "text/csv",
                          "text/comma-separated-values,text/plain",
                          ".csv",
                          ".xls"
                        )
                      ),
                      # actionButton("clearUpload", "Clear Upload"),
                      HTML("<p><b>Format Guide</b><p>"),
                      HTML("<p><em>CEDEN Direct Download</em> - data obtained from <a href=\"https://ceden.waterboards.ca.gov/AdvancedQueryTool\" target=\"_blank\">CEDEN's Advanced Query Tool</a>.</p>"),
                      HTML("<p><em>CEDEN Modified Download</em> - data obtained from <a href=\"https://ceden.waterboards.ca.gov/AdvancedQueryTool\" target=\"_blank\">CEDEN's Advanced Query Tool</a> and modifed within Excel after download.</p>"),
                      HTML("<p><em>CEDEN Submission Template</em> - data stored in CEDEN's <a href=\"http://www.ceden.org/ceden_datatemplates.shtml/#templates\">Water Chemistry submission template</a>.</p>"),
                      HTML("<p><em>General Template</em> - <a href=\"adaptive_management_data_template.xls\">Basic template</a> for data upload.</p>")
                    ),# /tabPanel
                    tabPanel(
                      "Select Existing Data",
                      selectInput("dataset", label = "Dataset", choices = as.list(list.files("./data")))
                    ) # /tabPanel
                  ) # /tabBox
                ) # /column
              ), # /fluidRow
              fluidRow(
                width = 12,
                column(12,
                  box(
                    width = 12,
                    title = "Filter Data (Required)",
                    p("Select a Parameter before viewing results or sampling frequency."),
                    choice_ui("choice_ui_watershed", "Watershed"),
                    choice_ui("choice_ui_station", "*Station"),
                    choice_ui("choice_ui_parameter", "*Parameter"),
                    choice_ui("choice_ui_unit", "Unit"),
                    choice_ui("choice_ui_fraction", "Fraction"),
                    HTML("<p>* indicates required</p>"),
                    # actionButton("reset", "Reset") # RESET BUTTON
                  )
                )
              )
            ), # /column
            column(8,
              box(
                width = 12,
                title = "Summary Information for Currently Selected Dataset",
                mod_inventory_ui("mod_inventory_ui_1")
              )
            ) # /column
          ) # /fluidRow
        ), # /tabItem

        ########### RESULTS and ANALYSIS TABs w/ NO SELECTION
        tabItem(
          tabName = "Results_no_selection",
          h1("View Monitoring Trends Over Time", align = 'center'),
          box(
            status = "primary",
            width = 12,
            p("Here you can view a scatter plot and fit a linear regression model to the currently selected data."),
            HTML("<h4>Please select a subset of the data using the <b>Select my data</b> tab to the left. Result options will display once a watershed, station or parameter are selected.")
          ),
        ),
        tabItem(
          tabName = "Analysis_no_selection",
          h1("Evaluate Sampling Methods", align = 'center'),
          box(
            status = "primary",
            width = 12,
            p("The following gauge shows the effect size for the number of observations contained in the currently selected dataset using a linear model. Colors red, yellow, and green correspond to small, medium, and large effect sizes respectively. The green region assumes a the given experiment calls for a number of observations suitable for achieving a medium effect size."),
            HTML("<h4>Please select a subset of the data using the <b>Select my data</b> tab to the left. Sampling frequency analysis options will display once a watershed, station or parameter are selected.")
          )
        ),


        ########### RESULTS TAB w/ Selection ###########
        tabItem(
          tabName = "Results",
          h1("View Scatter Plot and Fit a Linear Model to Selected Data", align = 'center'),
          box(
            status = "primary",
            width = 12,
            p("Here you can view a scatter plot and fit a linear regression model to the currently selected data.")
          ),
          fluidRow(
            width = 12,
            column(
              width = 12,
              box(
                width = 12,
                fluidRow(
                  width = 12,
                  column(
                    width = 6,
                    # h3("Trends in [parameter]"),
                    mod_results_trends_title_ui("mod_results_trends_title")
                  ),
                  column(
                    width = 3,
                    h4("Choose Y-Axis Scale")
                  ),
                  column(
                    width = 3,
                    scatter_plot_axis_ui("mod_results_scale")
                  )
                ),
                fluidRow(
                  width = 12,
                  collapsible = T,
                  column(
                    width = 12,
                    box(
                      width = 12,
                      mod_results_plotly_ui("mod_results_ui_4")
                    )
                  )
                )
              )
            )
          ),
          fluidRow(
            width = 12,
            column(
              width = 12,
              box(
                width = 12,
                column(
                  width = 4,
                  h3("Add Regression Line to the Scatter Plot Above"),
                  select_regression_ui("choice_ui_regression", "Add Regression Line?"),
                  select_regression_method("choice_ui_regression_method")
                ),
                column(
                  width = 8,
                  h3("Regression Results for Currently Selected Data"),
                  textOutput("Model summary information"),
                  mod_pwr_analysis_mr_ui("model_residuals")
                )
              )
            )
          )
          
        ), # /tabItem

        ########### ANALYSIS TAB  w/ Selection ###########
        tabItem(
          tabName = "Analysis",
          h1("Evaluate Sampling Frequency and Methods", align = 'center'),
          box(
            status = "primary",
            width = 12,
            HTML("<p>The following visualizations help assess sampling efforts required to detect a percent magnitude of trend change.<p><p style=\"color:red;font-weight:bold;\">NOTE: VISUALIZATIONS WILL TAKE APPROXIMATELY TWO MINUTES TO LOAD, PLEASE BE PATIENT</p>")
          ),
          fluidRow(
            width = 12,
            column(
              width = 12,
              box(
                width = 12,
                column(
                  width = 6,
                  # h3("Current level of effort:"),
                  mod_analysis_gauge_effort_title_ui("mod_analysis_gauge_effort_title"),
                  mod_analysis_gauge_effort_ui("mod_analysis_gauge_effort"),
                  mod_analysis_num_years_ui("mod_analysis_num_years")
                ),
                column(
                  width = 6,
                  h3("Compare with another number of observations:"),
                  mod_analysis_gauge_effort_ui("mod_analysis_gauge_effort_n"),
                  mod_analysis_num_years_n_ui("mod_analysis_num_years_n"),
                  mod_analysis_n_slider(
                    id = "mod_analysis_gauge_effort_n_input",
                    label = ""
                  ),
                  mod_analysis_num_years_n_per_year_ui("mod_analysis_num_years_n_per_year")
                  # ),
                  # mod_analysis_storm_per_year_slider(
                  #   id = "mod_analysis_gauge_effect_size_storm_input",
                  #   label = "Select the number of years:"
                  # )
                )
              )
            )
          ),
          fluidRow(
            width = 12,
            column(
              width = 12,
              box(
                width = 12,
                # title = "Effect Size Chart",
                h3("Effort Chart"),
                HTML("<p>The below chart shows percent magintude of trend change vs relative effort for the currently selected data set. The <span style='color:red;'>dotted red line</span> indicates your current effort, (i.e., relative effort = 1), the <span style='color:green;'>solid green line</span> indicates the optimal effort for effectively & efficiently detecting trends (i.e the inflection point on the curve), and the <span style='color:blue;'>solid blue line</span> indicates the effort represented by the slider on the right hand dial guage</p>"),
                mod_effort_analysis_chart_ui("mod_effort_analysis_chart")
              )
            )
          )
        ) # / tabItem
      ) # /tabItems
    ) # /dashboardBody
  )
}