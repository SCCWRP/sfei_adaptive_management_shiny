# Dependencies
library(shiny)
library(ggplot2)
library(plotly)
library(yaml)
library(mgcv)
library(pracma)
library(shinythemes)
library(shinycssloaders)
library(pwr)
library(purrr)
library(lubridate)
library(shinydashboard)
library(grid)
library(readxl)
library(foreach)
library(tidyverse)
library(doParallel)

# Load R code
source('helpers.R')
source('mod_inventory.R')
source('mod_results.R')
source('mod_analysis.R')
source('app_ui.R')
source('app_server.R')

# Increase upload file size
options(shiny.maxRequestSize=30*1024^2)

# Initialize Shiny appliction
shinyApp(
  ui = app_ui,
  server = app_server
)
