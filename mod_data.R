# mod_data_select_ui <- function(id){
#     ns <- NS(id)
#     tagList(
#         selectInput("dataset", label = "Dataset", choices = as.list(list.files("./data")))
#     )
# }

# mod_data_select_server <- function(id){
#     moduleServer(id, function(input, output, session){
#         return(
#             # Load dataset, use as.is for dropdown population
#             reactive({
#                 input$dataset
#             })
#         )
#     })
# }

# mod_data_upload_ui <- function(id){
#     ns <- NS(id)
#     tagList(
#         fileInput("uploaded_data", "Choose CSV File",
#           multiple = FALSE,
#           accept = c(
#             "text/csv",
#             "text/comma-separated-values,text/plain",
#             ".csv"
#           )
#         )
#     )
# }

# mod_data_upload_server <- function(id){
    

# }
