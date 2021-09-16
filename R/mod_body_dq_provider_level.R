#' body_dq_provider_level UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_dq_provider_level_ui <- function(id){
  ns <- NS(id)
  tagList(
    ui_header_row(),
    ui_row_box(
      ui_picker_project(options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchStyle = 'contains',
        actionsBox = TRUE
      ),
      multiple = TRUE,
      width = "100%",
      selected = "none"),
      ui_date_range(),
      width = 12
    ),
    fluidRow(uiOutput(ns("DQ_APs_w_EEs"))),
    fluidRow(uiOutput(ns("DQAPsNoReferrals"))),
    fluidRow(
      uiOutput(ns("DQHHIssues")),
      uiOutput(ns("DQDuplicateEEs")),
      # DQIncorrectEEType SP only
      uiOutput(ns("DQMissingLocation")),
      uiOutput(ns("DQPATHMissingContact"))
    ),
    fluidRow(uiOutput(ns("DQIneligible"))),
    fluidRow(uiOutput(ns("DQOverlappingEEs"))),
    fluidRow(
      box(
        DT::dataTableOutput(ns("DQErrors")),
        title = "Data Quality Errors",
        width = 12
      )
    ),
    fluidRow(
      box(
        DT::dataTableOutput(ns("DQWarnings")),
        title = "Data Quality Warnings",
        width = 12
      )
    )
    ,
    fluidRow(
      box(
        id = ns("DQSummaryProvider"),
        DT::dataTableOutput(ns("dq_provider_summary_table")),
        title = "Data Quality Guidance",
        width = 12,
        status = "info",
        solidHeader = TRUE
      )
    )
    
  )
}
    
#' body_dq_provider_level Server Functions
#'
#' @noRd 
mod_body_dq_provider_level_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI({
      list(h2("Data Quality"),
           h4(paste(
            paste0(input$date_range, sep = " to ")
           )))
    })
    .
  })
}
    
## To be copied in the UI
# mod_body_dq_provider_level_ui("body_dq_provider_level_1")
    
## To be copied in the server
# mod_body_dq_provider_level_server("body_dq_provider_level_1")
