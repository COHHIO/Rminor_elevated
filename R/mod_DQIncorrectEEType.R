#' DQIncorrectEEType UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @family ServicePoint
mod_DQIncorrectEEType_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("DQIncorrectEEType"))
}
    
#' DQIncorrectEEType Server Functions
#'
#' @noRd 
#' @family ServicePoint
mod_DQIncorrectEEType_server <- function(id){
  # Deprecated
  # TODO pass inputs
  # Check `ns` usage
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$DQIncorrectEETypeTable <- renderTable({
      ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
      ReportEnd <- format.Date(ymd(meta_HUDCSV$Export_End), "%m-%d-%Y")
      EEType <- dq_main() %>%
        filter(
          Issue == "Incorrect Entry Exit Type" &
            ProjectName %in% c(input$providerListDQ) &
            served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y")
        ) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate
        )
      EEType
    }) 
    
    output$DQIncorrectEEType <- renderUI({
      ReportStart <- format.Date(input$dq_startdate, "%m-%d-%Y")
      ReportEnd <- format.Date(ymd(meta_HUDCSV$Export_End), "%m-%d-%Y")
      
      EEType <- dq_main() %>%
        filter(
          Issue == "Incorrect Entry Exit Type" &
            ProjectName %in% c(input$providerListDQ) &
            served_between(., ReportStart, ReportEnd)
        ) 
      
      if (nrow(EEType) > 0) {
        box(
          id = "DQEEType",
          title = "Incorrect Entry Exit Type",
          status = "warning",
          solidHeader = TRUE,
          HTML(
            "If you are not sure which Entry Exit Type you should be using for 
          your provider, please contact the HMIS team."
          ),
          tableOutput(ns("DQIncorrectEETypeTable"))
        )
      }
      else {
        
      }
    })
  })
}
    
## To be copied in the UI
# mod_DQIncorrectEEType_ui("DQIncorrectEEType_1")
    
## To be copied in the server
# mod_DQIncorrectEEType_server("DQIncorrectEEType_1")
