#' DQunshIncorrectEEType UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @family ServicePoint
mod_DQunshIncorrectEEType_ui <- function(id){
  ns <- NS(id)
  uiOutput(ns("unshIncorrectEEType"))
}
    
#' DQunshIncorrectEEType Server Functions
#'
#' @noRd 
#' @family ServicePoint
mod_DQunshIncorrectEEType_server <- function(id){
  # Deprecated
  # TODO pass inputs
  # Check `ns` usage
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$unshIncorrectEETypeTable <- renderTable({
      ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
      ReportEnd <- format.Date(ymd(meta_HUDCSV$Export_End), "%m-%d-%Y")
      EEType <- dq_unsheltered() %>%
        filter(
          Issue == "Incorrect Entry Exit Type" &
            DefaultProvider == input$unshDefaultProvidersList &
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
    
    output$unshIncorrectEEType <- renderUI({
      ReportStart <- format.Date(input$unsh_dq_startdate, "%m-%d-%Y")
      ReportEnd <- format.Date(ymd(meta_HUDCSV$Export_End), "%m-%d-%Y")
      EEType <- dq_unsheltered() %>%
        filter(
          Issue == "Incorrect Entry Exit Type" &
            DefaultProvider == input$unshDefaultProvidersList &
            served_between(., ReportStart, ReportEnd)
        ) %>%
        mutate(
          PersonalID = format(PersonalID, digits = NULL),
          EntryDate = format(EntryDate, "%m-%d-%Y"),
          ExitDate = format(ExitDate, "%m-%d-%Y")
        ) %>%
        select(
          "Client ID" = PersonalID,
          "Entry Date" = EntryDate,
          "Exit Date" = ExitDate
        )
      if (nrow(EEType) > 0) {
        box(
          id = "unshEEType",
          title = "Incorrect Entry Exit Type",
          status = "danger",
          solidHeader = TRUE,
          HTML(
            "All households entered into the Unsheltered provider should have
          an Entry Exit Type of \"Standard\""
          ),
          tableOutput(ns("unshIncorrectEETypeTable"))
        )
      }
      else {
        
      }
    })
  })
}
    
## To be copied in the UI
# mod_DQunshIncorrectEEType_ui("DQunshIncorrectEEType_1")
    
## To be copied in the server
# mod_DQunshIncorrectEEType_server("DQunshIncorrectEEType_1")
