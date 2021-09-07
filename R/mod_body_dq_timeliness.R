#' body_dq_timeliness UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_dq_timeliness_ui <- function(id){
  ns <- NS(id)
  tagList(
 
  )
}
    
#' body_dq_timeliness Server Functions
#'
#' @noRd 
mod_body_dq_timeliness_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_body_dq_timeliness_ui("body_dq_timeliness_1")
    
## To be copied in the server
# mod_body_dq_timeliness_server("body_dq_timeliness_1")
