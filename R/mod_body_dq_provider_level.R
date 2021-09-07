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
 
  )
}
    
#' body_dq_provider_level Server Functions
#'
#' @noRd 
mod_body_dq_provider_level_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_body_dq_provider_level_ui("body_dq_provider_level_1")
    
## To be copied in the server
# mod_body_dq_provider_level_server("body_dq_provider_level_1")
