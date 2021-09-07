#' welcome UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_welcome_ui <- function(id){
  ns <- NS(id)
  bs4Dash::box(
    title = "Welcome",
    width = 12,
    HTML(
      "<p>R minor elevated is intended for use by the Ohio Balance of State CoC
        and the Mahoning County CoC HMIS users. This site requires a login
        because client-level data is shown (without Personally Identifying
        Information). Please use this site to verify that your HMIS data is
        accurate and complete.
        <p><a href=\"https://ohiobalanceofstatecoc.shinyapps.io/Rminor\"
        target=\"_blank\">R minor</a> is a separate COHHIO site used for
        performance reporting. Visitors to R minor will include HMIS users,
        program executives, funders, government representatives, advocates, and
        other interested parties. R minor contains no client-level data.<br>
        <p>We're glad you're here! Please select a report in the left sidebar."
    )
  )
}
    
#' welcome Server Functions
#'
#' @noRd 
mod_body_welcome_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
 
  })
}
    
## To be copied in the UI
# mod_welcome_ui("welcome_1")
    
## To be copied in the server
# mod_welcome_server("welcome_1")
