#' body UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#' @importFrom shiny NS tagList 

mod_body_ui <- function(id){
  ns <- NS(id)
  bs4Dash::dashboardBody(
    uiOutput(ns("bodyui"))
  )
}

#' body Server Functions
#'
#' @noRd 
mod_body_server <- function(id, active_tab){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
  
    active <<- reactiveValues()
    observe({
      
        active$ui <- paste0("mod_body_", active_tab(), "_ui")
        active$server <-  paste0("mod_body_", active_tab(), "_server")
        active$tab <- active_tab()
    })
    e <- environment()
      
    output$bodyui <- renderUI({
      req(active$ui)
      message("Tab: ", active_tab())
      if (exists(active$server))
        rlang::exec(active$server, id = paste0("body_", isolate(active$tab)), .env = e)
      # Render the body UIs here
      if (exists(active$ui))
        rlang::exec(active$ui, id = paste0(ns("body_"), active$tab))
    })
  })
}
    
## To be copied in the UI
# mod_body_ui("body_1")
    
## To be copied in the server
# mod_body_server("body_1")
