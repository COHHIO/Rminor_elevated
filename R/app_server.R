#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {

  # Inputs needed in submodules
  active_tab <- eventReactive(input$active_tab, {input$active_tab})
  dark_mode <- eventReactive(input$dark_mode, {input$dark_mode})
  # Top-level Modules
  mod_navbar_server("navbar")
  mod_sidebar_server("sidebar")
  mod_body_server("body", active_tab = active_tab)
  mod_theme_server("color_theme", dark_mode = dark_mode)
}
