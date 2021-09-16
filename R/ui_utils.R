#' @title A pickerInput that provides active projects to select from
#'
#' @param inputId \code{(namespaced character)} A character vector wrapped in `ns` from the parent environment.
#' @inherit shinyWidgets::pickerInput params return
#' @inheritDotParams shinyWidgets::pickerInput

#' @export

ui_picker_project <- function(
  label = "Select Project",
  inputId = rlang::caller_env()$ns("project"),
  choices = projects, 
  options = shinyWidgets::pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
  ...) {
  shinyWidgets::pickerInput(
    label = label,
    inputId = inputId,
    choices = choices,
    options = options,
    ...
  )
}

#' @title The UI Header output
#'
#' @param outputId \code{(namespaced character)} A character vector wrapped in `ns` from the parent environment.
#' @inheritParams bs4Dash::box
#' @return A fluidrow containing a minimizable box with the header
#' @export

ui_header_row <- function(outputId = rlang::caller_env()$ns("header")) {
  shiny::fluidRow(bs4Dash::box(shiny::htmlOutput(outputId), width = width))
}

#' @title A date range picker with sensible defaults
#'
#' @inherit shiny::dateRangeInput params return
#' @inheritDotParams shiny::dateRangeInput
#' @export

ui_date_range <- function(
  inputId = rlang::caller_env()$ns("date_range"),
  label = "Date Range",
  start = Sys.Date() - lubridate:::days(7),
  end = Sys.Date(),
  min = rm_dates()$meta_HUDCSV$Export_Start,
  width = 300,
  ...
  ) {
  shiny::dateRangeInput(
    inputId = inputId,
    label = label,
    start = start,
    end = end,
    min = min,
    width = width,
    ...
  )
}

#' @title A default full width row box.
#' @inheritParams bs4Dash::box
#' @return A full-width \link[bs4Dash]{box} nested in a row
#' @export
#'
#' @examples
#' ui_row_box(tags$p("Hi"))
ui_row_box <- function(title,
                       footer,
                       status,
                       solidHeader = FALSE,
                       background,
                       width = 12,
                       height,
                       collapsible = TRUE,
                       collapsed = FALSE,
                       closable = FALSE,
                       maximizable = FALSE,
                       icon,
                       gradient = FALSE,
                       boxToolSize = "sm",
                       elevation,
                       headerBorder = TRUE,
                       label,
                       dropdownMenu,
                       sidebar,
                       id,
                       ...) {
  
  .missing <- UU::missing_args(include_null = FALSE)
  .present <- ls() |> 
    {\(x) {x[!x %in% c(.missing, "...")]}}() |> 
    rlang::set_names()
  .dots <- rlang::dots_list(...)
  shiny::fluidRow(eval(rlang::call2(bs4Dash::box, !!!purrr::map(.present, rlang::sym), !!!.dots)))
}


fun_arg_maker <- function(fn) {
  rlang::fn_fmls(fn) |> purrr::imap_chr(~paste0(.y, ifelse(!is.null(.x), paste0(" = ", .x), ""),",")) |> cat(sep = "\n")
}

fun_arg_pass <- function(fn) {
  rlang::fn_fmls(fn) |> purrr::imap_chr(~paste0(.y, " = ",.y,",")) |> cat(sep = "\n")
}
