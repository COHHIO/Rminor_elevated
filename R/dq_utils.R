#' @title Filter data.frame with default filters for DQ
#' @description Filters by `input$project` & `input$date_range`
#' @param x \code{(data.frame)} with `ProjectName`, `EntryDate`, `ExitDate` 
#' @param ... \code{(character)} Expressions passed on to \link[dplyr]{filter}
#' @param env \code{(environment)} The parent environment from which to retrieve input reactiveValues
#'
#' @return \code{(data.frame)} filtered accordingly
#' @export
#'
#' @examples
#' test <- data.frame(Issue = 1:5, Type = sample(c("Warning", "Error"), 5, TRUE), ProjectName = letters[1:5], EntryDate = seq.Date(lubridate::floor_date(lubridate::today() - 4, "month"), Sys.Date(), length.out = 5), ExitDate = seq.Date(lubridate::today() - 4, Sys.Date(), by = "day"))
dq_filter_between <- function(x,
  ...,
  env = rlang::caller_env()
) {
  out <- x
  if (exists("input", env))
    out <- out |>
      HMIS::served_between(isolate(env$input$date_range[1]), isolate(env$input$date_range[2])) |> 
      dplyr::filter(ProjectName %in% isolate(env$input$project))
  
  
  .dots <- rlang::enexprs(...)
  
  
  if (length(.dots) > 1) {
    ex <- purrr::reduce(.dots, ~rlang::expr(!!.x & !!.y))
  } else {
    ex <- .dots
  }
    
  if (exists("ex", inherits = FALSE)) 
    out <- out |> 
      dplyr::filter(
      !!!ex
      )
  out
}


#' @title Select default display columns for Data Quality Tables
#' 
#' @param x \code{(data.frame)}
#' @param ... \code{(columns to select)} These can be unquoted or quoted.
#' @param default \code{(list)} Columns to select can also be supplied as a list. If using `...` and you wish to not select the default columns, set to `FALSE`
#'
#' @return \code{(data.frame)} with selected columns. See `default` argument for defaults that will be selected.
#' @export
#'
#' @examples
#' dq_select_cols(data.frame(UniqueID = 1:3, Issue = letters[1:3], EntryDate = 1:3, blah = 1:3), blah)
dq_select_cols <- function(x, ..., default = list(`Unique ID` = "UniqueID",
                                                  "Issue",
                                                  `Entry Date` = "EntryDate")) {
  
  ex <- rlang::enexprs(...)
  if (UU::is_legit(default))
    ex <- rlang::exprs(!!!default, !!!ex)
  dplyr::select(x, 
                !!!ex
                )
}