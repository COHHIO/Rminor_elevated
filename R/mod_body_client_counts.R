#' body_client_counts UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_body_client_counts_ui <- function(id){
  ns <- shiny::NS(id)
  shiny::tagList(
    ui_header_row(),
    ui_row_box(
      ui_picker_project()
    ),
    ui_row_box(
      title = "Summary",
      DT::dataTableOutput(ns("summary")),
    ),
    ui_row_box(
      title = "Client Details",
      DT::dataTableOutput(ns("dt_output")),
      width = 12
    )
  )
}

#' body_client_counts Server Functions
#'
#' @noRd 
mod_body_client_counts_server <- function(id){
  shiny::moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$header <- shiny::renderUI({
      list(shiny::h2("Client Counts Report"),
           shiny::h4(input$project))
    })
    
    output$dt_output <- DT::renderDataTable({
      
      
     
        validation()  |> 
          HMIS::served_between(input$date_range[1], input$date_range[2]) |> 
          dplyr::filter(ProjectName == input$project) |>
          dplyr::mutate(
            PersonalID = as.character(PersonalID),
            RelationshipToHoH = dplyr::case_when(
              RelationshipToHoH == 1 ~ "Head of Household",
              RelationshipToHoH == 2 ~ "Child",
              RelationshipToHoH == 3 ~ "Spouse or Partner",
              RelationshipToHoH == 4 ~ "Other relative",
              RelationshipToHoH == 5 ~ "Unrelated household member",
              RelationshipToHoH == 99 ~ "Data not collected (please correct)"
            ),
            Status = dplyr::case_when(
              ProjectType %in% c(3, 13) &
                is.na(MoveInDateAdjust) &
                is.na(ExitDate) ~ paste0("Currently Awaiting Housing (", 
                                         lubridate::today() - EntryDate,
                                         " days)"),
              ProjectType %in% c(3, 13) &
                !is.na(MoveInDateAdjust) &
                is.na(ExitDate) ~ paste0("Currently Moved In (",
                                         lubridate::today() - MoveInDateAdjust,
                                         " days)"),
              ProjectType %in% c(3, 13) &
                is.na(MoveInDateAdjust) &
                !is.na(ExitDate) ~ "Exited No Move-In",
              ProjectType %in% c(3, 13) &
                !is.na(MoveInDateAdjust) &
                !is.na(ExitDate) ~ "Exited with Move-In",
              !ProjectType %in% c(3, 13) &
                is.na(ExitDate) ~ paste0("Currently in project (",
                                         lubridate::today() - EntryDate, 
                                         " days)"),
              !ProjectType %in% c(3, 13) &
                !is.na(ExitDate) ~ "Exited project",
            ),
            sort = lubridate::today() - EntryDate
          ) |>
          dplyr::mutate(PersonalID = as.character(PersonalID)) |>
          dplyr::arrange(dplyr::desc(sort), HouseholdID, PersonalID) |>
          dplyr::select(
            "County" = CountyServed,
            "Client ID" = PersonalID,
            "Relationship to Head of Household" = RelationshipToHoH,
            "Entry Date" = EntryDate,
            "Move In Date (RRH/PSH Only)" = MoveInDateAdjust,
            "Exit Date" = ExitDate,
            Status
          ) |> 
            datatable_default()
    })
    
    output$summary <- DT::renderDataTable({
      
      hhs <- validation() |> 
        HMIS::served_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectName == input$project) |>
        dplyr::select(HouseholdID,
                      ProjectType,
                      EntryDate,
                      MoveInDateAdjust,
                      ExitDate) |>
        unique() |>
        dplyr::mutate(
          # Entered = if_else(between(EntryDate, input$date_range[1], input$date_range[2]),
          #                   "Entered in date range", "Entered outside date range"),
          # Leaver = if_else(!is.na(ExitDate), "Leaver", "Stayer"),
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Awaiting Housing",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Moved In",
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ "Currently in project",
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited project",
          )
        ) |>
        dplyr::group_by(Status) |>
        dplyr::summarise(Households = dplyr::n())
      
      clients <- validation()  |> 
        HMIS::served_between(input$date_range[1], input$date_range[2]) |> 
        dplyr::filter(ProjectName == input$project) |>
        dplyr::select(PersonalID,
                      ProjectType,
                      EntryDate,
                      MoveInDateAdjust,
                      ExitDate) |>
        unique() |>
        dplyr::mutate(
          Status = dplyr::case_when(
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Awaiting Housing",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              is.na(ExitDate) ~ "Currently Moved In",
            ProjectType %in% c(3, 13) &
              is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited No Move-In",
            ProjectType %in% c(3, 13) &
              !is.na(MoveInDateAdjust) &
              !is.na(ExitDate) ~ "Exited with Move-In",
            !ProjectType %in% c(3, 13) &
              is.na(ExitDate) ~ "Currently in project",
            !ProjectType %in% c(3, 13) &
              !is.na(ExitDate) ~ "Exited project",
          )
        ) |>
        dplyr::group_by(Status) |>
        dplyr::summarise(Clients = dplyr::n())
      
      final <- dplyr::full_join(clients, hhs, by = "Status")
      
      datatable_default(final)
    })
  })
}
    
## To be copied in the UI
# mod_body_client_counts_ui("body_client_counts_1")
    
## To be copied in the server
# mod_body_client_counts_server("body_client_counts_1")
