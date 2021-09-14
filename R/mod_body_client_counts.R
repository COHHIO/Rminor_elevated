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
  ns <- NS(id)
  tagList(
    fluidRow(box(htmlOutput("header"), width = 12)),
    fluidRow(box(
      pickerInput(
        label = "Select Project",
        inputId = "project",
        choices = projects,
        options = pickerOptions(liveSearch = TRUE,
                                liveSearchStyle = 'contains')
      ),
      dateRangeInput(
        "dateRangeCount",
        "Date Range",
        min = rm_dates$meta_HUDCSV$Export_Start,
        format = "mm/dd/yyyy",
        width = 300
      ),
      width = 12
    )),
    fluidRow(box(
      DT::dataTableOutput("summary"),
      width = 12
    )),
    fluidRow(box(
      DT::dataTableOutput("dt_output"),
      width = 12
    ))
  )
}
    
#' body_client_counts Server Functions
#'
#' @noRd 
mod_body_client_counts_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$heater <- renderUI({
      list(h2("Client Counts Report"),
           h4(input$provider))
    })
    
    output$dt_output <- DT::renderDataTable({
      ReportStart <- format.Date(input$dateRangeCount[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$dateRangeCount[2], "%m-%d-%Y")
      
      DT::datatable(
        validation()  |> 
          HMIS::served_between(ReportStart, ReportEnd) |> 
          dplyr::filter(ProjectName == input$project) %>%
          dplyr::mutate(
            PersonalID = as.character(PersonalID),
            RelationshipToHoH = case_when(
              RelationshipToHoH == 1 ~ "Head of Household",
              RelationshipToHoH == 2 ~ "Child",
              RelationshipToHoH == 3 ~ "Spouse or Partner",
              RelationshipToHoH == 4 ~ "Other relative",
              RelationshipToHoH == 5 ~ "Unrelated household member",
              RelationshipToHoH == 99 ~ "Data not collected (please correct)"
            ),
            Status = case_when(
              ProjectType %in% c(3, 13) &
                is.na(MoveInDateAdjust) &
                is.na(ExitDate) ~ paste0("Currently Awaiting Housing (", 
                                         today() - EntryDate,
                                         " days)"),
              ProjectType %in% c(3, 13) &
                !is.na(MoveInDateAdjust) &
                is.na(ExitDate) ~ paste0("Currently Moved In (",
                                         today() - MoveInDateAdjust,
                                         " days)"),
              ProjectType %in% c(3, 13) &
                is.na(MoveInDateAdjust) &
                !is.na(ExitDate) ~ "Exited No Move-In",
              ProjectType %in% c(3, 13) &
                !is.na(MoveInDateAdjust) &
                !is.na(ExitDate) ~ "Exited with Move-In",
              !ProjectType %in% c(3, 13) &
                is.na(ExitDate) ~ paste0("Currently in project (",
                                         today() - EntryDate, 
                                         " days)"),
              !ProjectType %in% c(3, 13) &
                !is.na(ExitDate) ~ "Exited project",
            ),
            sort = today() - EntryDate
          ) %>%
          dplyr::mutate(PersonalID = as.character(PersonalID)) %>%
          dplyr::arrange(desc(sort), HouseholdID, PersonalID) %>%
          dplyr::select(
            "County" = CountyServed,
            "Client ID" = PersonalID,
            "Relationship to Head of Household" = RelationshipToHoH,
            "Entry Date" = EntryDate,
            "Move In Date (RRH/PSH Only)" = MoveInDateAdjust,
            "Exit Date" = ExitDate,
            Status
          ),
        rownames = FALSE,
        filter = 'top',
        options = list(dom = 'ltpi')
      )
    })
    
    output$summary <- DT::renderDataTable({
      ReportStart <- format.Date(input$dateRangeCount[1], "%m-%d-%Y")
      ReportEnd <- format.Date(input$dateRangeCount[2], "%m-%d-%Y")
      
      hhs <- validation() |> 
        HMIS::served_between(ReportStart, ReportEnd)
        dplyr::filter(ProjectName == input$project) %>%
          dplyr::select(HouseholdID,
               ProjectType,
               EntryDate,
               MoveInDateAdjust,
               ExitDate) %>%
        unique() %>%
          dplyr::mutate(
          # Entered = if_else(between(EntryDate, ReportStart, ReportEnd),
          #                   "Entered in date range", "Entered outside date range"),
          # Leaver = if_else(!is.na(ExitDate), "Leaver", "Stayer"),
          Status = case_when(
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
        ) %>%
          dplyr::group_by(Status) %>%
          dplyr::summarise(Households = n())
      
      clients <- validation()  |> 
        HMIS::served_between(ReportStart, ReportEnd) |> 
        dplyr::filter(ProjectName == input$project) %>%
        dplyr::select(PersonalID,
               ProjectType,
               EntryDate,
               MoveInDateAdjust,
               ExitDate) %>%
        unique() %>%
        dplyr::mutate(
          Status = case_when(
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
        ) %>%
        dplyr::group_by(Status) %>%
        summarise(Clients = n())
      
      final <- dplyr::full_join(clients, hhs, by = "Status")
      
      DT::datatable(
        final,
        rownames = FALSE,
        filter = 'none',
        options = list(dom = 't')
      )
    })
  })
}
    
## To be copied in the UI
# mod_body_client_counts_ui("body_client_counts_1")
    
## To be copied in the server
# mod_body_client_counts_server("body_client_counts_1")
