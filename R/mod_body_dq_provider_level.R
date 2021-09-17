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
    ui_header_row(),
    ui_row_box(
      ui_picker_project(options = shinyWidgets::pickerOptions(
        liveSearch = TRUE,
        liveSearchStyle = 'contains',
        actionsBox = TRUE
      ),
      multiple = TRUE,
      width = "100%",
      selected = "none"),
      ui_date_range(),
      width = 12
    ),
    fluidRow(uiOutput(ns("dq_APs_w_EEs"))),
    fluidRow(uiOutput(ns("dq_APsNoReferrals"))),
    fluidRow(
      uiOutput(ns("dq_HHIssues")),
      uiOutput(ns("dq_DuplicateEEs")),
      # DQIncorrectEEType SP only
      uiOutput(ns("dq_MissingLocation")),
      uiOutput(ns("dq_PATHMissingContact"))
    ),
    fluidRow(uiOutput(ns("dq_Ineligible"))),
    fluidRow(uiOutput(ns("dq_OverlappingEEs"))),
    fluidRow(
      box(
        DT::dataTableOutput(ns("dq_Errors")),
        title = "Data Quality Errors",
        width = 12
      )
    ),
    fluidRow(
      box(
        DT::dataTableOutput(ns("dq_Warnings")),
        title = "Data Quality Warnings",
        width = 12
      )
    )
    ,
    ui_row_box(
      id = "dq_summary",
      DT::dataTableOutput(ns("dq_summary")),
      title = "Data Quality Guidance",
      status = "info",
      solidHeader = TRUE
    )
  
    
  )
}
    
#' body_dq_provider_level Server Functions
#'
#' @noRd 
mod_body_dq_provider_level_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    output$header <- renderUI({
      list(h2("Data Quality"),
           h4(paste(
            paste0(input$date_range, sep = " to ")
           )))
    })
    
    dq_profiles <- dq_main()
    
    eligibility_detail <- detail_eligibility()
    
    
    output$dq_APs_w_EEs <- renderUI({
      
      APs_w_EEs <- dq_profiles |> 
        dq_filter_between(Issue == "Access Point with Entry Exits") |> 
        dq_select_cols()
      
      
      purrr::when(APs_w_EEs, 
                  nrow(.) > 0 ~ 
                    ui_box_solid(
                      id = "APs_w_EEs",
                      title = "Access Points Do Not Create Entry Exits",
                      status = "danger",
                      HTML(
                        "Please consult the Coordinated Entry workflow. Access Point providers should not have any Entry Exits. These Entry Exits should be deleted."
                      ),
                      datatable_default(APs_w_EEs)
                    ),
                  ~ NULL)
    })
    
    # TODO Should be a descriptionBox, and go in a section with others.
    output$dq_APsNoReferrals <- renderUI({
      AP_not_doing_referrals <- aps_no_referrals() %>%
        dplyr::filter(ReferringProjectID %in% c(input$project))
      
      if (nrow(AP_not_doing_referrals) > 0) {
        ui_row_box(
          id = "noreferrals",
          title = "Access Point Has No Outgoing Referrals",
          status = "danger",
          width = 12,
          HTML(
            "Access Points should be creating Referrals in HMIS so that households can be connected to housing. Please 
<a href=\"http://hmis.cohhio.org/index.php?pg=kb.page&id=186\">click here</a> for more information."
          )
        )
      } else {
        
      }
    })
    
    output$dq_DuplicateEEs <- renderUI({
      
      DuplicateEEs <- dq_profiles %>%
        dq_filter_between(Issue == "Duplicate Entry Exits") %>%
        dq_select_cols(
          "Exit Date" = ExitDate
        ) 
      
      if (nrow(DuplicateEEs) > 0) {
        ui_box_solid(
          id = "dup_ees",
          title = "Duplicate Entry Exits",
          status = "warning",
          HTML(
            "Please correct this issue before moving on to your other errors.<br> Duplicate Entry Exits are created when the user clicks \"Add Entry Exit\" instead of clicking the Entry pencil to get back into an assessment. These must be deleted for each member of the household. Please take care to not delete Entry Exits with valid Interims attached."
          ),
          datatable_default(Duplicate_EEs, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    
    
    output$dq_HHIssues <- renderUI({
      
      HHIssues <- dq_profiles |> 
        dq_filter_between(c(
          "Too Many Heads of Household",
          "Missing Relationship to Head of Household",
          "No Head of Household",
          "Children Only Household"
        )) |> 
        dq_select_cols()
      
      if (nrow(HHIssues) > 0) {
        ui_box_solid(
          id = "hhs",
          title = "Household Issues",
          status = "warning",
          HTML(
            "Please correct your Household Issues before moving on to make other Data Quality corrections."
          ),
          datatable_default(HHIssues, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    output$dq_ClientLocation <- renderTable({
      
      HHIssues <- dq_profile |> 
        dq_filter_between(Issue == "Missing Client Location") |> 
        dq_select_cols()
      
      
      if (nrow(HHIssues) > 0) {
        ui_box_solid(
          id = "location",
          title = "Missing Client Location",
          status = "warning",
          HTML(
            "Households with a missing Client Location (the data element just after the Relationship to Head of Household) will be completely excluded from ALL HUD reporting."
          ),
          datatable_default(HHIssues, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    output$dq_PATHMissingContact <- renderUI({
      MissingPathContact<- dq_profile %>%
        dq_filter_between(Issue == "Missing PATH Contact") |> 
        dq_select_cols()
      
      if (nrow(no_contact) > 0) {
        ui_box_solid(
          id = "location",
          title = "Missing Contact (PATH)",
          status = "warning",
          MissingPathContact |> 
            dplyr::select(Guidance) |> 
            unique(),
          datatable_default(MissingPATHContact, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    output$dq_Ineligible <- renderUI({
    Ineligible <- eligibility_detail %>%
        dq_filter_between() |> 
        dplyr::mutate(
          PreviousStreetESSH = dplyr::if_else(PreviousStreetESSH == 1, "Yes", "No")
        ) %>%
        select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Residence Prior" = ResidencePrior,
          "Length of Stay" = LengthOfStay,
          "Literally Homeless Prior" = PreviousStreetESSH
        )
      
      if (nrow(Ineligible) > 0) {
        ui_box_solid(
          id = "eligibility",
          title = "Check Eligibility",
          status = "info",
          solidHeader = TRUE,
          width = 12,
          HTML(
            "Your Residence Prior data suggests that this project is either serving ineligible households, the household was entered into the wrong project, or the Residence Prior data at Entry is incorrect. Please check the terms of your grant or speak with the CoC team at COHHIO if you are unsure of eligibility criteria for your project type."
          ),
          datatable_default(Ineligible, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    
    
    
    output$DQOverlappingEEs <- renderUI({
      
      OverlappingEEs <- OverlappingEEs <- dq_overlaps() %>%
        dq_filter_between(Issue == "Overlapping Project Stays") %>%
        dplyr::select(
          "Client ID" = UniqueID,
          "Entry Date" = EntryDate,
          "Move In Date" = MoveInDateAdjust,
          "Exit Date" = ExitDate,
          "Overlaps With This Provider's Stay" = PreviousProject
        ) 
        
      if (nrow(OverlappingEEs) > 0) {
        ui_solid_box(
          id = "overlappers",
          title = "Overlapping Entry Exits",
          status = "warning",
          width = 12,
          HTML(
            "A client cannot reside in an ES, TH, or Safe Haven at the same time. Nor can they have a Move-In Date into a PSH or RRH project while they are still in an ES, TH, or Safe Haven. Further, they cannot be in any two RRH's or any two PSH's simultaneously, housed or not.<br> Please look the client(s) up in HMIS and determine which project stay's Entry/Move-In/or Exit Date is incorrect. PLEASE NOTE: It may be the \"Previous Provider's\" mistake, but if you are seeing clients here, it means your project stay was entered last. <br> If the overlap is not your project's mistake, please work with the project that has the incorrect Entry/Move-In/or Exit Date to get this corrected  or send an email to hmis@cohhio.org if you cannot get it resolved. These clients will NOT show on their Data Quality app. <br> If YOUR dates are definitely correct, it is fine to continue with other data corrections as needed."
          ),
        datatable_default(OverlappingEEs, escape = FALSE)
        )
      }
      else {
        
      }
    })
    
    output$dq_Errors <- DT::renderDataTable({
      
      dq_profile %>%
        dq_filter_between(
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Project Stays",
            "Duplicate Entry Exits",
            "Access Point with Entry Exits"
          )  &
            Type == "Error"
        ) %>%
        dq_select_cols() |> 
        datatable_default(escape = FALSE)
    })
    
    output$dq_Warnings <- DT::renderDataTable({
      
      DQWarnings <- dq_profile %>%
        dq_filter_between(
          !Issue %in% c(
            "Too Many Heads of Household",
            "Missing Relationship to Head of Household",
            "No Head of Household",
            "Children Only Household",
            "Overlapping Project Stays",
            "Duplicate Entry Exits",
            "Check Eligibility"
          ) &
            Type == "Warning"
        ) %>%
        dq_select_cols() |> 
        datatable_default(escape = FALSE)

    })
    
    output$dq_summary <- DT::renderDataTable({
      
      guidance <- dq_profile %>%
        dq_filter_between()
        dplyr::group_by(Type, Issue, Guidance) %>%
        dplyr::ungroup() %>%
        dplyr::select(Type, Issue, Guidance) %>%
        dplyr::mutate(Type = factor(Type, levels = c("High Priority",
                                              "Error",
                                              "Warning"))) %>%
        dplyr::arrange(Type) %>%
        unique() |> 
        datatable_default(escape = FALSE)
    })
    
  })
}
    
## To be copied in the UI
# mod_body_dq_provider_level_ui("body_dq_provider_level_1")
    
## To be copied in the server
# mod_body_dq_provider_level_server("body_dq_provider_level_1")
