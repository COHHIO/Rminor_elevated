#' sidebar UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_sidebar_ui <- function(id){
  ns <- NS(id)
  bs4Dash::bs4DashSidebar(
    title = HTML("<small>The Colorado River Basin Outlook</small>"),
    skin = "dark",
    status = "primary",
    elevation = 4,
    collapsed = TRUE,
    expandOnHover = TRUE,
    fixed = FALSE,
      bs4Dash::bs4SidebarMenu(
        id = "active_tab",
        compact = TRUE,
        bs4Dash::bs4SidebarMenuItem(
          text = "Welcome",
          tabName = "welcome", #homeTab
          icon = shiny::icon("home")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Prioritization",
          tabName = "prioritization", #prioritizationListTab
          icon = shiny::icon("sitemap")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Client Counts",
          tabName = "client_counts", #currentProviderLevel
          icon = shiny::icon("users") 
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "COVID-19",
          tabName = "covid19",
          icon = shiny::icon("syringe"),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Vaccine Status",
            tabName = "c19_vaccine_status"
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Second Dose Logistics", #vaccineStatus
            tabName = "c19_second_dose" #vaccineSecondDose
          )
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Bed & Unit Utilization",
          tabName = "utilization", #utilizationTab
          icon = shiny::icon("bed")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Data Quality",
          tabName = "dq",
          icon = shiny::icon("database"),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Provider-level",
            tabName = "dq_provider_level" #dqTab
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Data Entry Timeliness",
            tabName = "dq_timeliness" #deskTime
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Unsheltered",
            tabName = "dq_unsheltered"
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Region-level",
            tabName = "dq_region_level" #dqRegion
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "System-wide",
            tabName = "dq_system_wide" # dqCoC
          ),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "CE Summary",
            tabName = "dq_system_summary" #ceCoC
          )
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "BoS CoC Competition",
          tabName = "coc_competition", # cocCompetitionTab
          icon = shiny::icon("flag-checkered")
        ),
        bs4Dash::bs4SidebarMenuItem(
          text = "Quarterly Performance Report",
          icon = shiny::icon("file-medical-alt"),
          bs4Dash::bs4SidebarMenuSubItem(
            text = "Community Need",
            tabName = "qpr_community_need" #spdatTab
            # need to determne how to re-structure to remove another layer of nesting here
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Length of Stay",
            tabName = "qpr_length_of_stay" # LoS-Tab
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Exits to Permanent Housing",
            tabName = "qpr_permanent_housing" # PHTab
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Non-Cash Benefits at Exit",
            tabName = "qpr_noncash_benefits" # NCB-Tab
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Health Insurance at Exit",
            tabName = "qpr_health_insurance" # HI-Tab
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Income Growth",
            tabName = "qpr_income_growth" # income-Tab
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "Rapid Placement for RRH",
            tabName = "qpr_rrh_placement" # rapid-Tab
          ),
          bs4Dash::bs4SidebarMenuItem(
            text = "RRH Spending",
            tabName = "qpr_rrh_spending" # rapid-Tab
          )
        ),
        actionButton(
          inputId = "logOutButton",
          label = "Log Out",
          onclick =
            "window.open('https://ohiobalanceofstatecoc.shinyapps.io/Rminor_elevated/__logout__/')"
        )
      )
      
    )
}
    
#' sidebar Server Functions
#'
#' @noRd 
mod_sidebar_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}


## To be copied in the UI
# mod_sidebar_ui("sidebar_1")
    
## To be copied in the server
# mod_sidebar_server("sidebar_1")
