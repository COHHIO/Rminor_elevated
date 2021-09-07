#' data_quality UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_data_quality_ui <- function(id){
  ns <- NS(id)
  tagList(
  tabItem(
    tabName = "dqTab",
    fluidRow(box(htmlOutput(
      "headerDataQuality"
    ), width = 12)),
    fluidRow(box(
      pickerInput(
        label = "Select Provider",
        inputId = ns("providerListDQ"),
        choices = dq_providers,
        options = pickerOptions(
          liveSearch = TRUE,
          liveSearchStyle = 'contains',
          actionsBox = TRUE
        ),
        multiple = TRUE,
        width = "100%",
        selected = "none"
      ),
      dateInput(
        inputId = ns("dq_startdate"),
        label = "Report Start Date",
        format = "mm/dd/yyyy",
        value = hc$check_dq_back_to,
        min = meta_HUDCSV$Export_Start,
        width = "25%"
      ),
      width = 12
    )),
    fluidRow(uiOutput(ns("DQ_APs_w_EEs"))),
    fluidRow(uiOutput(ns("DQAPsNoReferrals"))),
    fluidRow(
      uiOutput(ns("DQHHIssues")),
      uiOutput(ns("DQDuplicateEEs")),
      # DQIncorrectEEType SP only
      uiOutput(ns("DQMissingLocation")),
      uiOutput(ns("DQPATHMissingContact"))
    ),
    fluidRow(uiOutput(ns("DQIneligible"))),
    fluidRow(uiOutput(ns("DQOverlappingEEs"))),
    fluidRow(
      box(
        DT::dataTableOutput(ns("DQErrors")),
        title = "Data Quality Errors",
        width = 12
      )
    ),
    fluidRow(
      box(
        DT::dataTableOutput(ns("DQWarnings")),
        title = "Data Quality Warnings",
        width = 12
      )
    )
    ,
    fluidRow(
      box(
        id = ns("DQSummaryProvider"),
        DT::dataTableOutput(ns("dq_provider_summary_table")),
        title = "Data Quality Guidance",
        width = 12,
        status = "info",
        solidHeader = TRUE
      )
    )
  ),
  tabItem(
    tabName = "deskTime",
    fluidRow(box(htmlOutput(ns("headerDeskTime")),
                 width = 12)),
    fluidRow(box(
      pickerInput(
        label = "Select Provider",
        inputId = ns("providerDeskTime"),
        choices = desk_time_providers,
        options = pickerOptions(liveSearch = TRUE,
                                liveSearchStyle = 'contains'),
        width = "100%",
        selected = desk_time_providers[1]
      ),
      width = 12
    )),
    fluidRow(box(
      plotOutput(ns("DeskTimePlotDetail")), width = 12
    )),
    fluidRow(
      box(
        uiOutput(ns("deskTimeNote")),
        title = "More Information",
        collapsible = TRUE,
        collapsed = TRUE,
        width = 12
      )
    )
  ),
  tabItem(tabName = "dqRegion",
          fluidRow(box(
            htmlOutput(ns("headerRegionDataQuality")), width = 12
          )),
          fluidRow(box(
            pickerInput(
              inputId = ns("regionList3"),
              choices = c(unique(regions()$RegionName)),
              options = pickerOptions(liveSearch = TRUE,
                                      liveSearchStyle = 'contains'),
              width = "70%"
            ),
            dateInput(
              inputId = ns("dq_region_startdate"),
              label = "Report Start Date",
              format = "mm/dd/yyyy",
              value = ymd(hc$check_dq_back_to),
              width = "25%"
            ),
            width = 12
          )),
          fluidRow(
            box(
              id = ns("DQSummaryRegion"),
              title = paste("Data Quality Summary"),
              status = "info",
              solidHeader = TRUE,
              DT::dataTableOutput(ns("dq_region_summary_table")),
              width = 12
            )
          )),
  tabItem(
    tabName = "unsheltered",
    fluidRow(box(
      htmlOutput(ns("headerUnshDataQuality")), width = 12
    )),
    fluidRow(box(
      pickerInput(
        inputId = ns("unshDefaultProvidersList"),
        label = "Select your DEFAULT Provider",
        choices = sort(dq_unsheltered()$DefaultProvider) %>%
          unique(),
        options = pickerOptions(liveSearch = TRUE,
                                liveSearchStyle = 'contains'),
        width = "100%"
      ),
      dateInput(
        inputId = ns("unsh_dq_startdate"),
        label = "Report Start Date",
        format = "mm/dd/yyyy",
        value = ymd(hc$check_dq_back_to),
        width = "25%"
      ),
      width = 12
    )),
    fluidRow(
      # unsIncorrectEEType SP only
      uiOutput(ns("unshIncorrectResPrior")),
      uiOutput(ns("unshMissingCounty")),
      uiOutput(ns("unshOverlaps")),
      uiOutput(ns("unshHHIssues")),
      uiOutput(ns("unshDuplicateEEs"))
    ),
    fluidRow(
      box(
        DT::dataTableOutput(ns("unshDQErrorsTable")),
        title = "Unsheltered Data Quality Errors",
        width = 12
      )
    ),
    fluidRow(
      box(
        DT::dataTableOutput(ns("unshDQWarningsTable")),
        title = "Unsheltered Data Quality Warnings",
        width = 12
      )
    )
    ,
    fluidRow(uiOutput(ns("dq_unsheltered_summary_box")))
  ),
  # tabItem(tabName = "diversion"),
  tabItem(
    tabName = "dqCoC",
    fluidRow(box(htmlOutput(ns("headerCocDQ")), width = 12)),
    # list(
    #   dq_plot_projects_errors =
    #     list(status = "danger",
    #          title = "Providers with the Most High Priority Issues and Errors"),
    #   dq_plot_hh_errors =
    #     list(status = "danger",
    #          title = "Providers with the Most Household Errors"),
    #   dq_plot_unsheltered_high =
    #     list(status = "danger",
    #          title = "Unsheltered High Priority Issues (User's Default Provider)"),
    #   dq_plot_projects_warnings =
    #     list(status = "warning",
    #          title = "Providers with the Most Data Quality Warnings"),
    #   DeskTimePlotCoC =
    #     list(status = "warning",
    #          title = "Longest Data Entry Delay Medians (in the past 365 days)"),
    #   dq_plot_errors =
    #     list(status = "primary",
    #          title = "Top 10 Error Types"),
    #   dq_plot_warnings =
    #     list(status = "primary",
    #          title = "Top 10 Warning Types"),
    #   dq_plot_eligibility =
    #     list(status = "warning",
    #          title = "Providers with Potential Eligibility Issues")
    # ) %>% 
    #   purrr::imap( ~ {
    #     if (.y == "DeskTimePlotCoC")
    #       .out <- plotOutput(.y)
    #     else
    #       .out <- imageOutput(.y, height = "auto")
    #     
    #     fluidRow(do.call(shinydashboard::box, purrr::list_modify(
    #       list(
    #         .out,
    #         width = 12,
    #         height = "auto",
    #         solidHeader = TRUE,
    #         status = "danger",
    #         title = NULL
    #       ),!!!.x
    #     )))
    #   })
    box(
      plotOutput(ns("cocDQErrors")),
      width = 12,
      solidHeader = TRUE,
      status = "danger",
      title = "Providers with the Most High Priority Issues and Errors"
    ),
    box(
      plotOutput(ns("cocHHErrors")),
      width = 12,
      solidHeader = TRUE,
      status = "danger",
      title = "Providers with the Most Household Errors"
    ),
    box(
      plotOutput(ns("cocUnshelteredHigh")),
      width = 12,
      solidHeader = TRUE,
      status = "danger",
      title = "Unsheltered High Priority Issues (User's Default Provider)"
    ),
    box(
      plotOutput(ns("DeskTimePlotCoC")),
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Longest Data Entry Delay Medians (in the past 365 days)"
    ),
    box(
      imageOutput(ns("cocDQWarnings")),
      width = 12,
      solidHeader = TRUE,
      height = "auto",
      status = "warning",
      title = "Providers with the Most Data Quality Warnings"
    ),
    box(
      plotOutput(ns("cocDQErrorTypes")),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = "Top 10 Error Types"
    ),
    box(
      plotOutput(ns("cocDQWarningTypes")),
      width = 12,
      solidHeader = TRUE,
      status = "primary",
      title = "Top 10 Warning Types"
    ),
    box(
      plotOutput(ns("cocEligibility")),
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Providers with Potential Eligibility Issues"
    )
    ,
    fluidRow(
      box(
        DT::dataTableOutput(ns("cocOverlap")),
        title = "Top 20 Providers with Overlapping Entry Exits",
        solidHeader = TRUE,
        status = "warning"
      ),
      box(
        DT::dataTableOutput(ns("cocLongStayers")),
        title = "Extremely Long Stayers",
        solidHeader = TRUE,
        status = "warning"
      ),
      box(
        DT::dataTableOutput(ns("cocRRHDestination")),
        title = "Destinations & Rapid Rehousing",
        solidHeader = TRUE,
        status = "warning"
      ),
      box(
        DT::dataTableOutput(ns("cocWidespreadIssues")),
        title = "Widespread Issues (Training focus)",
        solidHeader = TRUE,
        status = "primary"
      )
    )
  ),
  tabItem(
    tabName = "ceCoC",
    box(
      plotOutput(ns("cocAPsNoReferrals")),
      width = 6,
      title = "Access Points Creating Referrals"
    ),
    box(
      DT::dataTableOutput(ns("cocAPsNoReferralsList")),
      width = 6,
      title = "APs Not Creating Referrals"
    ),
    box(
      imageOutput(ns("dq_plot_hh_no_spdat")),
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Current Households Without SPDAT (minus Veterans)"
    ),
    box(
      imageOutput(ns("dq_plot_outstanding_referrals")),
      width = 12,
      solidHeader = TRUE,
      status = "warning",
      title = "Top 20 Providers with Old Outstanding Referrals"
    ),
    fluidRow(
      box(
        pickerInput(
          inputId = ns("unshEntriesByMonth_County"),
          label = "Select County/-ies",
          choices = sort(unsheltered_by_month()$County) %>%
            unique(),
          selected = c("Lake",
                       "Ashtabula",
                       "Trumbull",
                       "Geauga",
                       "Portage"),
          multiple = TRUE,
          options = pickerOptions(
            liveSearch = TRUE,
            liveSearchStyle = 'contains',
            actionsBox = TRUE
          ),
          width = "100%"
        ),
        airDatepickerInput(
          inputId = ns("unshEntriesByMonth_ReportStart"),
          label = "Report Start Month",
          dateFormat = "MM yyyy",
          max =
            ymd(floor_date(meta_HUDCSV$Export_Date, unit = "month") - days(1)),
          min =
            ymd(floor_date(meta_HUDCSV$Export_Start, unit = "month")),
          view = "month",
          value =
            ymd(floor_date(
              meta_HUDCSV$Export_Date - days(182), unit = "month"
            )),
          minView = "months",
          addon = "none",
          autoClose = TRUE,
          width = "25%"
        ),
        plotlyOutput(ns("cocUnshelteredEntriesByMonth")),
        width = 12,
        title = "Unsheltered Entries by Month",
        footer = "Where the CountyServed data was not answered, the
                  County where the user who created the project stay is based was
                  substituted.",
        status = "info",
        solidHeader = TRUE
      )
    )
  )
  )
}
    
#' data_quality Server Functions
#'
#' @noRd 
mod_data_quality_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
  })
}
    
## To be copied in the UI
# mod_data_quality_ui("data_quality_1")
    
## To be copied in the server
# mod_data_quality_server("data_quality_1")
