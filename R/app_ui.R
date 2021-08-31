#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic 
    dashboardPage(
      skin = "black",
      dashboardHeader(title = "R minor elevated"),
      dashboardSidebar(
        sidebarMenu(
          id = "sidebarmenuid",
          menuItem("Home",
                   tabName = "homeTab"),
          menuItem("Prioritization",
                   tabName = "prioritizationListTab"),
          menuItem("Client Counts",
                   tabName = "currentProviderLevel"),
          # menuItem(
          #   "Ending Veteran Homelessness",
          #   menuSubItem("Active List", tabName = "vetActiveList"),
          #   menuSubItem("USICH Benchmarks", tabName = "dashUSICH"),
          #   menuSubItem("Inflow Outflow", tabName = "flow")
          # ),
          menuItem("COVID-19 Vaccine Distribution",
                   menuSubItem("Vaccine Status", tabName = "vaccineStatus"),
                   menuSubItem("Second Dose Logistics", tabName = "vaccineSecondDose")
          ), 
          menuItem("Bed and Unit Utilization",
                   tabName = "utilizationTab"),
          menuItem(
            "Data Quality",
            menuSubItem("Provider-level", tabName = "dqTab"),
            menuSubItem("Data Entry Timeliness", tabName = "deskTime"),
            menuSubItem("Unsheltered", tabName = "unsheltered"),
            menuSubItem("Region-level", tabName = "dqRegion"),
            menuSubItem("System-wide", tabName = "dqCoC"),
            menuSubItem("CE Summary", tabName = "ceCoC")
          ),
          menuItem("BoS CoC Competition",
                   tabName = "cocCompetitionTab"),
          menuItem(
            "Quarterly Performance Report",
            menuItem(
              "Community Need",
              tabName = "spdatTab",
              menuSubItem("PSH/RRH Detail",
                          tabName = "spdat1-Tab"),
              menuSubItem("County Detail",
                          tabName = "spdat2-Tab")
            ),
            menuSubItem("Length of Stay",
                        tabName = "LoS-Tab"),
            menuSubItem("Exits to Permanent Housing",
                        tabName = "PHTab"),
            menuSubItem("Non-Cash Benefits at Exit",
                        tabName = "NCB-Tab"),
            menuSubItem("Health Insurance at Exit",
                        tabName = "HI-Tab"),
            menuSubItem("Income Growth",
                        tabName = "income-Tab"),
            menuSubItem("Rapid Placement for RRH",
                        tabName = "rapid-Tab"),
            menuSubItem("RRH Spending",
                        tabName = "spendingTab")
          )
        ),
        HTML(
          paste0(
            "<br>&emsp;Data last refreshed:&emsp;<br>&emsp;",
            format(meta_HUDCSV_Export_Date, "%m-%d-%Y %I:%M %p")
            ,
            "<p><p>&emsp;" # add short message here if you want <-
          )
        ),
        br(),
        br(),
        br(),
        br(),
        actionButton(
          inputId = "logOutButton",
          label = "Log Out",
          onclick =
            "window.open('https://ohiobalanceofstatecoc.shinyapps.io/Rminor_elevated/__logout__/')"
        )
      ),
      dashboardBody(
        tabItems(
          tabItem(
            tabName = "homeTab",
            htmlOutput("headerHome"),
            width = 12
          ),
          tabItem(
            tabName = "prioritizationListTab",
            fluidRow(box(
              htmlOutput("headerPrioritization"), width = 12
            )),
            fluidRow(box(
              pickerInput(
                label = "Select County/-ies",
                inputId = "prioritizationCounty",
                multiple = TRUE,
                choices = regions() %>%
                  filter(County != "Mahoning") %>%
                  arrange(County) %>% pull(County),
                options = pickerOptions(
                  liveSearch = TRUE,
                  liveSearchStyle = 'contains',
                  actionsBox = TRUE
                )
              ),
              downloadButton("downloadActiveList", "Download")
            ),
            width = 12),
            fluidRow(
              box(
                DT::dataTableOutput("prioritizationData"),
                width = 12,
                footer = "Dark gray cells mean the client has a Data Quality issue that may be causing incorrect information to show."
              )
            )
          ),
          tabItem(
            tabName = "currentProviderLevel",
            fluidRow(box(htmlOutput("headerCurrent"), width = 12)),
            fluidRow(box(
              pickerInput(
                label = "Select Provider",
                inputId = "currentProviderList",
                choices = providers,
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains')
              ),
              dateRangeInput(
                "dateRangeCount",
                "Date Range",
                min = meta_HUDCSV_Export_Start,
                format = "mm/dd/yyyy",
                width = 300
              ),
              width = 12
            )),
            fluidRow(box(
              DT::dataTableOutput("clientCountSummary"),
              width = 12
            )),
            fluidRow(box(
              DT::dataTableOutput("clientCountData"),
              width = 12
            ))
          ),
          tabItem(
            tabName = "vaccineSecondDose",
            fluidPage(
              fluidRow(box(htmlOutput(
                "headerVaccine",
                width = 12
              ))),
              fluidRow(box(
                pickerInput(
                  label = "Select County/-ies",
                  inputId = "vaccineCounty",
                  multiple = TRUE,
                  choices = regions() %>%
                    arrange(County) %>% pull(County),
                  options = pickerOptions(
                    liveSearch = TRUE,
                    liveSearchStyle = 'contains',
                    actionsBox = TRUE
                  )
                ),
                width = 12
              )),
              fluidRow(
                box(
                  DT::dataTableOutput("vaccineSecondDoseOverdue"),
                  title = "Overdue for Second Dose",
                  status = "danger",
                  width = 12
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("vaccineSecondDose3Days"),
                  title = "Second Dose Due in the Next 3 Days",
                  status = "warning",
                  width = 12
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("vaccineSecondDose7Days"),
                  title = "Second Dose Due in the Next 7 Days",
                  status = "info",
                  width = 12
                )
              ),
              fluidRow(
                box(
                  DT::dataTableOutput("vaccineSecondDoseNextWeek"),
                  title = "Second Dose Due in 8 Days or More",
                  status = "success",
                  width = 12
                )
              )
            )
          ),
          tabItem(
            tabName = "vaccineStatus",
            fluidPage(
              fluidRow(
                shinydashboard::box(htmlOutput("headerVaccineStatus", width = 12))
              ),
              fluidRow(shinydashboard::box(
                pickerInput(
                  label = "Select County/-ies",
                  inputId = "vaccineStatusCounty",
                  multiple = TRUE,
                  choices = regions() %>%
                    arrange(County) %>% pull(County),
                  options = pickerOptions(
                    liveSearch = TRUE,
                    liveSearchStyle = 'contains',
                    actionsBox = TRUE
                  )
                ),
                dateRangeInput(
                  "vaccine_status_daterange",
                  "Date Range",
                  start = ymd(hc_bos_start_vaccine_data),
                  end = today(),
                  min = meta_HUDCSV_Export_Start,
                  format = "mm/dd/yyyy",
                  width = 300
                ),
                width = 12
              )), 
              fluidRow(
                shinydashboard::box(
                  DT::dataTableOutput("vaccineStatusDataTable"),
                  width = 12
                )
              )
            )
          ),
          tabItem(
            tabName = "utilizationTab",
            fluidPage(
              fluidRow(box(htmlOutput(
                "headerUtilization"
              ), width = 12)),
              fluidRow(
                box(
                  title = "NOTICE",
                  status = "warning",
                  solidHeader = TRUE,
                  "During this time, congregate facilities should be aiming to deconcentrate. If this causes fluctuations in Utilization, that is okay. Please continue to keep your clients safe."
                  ,
                  width = 6
                )
              ),
              fluidRow(box(
                pickerInput(
                  label = "Select Provider",
                  inputId = "providerListUtilization",
                  choices = c(sort(utilization_bed()$ProjectName)),
                  options = pickerOptions(liveSearch = TRUE,
                                          liveSearchStyle = 'contains'),
                  width = "100%"
                ),
                airDatepickerInput(
                  inputId = "utilizationDate",
                  label = "Click to Choose a Month",
                  max = ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
                  min = ymd(floor_date(ymd(
                    meta_HUDCSV_Export_End
                  ), "month") - years(2) + days(1)),
                  dateFormat = "MM yyyy",
                  view = "month",
                  value =
                    ymd(floor_date(meta_HUDCSV_Export_Date, unit = "month") - days(1)),
                  minView = "months",
                  addon = "none",
                  autoClose = TRUE,
                  width = '50%'
                ),
                width = 12
              )),
              fluidRow(box(
                infoBoxOutput("utilizationSummary0", width = '100%'),
                infoBoxOutput("utilizationSummary1", width = '100%'),
                infoBoxOutput("utilizationSummary2", width = '100%'),
                width = 12
              )),
              fluidRow(box(
                DT::dataTableOutput("utilizationDetail"), width = 12
              ))
            )
          ),
          tabItem(tabName = "vetActiveList",
                  fluidRow(
                    box(
                      pickerInput(
                        label = "Select County/-ies",
                        inputId = "vetCounty",
                        multiple = TRUE,
                        choices = regions() %>%
                          arrange(County) %>% pull(County),
                        options = pickerOptions(
                          liveSearch = TRUE,
                          liveSearchStyle = 'contains',
                          actionsBox = TRUE
                        )
                      ),
                      downloadButton("downloadVeteranActiveList", "Download")
                    ),
                    width = 12
                  ),
                  fluidRow(
                    box(
                      DT::dataTableOutput("VeteranActiveList"),
                      title = "Veteran Active List",
                      width = 12
                    )
                  )),
          mod_data_quality_ui("dq")
          ,
          tabItem(
            tabName = "cocCompetitionTab",
            fluidRow(box(
              htmlOutput("headerCoCCompetitionProjectLevel"),
              width = 12
            )),
            fluidRow(box(
              pickerInput(
                inputId = "pe_provider",
                label = "Select your CoC-funded Provider",
                choices = sort(pe_validation_summary()$AltProjectName) %>%
                  unique(),
                selected = pe_validation_summary()$AltProjectName[1],
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
                width = "100%"
              ),
              width = 12
            )),
            fluidRow(
              box(
                DT::dataTableOutput("pe_ProjectSummary"),
                width = 12,
                title = "Score Summary",
                status = "info",
                solidHeader = TRUE,
                collapsible = TRUE
              )
            ),
            fluidRow(
              tabBox(
                id = "tabs",
                # title = "Client Detail",
                tabPanel(
                  "Exits to Permanent Housing",
                  DT::dataTableOutput("pe_ExitsToPH")
                ),
                # tabPanel("Moved into Own Housing",
                #          DT::dataTableOutput("pe_OwnHousing")),
                # tabPanel(
                #   "Increased Income",
                #   DT::dataTableOutput("pe_IncreasedIncome")
                # ),
                tabPanel(
                  "Benefits & Health Insurance at Exit",
                  DT::dataTableOutput("pe_BenefitsAtExit")
                ),
                tabPanel(
                  "Living Situation at Entry",
                  DT::dataTableOutput("pe_LivingSituationAtEntry")
                ),
                tabPanel(
                  "No Income at Entry",
                  DT::dataTableOutput("pe_NoIncomeAtEntry")
                ),
                tabPanel("Length of Stay",
                         DT::dataTableOutput("pe_LengthOfStay")),
                tabPanel(
                  "Median Homeless History Index",
                  DT::dataTableOutput("pe_MedianHHI")
                ),
                tabPanel(
                  "Long Term Homeless",
                  DT::dataTableOutput("pe_LongTermHomeless")
                ),
                tabPanel(
                  "VISPDAT Score Completion",
                  DT::dataTableOutput("pe_ScoredAtPHEntry")
                ),
                width = 12
              )
            )
          ),
          mod_QPR_tabItem_ui("spdat1"),
          mod_QPR_tabItem_ui("spdat2"),
          mod_QPR_tabItem_ui("LoS"),
          tabItem(
            tabName = "PHTab",
            fluidRow(box(htmlOutput("headerExitsToPH"), width = 12)),
            fluidRow(box(
              pickerInput(
                inputId = "ExitsToPHProjectList",
                choices = c(unique(qpr_leavers()$ProjectName[qpr_leavers()$ProjectType %in% c(1:4, 8:9, 12:13)])),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
                width = "70%"
              ),
              
              dateRangeInput(
                "ExitsToPHDateRange",
                "Date Range",
                start = floor_date(today() - days(31), "year"),
                end = today(),
                min = meta_HUDCSV_Export_Start,
                format = "mm/dd/yyyy",
                width = 300
              )
            )),
            fluidRow(infoBoxOutput("ExitsToPHSummary", width = 12)),
            fluidRow(box(
              DT::dataTableOutput("ExitsToPH"), width = 12
            )),
            br(),
            br(),
            fluidRow(box(
              DT::dataTableOutput("ExitsToPHOutreach"),
              width = 12
            ))
          ),
          mod_QPR_tabItem_ui("NCB"),
          mod_QPR_tabItem_ui("HI"),
          mod_QPR_tabItem_ui("income"),
          mod_QPR_tabItem_ui("rapid"),
          tabItem(
            tabName = "spendingTab",
            fluidRow(box(htmlOutput(
              "headerRRHSpending"
            ), width = 12)),
            fluidRow(box(
              setSliderColor("#56B4E9", 1),
              pickerInput(
                inputId = "RRHSpendingOrganizationList",
                label = "Select Organization",
                choices = c(unique(sort(
                  qpr_spending()$OrganizationName
                ))),
                options = pickerOptions(liveSearch = TRUE,
                                        liveSearchStyle = 'contains'),
                width = "100%"
              ),
              dateRangeInput(
                "RRHSpendingDateRange",
                "Date Range",
                start = floor_date(today() - days(31), "year"),
                end = today(),
                min = meta_HUDCSV_Export_Start,
                format = "mm/dd/yyyy",
                width = 300
              )
            )),
            # fluidRow(infoBoxOutput("notCreatedYet"), width = 3),
            fluidRow(
              box(
                DT::dataTableOutput("RRHSpending"),
                title = "Rapid Rehousing Spending",
                width = 12
              )
            ),
            fluidRow(
              box(
                DT::dataTableOutput("HPSpending"),
                title = "Prevention Spending",
                width = 12
              )
            )
          )
          
        )
      )
    )
    
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(ext = "png"),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RminorElevated'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

