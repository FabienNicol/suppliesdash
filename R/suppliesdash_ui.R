## UI -----------------------------------------------------------------------------------
## |__sidebar ---------------------------------------------------------------------------
sidebar <- function(myData) {
  shinydashboard::dashboardSidebar(

  # shiny::includeCSS("www/style.css"),
  shiny::includeCSS(system.file("dash_app", "www", "style.css", package = "suppliesLFPefs")),
  width = 500,
  shinydashboard::sidebarMenu(id = "sidebar_menu",
                              # shiny::fluidRow(
                              #   column(width=6, textInput("USER", "Login", "fabien")),
                              #   column(width=6, passwordInput("PSW", "Password", "fabien"))
                              # ),
                              # hr(),
                              shiny::fluidRow(
                                shiny::column(width = 7,
                                              shiny::selectInput("top_SuppliesValue", "select supplies:",
                                                                 choices = myData$RVP %>%
                                                                   dplyr::filter(!is.na(SuppliesLevel0)) %>%
                                                                   dplyr::select(dplyr::matches("SuppliesLevel[0-4]")) %>%
                                                                   dplyr::distinct() %>%
                                                                   # dplyr::arrange(SuppliesLevel0, SuppliesLevel1, SuppliesLevel2,
                                                                   #                SuppliesLevel3, SuppliesLevel4) %>%
                                                                   #sort columns alphabetically
                                                                   dplyr::select(order(colnames(.))) %>%
                                                                   dplyr::arrange_all() %>%
                                                                   set_names_vect(),  # suppliesInitValues,
                                                                 # selected = "selected_SuppliesValue", # suppliesInitValue(),
                                                                 multiple = F, selectize = T),
                                              shiny::selectInput("top_SuppliesSplit", "Select Supplies to split on:",
                                                                 choices = c("LFP"),  # suppliesInitLevel(),
                                                                 # selected = "selected_SuppliesSplit", # suppliesInitLevel(),
                                                                 multiple = F, selectize = F),
                                              shiny::selectInput("filter_part", "optional, filter a subcategory:",
                                                                 choices = c("all" = "all",
                                                                             "ink_Liters" = "ink",
                                                                             "media_000sqm" = "media",
                                                                             "part_units" = "part",
                                                                             "hw_units" = "hw-cp"),
                                                                 selected = "all",
                                                                 multiple = F, selectize = F)
                                ),
                                shiny::column(width = 5,
                                              shiny::selectInput("top_RegionValue", "select region:",
                                                                 choices = myData$RVP %>%
                                                                   dplyr::select(dplyr::matches("RegionLevel[0-4]")) %>%
                                                                   dplyr::bind_rows(dplyr::tibble(RegionLevel0 = "EMEAR",
                                                                                                  RegionLevel1 = "Europe",
                                                                                                  RegionLevel2 = "CEE",
                                                                                                  RegionLevel4 = "EE_Balk&Gr_TR")) %>%
                                                                   dplyr::distinct() %>%
                                                                   dplyr::filter(RegionLevel0 == 'EMEAR') %>%
                                                                   dplyr::arrange(RegionLevel1, RegionLevel2, RegionLevel4) %>%
                                                                   #sort columns alphabetically
                                                                   dplyr::select(order(colnames(.)))  %>%
                                                                   unlist(use.names = F) %>%
                                                                   unique(),  # regionInitValues,
                                                                 # selected = "selected_regionValue", # regionInitValue(),
                                                                 multiple = F, selectize = F),
                                              shiny::selectInput("top_RegionSplit", "Select region to split on:",
                                                                 choices = c("SuppliesLevel0"),   # regionInitLevel(),
                                                                 # selected = "selected_regionLevel", # regionInitLevel(),
                                                                 multiple = F, selectize = F)
                                )
                              ),
                              shiny::hr(),
                              shiny::fluidRow(
                                shiny::column(width = 7,
                                              shiny::selectInput("top_yearRef", "Select year to rank supplies:",
                                                                 choices = c(2017, 2018), selected = 2018,
                                                                 multiple = F, selectize = F),
                                              shiny::selectInput("top_FYs", "Display FYs:",
                                                                 choices = c("All", 2017, 2018),
                                                                 selected = "All",
                                                                 multiple = T, selectize = F)
                                ),
                                shiny::column(width = 5,
                                              shiny::selectInput("top_topN", "Select max number of models to display:",
                                                                 choices = 3:25, selected = 10,
                                                                 multiple = F, selectize = F),
                                              shiny::checkboxInput("checkbox_free_y", label = "free y-axis scale",
                                                                   value = TRUE)
                                )
                              ),
                              shiny::hr(),
                              shiny::fluidRow(
                                # shiny::column(width = 6,
                                #               shiny::radioButtons("graphType", "Select graph type:",
                                #                                   c("interactive", "plain"),
                                #                                   selected = "plain")
                                # ),
                                shiny::column(width = 12, #6,
                                              align = "center",
                                              shiny::actionButton("goDetailsTemp", label = "",
                                                                  icon = shiny::icon("calculator"), #width = "80%",
                                                                  style = 'padding:15px; font-size:150%')
                                )
                              ),
                              shiny::hr(),
                              shinydashboard::menuItem("Dashboard", tabName = "dashboard", icon = shiny::icon("dashboard")),
                              #alternative icons #list-alt, clipboard-list, list-ul
                              shinydashboard::menuItem("Sku in the selection", tabName = "sku_in_selection", icon = shiny::icon("list")),
                              shinydashboard::menuItem("About", tabName = "about", icon = shiny::icon("th")),
                              shinydashboard::menuItem("Debug", tabName = "debug", icon = shiny::icon("bug")),
                              shiny::hr(),
                              shiny::uiOutput("menuItem_specific_ui")
  )
  )
}

## |__body ------------------------------------------------------------------------------
body <- shinydashboard::dashboardBody(
  ## |_____persistent info --------------------------------------------------------------
  shiny::fluidRow(
    shiny::column(6, shinydashboard::infoBoxOutput("topBox", width = NULL)),
    shiny::column(6, shinydashboard::infoBoxOutput("TotBox", width = NULL))
  ),
  shinydashboard::tabItems(
    ## |_____dashboard tab --------------------------------------------------------------
    shinydashboard::tabItem("dashboard",
                            shinydashboard::tabBox(id = "tabs", width = 12,
                                                   shiny::tabPanel(title = "Revenue", value = "rev",
                                                                   shiny::hr(),
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 6,
                                                                                         shiny::h3(shiny::textOutput("RegionSplityearRef")),
                                                                                         shiny::plotOutput("plotRegion")
                                                                                         #plotly NOT YET AVAILABLE FOR TREEMAP Aug18#
                                                                     ),
                                                                     shinydashboard::box(width = 6,
                                                                                         shiny::h3(shiny::textOutput("SuppliesSplityearRef")),
                                                                                         shiny::plotOutput("plotSupplies")
                                                                                         #plotly NOT YET AVAILABLE FOR TREEMAP Aug18#
                                                                     )
                                                                   ),
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::fluidRow(
                                                                                           shinydashboard::box(width = 12,
                                                                                                               shiny::column(12,
                                                                                                                             shiny::h3("Time series"),
                                                                                                                             shiny::uiOutput("timeSeries_R")
                                                                                                                             # plotlyOutput("timeSeries")
                                                                                                               )
                                                                                           )
                                                                                         ),
                                                                                         # HIDE, FOR SIMPLIFICATION, NOT SO USEFUL
                                                                                         # shiny::fluidRow(
                                                                                         #   shinydashboard::box(width = 12,
                                                                                         #                       shiny::column(12,
                                                                                         #                                     shiny::h3("Time series data"),
                                                                                         #                                     DT::dataTableOutput("ts_input_Rdata")
                                                                                         #                       )
                                                                                         #   )
                                                                                         # ),
                                                                                         shiny::fluidRow(
                                                                                           shinydashboard::box(width = 12,
                                                                                           #                     shiny::column(1,
                                                                                           #                                   shiny::numericInput("height_lollipop_R", "height",
                                                                                           #                                                       value = 400, min = 400, max = 2000,
                                                                                           #                                                       width = "100%")
                                                                                           #                     ),
                                                                                                               shiny::column(12,
                                                                                                                             shiny::h3("Ranking"),
                                                                                                                             shiny::uiOutput("lollipop_R")
                                                                                                               )
                                                                                           )
                                                                                         ),
                                                                                         shiny::fluidRow(
                                                                                           shinydashboard::box(width = 12,
                                                                                                               shiny::column(12,
                                                                                                                             shiny::h3("Categories"),
                                                                                                                             shiny::uiOutput("mosaic_R")
                                                                                                               )
                                                                                           )
                                                                                         )
                                                                     )
                                                                   )
                                                   ),
                                                   shiny::tabPanel(title = "Volume", value = "vol",
                                                                   shiny::fluidRow(
                                                                     shiny::fluidRow(
                                                                       shinydashboard::box(width = 12,
                                                                         shiny::column(8,
                                                                                       shiny::h5("On this panel, and the fit rate panel, the volume for 4 colors and 8-10 are an ", shiny::strong("estimate"), " of the ink volume for the corresponding HW models, in particular:"),
                                                                                       shiny::br(),
                                                                                       shiny::h5("  - 4c on SPx700 isn't the sum of T636-4C and T596-4C and T642-4C, only a fraction of it."),
                                                                                       shiny::h5("  - 4C on SC-S406x0 and SC-S606x0 isn't the sum of T890-4C or T891-4C, only a fraction of them."),
                                                                                       shiny::br(),
                                                                                       shiny::h5("But, on the panels with Revenue information (Revenue, RVP, Variance), 4 colours means 4 colour cartridges (not 4 color printers).")
                                                                         ),
                                                                         shiny::column(4,
                                                                                       shiny::img(src = "www/cartridge_vs_ptr_compatibility.png",
                                                                                      # now use shiny::addResourcePath in launchApp
                                                                                      # shiny::tags$img(src = "cartridge_vs_ptr_compatibility.png",
                                                                                      # shiny::img(# src = "cartridge_vs_ptr_compatibility.png",
                                                                                                  # src = system.file("dash_app", "www", "cartridge_vs_ptr_compatibility.png",
                                                                                                  #                   package = "suppliesLFPefs"),
                                                                                                  width = 350,
                                                                                                  height = 350,
                                                                                                  align = "center")
                                                                         )
                                                                       )
                                                                     ),
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::fluidRow(
                                                                                           shinydashboard::box(width = 12,
                                                                                                               shiny::column(12,
                                                                                                                             shiny::h3("Time series"),
                                                                                                                             shiny::uiOutput("timeSeries_V")
                                                                                                                             # plotlyOutput("timeSeries")
                                                                                                               )
                                                                                           )
                                                                                         ),
                                                                                         # HIDE, FOR SIMPLIFICATION, NOT SO USEFUL
                                                                                         # shiny::fluidRow(
                                                                                         #   shinydashboard::box(width = 12,
                                                                                         #                       shiny::column(12,
                                                                                         #                                     shiny::h3("Time series data"),
                                                                                         #                                     DT::dataTableOutput("ts_input_Vdata")
                                                                                         #                       )
                                                                                         #   )
                                                                                         # ),
                                                                                         shiny::fluidRow(
                                                                                           shinydashboard::box(width = 12,
                                                                                                               # shiny::column(1,
                                                                                                               #               shiny::numericInput("height_lollipop_V", "height",
                                                                                                               #                                   value = 400, min = 400, max = 2000,
                                                                                                               #                                   width = "100%")
                                                                                                               # ),
                                                                                                               shiny::column(12,
                                                                                                                             shiny::h3("Ranking"),
                                                                                                                             shiny::uiOutput("lollipop_V")
                                                                                                                             #plotlyOutput("lollipop")
                                                                                                               )
                                                                                           )
                                                                                         ),
                                                                                         shiny::fluidRow(
                                                                                           shinydashboard::box(width = 12,
                                                                                                               shiny::column(12,
                                                                                                                             shiny::h3("Categories"),
                                                                                                                             shiny::uiOutput("mosaic_V")
                                                                                                                             # plotlyOutput("mosaic")
                                                                                                               )
                                                                                           )
                                                                                         )
                                                                     )
                                                                   )
                                                   ),
                                                   shiny::tabPanel(title = "RVP_region", value = "rvp_region",
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::column(1,
                                                                                                       shiny::numericInput("height_rvp_region", "height",
                                                                                                                           value = 700, min = 400, max = 2000,
                                                                                                                           width = "100%")
                                                                                         ),
                                                                                         shiny::column(11,
                                                                                                       shiny::h6("MPS revenue not taken into account for now."),
                                                                                                       shiny::h6("A detailled view per channel (transactional, QT, Store, but not yet MPS) can be viewed on the next tab, RVP_channel."),
                                                                                                       shiny::h3("Revenue = Volume x Price"),
                                                                                                       shiny::uiOutput("RVP_region")
                                                                                         )
                                                                     )
                                                                   )
                                                   ),
                                                   shiny::tabPanel(title = "RVP_channel", value = "rvp_channel",
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::column(1,
                                                                                                       shiny::numericInput("height_rvp_channel", "height",
                                                                                                                           value = 700, min = 400, max = 2000,
                                                                                                                           width = "100%")
                                                                                         ),
                                                                                         shiny::column(11,
                                                                                                       shiny::h3("Revenue = Volume x Price"),
                                                                                                       shiny::uiOutput("RVP_channel")
                                                                                         )
                                                                     )
                                                                   )
                                                   ),
                                                   shiny::tabPanel(title = "RVP_mediaSize", value = "rvp_mediasize",
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::column(1,
                                                                                                       shiny::numericInput("height_rvp_mediasize", "height",
                                                                                                                           value = 700, min = 400, max = 2000,
                                                                                                                           width = "100%")
                                                                                         ),
                                                                                         shiny::column(11,
                                                                                                       shiny::h6("Tab only relevant for Media."),
                                                                                                       shiny::h3("Revenue = Volume x Price"),
                                                                                                       shiny::uiOutput("RVP_mediaSize")
                                                                                         )
                                                                     )
                                                                   )
                                                   ),
                                                   shiny::tabPanel(title = "FitRate", value = "fr",
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 12,
                                                                                         # SIMPLIFICATION
                                                                                         # shiny::column(2,
                                                                                         #               # shiny::h6("Max fitrate displayed"),
                                                                                         #               shiny::numericInput("frmax", "Max fit rate",
                                                                                         #                                   value = 2.0, min = 0, max = 100, step = 0.1)
                                                                                         # ),
                                                                                         shiny::column(12,
                                                                                                       shiny::h6("Only relevant for SuppliesLevel4 inks. For now, does not work with higher level of aggregation."),
                                                                                                       shiny::h6("HW sales are from BPT for sales from FY2011."),
                                                                                                       shiny::h6("Estimated Machine in Field (MIF) and cumulated HW unit sold (cumQty) are different, as there is an attrition rate applied to MIF."),
                                                                                                       shiny::h6(""),
                                                                                                       shiny::h6("The attrition function is the same for all models, and it considers printers are sold in 3 segment with 3 different attrition profiles:"),
                                                                                                       shiny::h6("  - segment A (10% of printers sold), the number of printers using Epson Genuine decline from 100% to 0% from month 25 to month 48."),
                                                                                                       shiny::h6("  - segment B (70%), from month 36 to month 96 (end of 8th year)."),
                                                                                                       shiny::h6("  - segment C (20%), from beginning of 6th year to end of 20th year)."),
                                                                                                       shiny::h3("Volume = Fit rate x MIF"),
                                                                                                       shiny::strong("Select ONE SuppliesLevel4, such as T800_Fusion"),
                                                                                                       shiny::uiOutput("FR")
                                                                                         )
                                                                     ),
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::plotOutput("attrition")
                                                                     )
                                                                   )
                                                   ),
                                                   shiny::tabPanel(title = "Variance", value = "gap",
                                                                   shiny::fluidRow(
                                                                     shinydashboard::box(width = 12,
                                                                                         shiny::column(2,
                                                                                                       shiny::selectInput("VarianceDriver", "Select a driver",
                                                                                                                          choices = c("RegionLevel2", "RegionLevel4", "SuppliesLevel2", "SuppliesLevel3", "SuppliesLevel4", "fDate"),
                                                                                                                          multiple = F, selectize = F),
                                                                                                       shiny::dateRangeInput("VarianceDateRange",
                                                                                                                             label = 'Date range input of the reference period (for instance FY-1_YTD): ',
                                                                                                                             start = lubridate::ymd("2017-04-01"),
                                                                                                                             end = lubridate::ymd("2017-12-01") #suppliesdash_PreviousMonth(Sys.Date()) #PreviousMonth
                                                                                                       # input$dateRange[1], input$dateRange[2]
                                                                                                       )
                                                                                         ),
                                                                                         shiny::column(10,
                                                                                                       shiny::h3("Revenue Variance"),
                                                                                                       shiny::uiOutput("Variance")
                                                                                         )
                                                                     )
                                                                   )
                                                   )
                            )
    ),
    ## |_____sku list tab ----------------------------------------------------------------
    shinydashboard::tabItem("sku_in_selection",
                            shiny::fluidRow(
                              shinydashboard::box(width = 12,
                                                  shiny::column(12,
                                                                shiny::h3("List of skus in the supplies and region selection"),
                                                                DT::dataTableOutput("sku_in_selection")  #based on "ts_input_Rdata"
                                                  )
                              )
                            )
    ),
    ## |_____about tab -------------------------------------------------------------------
    shinydashboard::tabItem("about",
                            shiny::helpText("The data is from ", shiny::br(),
                                            "  - SAP (COPA daily sales, SD_billing) ", shiny::br(),
                                            "  - and CRM (QT and Store)", shiny::br(),
                                            shiny::br(),
                                            "The data 3 different granularity is contrained by the type of data:", shiny::br(),
                                            "  - RVP, analysis of (Revenue) or (Revenue and its decomposition in Volume and Price).", shiny::br(),
                                            "  - V, volume analysis (enables to split 4 colour models from 8/10 colour models (T636, T890)", shiny::br(),
                                            "  - FR, Fit Rate (requiring long sales history, the CEE can't be split).", shiny::br(),
                                            shiny::br(),
                                            "The HW installed base is computed with", shiny::br(),
                                            "  - HW unit sales", shiny::br(),
                                            "  - attrition rate")
    ),
    ## |_____debug -----------------------------------------------------------------------
    shinydashboard::tabItem("debug",
                            shiny::fluidRow(
                              shiny::column(6,
                                            shiny::verbatimTextOutput("out0"),
                                            shiny::verbatimTextOutput("out1"),
                                            shiny::verbatimTextOutput("out2"),
                                            shiny::verbatimTextOutput("out4"),
                                            shiny::verbatimTextOutput("out5"),
                                            shiny::verbatimTextOutput("out6"),
                                            shiny::verbatimTextOutput("out7"),
                                            shiny::hr(),
                                            shiny::verbatimTextOutput("out20"),
                                            shiny::verbatimTextOutput("out21"),
                                            shiny::verbatimTextOutput("out22"),
                                            shiny::verbatimTextOutput("out41"),
                                            shiny::verbatimTextOutput("out43"),
                                            shiny::verbatimTextOutput("out44"),
                                            shiny::verbatimTextOutput("out45"),
                                            shiny::verbatimTextOutput("outlol1"),
                                            shiny::verbatimTextOutput("outlol3"),
                                            shiny::verbatimTextOutput("outlol4"),
                                            shiny::verbatimTextOutput("outlol6"),
                                            shiny::verbatimTextOutput("outlol8")
                              ),
                              shiny::column(6,
                                            shiny::verbatimTextOutput("out8"),
                                            shiny::verbatimTextOutput("out10"),
                                            shiny::verbatimTextOutput("out11"),
                                            shiny::verbatimTextOutput("out12"),
                                            shiny::hr(),
                                            shiny::verbatimTextOutput("outts1"),
                                            shiny::verbatimTextOutput("outts3"),
                                            shiny::verbatimTextOutput("outts4"),
                                            shiny::verbatimTextOutput("outts6"),
                                            shiny::verbatimTextOutput("outts7"),
                                            shiny::verbatimTextOutput("outts8"),
                                            shiny::verbatimTextOutput("outts9")
                              )
                            )
    )
)
)


## |__page ------------------------------------------------------------------
# In order to have the data available use factory pattern for ui and server definition

suppliesdash_ui <- function(myData) {
  shinydashboard::dashboardPage(
    skin = "purple",
    shinydashboard::dashboardHeader(title = "Supplies Data Exploration Dashboard",
                                    titleWidth = 400),
    sidebar(myData), body
  )
}
