## DEPENDENCIES -------------------------------------------------------------------------
.libPaths("C:/Users/frfn0735/Local Data/R/R-3.5.1")
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(lubridate)
library(DT)
source("Supplies_helpers.R")

## DATA IMPORT --------------------------------------------------------------------------
# dat <- read_csv2("./temp/inkSalesEnriched.csv")

USERS <- read_csv2("./input/dashboard_users.csv")

dat <- list()
dat$FR <- read_csv2("./temp/FR.csv", col_types = "cccDcddd")
dat$V <- read_csv2("./temp/inkSalesEnriched_Vol.csv",   col_types = "ccDccccccccccdddd")
dat$RVP <- read_csv2("./temp/inkSalesEnriched_RVP.csv", col_types = "ccDccccccccccdddd")

PreviousMonth <- Sys.Date()
day(PreviousMonth) <- 1
PreviousMonth <- PreviousMonth + years(-1) + days(-1)

# https://stackoverflow.com/questions/28987622/starting-shiny-app-after-password-input
# https://stackoverflow.com/questions/45612126/username-and-password-validation-in-r-shiny-app
# access <- eventReactive(input$PSW, { USERS %>% filter(login == input$USER, psw == input$PSW) %>% select(RegionAccess, SuppliesAccess) })
access <- USERS %>% filter(login == "all", psw == "all") %>% select(RegionAccess, SuppliesAccess)

regionInitValue <- access %>% 
  mutate(value = str_replace(RegionAccess, ".*==", "")) %>% 
  mutate(value = str_replace_all(value,"'|\"", "")) %>% `[[`(1,"value")

regionInitLevel <- access %>% 
  mutate(value = str_replace(RegionAccess, "==.*", "")) %>% 
  mutate(value = str_replace_all(value,"'|\"", "")) %>% `[[`(1,"value")

suppliesInitValue <- access %>% 
  mutate(value = str_replace(SuppliesAccess, ".*==", "")) %>% 
  mutate(value = str_replace_all(value,"'|\"", "")) %>% `[[`(1,"value")

suppliesInitLevel <- access %>% 
  mutate(value = str_replace(SuppliesAccess, "==.*", "")) %>% 
  mutate(value = str_replace_all(value,"'|\"", "")) %>% `[[`(1,"value")


## UI -----------------------------------------------------------------------------------
## |__sidebar ---------------------------------------------------------------------------
sidebar <- dashboardSidebar(
  includeCSS("www/style.css"),
  width = 400,
  sidebarMenu(id = "sidebar_menu",
              fluidRow(
                column(width=6, textInput("USER", "Login", "fabien")),
                column(width=6, passwordInput("PSW", "Password", "fabien"))
              ),
              hr(), 
              fluidRow(
                column(width = 6,
                       selectInput("top_SuppliesValue", "select supplies:", choices=suppliesInitValue, 
                                   selected=suppliesInitValue, multiple=F, selectize=F),
                       selectInput("top_SuppliesSplit", "Select Supplies to split on:", choices=suppliesInitLevel, 
                                   selected=suppliesInitLevel, multiple=F, selectize=F)
                ),
                column(width = 6,
                       selectInput("top_RegionValue", "select region:", choices=regionInitValue, 
                                   selected=regionInitValue, multiple=F, selectize=F),
                       selectInput("top_RegionSplit", "Select region to split on:", choices=regionInitLevel, 
                                   selected=regionInitLevel, multiple=F, selectize=F)
                )
              ),
              hr(), 
              fluidRow(
                column(width = 6,
                       selectInput("top_yearRef", "Select year to rank supplies:", choices = c(2017, 2018), selected = 2018, multiple=F, selectize=F),
                       selectInput("top_topN", "Select max number of models to display:", choices=5:20, selected=10, multiple=F, selectize=F)
                ),
                column(width = 6, selectInput("top_FYs", "Display FYs:", choices = c("All", 2017,2018), selected = "All", multiple=T, selectize=F))
              ),
              hr(), 
              fluidRow(
                column(width=6, radioButtons("graphType", "Select graph type:", c("interactive", "plain"), selected = "plain")),
                column(width=6, actionButton("goDetailsTemp",label = "GO"))
              ),
              hr(), 
              hr(),
              menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
              menuItem("About", tabName = "about", icon = icon("th")),
              menuItem("Debug", tabName = "debug", icon = icon("bug")),
              hr(), 
              uiOutput("menuItem_specific_ui")
  )
)

## |__body ------------------------------------------------------------------------------
body <- dashboardBody(
  ## |_____persistent info --------------------------------------------------------------
  fluidRow(
    column(6, infoBoxOutput("topBox", width = NULL)),
    column(6, infoBoxOutput("TotBox", width = NULL))
  ),
  tabItems(
    ## |_____dashboard tab --------------------------------------------------------------
    tabItem("dashboard",
            tabBox(id = "tabs", width = 12,
                   tabPanel(title = "Revenue", value = "rev",
                            hr(),
                            fluidRow(
                              box(width = 6, h3(textOutput("RegionSplityearRef")),
                                  plotOutput("plotRegion") #plotly NOT YET AVAILABLE FOR TREEMAP Aug18#
                              ),
                              box(width = 6, h3(textOutput("SuppliesSplityearRef")),
                                  plotOutput("plotSupplies") #plotly NOT YET AVAILABLE FOR TREEMAP Aug18#
                              )
                            ),
                            fluidRow(
                              box(width=12,
                                  fluidRow(
                                    box(width=12,
                                        column(12, h3("Time series"),
                                               uiOutput("timeSeries_R")
                                               # plotlyOutput("timeSeries")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width=12,
                                        column(12, h3("Time series data"),
                                               DT::dataTableOutput("ts_input_Rdata")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width=12,
                                        column(1, numericInput("height_lollipop_R", "height_lollipop_R", value = 400, min = 400, max = 2000, width = "100%")),
                                        column(11, h3("Ranking"), uiOutput("lollipop_R")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width=12,
                                        column(12, h3("Categories"),
                                               uiOutput("mosaic_R")
                                        )
                                    )
                                  )
                              )
                            )
                   ),
                   tabPanel(title = "Volume", value = "vol",
                            fluidRow(
                              box(width=12,
                                  fluidRow(
                                    box(width=12,
                                        column(12, h3("Time series"),
                                               uiOutput("timeSeries_V")
                                               # plotlyOutput("timeSeries")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width=12,
                                        column(12, h3("Time series data"),
                                               DT::dataTableOutput("ts_input_Vdata")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width=12,
                                        column(1, numericInput("height_lollipop_V", "height_lollipop_V", value = 400, min = 400, max = 2000, width = "100%")),
                                        column(11, h3("Ranking"), 
                                               uiOutput("lollipop_V")
                                               #plotlyOutput("lollipop")
                                        )
                                    )
                                  ),
                                  fluidRow(
                                    box(width=12,
                                        column(12, h3("Categories"),
                                               uiOutput("mosaic_V")
                                               # plotlyOutput("mosaic")
                                        )
                                    )
                                  )
                              )
                            )
                   ),
                   tabPanel(title = "RVP_region", value = "rvp_region",
                            fluidRow(
                              box(width=12,
                                  column(1, numericInput("height_rvp_region", "height_rvp_region", value = 700, min = 400, max = 2000, width = "100%")),
                                  column(11, h3("Revenue = Volume x Price"), uiOutput("RVP_region"))
                              )
                            )
                   ),
                   tabPanel(title = "RVP_channel", value = "rvp_channel",
                            fluidRow(
                              box(width=12,
                                  column(1, numericInput("height_rvp_channel", "height_rvp_channel", value = 700, min = 400, max = 2000, width = "100%")),
                                  column(11, h3("Revenue = Volume x Price   - select the same region in side panel and at the top -"),
                                         uiOutput("RVP_channel")
                                  )
                              )
                            )
                   ),
                   tabPanel(title = "RVP_mediaSize", value = "rvp_mediasize",
                            fluidRow(
                              box(width=12,
                                  column(1, numericInput("height_rvp_mediasize", "height_rvp_mediasize", value = 700, min = 400, max = 2000, width = "100%")),
                                  column(11, h3("Revenue = Volume x Price   - select the same region in side panel and at the top -"),
                                         uiOutput("RVP_mediaSize")
                                  )
                              )
                            )
                   ),
                   tabPanel(title = "FitRate", value = "fr",
                            fluidRow(
                              box(width=12,
                                  column(2, h3("Max fit rate displayed"), 
                                         numericInput("frmax", "frmax", value=2.0, min = 0, max = 100, step = 0.1)
                                  ),
                                  column(10, h3("fit rate, select ONE SuppliesLevel4"),
                                         uiOutput("FR")
                                  )
                              )
                            )
                   ),
                   tabPanel(title = "Variance", value = "gap",
                            fluidRow(
                              box(width=12,
                                  column(2, 
                                         selectInput("VarianceDriver", "drivers", choices=c("RegionLevel2", "RegionLevel4", "SuppliesLevel2", "SuppliesLevel3", "SuppliesLevel4", "fDate"), 
                                                     multiple = F, selectize = F),
                                         dateRangeInput("VarianceDateRange", label = 'Date range input of the reference period (for instance FY-1_YTD): ', start = ymd("2017-04-01"), end = PreviousMonth)
                                         # input$dateRange[1], input$dateRange[2]
                                  ),
                                  column(10, h3("Variance"),
                                         uiOutput("Variance")
                                  )
                              )
                            )
                   )
            )
    ),
    ## |_____about tab -------------------------------------------------------------------
    tabItem("about",
            helpText("The data is from ", br(),
                     "  - SAP (COPA daily sales, SD_billing) ", br(),
                     "  - and CRM (QT and Store)", br(),
                     br(),
                     "The data 3 different granularity depending on the aim:", br(),
                     "  - RVP, analysis of (Revenue) or (Revenue and its decomposition in Volume and Price).", br(),
                     "  - V, volume analysis (enables to split 4 color models from 8/10 color models (T636, T890)", br(),
                     "  - FR, Fit Rate (requiring long sales history, the CEE can't be split).", br(),
                     br(),
                     "The HW installed base is computed with", br(),
                     "  - HW unit sales", br(),
                     "  - attrition rate")
    ),
    tabItem("debug",
            fluidRow(
              column(6,
                     verbatimTextOutput("out0"),
                     verbatimTextOutput("out1"),
                     verbatimTextOutput("out2"),
                     verbatimTextOutput("out3"),
                     verbatimTextOutput("out4"),
                     verbatimTextOutput("out5"),
                     verbatimTextOutput("out6"),
                     verbatimTextOutput("out7"),
                     hr(),
                     verbatimTextOutput("out20"),
                     verbatimTextOutput("out21"),
                     verbatimTextOutput("out22"),
                     verbatimTextOutput("out41"),
                     verbatimTextOutput("out43"),
                     verbatimTextOutput("out44"),
                     verbatimTextOutput("out45"),
                     verbatimTextOutput("outlol1"),
                     verbatimTextOutput("outlol3"),
                     verbatimTextOutput("outlol4"),
                     verbatimTextOutput("outlol6"),
                     verbatimTextOutput("outlol8")
              ),
              column(6,
                     verbatimTextOutput("out8"),
                     verbatimTextOutput("out9"),
                     verbatimTextOutput("out10"),
                     verbatimTextOutput("out11"),
                     verbatimTextOutput("out12"),
                     hr(),
                     verbatimTextOutput("outts1"),
                     verbatimTextOutput("outts3"),
                     verbatimTextOutput("outts4"),
                     verbatimTextOutput("outts6"),
                     verbatimTextOutput("outts7"),
                     verbatimTextOutput("outts8"),
                     verbatimTextOutput("outts9")
              )
            )
    )    
  )
)


## |__page ------------------------------------------------------------------
ui <- dashboardPage(skin = "purple",
                    dashboardHeader(title = "Supplies Data Exploration Dashboard",
                                    titleWidth = 400), 
                    sidebar, body
)


## SERVER -------------------------------------------------------------------------------
server <- function(input, output, session) { 
  
  ## HIERARCHY SELECTION ------------------------------------------------------------------------
  
  # --- filter the data based on the USER ---
  # 1/ populate the input boxes
  # 2/ filter based on the inputs
  
  access <- eventReactive(input$PSW, {
    USERS %>% filter(login == "all", psw == "all") %>% select(RegionAccess, SuppliesAccess)
  })
  
  sLevel <- reactive({
    dat$RVP %>% 
      select(matches("SuppliesLevel[0-4]")) %>%
      distinct %>%
      filter(!! rlang::parse_expr(access()[[1, "SuppliesAccess"]])) %>%
      arrange(SuppliesLevel0, SuppliesLevel1, SuppliesLevel2, SuppliesLevel3, SuppliesLevel4) %>%
      select(order(colnames(.))) #sort columns alphabetically
  })
  
  # list to pupulate the input box
  sValues <- reactive({
    sLevel() %>% unlist(use.names = F) %>% unique %>% 
      `[`(seq(match(input$top_SuppliesValue, .), length(.)))
    
  })
  
  # the Level of the selected value (a value is supposed to appear in only one level)
  SupLevD <- eventReactive(input$top_SuppliesValue, { map(sLevel(), ~ str_detect(.x, paste0("^", input$top_SuppliesValue, "$")) %>% sum) %>% 
      keep(~.x>0) %>% names
  })
  
  rLevel <- reactive({
    dat$RVP %>% 
      select(matches("RegionLevel[0-4]")) %>%
      bind_rows(tibble(RegionLevel0="EMEAR", RegionLevel1="Europe", RegionLevel2="CEE", RegionLevel4="EE_Balk&Gr_TR")) %>%
      distinct %>%
      filter(!! rlang::parse_expr(access()[[1, "RegionAccess"]])) %>%
      arrange(RegionLevel1, RegionLevel2, RegionLevel4) %>%
      select(order(colnames(.))) #sort columns alphabetically
  })
  
  rValues <- reactive({
    rLevel() %>% unlist(use.names = F) %>% unique %>%
      `[`(seq(match(input$top_RegionValue, .), length(.)))
  })
  
  RegLevD <- eventReactive(input$top_RegionValue, { map(rLevel(), ~ str_detect(.x, paste0("^", input$top_RegionValue, "$")) %>% sum) %>% 
      keep(~.x>0) %>% names
  })
  
  FYlist <- reactive({
    dat_RVP() %>% select(fDate) %>% mutate(FY=year(fDate)) %>% select(FY) %>% distinct %>% unlist(use.names = F)
  })
  
  observeEvent(input$PSW, {
    updateSelectInput(session, "top_SuppliesValue", choices = sValues(), selected=suppliesInitValue)
    updateSelectInput(session, "top_SuppliesSplit", choices = names(sLevel()) %>% `[`(seq(match(SupLevD(), .), length(.))), selected=suppliesInitLevel)
    updateSelectInput(session, "top_RegionValue", choices = rValues(), selected=regionInitValue)
    updateSelectInput(session, "top_RegionSplit", choices = names(rLevel()) %>% `[`(seq(match(RegLevD(), .), length(.))), selected=regionInitLevel)
  }) 
  
  observeEvent(list(input$top_SuppliesValue, input$RegionValue), {
    updateSelectInput(session, "top_yearRef", choices = FYlist(), selected = 2017)
    updateSelectInput(session, "top_FYs", choices = c("All", FYlist()), selected = "All")
  }) 
  
  
  # FILTER DATA according to hierarchy selection --------------------------------------------------
  # 1- issue for RegionLevel4 list (branches from RegionLevel3=="CEE")
  #    SelectionRegion() is only used to filter dat$, so bind_rows(tibble(RegionLevel0="EMEAR", RegionLevel1="Europe", RegionLevel2="CEE", RegionLevel4="EE_Balk&Gr_TR"))
  # 2- could be an issue for the tweaked SuppliesLevel4. 
  #    but it isn't as long as the SuppliesLevel names aren't changed when the Volume value are played with.
  dat_RVP <- reactive({
    sLevel() %>% left_join(dat$RVP) %>% right_join(rLevel())
  })
  dat_V <- reactive({
    sLevel() %>% left_join(dat$V) %>% right_join(rLevel())
  })
  
  dat_FR <- eventReactive(list(input$goDetailsTemp, input$frmax), {
    dat$FR %>% 
      # f_fr_filter(regionValue="EDG", regionValueLevel="RegionLevel4", regionSplit="RegionLevel4", suppliesValue="T591_11880", suppliesValueLevel="SuppliesLevel4") %>%
      f_fr_filter(regionValue=input$top_RegionValue, regionValueLevel=RegLevD(), regionSplit=input$top_RegionSplit, 
                  suppliesValue=input$top_SuppliesValue, suppliesValueLevel="SuppliesLevel4",
                  input$frmax)
  })
  
  
  # SelectionRootRegion()[1] finer(rleveD(), names(rlevel()))
  # SelectionRootRegion()[2] inputSplit
  # SelectionRootRegion()[3] inputValue
  
  # OUTPUT OBJECT ---------------------------------------------------------------------------------
  
  
  Tot <- reactive(summarise(dataRegionTree(), total=sum(total)) %>% unlist(use.names = F))
  
  output$topBox <- renderInfoBox({
    infoBox("Report for", paste0(input$top_RegionValue, " and ", input$top_SuppliesValue), icon = icon("heartbeat"), color = "purple", fill=TRUE)
  })
  
  output$TotBox <- renderInfoBox({
    infoBox(paste0("Total in FY ", input$top_yearRef), format(round(Tot(),0), big.mark=" "), icon = icon("euro"), color = "purple", fill=TRUE)
  })
  
  
  output$yearRef <- renderText(input$top_yearRef)
  output$RegionSplityearRef <- renderText({ paste0("Region Split in FY", input$top_yearRef) })
  output$SuppliesSplityearRef <- renderText({ paste0("Supplies Split in FY", input$top_yearRef) })
  
  
  
  output$out0 <- renderPrint({ list("access()", access()) })
  output$out1 <- renderPrint({ list("FYlist()", FYlist()) })
  output$out2 <- renderPrint({ list("sLevel()", sLevel()) })
  output$out3 <- renderPrint({ list("sValues()", sValues()) })
  output$out4 <- renderPrint({ list("SupLevD()", SupLevD()) })
  output$out5 <- renderPrint({ list("suppliesInitValue()", suppliesInitValue()) })
  output$out6 <- renderPrint({ list("suppliesInitLevel()", suppliesInitLevel()) })
  output$out7 <- renderPrint({ list("finer(SupLevelD(), names(sLevel()))", finer(SupLevelD(), names(sLevel()))) })
  output$out8 <- renderPrint({ list("rLevel()", rLevel()) })
  output$out9 <- renderPrint({ list("rValues()", rValues()) })
  output$out10 <- renderPrint({ list("RegLevD()", RegLevD()) })
  output$out11 <- renderPrint({ list("regionInitValue()", regionInitValue()) })
  output$out12 <- renderPrint({ list("regionInitLevel()", regionInitLevel()) })
  output$out13 <- renderPrint({ list("finer(RegLevelD(), names(rLevel()))", finer(RegLevelD(), names(rLevel()))) })
  output$out20 <- renderPrint({ list("dat_RVP()", dat_RVP()) })
  output$out21 <- renderPrint({ list("dataSuppliesTree()", dataSuppliesTree()) })
  output$out22 <- renderPrint({ list("dataRegionTree()", dataRegionTree()) })
  output$out41 <- renderPrint({ list("input$top_RegionValue", input$top_RegionValue) })
  output$out43 <- renderPrint({ list("input$top_SuppliesValue", input$top_SuppliesValue) })
  output$out44 <- renderPrint({ list("lollipop_input_V()$data", lollipop_input_V()$data) })
  output$out45 <- renderPrint({ list("subsetData_V()", subsetData_V()) })
  
  
  output$outlol1 <- renderPrint({ list("lol1: subsetData_V()", subsetData_V()) })
  output$outlol3 <- renderPrint({ list("lol3: input$top_RegionValue", input$top_RegionValue) })
  output$outlol4 <- renderPrint({ list("lol4: input$top_RegionSplit", input$top_RegionSplit) })
  output$outlol6 <- renderPrint({ list("lol6: input$top_SuppliesValue", input$top_SuppliesValue) })
  output$outlol8 <- renderPrint({ list("lol8: input$top_topN", input$top_topN) })
  
  output$outts1 <- renderPrint({ list("ts1: subsetData_V()", subsetData_V()) })
  output$outts3 <- renderPrint({ list("ts3: input$top_RegionValue", input$top_RegionValue) })
  output$outts4 <- renderPrint({ list("ts4: input$top_RegionSplit", input$top_RegionSplit) })
  output$outts6 <- renderPrint({ list("ts6: input$top_SuppliesValue", input$top_SuppliesValue) })
  output$outts7 <- renderPrint({ list("ts7: input$top_SuppliesSplit", input$top_SuppliesSplit) })
  output$outts8 <- renderPrint({ list("ts8: as.numeric(input$top_yearRef)", as.numeric(input$top_yearRef)) })
  output$outts9 <- renderPrint({ list("ts9: as.numeric(input$top_topN)", as.numeric(input$top_topN)) })
  
  # MANAGE THE DETAILS TABS -----------------------------------------------------------------------
  # top_RegionValue
  # top_SuppliesValue
  # goDetails
  
  # --- sub selection of data ---
  
  # data_RVP() recalculé qd SelectionSupplies/Region sont recalculés, qd le volet de gauche validé
  subsetData_RVP <- eventReactive(input$goDetailsTemp, {
    # select the dataset
    dat_RVP() %>% filter((!!sym(RegLevD())) == input$top_RegionValue, 
                         (!!sym(SupLevD())) == input$top_SuppliesValue)
  })
  subsetData_V <- eventReactive(input$goDetailsTemp, {
    # select the dataset
    dat_V() %>% filter((!!sym(RegLevD())) == input$top_RegionValue, 
                       (!!sym(SupLevD())) == input$top_SuppliesValue)
  })
  
  # --- build treeplot ---
  
  cat <- list()
  cat$Reg <- reactive({ finer(RegLevD(), names(rLevel())) })
  cat$subReg <- reactive({ input$top_RegionSplit })
  cat$Supplies <- reactive({ finer(SupLevD(), names(sLevel())) })
  cat$subSupplies <- reactive({ input$top_SuppliesSplit })
  
  dataRegionTree <- reactive(dataTree(subsetData_RVP(), cat=cat$Reg(), subcat=cat$subReg(), RevVol="Rev", FY=input$top_yearRef))
  dataSuppliesTree <- reactive(dataTree(subsetData_RVP(), cat=cat$Supplies(), subcat=cat$subSupplies(), RevVol="Rev", FY=input$top_yearRef))
  
  output$plotRegion <- renderPlot({
    plotTree(dataRegionTree())
  })
  
  output$plotSupplies <- renderPlot({
    plotTree(dataSuppliesTree())
  })
  
  # --- build input data for lollipop ranking on volume ---
  
  lollipop_input_V <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                         input$top_yearRef, input$top_topN, input$top_FYs), {
                                           
                                           data4plot_Lollipop(subsetData_V(), RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit, 
                                                              SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit,
                                                              yearRef=as.numeric(input$top_yearRef), rankBy="Liters", topN=as.numeric(input$top_topN), showFY=input$top_FYs,
                                                              lollipop_measure="Liters")
                                         })
  
  # --- build input data for lollipop ranking on revenue ---
  
  lollipop_input_R <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                         input$top_yearRef, input$top_topN, input$top_FYs), {
                                           
                                           data4plot_Lollipop(subsetData_RVP(), RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit, 
                                                              SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit,
                                                              yearRef=as.numeric(input$top_yearRef), rankBy="Rev", topN=as.numeric(input$top_topN), showFY=input$top_FYs,
                                                              lollipop_measure="Rev")
                                         })
  
  # --- build input data for times series for volume ---
  
  output$ts_input_Vdata <- DT::renderDataTable({ datatable(data = ts_input_V()$data %>% mutate_if(is.numeric, funs(signif(., 3))), 
                                                           extensions = 'Buttons', 
                                                           options = list(dom = "Blfrtip", 
                                                                          buttons=list("copy", 
                                                                                       list(extend="collection", 
                                                                                            buttons=c("csv"), 
                                                                                            text="Download")
                                                                          )
                                                           )
  )
  })
  
  ts_input_V <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                   input$top_yearRef, input$top_topN), {
                                     
                                     data4plot_ts(subsetData_V(), RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit,
                                                  SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit, 
                                                  yearRef=as.numeric(input$top_yearRef), rankBy="Liters_sm", topN=as.numeric(input$top_topN),
                                                  ts_measure="Liters_sm")
                                   })
  
  
  # --- build input data for times series for revenue ---
  
  ts_input_R <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                   input$top_yearRef, input$top_topN), {
                                     
                                     data4plot_ts(subsetData_RVP(), RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit,
                                                  SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit, 
                                                  yearRef=as.numeric(input$top_yearRef), rankBy="Rev_sm", topN=as.numeric(input$top_topN),
                                                  ts_measure="Rev_sm")
                                   })
  
  # --- build input data for mosaic for volume ---
  
  mosaic_input_V <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                       input$top_yearRef, input$top_FYs), {
                                         data4plot_Mosaic(subsetData_V(), RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit, 
                                                          SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit,
                                                          yearRef=as.numeric(input$top_yearRef), rankBy="Liters", topN=as.numeric(input$top_topN), showFY=input$top_FYs,
                                                          mosaic_measure="Liters")
                                       })
  
  # --- build input data for mosaic for volume ---
  
  mosaic_input_R <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                       input$top_yearRef, input$top_FYs), {
                                         data4plot_Mosaic(subsetData_RVP(), RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit, 
                                                          SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit,
                                                          yearRef=as.numeric(input$top_yearRef), rankBy="Rev", topN=as.numeric(input$top_topN), showFY=input$top_FYs,
                                                          mosaic_measure="Rev")
                                       })
  
  # --- build input data for RevVolPrice charts ---
  
  RVP_input_RVP_channel <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                              input$top_yearRef, input$top_topN), {
                                                
                                                data4plot_RVP(subsetData_RVP(), keepSize=F, keepSource=T, RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit,
                                                              SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit, 
                                                              yearRef=as.numeric(input$top_yearRef), rankBy="Rev", topN=as.numeric(input$top_topN),
                                                              RVP_facetY="source")
                                              })
  
  RVP_input_RVP_region <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                             input$top_yearRef, input$top_topN), {
                                               
                                               data4plot_RVP(subsetData_RVP(), keepSize=F, keepSource=F, RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit,
                                                             SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit, 
                                                             yearRef=as.numeric(input$top_yearRef), rankBy="Rev", topN=as.numeric(input$top_topN),
                                                             RVP_facetY="RegionSplit")
                                             })
  
  # --- build input data for media size ---
  
  RVP_input_RVP_mediaSize <- eventReactive(list(input$goDetailsTemp, input$top_RegionValue, input$top_RegionSplit, input$top_SuppliesSplit, 
                                                input$top_yearRef, input$top_topN), {
                                                  
                                                  data4plot_RVP(subsetData_RVP(), keepSize=T, keepSource=F, RRL=RegLevD(), RRV=input$top_RegionValue, RSL=input$top_RegionSplit,
                                                                SPL=SupLevD(), SPV=input$top_SuppliesValue, SSL=input$top_SuppliesSplit, 
                                                                yearRef=as.numeric(input$top_yearRef), rankBy="Rev", topN=as.numeric(input$top_topN),
                                                                RVP_facetY="size")
                                                })
  
  # --- build input for variance analysis ---
  
  Variance_input <- eventReactive(list(input$VarianceDateRange, input$VarianceDriver), {
    f_waterfall_input(subsetData_RVP(), input$VarianceDriver, input$VarianceDateRange)
  })
  
  # --- build charts output object ---
  
  output$lollipop_R <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("lollipopLY_R", height = input$height_lollipop_R),
           "plain" = plotOutput("lollipopGG_R", height = input$height_lollipop_R))
  })
  
  output$timeSeries_R <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("timeSeriesLY_R"),
           "plain" = plotOutput("timeSeriesGG_R"))
  })
  
  output$mosaic_R <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("mosaicLY_R"),
           "plain" = plotOutput("mosaicGG_R"))
  })
  
  
  output$lollipop_V <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("lollipopLY_V", height = input$height_lollipop_V),
           "plain" = plotOutput("lollipopGG_V", height = input$height_lollipop_V))
  })
  
  output$timeSeries_V <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("timeSeriesLY_V"),
           "plain" = plotOutput("timeSeriesGG_V"))
  })
  
  output$mosaic_V <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("mosaicLY_V"),
           "plain" = plotOutput("mosaicGG_V"))
  })
  
  output$RVP_channel <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("RVP_channelLY", height = input$height_rvp_channel),
           "plain" = plotOutput("RVP_channelGG", height = input$height_rvp_channel))
  })
  
  output$RVP_region <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("RVP_regionLY", height = input$height_rvp_region),
           "plain" = plotOutput("RVP_regionGG", height = input$height_rvp_region))
  })
  
  output$RVP_mediaSize <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("RVP_mediaSizeLY", height = input$height_rvp_mediasize),
           "plain" = plotOutput("RVP_mediaSizeGG", height = input$height_rvp_mediasize))
  })
  
  output$Variance <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("VarianceLY", height = "700px"),
           "plain" = plotOutput("VarianceGG", height = "700px"))
  })
  
  output$FR <- renderUI({
    switch(input$graphType,
           "interactive" = plotlyOutput("FRLY", height = "700px"),
           "plain" = plotOutput("FRGG", height = "700px"))
  })
  
  
  
  output$lollipopLY_V <- renderPlotly({
    plot_lollipop(lollipop_input_V()) %>% ggplotly()
  })
  
  output$timeSeriesLY_V <- renderPlotly({
    plot_ts(ts_input_V()) %>% ggplotly()
  })
  
  output$mosaicLY_V <- renderPlotly({
    plot_mosaic(mosaic_input_V()) %>% ggplotly()
  })
  
  output$lollipopLY_R <- renderPlotly({
    plot_lollipop(lollipop_input_R()) %>% ggplotly()
  })
  
  output$timeSeriesLY_R <- renderPlotly({
    plot_ts(ts_input_R()) %>% ggplotly()
  })
  
  output$mosaicLY_R <- renderPlotly({
    plot_mosaic(mosaic_input_R()) %>% ggplotly()
  })
  
  output$RVP_channelLY <- renderPlotly({
    plot_RVP(RVP_input_RVP_channel()) %>% ggplotly()
  })
  
  output$RVP_regionLY <- renderPlotly({
    plot_RVP(RVP_input_RVP_region()) %>% ggplotly()
  })
  
  output$RVP_mediaSizeLY <- renderPlotly({
    plot_RVP(RVP_input_RVP_mediaSize()) %>% ggplotly()
  })
  
  output$VarianceLY <- renderPlotly({
    plot_waterfall(Variance_input(), input$VarianceDriver) %>% ggplotly()
  })
  
  output$FRLY <- renderPlotly({
    plot_fr(dat_FR()) %>% ggplotly()
  })
  
  
  
  output$lollipopGG_V <- renderPlot({
    plot_lollipop(lollipop_input_V())
  })
  
  output$timeSeriesGG_V <- renderPlot({
    plot_ts(ts_input_V())
  })
  
  output$mosaicGG_V <- renderPlot({
    plot_mosaic(mosaic_input_V())
  })
  
  output$lollipopGG_R <- renderPlot({
    plot_lollipop(lollipop_input_R())
  })
  
  output$timeSeriesGG_R <- renderPlot({
    plot_ts(ts_input_R())
  })
  
  output$mosaicGG_R <- renderPlot({
    plot_mosaic(mosaic_input_R())
  })
  
  output$RVP_channelGG <- renderPlot({
    plot_RVP(RVP_input_RVP_channel())
  })
  
  output$RVP_regionGG <- renderPlot({
    plot_RVP(RVP_input_RVP_region())
  })
  
  output$RVP_mediaSizeGG <- renderPlot({
    plot_RVP(RVP_input_RVP_mediaSize())
  })
  
  output$VarianceGG <- renderPlot({
    plot_waterfall(Variance_input(), input$VarianceDriver)
  })
  
  output$FRGG <- renderPlot({
    plot_fr(dat_FR())
  })
  
  
}

## ShinyApp ------------------------------------------------------------------------------
shinyApp(ui, server)
