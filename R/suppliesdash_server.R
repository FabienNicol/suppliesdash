# options(shiny.error = browser)
# options(shiny.trace = TRUE)
# options(shiny.fullstacktrace = TRUE)

## SERVER ---------------------------------------------------------------------
# In order to have the data available use factory pattern for ui and server definition

suppliesdash_server <- function(myData) {
  function(input, output, session) {
  # all the variables are visible for all session and in server.R (not ui.R) ... they are used in ui.R
  # data_ISenriched in data/data_ISenriched.rda (opens silently with DESCRIPTION file comment (LazyData: true))
  # functions from suppliesdash_input.R
  PreviousMonth <- suppliesdash_PreviousMonth(Sys.Date())

  dat <- list()
  dat <- suppliesdash_data(myData)

  sLevel <- suppliesdash_sLevel(dat)
  rLevel <- suppliesdash_rLevel(dat)

  suppliesInitValues <- unlist_unique(sLevel)
  regionInitValues <- unlist_unique(rLevel)

  ## HIERARCHY SELECTION ------------------------------------------------------

  # --- filter the data based on the USER ---
  # 1/ populate the input boxes
  # 2/ filter based on the inputs



  # the Level of the selected value (a value is supposed to appear
  # in only one level)
  SupLevD <- shiny::eventReactive(input$top_SuppliesValue, {

    # DEBUG
    # test <- purrr::map(
    #   sLevel,
    #   ~ stringr::str_detect(.x, paste0("^", input$top_SuppliesValue, "$")
    #   ) %>% sum
    # ) %>%
    #   purrr::keep(~.x>0)
    # c <- capture.output(test) %>% paste("\n", sep="")
    # cat("the input$top: ", input$top_SuppliesValue, "\n",
    #     "SupLevD primitive is:\n", c, "\n")

    purrr::map(
      sLevel,
      ~ stringr::str_detect(.x, paste0("^", input$top_SuppliesValue, "$")
        ) %>% sum
    ) %>%
      purrr::keep(~.x>0) %>%
      names
  })

  RegLevD <- shiny::eventReactive(input$top_RegionValue, {
    purrr::map(
      rLevel,
      ~ stringr::str_detect(.x, paste0("^", input$top_RegionValue, "$")
        ) %>% sum
    ) %>%
      purrr::keep(~.x>0) %>%
      names
  })

  FYlist <- shiny::eventReactive(list(input$top_RegionValue, input$top_SuppliesValue), {
    # dat_RVP() %>%
    subsetData_RVP() %>%
      dplyr::select(fDate) %>%
      dplyr::mutate(FY = lubridate::year(fDate)) %>%
      dplyr::select(FY) %>%
      dplyr::distinct() %>%
      unlist(use.names = F)
  })

  shiny::observeEvent(input$top_SuppliesValue, {
    # DEBUG
    # cat(file = stderr(), "server-raw76> names(sLevel) ", names(sLevel), "\n")
    # cat(file = stderr(), "server-raw77> SupLevD() ", SupLevD(), "\n")
    shiny::updateSelectInput(
      session, "top_SuppliesSplit",
      choices = names(sLevel) %>% `[`(seq(match(SupLevD(), .), length(.)))
      # ,selected = suppliesInitLevel #selected default to the first item of the list
    )
  })

  shiny::observeEvent(input$top_RegionValue, {
    shiny::updateSelectInput(
      session, "top_RegionSplit",
      choices = names(rLevel) %>% `[`(seq(match(RegLevD(), .), length(.)))
      # ,selected = regionInitLevel #selected default to the first item of the list
    )
  })

  # shiny::observeEvent({
  #   shiny::updateSelectInput(
  #     session, "top_SuppliesValue",
  #     choices = suppliesInitValues #suppliesdash_SIV(sLevel)
  #   )
  # })
  #
  # shiny::observeEvent({
  #   shiny::updateSelectInput(
  #     session, "top_RegionValue",
  #     choices = regionInitValues
  #   )
  # })

  # shiny::observeEvent({
  #   shiny::updateDateRangeInput(
  #     session, "VarianceDateRange",
  #     start = lubridate::ymd("2017-04-01"),
  #     end = PreviousMonth
  #   )
  # })

  shiny::observeEvent(list(input$top_SuppliesValue, input$top_RegionValue), {
    shiny::updateSelectInput(session, "top_yearRef",
                             choices = FYlist(), selected = 2017)
    shiny::updateSelectInput(session, "top_FYs",
                             choices = c("All", FYlist()), selected = "All")
  })


  # FILTER DATA according to hierarchy selection ------------------------------
  # 1- issue for RegionLevel4 list (branches from RegionLevel3 == "CEE")
  #    SelectionRegion() is only used to filter dat$, so
  #    bind_rows(tibble(RegionLevel0 = "EMEAR", RegionLevel1="Europe",
  #              RegionLevel2 = "CEE", RegionLevel4 = "EE_Balk&Gr_TR"))
  # 2- could be an issue for the tweaked SuppliesLevel4.
  #    but it isn't as long as the SuppliesLevel names aren't changed when
  #    the Volume value are played with.

  subsetData_FR <- shiny::eventReactive(input$goDetailsTemp, {
    # with FR only SuppliesLevel4
    # pattern <- data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == input$filter_part, "regex"][[1]]
    dat$FR %>%
      # dplyr::filter(stringr::str_detect(SuppliesLevel2, pattern)) %>%
      f_fr_filter(regionValue = input$top_RegionValue,
                  regionValueLevel = RegLevD(),
                  regionSplit = input$top_RegionSplit,
                  suppliesValue = input$top_SuppliesValue,
                  suppliesValueLevel = "SuppliesLevel4")
                  # ,input$frmax # SIMPLICATION REMOVE frmax
  })

  sku_in_selection <- shiny::eventReactive(input$goDetailsTemp, {
    subsetData_RVP() %>%
      dplyr::select(dplyr::matches("SuppliesLevel[0-4]")) %>%
      dplyr::distinct() %>%
      #join by SuppliesLevel4, return several material for each SuppliesLevel4
      dplyr::left_join(data_sku)
  })

  # OUTPUT OBJECT -------------------------------------------------------------


  Tot <- shiny::reactive({
    dplyr::summarise(dataRegionTree(), total = sum(total)) %>%
      unlist(use.names = F)
  })

  output$topBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      "Report for",
      paste0(input$top_RegionValue, " and ", input$top_SuppliesValue),
      icon = shiny::icon("heartbeat"),
      color = "purple",
      fill = TRUE)
  })

  output$TotBox <- shinydashboard::renderInfoBox({
    shinydashboard::infoBox(
      paste0("Total in FY ", input$top_yearRef),
      format(round(Tot(),0), big.mark = " "),
      icon = icon("euro"),
      color = "purple",
      fill = TRUE)
  })


  output$yearRef <- shiny::renderText(input$top_yearRef)
  output$RegionSplityearRef <- shiny::renderText({
    paste0("Region Split in FY", input$top_yearRef)
  })
  output$SuppliesSplityearRef <- shiny::renderText({
    paste0("Supplies Split in FY", input$top_yearRef)
  })


  output$out1 <- shiny::renderPrint({ list("FYlist()", FYlist()) })
  output$out2 <- shiny::renderPrint({ list("sLevel", sLevel) })
  output$out4 <- shiny::renderPrint({ list("SupLevD()", SupLevD()) })
  output$out7 <- shiny::renderPrint({
    list("input$filter_part", paste0(input$filter_part, " <|> ",
                                     data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == input$filter_part, "regex"][[1]])
    )
  })
  output$out8 <- shiny::renderPrint({ list("rLevel", rLevel) })
  output$out10 <- shiny::renderPrint({ list("RegLevD()", RegLevD()) })
  output$out13 <- shiny::renderPrint({
    list("finer(RegLevelD(), names(rLevel))",
         finer(RegLevelD(), names(rLevel)))
  })
  output$out21 <- shiny::renderPrint({
    list("dataSuppliesTree()", dataSuppliesTree())
  })
  output$out22 <- shiny::renderPrint({
    list("dataRegionTree()", dataRegionTree())
  })
  output$out41 <- shiny::renderPrint({
    list("input$top_RegionValue", input$top_RegionValue)
  })
  output$out43 <- shiny::renderPrint({
    list("input$top_SuppliesValue", input$top_SuppliesValue)
  })
  output$out44 <- shiny::renderPrint({
    list("lollipop_input_V()$data", lollipop_input_V()$data)
  })
  output$out45 <- shiny::renderPrint({list("subsetData_V()", subsetData_V())})


  output$outlol1 <- shiny::renderPrint({
    list("lol1: subsetData_V()", subsetData_V())
  })
  output$outlol3 <- shiny::renderPrint({
    list("lol3: input$top_RegionValue", input$top_RegionValue)
  })
  output$outlol4 <- shiny::renderPrint({
    list("lol4: input$top_RegionSplit", input$top_RegionSplit)
  })
  output$outlol6 <- shiny::renderPrint({
    list("lol6: input$top_SuppliesValue", input$top_SuppliesValue)
  })
  output$outlol8 <- shiny::renderPrint({
    list("lol8: input$top_topN", input$top_topN)
  })

  output$outts1 <- shiny::renderPrint({
    list("ts1: subsetData_V()", subsetData_V())
  })
  output$outts3 <- shiny::renderPrint({
    list("ts3: input$top_RegionValue", input$top_RegionValue)
  })
  output$outts4 <- shiny::renderPrint({
    list("ts4: input$top_RegionSplit", input$top_RegionSplit)
  })
  output$outts6 <- shiny::renderPrint({
    list("ts6: input$top_SuppliesValue", input$top_SuppliesValue)
  })
  output$outts7 <- shiny::renderPrint({
    list("ts7: input$top_SuppliesSplit", input$top_SuppliesSplit)
  })
  output$outts8 <- shiny::renderPrint({
    list("ts8: as.numeric(input$top_yearRef)", as.numeric(input$top_yearRef))
  })
  output$outts9 <- shiny::renderPrint({
    list("ts9: as.numeric(input$top_topN)", as.numeric(input$top_topN))
  })

  # MANAGE THE DETAILS TABS ---------------------------------------------------
  # top_RegionValue
  # top_SuppliesValue
  # goDetails

  # --- sub selection of data ---
  # SHOULDNT THAT RATHER BE IN the FILTER DATA section

  # data_RVP() recalculé qd SelectionSupplies/Region sont recalculés,
  # qd le volet de gauche validé

  subsetData_RVP <- shiny::eventReactive(input$goDetailsTemp, {
    pattern <- data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == input$filter_part, "regex"][[1]]
    dat$RVP %>%
      dplyr::filter(stringr::str_detect(SuppliesLevel2, pattern)) %>%
      dplyr::filter((!!dplyr::sym(RegLevD())) == input$top_RegionValue,
                    (!!dplyr::sym(SupLevD())) == input$top_SuppliesValue)
  })

  subsetData_V <- shiny::eventReactive(input$goDetailsTemp, {
    pattern <- data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == input$filter_part, "regex"][[1]]
    dat$V %>%
      dplyr::filter(stringr::str_detect(SuppliesLevel2, pattern)) %>%
      dplyr::filter((!!dplyr::sym(RegLevD())) == input$top_RegionValue,
                    (!!dplyr::sym(SupLevD())) == input$top_SuppliesValue)
  })

  attrition_input <- shiny::eventReactive(input$goDetailsTemp, {
    data_plot_attrition(input$top_SuppliesValue, data_attrition)
  })

  # --- build treeplot ---

  cat <- list()
  cat$Reg <- shiny::reactive({ finer(RegLevD(), names(rLevel)) })
  cat$subReg <- shiny::reactive({ input$top_RegionSplit })
  cat$Supplies <- shiny::reactive({ finer(SupLevD(), names(sLevel)) })
  cat$subSupplies <- shiny::reactive({ input$top_SuppliesSplit })

  dataRegionTree <- shiny::reactive({
    dataTree(subsetData_RVP(), cat = cat$Reg(), subcat = cat$subReg(),
             RevVol = "Rev", FY = input$top_yearRef)
  })
  dataSuppliesTree <- shiny::reactive({
    dataTree(subsetData_RVP(), cat = cat$Supplies(), subcat = cat$subSupplies(),
             RevVol = "Rev", FY = input$top_yearRef)
  })

  output$plotRegion <- shiny::renderPlot({
    plotTree(dataRegionTree())
  })

  output$plotSupplies <- shiny::renderPlot({
    plotTree(dataSuppliesTree())
  })

  # --- build input data for lollipop ranking on volume ---

  lollipop_input_V <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN, input$top_FYs
    ), {

      data4plot_Lollipop(subsetData_V(),
                         RRL = RegLevD(),
                         RRV = input$top_RegionValue,
                         RSL = input$top_RegionSplit,
                         SPL = SupLevD(),
                         SPV = input$top_SuppliesValue,
                         SSL = input$top_SuppliesSplit,
                         yearRef = as.numeric(input$top_yearRef),
                         rankBy = "Liters",
                         topN = as.numeric(input$top_topN),
                         showFY = input$top_FYs,
                         lollipop_measure = "Liters",
                         SP_filter_part = input$filter_part)
    })

  # --- build input data for lollipop ranking on revenue ---

  lollipop_input_R <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN, input$top_FYs
    ), {

      data4plot_Lollipop(subsetData_RVP(),
                         RRL = RegLevD(),
                         RRV = input$top_RegionValue,
                         RSL = input$top_RegionSplit,
                         SPL = SupLevD(),
                         SPV = input$top_SuppliesValue,
                         SSL = input$top_SuppliesSplit,
                         yearRef = as.numeric(input$top_yearRef),
                         rankBy = "Rev",
                         topN = as.numeric(input$top_topN),
                         showFY = input$top_FYs,
                         lollipop_measure = "Rev",
                         SP_filter_part = input$filter_part)
    })

  # --- build sku list to display ---

  output$sku_in_selection <- DT::renderDataTable({
    DT::datatable(data = sku_in_selection())
  })

  # --- build input data for times series for volume ---

  # HIDE, FOR SIMPLIFICATION, NOT SO USEFUL
  # output$ts_input_Vdata <- DT::renderDataTable({
  #   DT::datatable(data = ts_input_V()$data %>%
  #                   dplyr::mutate_if(is.numeric, funs(signif(., 3))),
  #                 extensions = 'Buttons',
  #                 options = list(dom = "Blfrtip",
  #                                buttons = list("copy",
  #                                               list(extend = "collection",
  #                                                    buttons = c("csv"),
  #                                                    text = "Download")
  #                                )
  #                 )
  #   )
  # })

  ts_input_V <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN
    ), {

      data4plot_ts(subsetData_V(),
                   RRL = RegLevD(),
                   RRV = input$top_RegionValue,
                   RSL = input$top_RegionSplit,
                   SPL = SupLevD(),
                   SPV = input$top_SuppliesValue,
                   SSL = input$top_SuppliesSplit,
                   yearRef = as.numeric(input$top_yearRef),
                   rankBy = "Liters_sm", topN = as.numeric(input$top_topN),
                   ts_measure = "Liters_sm",
                   SP_filter_part = input$filter_part,
                   free_y = ifelse(isTRUE(input$checkbox_free_y), "free_y", "fixed"))
    })


  # --- build input data for times series for revenue ---

  ts_input_R <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN
    ), {

      data4plot_ts(subsetData_RVP(),
                   RRL = RegLevD(),
                   RRV = input$top_RegionValue,
                   RSL = input$top_RegionSplit,
                   SPL = SupLevD(),
                   SPV = input$top_SuppliesValue,
                   SSL = input$top_SuppliesSplit,
                   yearRef = as.numeric(input$top_yearRef),
                   rankBy = "Rev_sm",
                   topN = as.numeric(input$top_topN),
                   ts_measure = "Rev_sm",
                   SP_filter_part = input$filter_part,
                   free_y = ifelse(isTRUE(input$checkbox_free_y), "free_y", "fixed"))
    })

  # --- build input data for mosaic for volume ---

  mosaic_input_V <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_FYs
    ), {

      data4plot_Mosaic(subsetData_V(),
                       RRL = RegLevD(),
                       RRV = input$top_RegionValue,
                       RSL = input$top_RegionSplit,
                       SPL = SupLevD(),
                       SPV = input$top_SuppliesValue,
                       SSL = input$top_SuppliesSplit,
                       yearRef = as.numeric(input$top_yearRef),
                       rankBy = "Liters",
                       topN = as.numeric(input$top_topN),
                       showFY = input$top_FYs,
                       mosaic_measure = "Liters",
                       SP_filter_part = input$filter_part)
    })

  # --- build input data for mosaic for volume ---

  mosaic_input_R <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_FYs
    ), {
      data4plot_Mosaic(subsetData_RVP(),
                       RRL = RegLevD(),
                       RRV = input$top_RegionValue,
                       RSL = input$top_RegionSplit,
                       SPL = SupLevD(),
                       SPV = input$top_SuppliesValue,
                       SSL = input$top_SuppliesSplit,
                       yearRef = as.numeric(input$top_yearRef),
                       rankBy = "Rev",
                       topN = as.numeric(input$top_topN),
                       showFY = input$top_FYs,
                       mosaic_measure = "Rev",
                       SP_filter_part = input$filter_part)
    })

  # --- build input data for RevVolPrice charts ---

  RVP_input_RVP_channel <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN
    ), {

      data4plot_RVP(subsetData_RVP(),
                    keepSize = F,
                    keepSource = T,
                    RRL = RegLevD(),
                    RRV = input$top_RegionValue,
                    #IMPROVE RSL: highest region level, RegLevD() instead of input$top_RegionSplit
                    RSL = RegLevD(),
                    SPL = SupLevD(),
                    SPV = input$top_SuppliesValue,
                    SSL = input$top_SuppliesSplit,
                    yearRef = as.numeric(input$top_yearRef),
                    rankBy = "Rev",
                    topN = as.numeric(input$top_topN),
                    RVP_facetY = "source",
                    SP_filter_part = input$filter_part,
                    free_y = ifelse(isTRUE(input$checkbox_free_y), "free_y", "fixed"))
    })

  RVP_input_RVP_region <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN
    ), {

      data4plot_RVP(subsetData_RVP(),
                    keepSize = F,
                    keepSource = F,
                    RRL = RegLevD(),
                    RRV = input$top_RegionValue,
                    RSL = input$top_RegionSplit,
                    SPL = SupLevD(),
                    SPV = input$top_SuppliesValue,
                    SSL = input$top_SuppliesSplit,
                    yearRef = as.numeric(input$top_yearRef),
                    rankBy = "Rev",
                    topN = as.numeric(input$top_topN),
                    RVP_facetY = "RegionSplit",
                    SP_filter_part = input$filter_part,
                    free_y = ifelse(isTRUE(input$checkbox_free_y), "free_y", "fixed"))
    })

  # --- build input data for media size ---

  RVP_input_RVP_mediaSize <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$top_RegionValue, input$top_RegionSplit,
         # input$top_SuppliesSplit,
         # input$top_yearRef, input$top_topN
         # input$top_yearRef, input$top_topN
         # input$top_yearRef, input$top_topN
         # input$top_yearRef, input$top_topN
    ), {

      data4plot_RVP(subsetData_RVP(),
                    keepSize = T,
                    keepSource = F,
                    RRL = RegLevD(),
                    RRV = input$top_RegionValue,
                    #IMPROVE RSL: highest region level, RegLevD() instead of input$top_RegionSplit
                    RSL = RegLevD(),
                    SPL = SupLevD(),
                    SPV = input$top_SuppliesValue,
                    SSL = input$top_SuppliesSplit,
                    yearRef = as.numeric(input$top_yearRef),
                    rankBy = "Rev",
                    topN = as.numeric(input$top_topN),
                    RVP_facetY = "size",
                    SP_filter_part = input$filter_part,
                    free_y = ifelse(isTRUE(input$checkbox_free_y), "free_y", "fixed"))
    })

  # --- build input for variance analysis ---

  #DEBUG (don't work)
  observe({ print(input$top_topN) })

  Variance_input <- shiny::eventReactive(
    list(input$goDetailsTemp
         # , input$VarianceDateRange, input$VarianceDriver, input$top_topN
         ), {
      # DEBUG
      # print(paste0("server-raw575>>> Variance_input ", "\n"))
      # cat(file = stderr(), "server-raw575> Variance_input ", toString(input$top_topN), "\n")
      f_waterfall_input(data = subsetData_RVP(),
                        Driver = input$VarianceDriver,
                        cal_period = input$VarianceDateRange,
                        topN_driver = input$top_topN)
  })

  # --- build charts output object ---

  height_lollipop_R <- reactive({
    # DEBUG
    # cat(file = stderr(), "server-raw564> height_lollipop_R ", names(lollipop_input_R()$data), "\n")
    lollipop_input_R()$data$RegionSplit %>% unique() %>% length %>% `*`(120) %>% `+`(450)
  })
  height_lollipop_V <- reactive({
    # DEBUG
    # cat(file = stderr(), "server-raw568> height_lollipop_V ", names(lollipop_input_V()$data), "\n")
    lollipop_input_V()$data$RegionSplit %>% unique() %>% length %>% `*`(120) %>% `+`(450)
  })
  height_mosaic_R <- reactive({
    # DEBUG
    # cat(file = stderr(), "server-raw587> height_mosaic_R ", names(mosaic_input_R()$data), "\n")
    mosaic_input_R()$data$RegionSplit %>% unique() %>% length %>% `*`(100) %>% `+`(350)
  })
  height_mosaic_V <- reactive({
    # DEBUG
    # cat(file = stderr(), "server-raw591> height_mosaic_V ", names(mosaic_input_V()$data), "\n")
    mosaic_input_V()$data$RegionSplit %>% unique() %>% length %>% `*`(100) %>% `+`(350)
  })

  output$lollipop_R <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("lollipopLY_R",
    #                                             height = height_lollipop_R()),
    #        "plain" =       shiny::plotOutput("lollipopGG_R",
    #                                          height = height_lollipop_R()))
    shiny::plotOutput("lollipopGG_R", height = height_lollipop_R())
  })

  output$timeSeries_R <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("timeSeriesLY_R"),
    #        "plain" =       shiny::plotOutput("timeSeriesGG_R"))
    plotly::plotlyOutput("timeSeriesLY_R")
  })

  output$mosaic_R <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("mosaicLY_R",
    #                                             height = height_mosaic_R()),
    #        "plain" =       shiny::plotOutput("mosaicGG_R",
    #                                          height = height_mosaic_R()))
    shiny::plotOutput("mosaicGG_R", height = height_mosaic_R())
  })


  output$lollipop_V <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("lollipopLY_V",
    #                                             height = height_lollipop_V()),
    #        "plain" =       shiny::plotOutput("lollipopGG_V",
    #                                          height = height_lollipop_V()))
    shiny::plotOutput("lollipopGG_V", height = height_lollipop_V())
  })

  output$timeSeries_V <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("timeSeriesLY_V"),
    #        "plain" =       shiny::plotOutput("timeSeriesGG_V"))
    plotly::plotlyOutput("timeSeriesLY_V")
  })

  output$mosaic_V <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("mosaicLY_V",
    #                                             height = height_mosaic_V()),
    #        "plain" =       shiny::plotOutput("mosaicGG_V",
    #                                          height = height_mosaic_V()))
    shiny::plotOutput("mosaicGG_V", height = height_mosaic_V())
  })

  output$RVP_channel <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("RVP_channelLY",
    #                                             height = input$height_rvp_channel),
    #        "plain" =       shiny::plotOutput("RVP_channelGG",
    #                                          height = input$height_rvp_channel))
    plotly::plotlyOutput("RVP_channelLY", height = input$height_rvp_channel)
  })

  output$RVP_region <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("RVP_regionLY",
    #                                             height = input$height_rvp_region),
    #        "plain" =       shiny::plotOutput("RVP_regionGG",
    #                                          height = input$height_rvp_region))
    plotly::plotlyOutput("RVP_regionLY", height = input$height_rvp_region)
  })

  output$RVP_mediaSize <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("RVP_mediaSizeLY",
    #                                             height = input$height_rvp_mediasize),
    #        "plain" =       shiny::plotOutput("RVP_mediaSizeGG",
    #                                          height = input$height_rvp_mediasize))
    plotly::plotlyOutput("RVP_mediaSizeLY", height = input$height_rvp_mediasize)
  })

  output$Variance <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = shiny::plotOutput("VarianceGG", height = "700px"),
    #                        # plotly do not work with this chart
    #                        # plotly::plotlyOutput("VarianceLY", height = "700px"),
    #        "plain" =       shiny::plotOutput("VarianceGG", height = "700px"))
    shiny::plotOutput("VarianceGG", height = "700px")
  })

  output$FR <- shiny::renderUI({
    # switch(input$graphType,
    #        "interactive" = plotly::plotlyOutput("FRLY", height = "700px"),
    #        "plain" =       shiny::plotOutput("FRGG", height = "700px"))
    plotly::plotlyOutput("FRLY", height = "700px")
  })



  # output$lollipopLY_V <- plotly::renderPlotly({
  #   plot_lollipop(lollipop_input_V()) %>% plotly::ggplotly()
  # })

  output$timeSeriesLY_V <- plotly::renderPlotly({
    plot_ts(ts_input_V()) %>% plotly::ggplotly()
  })

  # output$mosaicLY_V <- plotly::renderPlotly({
  #   plot_mosaic(mosaic_input_V()) %>% plotly::ggplotly()
  # })

  # output$lollipopLY_R <- plotly::renderPlotly({
  #   plot_lollipop(lollipop_input_R()) %>% plotly::ggplotly()
  # })

  output$timeSeriesLY_R <- plotly::renderPlotly({
    plot_ts(ts_input_R()) %>% plotly::ggplotly()
  })

  # output$mosaicLY_R <- plotly::renderPlotly({
  #   plot_mosaic(mosaic_input_R()) %>% plotly::ggplotly()
  # })

  output$RVP_channelLY <- plotly::renderPlotly({
    plot_RVP(RVP_input_RVP_channel()) %>% plotly::ggplotly()
  })

  output$RVP_regionLY <- plotly::renderPlotly({
    plot_RVP(RVP_input_RVP_region()) %>% plotly::ggplotly()
  })

  output$RVP_mediaSizeLY <- plotly::renderPlotly({
    plot_RVP(RVP_input_RVP_mediaSize()) %>% plotly::ggplotly()
  })

  # output$VarianceLY <- plotly::renderPlotly({
  #   plot_waterfall(Variance_input(), input$VarianceDriver) %>% plotly::ggplotly()
  #   # Plotly do not work with this Graph for now
  # })

  output$FRLY <- plotly::renderPlotly({
    plot_fr(subsetData_FR()) %>% plotly::ggplotly()
  })



  output$lollipopGG_V <- shiny::renderPlot({
    plot_lollipop(lollipop_input_V())
  })

  output$timeSeriesGG_V <- shiny::renderPlot({
    plot_ts(ts_input_V())
  })

  output$mosaicGG_V <- shiny::renderPlot({
    # DEBUG
    # cat(file = stderr(), "server-raw721> mosaic_input_V ", names(mosaic_input_V()$data), "\n")
    # cat(file = stderr(), "server-raw722> mosaic_input_V ", sapply(mosaic_input_V()$data, class), "\n")
    plot_mosaic(mosaic_input_V())
  })

  output$lollipopGG_R <- shiny::renderPlot({
    plot_lollipop(lollipop_input_R())
  })

  output$timeSeriesGG_R <- shiny::renderPlot({
    plot_ts(ts_input_R())
  })

  output$mosaicGG_R <- shiny::renderPlot({
    # DEBUG
    # cat(file = stderr(), "server-raw734> mosaic_input_R ", names(mosaic_input_R()$data), "\n")
    # cat(file = stderr(), "server-raw734> mosaic_input_R ", sapply(mosaic_input_R()$data, class), "\n")
    # server-raw721> mosaic_input_R  source RegionSplit RegionReport SuppliesSplit SuppliesPlot fYear Rev Liters Rev_sm Liters_sm measure
    # server-raw721> mosaic_input_R  factor factor character factor character                  integer numeric numeric numeric numeric numeric
    plot_mosaic(mosaic_input_R())
  })

  output$RVP_channelGG <- shiny::renderPlot({
    plot_RVP(RVP_input_RVP_channel())
  })

  output$RVP_regionGG <- shiny::renderPlot({
    plot_RVP(RVP_input_RVP_region())
  })

  output$RVP_mediaSizeGG <- shiny::renderPlot({
    plot_RVP(RVP_input_RVP_mediaSize())
  })

  output$VarianceGG <- shiny::renderPlot({
    plot_waterfall(Variance_input(), input$VarianceDriver)
  })

  output$FRGG <- shiny::renderPlot({
    plot_fr(subsetData_FR())
  })

  output$attrition <- shiny::renderPlot({
    attrition_input() %>%
      dplyr::select(plot) %>%
      dplyr::pull(1)
  })

  }
}
