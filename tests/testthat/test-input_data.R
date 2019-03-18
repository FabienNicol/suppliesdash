context("test-input_data")

### CREATE DATA ###
# data source for unit test
# test_ISenriched <- list()
# load(file = "../Supplies/output/LFP/data_ISenriched.rda")
# test_ISenriched$RVP <-
#   data_ISenriched$RVP %>%
#   filter(RegionLevel4 %in% c("EDG", "EFS"),
#          SuppliesLevel4 %in% c("T653_4900", "T591_11880"))
#
# test_ISenriched$V <-
#   data_ISenriched$V %>%
#   filter(RegionLevel4 %in% c("EDG", "EFS"),
#          SuppliesLevel4 %in% c("T653_4900", "T591_11880"))
#
# test_ISenriched$FR <-
#   data_ISenriched$FR %>%
#   filter(RegionLevel4 %in% c("EDG", "EFS"),
#          SuppliesLevel4 %in% c("T653_4900", "T591_11880"))
#
### SAVE DATA ###
# save(test_ISenriched, file = "./data-raw/test_ISenriched.rda")


### MAKE THE TEST AT CMD LINE ###
#
# load("./data-raw/test_ISenriched.rda")
subset_test_RVP <- test_ISenriched$RVP
subset_test_FR <- test_ISenriched$FR
#
#
# #load the tibble data_attrition
# load("./data/data_attrition.rda")
# #load "data_SL2_regex" for parameter "SP_filter_part".
# load("./R/sysdata.rda")
#
# library(testthat)
# source("./R/input_fr.R")
# source("./R/input_RVP.R")
# source("./R/input_treemap.R")
# source("./R/input_waterfall.R")
# source("./R/plot_attrition.R")
# source("./R/plot_fr.R")
# source("./R/plot_RVP.R")
# source("./R/plot_treemap.R")
# source("./R/plot_waterfall.R")
# source("./R/suppliesdash_input.R")

library(testthat)

test_that("data4plot for lollipop", {
  x <-
    data4plot(subset_test_RVP, keepSize = F, keepSource = F,
            RRL = "RegionLevel0", RRV = "EMEAR", RSL = "RegionLevel4",
            SPL = "SuppliesLevel1", SPV = "LFPaqueous", SSL = "SuppliesLevel4",
            yearRef = 2017, rankBy = "Rev", topN = 10, ditch = "fDate", showFY = "All",
            lollipop_measure = "Rev", ts_measure = NULL,
            mosaic_measure = NULL, RVP_facetY = NULL)
  expect_equal(dim(x)[[1]], 20)
  expect_equal(dim(x)[[2]], 9)
})

test_that("data4_Lollipop and plot_lollipop", {
  lollipop_input_R <-
    data4plot_Lollipop(subset_test_RVP,
                       RRL = "RegionLevel0",
                       RRV = "EMEAR",
                       RSL = "RegionLevel4",
                       SPL = "SuppliesLevel1",
                       SPV = "LFPaqueous",
                       SSL = "SuppliesLevel4",
                       yearRef = 2017,
                       rankBy = "Rev",
                       topN = 10,
                       showFY = "All",
                       lollipop_measure = "Rev",
                       SP_filter_part = "all")

  #a list of a tibble (data), and a litst of 7 items
  expect_equal(length(lollipop_input_R), 2)
  expect_equal(dim(lollipop_input_R[[1]]), c(20, 9))
  expect_equal(length(lollipop_input_R[[2]]), 7)
  #a plot with facets c("EDG", "EFS") and c(2014, 2015, 2016, 2017, 2018)
  expect_equal(class(plot_lollipop(lollipop_input_R)), c("gg", "ggplot"))
})

test_that("data4plot for ts", {
  x <-
    data4plot(subset_test_RVP, keepSize = F, keepSource = F,
            RRL = "RegionLevel0", RRV = "EMEAR", RSL = "RegionLevel4",
            SPL = "SuppliesLevel1", SPV = "LFPaqueous", SSL = "SuppliesLevel4",
            yearRef = 2017, rankBy = "Rev_sm", topN = 10, ditch = "fYear", showFY = "All",
            lollipop_measure = NULL, ts_measure = "Rev_sm",
            mosaic_measure = NULL, RVP_facetY = NULL)

  expect_equal(dim(x), c(216, 10))
})

test_that("data4plot_ts and plot_ts", {
  ts_input_R <-
    data4plot_ts(subset_test_RVP,
                 RRL = "RegionLevel0",
                 RRV = "EMEAR",
                 RSL = "RegionLevel4",
                 SPL = "SuppliesLevel1",
                 SPV = "LFPaqueous",
                 SSL = "SuppliesLevel4",
                 yearRef = 2017,
                 rankBy = "Rev_sm",
                 topN = 10,
                 ts_measure = "Rev_sm",
                 SP_filter_part = "all",
                 free_y = "free_y")

  #a list containing a tibble (data) and a list of 4 string items
  expect_equal(length(ts_input_R), 2)
  expect_equal(dim(ts_input_R[[1]]), c(216, 10))
  expect_equal(length(ts_input_R[[2]]), 4)
  #a line plot with a faceet c("EDG", "EFS")
  expect_equal(class(plot_ts(ts_input_R)), c("gg", "ggplot"))
})

test_that("data4plot for mosaic", {
  x <-
    data4plot(subset_test_RVP, keepSize = F, keepSource = T,
              RRL = "RegionLevel0", RRV = "EMEAR", RSL = "RegionLevel4",
              SPL = "SuppliesLevel1", SPV = "LFPaqueous", SSL = "SuppliesLevel4",
              yearRef = 2017, rankBy = "Rev", topN = 10, ditch = "fDate", showFY = "All",
              lollipop_measure = NULL, ts_measure = NULL,
              mosaic_measure = "Rev", RVP_facetY = NULL)

  expect_equal(dim(x), c(80, 7))
})

test_that("data4plot_Mosaic and plot_mosaic", {
  mosaic_input_R <-
    data4plot_Mosaic(subset_test_RVP,
                   RRL = "RegionLevel0",
                   RRV = "EMEAR",
                   RSL = "RegionLevel4",
                   SPL = "SuppliesLevel1",
                   SPV = "LFPaqueous",
                   SSL = "SuppliesLevel4",
                   yearRef = 2017,
                   rankBy = "Rev",
                   topN = 10,
                   showFY = "All",
                   mosaic_measure = "Rev",
                   SP_filter_part = "all")

  #a list containing a tibble (data) and a vector of string with 3 items
  expect_equal(length(mosaic_input_R), 2)
  expect_equal(dim(mosaic_input_R[[1]]), c(80, 7))
  expect_equal(length(mosaic_input_R[[2]]), 3)
  # a plot with facets c("EDG", "EFS") and c(2014, 2015, 2016, 2017, 2018)
  expect_equal(class(plot_mosaic(mosaic_input_R)), c("gg", "ggplot"))
})


test_that("data4plot for RVP_channel", {
  x <-
    data4plot(subset_test_RVP, keepSize = F, keepSource = T,
            RRL = "RegionLevel0", RRV = "EMEAR", RSL = "RegionLevel4",
            SPL = "SuppliesLevel1", SPV = "LFPaqueous", SSL = "SuppliesLevel4",
            yearRef = 2017, rankBy = "Rev", topN = 10, ditch = "fYear", showFY = "All",
            lollipop_measure = NULL, ts_measure = NULL,
            mosaic_measure = NULL, RVP_facetY = "source")
  # 3: funs() is soft deprecated as of dplyr 0.8.0
  # please use list() instead
  # # Before: funs(name = f(.)
  # # After:  list(name = ~f(.))
  expect_equal(dim(x), c(2553, 8))
})

test_that("data4plot_RVP and plot_RVP", {
  RVP_input_RVP_channel <-
    data4plot_RVP(subset_test_RVP,
                  keepSize = F,
                  keepSource = T,
                  RRL = "RegionLevel0",
                  RRV = "EMEAR",
                  RSL = "RegionLevel0", #need to be the same as RRL
                  SPL = "SuppliesLevel1",
                  SPV = "LFPaqueous",
                  SSL = "SuppliesLevel4",
                  yearRef = 2017,
                  rankBy = "Rev",
                  topN = 10,
                  RVP_facetY = "source",
                  SP_filter_part = "all",
                  free_y = "free_y")

  #a list containing a tibble (data) and a list of 4 string items
  expect_equal(length(RVP_input_RVP_channel), 2)
  expect_equal(dim(RVP_input_RVP_channel[[1]]), c(1296, 8))
  expect_equal(length(RVP_input_RVP_channel[[2]]), 4)
  #a line plot with facet on KEY c(Rev_sm, Liters_sm, price) and
  #facetY c(transac, QT, MPS, store)
  expect_equal(class(plot_RVP(RVP_input_RVP_channel)), c("gg", "ggplot"))
})

# ===
# source("./R/input_treemap.R")
# source("./R/plot_treemap.R")

test_that("dataSuppliesTree and plotTree", {
  dataSuppliesTree <-
    dataTree(subset_test_RVP,
             cat = "SuppliesLevel2",      #(finer(SuppliesLevel1))
             subcat = "SuppliesLevel4",   #SuppliesSplitLevel
             RevVol = "Rev",
             FY = 2017)

  expect_equal(dim(dataSuppliesTree), c(2, 3))
  expect_equal(class(plotTree(dataSuppliesTree)), c("gg", "ggplot"))
})

# ===
# source("./R/input_waterfall.R")
# source("./R/plot_waterfall.R")
library(lubridate) # for %within%

test_that("f_waterfall_input and plot_waterfall", {
  Variance_input <-
    f_waterfall_input(data = subset_test_RVP,
                      Driver = "SuppliesLevel4",
                      cal_period = c(lubridate::ymd("2017-04-01"),
                                     lubridate::ymd("2017-07-31")),
                      topN_driver = 10)
  expect_equal(dim(Variance_input), c(4, 5))
  expect_equal(class(plot_waterfall(Variance_input, "SuppliesLevel4")),
               c("gg", "ggplot"))
})


# ===
# source("./R/plot_attrition.R")

# a tibble with 1 row and 14 columns
test_that("attrition input and plot, data_plot_attrition", {
  attrition_input <-
    data_plot_attrition("T653_4900", data_attrition)

  x <- attrition_input %>%
    dplyr::select(plot) %>%
    purrr::pluck(1, 1)

  expect_equal(dim(attrition_input), c(1, 14))
  expect_equal(class(x),  c("gg", "ggplot"))
})


# ===
# source("./R/input_fr.R")
# source("./R/plot_fr.R")

# a tibble of 472 rows and 6 columns
test_that("f_fr_filter and plot_fr", {
  subsetData_FR <-
    subset_test_FR %>%
    f_fr_filter(regionValue = "EMEAR",
                regionValueLevel = "RegionLevel0",
                regionSplit = "RegionLevel4",
                suppliesValue = "T653_4900",
                suppliesValueLevel = "SuppliesLevel4")

  expect_equal(dim(subsetData_FR), c(472, 6))
  # a plot with facet on KEY c(Liters_sm, fr, MIF, cumqty)
  expect_equal(class(plot_fr(subsetData_FR)), c("gg", "ggplot"))
})


# ===
# source("./R/suppliesdash_input.R")

test_that("suppliesdash_sLevel", {
  sLevel <- suppliesdash_sLevel(test_ISenriched)
  expect_equal(dim(sLevel), c(2, 5))
})

test_that("suppliesdash_rLevel", {
  rLevel <- suppliesdash_rLevel(test_ISenriched)
  expect_equal(dim(rLevel), c(3, 4))
})

test_that("suppliesdash_data", {
  x <- suppliesdash_data(test_ISenriched)
  # a list of 3 dataframes, RVP (944 x 17), FR (236 x 8), V (944 x 17)
  expect_equal(length(x), 3)
  expect_equal(dim(x$RVP), c(944, 17))
  expect_equal(dim(x$FR), c(236, 8))
  expect_equal(dim(x$V), c(944, 17))
})

test_that("unlist_unique", {
  sLevel <- suppliesdash_sLevel(test_ISenriched)
  x <- unlist_unique(sLevel)
  # a string vector of 7 items
  expect_equal(length(x), 7)
})
