# --- PLOTTING PIPELINE FUNCTIONS ---------------------------------------------


#' Remove, or not, the size column.
#'
#' \code{f_keepSize} is used in \code{\link{data4plot}}.
#'
#' @param keepSize A boolean
#' @param data A dataframe
#' @return a dataframe.
f_keepSize <- function(data, keepSize) {
  if (isTRUE(keepSize)) data else data %>% dplyr::select(-size)
}


#' Remove, or not, the column source.
#'
#' \code{f_keepSource} is used in \code{\link{data4plot}}.
#' In \code{f_keepSource}, if the source column is kept, it is mutated into an
#' ordered factor.
#'
#' @param keepSource A boolean
#' @param data A dataframe
#' @return a dataframe.
f_keepSource <- function(data, keepSource) {
  if (isTRUE(keepSource)) {
    data %>%
      dplyr::mutate(source = factor(source, c("Transac", "QT", "MPS", "Store")))
  } else {
    data %>%
      dplyr::select(-source)
  }
}


#' Keep the relevant region columns (1 or 2 distinct).
#'
#' \code{f_keepRegion} is used in \code{\link{data4plot}}.
#' If ReportRegionLevel == RegionSplitLevel, the 2 output region colums will
#' have the same value.
#' NA values and "PEA" are removed silently.
#'
#' @param ReportRegionLevel A string, such as "RegionLevel0".
#' @param ReportRegionValue A string, such as "EMEAR".
#' @param RegionSplitLevel A string, such as "RegionLevel2".
#' @param data A dataframe with columns RegionLevel0, 1, 2, 4.
#' @return a dataframe with columns RegionReport, RegionSplit.
f_keepRegion <- function(data, ReportRegionLevel, ReportRegionValue, RegionSplitLevel) {

  regionFacet <- (ReportRegionLevel == RegionSplitLevel)
  ReportRegionLevel <- dplyr::sym(ReportRegionLevel)
  RegionSplitLevel <- dplyr::sym(RegionSplitLevel)

  data %>%
    dplyr::mutate(RegionSplit = !!RegionSplitLevel) %>%
    dplyr::mutate(temp = !!ReportRegionLevel) %>%
    # take into account the case where RRL == RSL (no facet on region)
    dplyr::mutate(RegionReport = if (regionFacet) RegionSplit else temp) %>%
    dplyr::filter(RegionReport == ReportRegionValue,
                  RegionSplit != "PEA",
                  !is.na(RegionSplit)) %>%
    dplyr::select_at(dplyr::vars(-dplyr::matches("^RegionLevel|temp")))
}



#' Keep the relevant supplies columns (1 or 2 distinct).
#'
#' \code{f_keepSupplies} is used in \code{\link{data4plot}}.
#' If SuppliesPlotLevel == SuppliesSplitLevel, the 2 output supplies columns
#' are identical.
#' NA values are removed silently.
#'
#' @param data a dataframe with columns SuppliesLevel0, 1, 2, 3, 4.
#' @param SuppliesPlotLevel a string, such as "SuppliesLevel0".
#' @param SuppliesPlotValue a string, such as "LFP aqueous ink".
#' @param SuppliesSplitLevel a string, such as "SuppliesLevel3".
#' @return a dataframe with columns SuppliesPlot, SuppliesSplit.
f_keepSupplies <- function(data, SuppliesPlotLevel, SuppliesPlotValue, SuppliesSplitLevel) {

  SuppliesPlotLevel <- dplyr::sym(SuppliesPlotLevel)
  SuppliesSplitLevel <- dplyr::sym(SuppliesSplitLevel)

  data %>%
    dplyr::mutate(SuppliesSplit = !!SuppliesSplitLevel,
                  SuppliesPlot = !!SuppliesPlotLevel) %>%
    dplyr::filter(!is.na(SuppliesSplit),
                  SuppliesPlot == SuppliesPlotValue) %>%
    dplyr::select_at(dplyr::vars(-dplyr::matches("^SuppliesLevel")))
}



#' Rank Region according to revenue (or volume) within the reference year.
#'
#' \code{f_rkRegion} is used in \code{\link{data4plot}}.
#'
#' @param data a dataframe with columns: fDate, RegionSplit, Rev, Vol.
#' @param yearRef an Integer representing the reference year.
#' @param rankBy a string representing the measure on which the Region are
#'   ranked, "Rev" or "Vol".
#' @return a dataframe with an ordered factor for column RegionSplit and
#'   column fYear.
f_rkRegion <- function(data, yearRef = 2017, rankBy = "Rev") {
  rankBy <- dplyr::sym(rankBy)
  data2 <-
    data %>%
    dplyr::mutate(fYear = lubridate::year(fDate)) %>%
    dplyr::group_by(RegionSplit) %>%
    dplyr::mutate(Region_rk = sum(ifelse(fYear == yearRef, (!!rankBy), 0), na.rm = T)) %>%
    dplyr::ungroup()

  # pipeline fail for intance for regionreportvalue=="EFS" and inkreportvalue=="G&J", need to break here
  if (nrow(data2) == 0) return("no data f_rkRegion")

  data2 %>%
    dplyr::mutate(RegionSplit = forcats::fct_reorder(RegionSplit, Region_rk, .desc = TRUE)) %>%
    dplyr::select(-Region_rk)
}


#' Rank supplies according to revenue (or volume) within the reference year.
#'
#' \code{f_rkSupplies} is used in \code{\link{data4plot}}.
#'
#' @param data a dataframe with columns: fYear, SuppliesSplit, SuppliesPlot,
#'   Rev, Vol.
#' @param yearRef an Integer representing the reference year.
#' @param rankBy a string representing the measure on which the Region are
#'   ranked, "Rev" or "Vol".
#' @return a dataframe with an ordered factor for column SuppliesSplit
#'   and fYear.
f_rkSupplies <- function(data, yearRef = 2017, rankBy = "Rev") {

  # the case of "no data ..."
  if (is.character(data)) return(data)

  rankBy <- dplyr::sym(rankBy)
  data2 <-
    data %>%
    dplyr::group_by(SuppliesPlot, SuppliesSplit) %>%
    dplyr::mutate(supplies_rk = sum(ifelse(fYear == yearRef, (!!rankBy), 0), na.rm = T)) %>%
    dplyr::ungroup()

  if (nrow(data2) == 0) return("no data f_rkSupplies")

  data2 %>%
    dplyr::mutate(rk = dplyr::dense_rank(dplyr::desc(supplies_rk)),
                  SuppliesSplit = paste0(rk, "-", SuppliesSplit),
                  SuppliesSplit = forcats::fct_reorder(
                    SuppliesSplit, supplies_rk, .desc = TRUE)
    ) %>%
    dplyr::select(-supplies_rk)
}

#' Keep the topN supplies items and lump together the Others.
#'
#' \code{f_topSupplies} keeps the topN items on SuppliesSplit column.
#' Used in \code{\link{data4plot}}.
#'
#' @param data A dataframe with columns rk, SuppliesSplit.
#' @param topN An integer representing the number of items to display, the
#'   others will be grouped together.
#' @return a dataframe with a column SuppliesSplit, an ordered factor.
f_topSupplies <- function(data, topN = 10) {

  # the case of "no data ..."
  if (is.character(data)) return(data)
  #above, return(data %>% dplyr::select(-rk)), will generate:
  #no applicable method for 'select_' applied to an object of class "character"
  if (is.null(topN)) return(data)

  data %>%
  {dplyr::bind_rows(dplyr::filter(., rk <= topN),
                    dplyr::filter(., rk > topN) %>%
                      dplyr::mutate(SuppliesSplit = "Others", rk = topN+1)
             # NO NEED TO summarise (it's done later)
             # %>% group_by(SuppliesSplit, fYear, RegionSplit, rk) %>%
             #   summarise_if(is.numeric, sum, na.rm = T) %>%
             #   ungroup
  ) %>%
      dplyr::mutate(SuppliesSplit = forcats::fct_reorder(
        SuppliesSplit, rk, .desc = F)) %>%
      dplyr::select(-rk)
    # ,label = paste0(round(Liters / roundMultiple, ifelse(abs(Liters) < roundThreshold, 1, 0))))
  }
}


#' Removes a column to a dataframe.
#'
#' \code{f_keepDateYear} removes a column to a dataframe.
#' Used in \code{\link{data4plot}}.
#'
#' @param data a dataframe
#' @param ditch a string, a column name.
#' @return a dataframe without the column ditch.
f_keepDateYear <- function(data, ditch = "fYear") {

  # the case of "no data ..."
  if (is.character(data)) return(data)

  ditch <- dplyr::sym(ditch)
  data %>% dplyr::select(-(!!ditch))
}



#' Summarise all numeric columns (sum)
#'
#' \code{f_summariseIFnum} is grouping by the non numeric columns and
#' summarises, with a sum, the numeric columns. If there is a column fYear
#' the column is converted in text during the calculation and return as an
#' integer.
#' Used in \code{\link{data4plot}}.
#'
#' @param data a dataframe
#' @return a dataframe
f_summariseIFnum <- function(data) {

  # the case of "no data ..."
  if (is.character(data)) return(data)
  # mutate fYear as character for column selection (group_by_if)
  if ("fYear" %in% names(data)) {
    data <- data %>% dplyr::mutate(fYear = as.character(fYear))
  }

  data2 <-
    data %>%
    # numeric can be Rev, Rev_sm, Liters, Liters_sm
    dplyr::group_by_if(~ !is.numeric(.x)) %>%
    dplyr::summarise_if(is.numeric, sum, na.rm = T) %>%
    dplyr::ungroup()

  if ("fYear" %in% names(data)) {
    data2 <- data2 %>% dplyr::mutate(fYear = as.integer(fYear))
  }
  return(data2)
}



#' Prepare data for lollipop plots.
#'
#' \code{f_summarise4lollipop} adds information at level (RegionSplit, fYear),
#' and level (RegionSplit, SuppliesSplit, fYear), while keeping all the rows of
#' the data frame.
#' Used in \code{\link{data4plot}}.
#'
#' @param data a dataframe with column fYear, lollipop_measure, regionSplit,
#'   SuppliesSplit
#' @param lollipop_measure a string, such as "Rev" or "Liters". NULL if the
#'   plot isn't a lollipop.
#' @param showFY a string vector, such as "All" or c("2017", "2018").
#' @return a dataframe with columns RegionSplit, SuppliesSplit, fYear,
#'   percentage, measure.
f_summarise4lollipop <- function(data, lollipop_measure = "Rev", showFY) {
  # lollipop_measure: Null if not lollipop chart, Rev if based on revenue, Liters if based on volume (Liters, sqm)
  # the case of "no data ..."
  if (is.character(data)) return(data)
  if (is.null(lollipop_measure)) return(data)

  lollipop_measure <- dplyr::sym(lollipop_measure)
  data %>%
    dplyr::filter(if (showFY != "All") fYear %in% showFY else TRUE) %>%
    dplyr::mutate(measure = !!lollipop_measure) %>%
    dplyr::group_by(RegionSplit, fYear) %>%
    dplyr::mutate(tot = sum(measure, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(RegionSplit, SuppliesSplit, fYear) %>%
    dplyr::mutate(percentage = sum(measure, na.rm = T) / tot) %>%
    dplyr::ungroup() %>%
    # remove tot, x_sm (and the one not selected among RevVol)
    dplyr::select_at(dplyr::vars(-dplyr::matches("^tot$|_sm$")))
}


#' Prepare data for time series plots.
#'
#' \code{f_summarise4ts} filters out old data ans a few old models.
#' Identify the column measure.
#' Used in \code{\link{data4plot}}.
#'
#' @param data a dataframe with columns fDate, ts_measure, SuppliesSplit.
#' @param ts_measure a string, such as "Rev_sm" or "Liters_sm". NULL if not a
#'   ts plot.
#' @return a dataframe with column fDate, measure, SuppliesSplit.
f_summarise4ts <- function(data, ts_measure = "Rev_sm") {

  # the case of "no data ..."
  if (is.character(data)) return(data)
  if (is.null(ts_measure)) return(data)

  # remove the (window-1)th first points of the smoothed values
  date2 <- "2014/05/03"

  ts_measure <- dplyr::sym(ts_measure)
  data2 <-
    data %>%
    dplyr::filter(# DELETE fDate < lubridate::ymd(date1),
                  fDate > lubridate::ymd(date2),
                  !(stringr::str_detect(SuppliesSplit,
"T549_10600|T46X_7000|T47X_9500|T480_7500|T486_5500|T500_10000|T40X_9000"))) %>%
    dplyr::mutate(measure = !!ts_measure)

  if (nrow(data2) == 0) return("no data f_summarise4ts")
  return(data2)

  # MUST BE DONE ALREADY IN f_rkSupplies
  #   group_by(SuppliesPlot, SuppliesSplit, fYear, RegionReport, RegionSplit, fDate) %>%
  #   summarise(Liters = sum(Liters.sm, na.rm = T),
  #             LitersFY = sum(Liters, na.rm = T)) %>%
  #   ungroup %>%
  #   group_by(SuppliesPlot, SuppliesSplit) %>%
  #   mutate(ink_rk = sum(Liters, na.rm = T)) %>%
  #   ungroup
  # if (nrow(IS1) == 0) return("no data")
  #
  # IS1 <- IS1 %>%
  #   mutate(rk = dense_rank(desc(ink_rk)),
  #          SuppliesSplit = paste0(rk, "-", SuppliesSplit),
  #          SuppliesSplit = forcats::fct_reorder(SuppliesSplit, ink_rk, .desc = TRUE)
  #          # InkLevel = paste0(as.character(as.numeric(InkLevel)), "-", InkLevel)
  #   ) %>%
  #   select(-ink_rk, rk)

  # ADD THOSE INFORMATION LATER
  # find region_RHS, place a label with the inkLevel name at the end of the line of the most RHS region
  # also compute the % per region, to be included in the strip.text of the facet
  # IS2 <- IS1 %>%
  #   filter(fYear == yearRef) %>%
  #   group_by(RegionSplit) %>%
  #   summarise(LitersFY = sum(LitersFY, na.rm = T)) %>%
  #   ungroup %>%
  #   arrange(desc(LitersFY)) %>%
  #   mutate(perc = 100 * LitersFY/sum(LitersFY))
  # region_RHS <- IS2 %>%
  #   #first, wrapper around [[
  #   select(RegionSplit) %>% unlist %>% last
  #
  # IS1 <- IS1 %>%
  #   group_by(RegionSplit) %>%
  #   mutate(label = if_else(fDate == max(fDate) & RegionSplit == region_RHS, as.character(as.numeric(SuppliesSplit)), NA_character_))
  #
  # add in the strip.text of the facet the % represented by the region
  # if (length(levels(IS1$RegionSplit)) != nrow(IS2)) print("inkLiLj. Error with number of levels in region_RHS and yearRef")
  # if (length(levels(IS1$RegionSplit))>1) levels(IS1$RegionSplit) <- paste0(levels(IS1$RegionSplit), "\n", round(IS2$perc,0), "% in FY", yearRef)

}



#' Prepare data for mosaic plots.
#'
#' \code{f_summarise4mosaic} filters the fiscal years, identifies the measure
#' column.
#' Used in \code{\link{data4plot}}.
#'
#' @param data a dataframe with columns source, RegionSplit, RegionReport,
#'   SuppliesSplit, SuppliesPlot, fYear, mosaic_measure.
#' @param mosaic_measure a string, such as "Rev" or "Liters". NULL if the
#'   plot isn't a mosaic.
#' @param showFY a string vector, such as "All" or c("2017", "2018")
#' @return a dataframe with columns source, RegionSplit, RegionReport,
#'   SuppliesSplit, SuppliesPlot, fYear, measure.
f_summarise4mosaic <- function(data, mosaic_measure, showFY) {

  # the case of "no data ..."
  if (is.character(data)) return(data)
  if (is.null(mosaic_measure)) return(data)

  mosaic_measure <- dplyr::sym(mosaic_measure)
  data %>%
    dplyr::mutate(measure = !!mosaic_measure) %>%
    dplyr::filter(if (showFY != "All") fYear %in% showFY else TRUE) %>%
    dplyr::select(source, RegionSplit, RegionReport, SuppliesSplit, SuppliesPlot, fYear, measure)
}


#' Prepare data for RVP plots.
#'
#' \code{f_summarise4RVP} adds information at level (RegionSplit, fYear),
#' and level (RegionSplit, SuppliesSplit, fYear), while keeping all the rows of
#' the data frame.
#' Requires to drop fYear before (\code{f_keepDateYear}).
#'
#' @param data a dataframe with columns SuppliesSplit, RegionReport, Supplies,
#'   facetY, Rev, Liters, Rev_sm, Liters_sm, Date, fDate.
#' @param facetY a string, such as "RegionSplit", "source". NULL is the plot
#'   isn't an RVP.
#' @return a dataframe with columns SuppliesSplit, RegionReport, Supplies,
#'   facetY, KEY, VALUE, Date, fDate, label. KEY (Rev_sm, Liters_sm, price) a
#'   factor.
f_summarise4RVP <- function(data, facetY) {

  if (is.null(facetY)) return(data)

  strip_sep <- ifelse(facetY == "size", "\n", "*")
  # cat(file = stderr(), "f_summarise4RVP > facetY: ", facetY, ", selected sep: ", strip_sep, "\n")

  # remove the first (window-1)th month
  date2 <- "2014/05/03"

  data %>%
    dplyr::filter(fDate > lubridate::ymd(date2),
                  !(stringr::str_detect(SuppliesSplit,
      "T549_10600|T46X_7000|T47X_9500|T480_7500|T486_5500|T500_10000|T40X_9000"))) %>%
    dplyr::mutate(facetY = !!dplyr::sym(facetY)) %>%
    dplyr::select(-Rev, -Liters) %>%
    dplyr::group_by_at(dplyr::vars(dplyr::matches(
      "facetY|RegionReport|Supplies|Rev_sm|Liters_sm|Date"))) %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(sum(., na.rm = T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(price = ifelse(Liters_sm == 0 | is.na(Liters_sm), 0, Rev_sm / Liters_sm)) %>%
    tidyr::gather(key = KEY, value = VALUE, Rev_sm, Liters_sm, price) %>%
    dplyr::mutate(KEY = factor(KEY, c("Rev_sm", "Liters_sm", "price"))) %>%
    # size = factor(size, c("RA4", "R13", "R24", "R36", "R44", "R64", "SA4", "SA3", "SA2", "SA1"))
    #factor created in the order of appearance wit forcarts::as_factor
    dplyr::arrange(KEY, facetY) %>%
    dplyr::mutate(label = paste0(KEY, strip_sep, facetY),
                  label = forcats::as_factor(label))

}

# ===< test ===
# dat <- read_csv2("./temp/inkSalesEnriched_RVP.csv", col_types = "ccDccccccccccdddd")
# dat2 <- dat %>%
#   f_keepSize(TRUE) %>%
#   f_keepSource(FALSE) %>%
#   f_keepRegion(ReportRegionLevel = "RegionLevel2", ReportRegionValue = "EU5-NB", RegionSplitLevel = "RegionLevel4") %>%
#   f_keepSupplies(SuppliesPlotLevel = "SuppliesLevel1", SuppliesPlotValue = "LFPmedia", SuppliesSplitLevel = "SuppliesLevel2") %>%
#   f_rkRegion(yearRef = 2017, rankBy = "Rev") %>%
#   f_rkSupplies(yearRef = 2017, rankBy = "Rev") %>%
#   f_topSupplies(topN = 10) %>%
#   f_keepDateYear(ditch = "fYear") %>%
#   f_summariseIFnum %>%
#   f_summarise4lollipop(lollipop_measure = NULL, showFY = "All") %>%
#   f_summarise4ts(ts_measure = NULL) %>%
#   f_summarise4mosaic(mosaic_measure = NULL, showFY = "All") %>%
#   f_summarise4RVP(facetY = "size")
# -----
# lollipop_input <- data4plot_Lollipop(dat, RRL = "RegionLevel2", RRV = "EU5-NB", RSL = "RegionLevel2",
#                                      SPL = "SuppliesLevel1", SPV = "aqueous", SSL = "SuppliesLevel2",
#                                      yearRef = 2017, rankBy = "Rev", topN = 10,
#                                      lollipop_measure = "Rev")
# p <- plot_lollipop(lollipop_input$data, lollipop_input$labs)

# -----
# ts_input <- data4plot_ts(dat, RRL="RegionLevel2", RRV="EU5-NB", RSL="RegionLevel2",
#                          SPL="SuppliesLevel1", SPV="aqueous", SSL="SuppliesLevel2",
#                          yearRef=2017, rankBy="Rev", topN=10, ts_measure="Rev")
# plot_ts(ts_input$data, ts_input$labs)

# -----
# mosaic_input <- data4plot_Mosaic(dat, RRL="RegionLevel2", RRV="EU5-NB", RSL="RegionLevel2",
#                                  SPL="SuppliesLevel1", SPV="aqueous", SSL="SuppliesLevel2",
#                                  yearRef=2017, showFY=2017, rankBy="Rev", topN=5, mosaic_measure="Rev")
# plot_mosaic(mosaic_input$data, mosaic_input$labs)
# plot_mosaic(dat2, c("labs1", "labs2", "labs3")) %>% ggplotly

# === end test >===


#' Prepare data for plot lollipop, ts, mosaic, RVP.
#'
#' \code{data4plot} regroups several functions, in order:
#' \code{\link{f_keepSize}}, \code{\link{f_keepSource}},
#' \code{\link{f_keepRegion}}, \code{\link{f_keepSupplies}},
#' \code{\link{f_rkRegion}}, \code{\link{f_rkSupplies}},
#' \code{\link{f_topSupplies}}, \code{\link{f_keepDateYear}},
#' \code{\link{f_summariseIFnum}}, \code{\link{f_summarise4lollipop}},
#' \code{\link{f_summarise4ts}}, \code{\link{f_summarise4mosaic}},
#' \code{\link{f_summarise4RVP}}.
#' \code{data4plot} is used in:
#' \code{\link{data4plot_RVP}}
#' \code{\link{data4plot_Lollipop}}
#' \code{\link{data4plot_Mosaic}}
#' \code{\link{data4plot_ts}}
#'
#' @param data a dataframe.
#' @param keepSize a boolean.
#' @param keepSource a boolean.
#' @param RRL a string, reportRegionLevel.
#' @param RRV a string, ReportRegionValue.
#' @param RSL a string, RegionSplitLevel.
#' @param SPL a string, SuppliesPlotLevel.
#' @param SPV a string, SuppliesPlotValue.
#' @param SSL a string, SppliesSplitLevel.
#' @param yearRef an Integer representing the reference year.
#' @param rankBy a string representing the measure on which the Region are
#'   ranked, "Rev" or "Vol".
#' @param topN An integer representing the number of items to display, the
#'   others will be grouped together.
#' @param ditch a string, a column name.
#' @param showFY a string vector, such as "All" or c("2017", "2018").
#' @param lollipop_measure a string, such as "Rev" or "Liters". NULL if the
#'   plot isn't a lollipop.
#' @param ts_measure a string, such as "Rev_sm" or "Liters_sm". NULL if not a
#'   ts plot.
#' @param mosaic_measure a string, such as "Rev" or "Liters". NULL if the
#'   plot isn't a mosaic.
#' @param RVP_facetY a string, such as "RegionSplit", "source". NULL is the
#'   plot isn't an RVP.
#' @return a dataframe expected by one of the plot functions.
data4plot <- function(data, keepSize, keepSource,
                      RRL, RRV, RSL,
                      SPL, SPV, SSL,
                      yearRef, rankBy, topN, ditch, showFY,
                      lollipop_measure, ts_measure, mosaic_measure, RVP_facetY) {

  # DEBUG
  # cat(file = stderr(), "data4plot > RP_facetY: ", RVP_facetY, "\n")

  data2 <- data %>%
    f_keepSize(keepSize) %>%
    f_keepSource(keepSource) %>%
    f_keepRegion(ReportRegionLevel = RRL,
                 ReportRegionValue = RRV,
                 RegionSplitLevel = RSL) %>%
    f_keepSupplies(SuppliesPlotLevel = SPL,
                   SuppliesPlotValue = SPV,
                   SuppliesSplitLevel = SSL) %>%
    f_rkRegion(yearRef, rankBy) %>%
    f_rkSupplies(yearRef, rankBy) %>%
    f_topSupplies(topN) %>%
    f_keepDateYear(ditch) %>%
    f_summariseIFnum %>%
    f_summarise4lollipop(lollipop_measure, showFY) %>%
    f_summarise4ts(ts_measure) %>%
    f_summarise4mosaic(mosaic_measure, showFY) %>%
    f_summarise4RVP(RVP_facetY)

  return(data2)
}



#' Returns data for plot_RVP.
#'
#' \code{data4plot_RVP} uses \code{\link{data4plot}}.
#'
#' @param SP_filter_part a string helping to selct a regular expression in the
#'   dataframe data_SL2_regex.
#' @param free_y a string, such as "free_y".
#' @return a list containing a dataframe, data, and a string vector, labs.
data4plot_RVP <- function(data, keepSize, keepSource, RRL, RRV, RSL,
                          SPL, SPV, SSL,
                          yearRef, rankBy, topN,
                          RVP_facetY,
                          SP_filter_part, free_y) {

  data2 <- data4plot(data, keepSize, keepSource,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch = "fYear", showFY = "All",
                     lollipop_measure = NULL, ts_measure = NULL,
                     mosaic_measure = NULL, RVP_facetY)

  labs <- list(RVP_facetY = RVP_facetY, SPV = SPV, SP_filter_part = SP_filter_part, free_y = free_y)
  return(list(data = data2, labs = labs))
}


#' Returns the order or magnitude of a double (1, 1 000, 100 000, ...).
#'
#' \code{divid} is used \code{\link{data4plot_Lollipop}}.
#'
#' @param y a double.
#' @return an integer representing the order of magnitude.
divid <- function(y) {
  signif(y,3) %>% log10 %>% `%/%`(3) %>% floor %>% (function(x) {10^(3*x)})
}


#' Returns data for plot_lollipop.
#'
#' \code{data4plot_Lollipop} uses \code{\link{data4plot}}.
#'
#' @param SP_filter_part a string helping to selct a regular expression in the
#'   dataframe data_SL2_regex.
#' @return a list containing a dataframe, data, and a string vector, labs.
data4plot_Lollipop <- function(data, RRL, RRV, RSL,
                               SPL, SPV, SSL,
                               yearRef, rankBy, topN, showFY,
                               lollipop_measure,
                               SP_filter_part) {

  data2 <- data4plot(data, keepSize = F, keepSource = F,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch = "fDate", showFY,
                     lollipop_measure, ts_measure = NULL,
                     mosaic_measure = NULL, RVP_facetY = NULL)

  div <- data2 %>%
    dplyr::select(measure) %>% unlist %>% max(na.rm = T) %>% divid

  vol_unit <- data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == SP_filter_part, "unit"][[1]]
  measure_txt <- ifelse(stringr::str_detect(lollipop_measure, "Rev"), "Revenue", "Volume")
  vol_unit <- ifelse(measure_txt == "Revenue", "Euros NetNet", vol_unit)

  labs <- list(title = paste0(measure_txt,
                              " (x ", format(div, scientific = FALSE, big.mark = " "), " ", vol_unit,
                              ") per FY. Rank according to FY", yearRef),
               subtitle = paste0("sell-in data for the top ", topN, " models in product group ", SPV, "."),
               x_lab = "% of total",
               y_lab = "unit",
               not_use1 = "Ink / Printer model",
               caption = paste0(RRL, RRV, RSL,"_", SPL, SPV, SSL, "_", yearRef,
                                lollipop_measure, collapse = "_"),
               div = div)
  # labs <- c(paste0("Volume (x", div," sqm or Liters) per FY. Rank according to FY", yearRef),
  #              paste0("sell-in data for the top ", topN, " models in product group ", SPV, "."),
  #              "% of total ",
  #              "unit",
  #              "Ink / Printer model",
  #              paste0(RRL, RRV, RSL,"_", SPL, SPV, SSL, "_", yearRef,
  #                     lollipop_measure, collapse = "_"),
  #              div)
  return(list(data = data2, labs = labs))
}

#' Returns data for plot_mosaic.
#'
#' \code{data4plot_Mosaic} uses \code{\link{data4plot}}.
#'
#' @param SP_filter_part a string helping to selct a regular expression in the
#'   dataframe data_SL2_regex.
#' @return a list containing a dataframe, data, and a string vector, labs.
data4plot_Mosaic <- function(data, RRL, RRV, RSL,
                             SPL, SPV, SSL,
                             yearRef, rankBy, topN, showFY,
                             mosaic_measure,
                             SP_filter_part) {

  data2 <- data4plot(data, keepSize = F, keepSource = T,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch = "fDate", showFY,
                     lollipop_measure = NULL, ts_measure = NULL,
                     mosaic_measure, RVP_facetY = NULL)

  vol_unit <- data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == SP_filter_part, "unit"][[1]]
  measure_txt <- ifelse(stringr::str_detect(mosaic_measure, "Rev"), "Revenue", "Volume")
  vol_unit <- ifelse(measure_txt == "Revenue", "Euros NetNet", vol_unit)

  labs <- c(paste0("Split of ", measure_txt, " (", vol_unit, "), per categories (within ", SPV, ") and channel ", RRV),
            paste0("Top", topN, " categories | QTs are QTs approved by EMD"),
            "plotNameMosaic")
  #test
  write.csv2(data2, "test_mosaic_jan18.csv")
  return(list(data = data2, labs = labs))
}


#' Returns data for plot_ts.
#'
#' \code{data4plot_ts} uses \code{\link{data4plot}}.
#'
#' @param SP_filter_part a string helping to selct a regular expression in the
#'   dataframe data_SL2_regex.
#' @param free_y a string, such as "free_y".
#' @return a list containing a dataframe, data, and a string vector, labs.
data4plot_ts <- function(data, RRL, RRV, RSL,
                         SPL, SPV, SSL,
                         yearRef, rankBy, topN,
                         ts_measure,
                         SP_filter_part, free_y) {

  data2 <- data4plot(data, keepSize = F, keepSource = F,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch = "fYear", showFY = "All",
                     lollipop_measure = NULL, ts_measure,
                     mosaic_measure = NULL, RVP_facetY = NULL)

  labs <- list(SPV = SPV, SP_filter_part = SP_filter_part, ts_measure = ts_measure, free_y = free_y)
  return(list(data = data2, labs = labs))
}
