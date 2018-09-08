.libPaths("C:/Users/frfn0735/Local Data/R/R-3.5.1")
library(tidyverse)
library(lubridate)
library(ggplot2)
library(treemapify)
library(ggmosaic)
library(plotly)

finer <- function(x, vx) {
  # take any of names(sLeveln())/names(rLeveln())
  vx <- vx[order(vx)]
  pos <- match(x, vx)
  pos <- ifelse(pos<length(vx), pos+1, pos)
  vx[pos]
}

dataTree <- function(dat, cat, subcat, RevVol, FY) {
  
  # need to find category and sub cat
  # summarize the data
  # cat <- enquo(cat)
  # subcat <- enquo(subcat)
  RevVol <- sym(RevVol)
  dat %>% 
    filter(year(fDate) == FY) %>%
    # rename(category := !!rlang::sym(cat), DO NOT WORK WHEN CAT==SUBCAT
    #        subcategory := !!rlang::sym(subcat)) %>%
    mutate(category = !!rlang::sym(cat),
           subcategory = !!rlang::sym(subcat)) %>%
    # group_by_at(vars(cat, subcat)) %>% 
    group_by(category, subcategory) %>%
    summarise(total = sum(!!RevVol, na.rm=T)) %>%
    ungroup %>%
    mutate(category = replace_na(category, "NA"),
           subcategory = replace_na(subcategory, "NA"))
}


# --- treemap function --- #

plotTree <- function(dat) {
  
  # colors <- c(
  #   income = "#975c72", 
  #   expenses = "#724678", 
  #   savings = "#545294"
  # )
  
  ggplot(dat, aes(
    area = total, fill = category, label = subcategory,
    subgroup = subcategory
  )) +
    geom_treemap() + 
    geom_treemap_text(color = "white", fontface = 2) +
    # scale_fill_manual(values = colors) +
    scale_fill_brewer() +
    theme(
      legend.key.width = unit(2, "cm"),
      legend.key.size = unit(1, "cm"),
      legend.background = element_rect(fill = "gray90", size = 0.5, linetype = "dotted"),
      legend.position = "bottom",
      legend.title = element_text(
        face = "bold", 
        inherit.blank = TRUE)
    )
}

renderLandingPagePlot <- function(basePlot) {
  
  
  
}

# --- PLOTTING PIPELINE FUNCTIONS -----------------------------------------------------------------

f_keepSize <- function(data, keepSize) {
  if (isTRUE(keepSize)) data else data %>% select(-size)
}

f_keepSource <- function(data, keepSource) {
  if (isTRUE(keepSource)) data %>% mutate(source=factor(source, c("Transac", "QT", "MPS", "Store"))) else data %>% select(-source)
}

f_keepRegion <- function(data, ReportRegionLevel, ReportRegionValue, RegionSplitLevel) {
  
  regionFacet <- (ReportRegionLevel == RegionSplitLevel)
  ReportRegionLevel <- rlang::sym(ReportRegionLevel)
  RegionSplitLevel <- rlang::sym(RegionSplitLevel)
  
  data %>% 
    mutate(RegionSplit = !!RegionSplitLevel) %>%
    mutate(temp = !!ReportRegionLevel) %>%
    # take into account the case where RRL == RSL (no facet on region)
    mutate(RegionReport = if (regionFacet) RegionSplit else temp) %>%
    filter(RegionReport == ReportRegionValue,
           RegionSplit != "PEA",
           !is.na(RegionSplit)) %>%
    select_at(vars(-matches("^RegionLevel|temp")))
}

f_keepSupplies <- function(data, SuppliesPlotLevel, SuppliesPlotValue, SuppliesSplitLevel) {
  
  SuppliesPlotLevel <- rlang::sym(SuppliesPlotLevel)
  SuppliesSplitLevel <- rlang::sym(SuppliesSplitLevel)
  
  data %>% 
    mutate(SuppliesSplit = !!SuppliesSplitLevel,
           SuppliesPlot = !!SuppliesPlotLevel) %>%
    filter(!is.na(SuppliesSplit),
           SuppliesPlot == SuppliesPlotValue) %>%
    select_at(vars(-matches("^SuppliesLevel")))
}

f_rkRegion <- function(data, yearRef = 2017, rankBy="Rev") {
  rankBy <- sym(rankBy)
  data2 <- data %>%
    mutate(fYear = year(fDate)) %>%
    group_by(RegionSplit) %>%
    mutate(Region_rk = sum(ifelse(fYear == yearRef, (!!rankBy), 0), na.rm=T)) %>%
    ungroup 
  
  # pipeline fail for intance for regionreportvalue=="EFS" and inkreportvalue=="G&J", need to break here
  if (nrow(data2)==0) return("no data f_rkRegion")
  
  data2 %>%
    mutate(RegionSplit = forcats::fct_reorder(RegionSplit, Region_rk, .desc = TRUE)) %>%
    select(-Region_rk)
}

f_rkSupplies <- function(data, yearRef = 2017, rankBy="Rev") {
  
  # the case of "no data ..."
  if (is.character(data)) return(data)
  
  rankBy <- sym(rankBy)
  data2 <- data %>%
    group_by(SuppliesPlot, SuppliesSplit) %>%
    mutate(supplies_rk = sum(ifelse(fYear == yearRef, (!!rankBy), 0), na.rm=T)) %>%
    ungroup
  
  if (nrow(data2)==0) return("no data f_rkSupplies")
  
  data2 %>%
    mutate(rk = dense_rank(desc(supplies_rk)),
           SuppliesSplit = paste0(rk, "-", SuppliesSplit),
           SuppliesSplit = forcats::fct_reorder(SuppliesSplit, supplies_rk, .desc = TRUE)
    ) %>%
    select(-supplies_rk)
}

f_topSupplies <- function(data, topN = 10) {
  
  
  # the case of "no data ..."
  if (is.character(data)) return(data %>% select(-rk))
  if (is.null(topN)) return(data)
  
  data %>% 
  {bind_rows(filter(., rk <= topN),
             filter(., rk > topN) %>% mutate(SuppliesSplit = "Others", 
                                             rk = topN+1) 
             # NO NEED TO summarise (it's done later)
             # %>% group_by(SuppliesSplit, fYear, RegionSplit, rk) %>%
             #   summarise_if(is.numeric, sum, na.rm=T) %>%
             #   ungroup
  ) %>%
      mutate(SuppliesSplit = forcats::fct_reorder(SuppliesSplit, rk, .desc = F)) %>%
      select(-rk)
    # ,label = paste0(round(Liters / roundMultiple, ifelse(abs(Liters) < roundThreshold, 1, 0))))
  }
}

f_keepDateYear <- function(data, ditch="fYear") {
  
  # the case of "no data ..."
  if (is.character(data)) return(data)
  
  ditch <- rlang::sym(ditch)
  data %>% select(-(!!ditch))
}

f_summariseIFnum <- function(data) {
  
  # the case of "no data ..."
  if (is.character(data)) return(data)
  # mutate fYear as character for column selection (group_by_if)
  if ("fYear" %in% names(data)) data <- data %>% mutate(fYear=as.character(fYear))
  
  data2 <- data %>%
    # numeric can be Rev, Rev_sm, Liters, Liters_sm
    group_by_if(~ !is.numeric(.x)) %>%
    summarise_if(is.numeric, sum, na.rm=T) %>%
    ungroup 
  
  if ("fYear" %in% names(data)) data2 <- data2 %>% mutate(fYear=as.integer(fYear))
  return(data2)
}

f_summarise4lollipop <- function(data, lollipop_measure="Rev", showFY) {
  # lollipop_measure: Null if not lollipop chart, Rev if based on revenue, Liters if based on volume (Liters, sqm)
  # the case of "no data ..."
  if (is.character(data)) return(data)
  if (is.null(lollipop_measure)) return(data)
  
  lollipop_measure <- sym(lollipop_measure)
  data %>%
    filter(if (showFY!="All") fYear %in% showFY else TRUE) %>% 
    mutate(measure = !!lollipop_measure) %>%
    group_by(RegionSplit, fYear) %>%
    mutate(tot = sum(measure, na.rm=T)) %>%
    ungroup %>%
    group_by(RegionSplit, SuppliesSplit, fYear) %>%
    mutate(percentage = sum(measure, na.rm=T) / tot) %>%
    ungroup %>%
    # remove tot, x_sm (and the one not selected among RevVol)
    select_at(vars(-matches("^tot$|_sm$")))
}

f_summarise4ts <- function(data, ts_measure="Rev_sm") {
  # ts_measure: Null if not timeSeries chart, Rev_sm if based on revenue, Liters_sm if based on volume (Liters, sqm)
  # the case of "no data ..."
  if (is.character(data)) return(data)
  if (is.null(ts_measure)) return(data)
  date1 = str_sub(Sys.time(), 1, 10) %>% ymd %m+% months(-4)
  date2 = "2014/05/03"
  
  ts_measure <- rlang::sym(ts_measure)
  data2 <- data %>% 
    filter(fDate < ymd(date1),
           fDate > ymd(date2),
           !(str_detect(SuppliesSplit, "T549_10600|T46X_7000|T47X_9500|T480_7500|T486_5500|T500_10000|T40X_9000"))) %>%
    mutate(measure = !!ts_measure)
  
  if (nrow(data2)==0) return("no data f_summarise4ts") 
  return(data2)
  
  # MUST BE DONE ALREADY IN f_rkSupplies
  #   group_by(SuppliesPlot, SuppliesSplit, fYear, RegionReport, RegionSplit, fDate) %>%
  #   summarise(Liters = sum(Liters.sm, na.rm=T),
  #             LitersFY =sum(Liters, na.rm=T)) %>%
  #   ungroup %>%
  #   group_by(SuppliesPlot, SuppliesSplit) %>%
  #   mutate(ink_rk = sum(Liters, na.rm=T)) %>%
  #   ungroup
  # if (nrow(IS1)==0) return("no data")
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
  #   summarise(LitersFY = sum(LitersFY, na.rm=T)) %>% 
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

f_summarise4mosaic <- function(data, mosaic_measure, showFY) {
  
  # the case of "no data ..."
  if (is.character(data)) return(data)
  if (is.null(mosaic_measure)) return(data)
  
  mosaic_measure <- sym(mosaic_measure)
  data %>% mutate(measure = !!mosaic_measure) %>%
    filter(if (showFY!="All") fYear %in% showFY else TRUE)
}

f_summarise4RVP <- function(data, facetY) {
  # facetY = "RegionSplit" or "source"
  # need to drop fYear (keep fDate) 
  if (is.null(facetY)) return(data)
  date1 = str_sub(Sys.time(), 1, 10) %>% ymd %m+% months(-4)
  date2 = "2014/05/03"
  
  data %>% 
    filter(fDate < ymd(date1),
           fDate > ymd(date2),
           !(str_detect(SuppliesSplit, "T549_10600|T46X_7000|T47X_9500|T480_7500|T486_5500|T500_10000|T40X_9000"))) %>% 
    mutate(facetY = !!rlang::sym(facetY)) %>%
    select(-Rev, -Liters) %>%
    group_by_at(vars(matches("facetY|RegionReport|Supplies|Rev_sm|Liters_sm|Date"))) %>%
    summarise_if(is.numeric, funs(sum(., na.rm=T))) %>%
    ungroup %>%
    mutate(price = ifelse(Liters_sm == 0 | is.na(Liters_sm), 0, Rev_sm / Liters_sm)) %>%
    gather(key = KEY, value = VALUE, Rev_sm, Liters_sm, price) %>%
    mutate(KEY = factor(KEY, c("Rev_sm", "Liters_sm", "price")))
  # size = factor(size, c("RA4", "R13", "R24", "R36", "R44", "R64", "SA4", "SA3", "SA2", "SA1"))
}

# ===< test ===
# dat <- read_csv2("./temp/inkSalesEnriched_RVP.csv", col_types = "ccDccccccccccdddd")
# dat2 <- dat %>%
#   f_keepSize(TRUE) %>%
#   f_keepSource(FALSE) %>%
#   f_keepRegion(ReportRegionLevel="RegionLevel2", ReportRegionValue="EU5-NB", RegionSplitLevel="RegionLevel4") %>% 
#   f_keepSupplies(SuppliesPlotLevel="SuppliesLevel1", SuppliesPlotValue="LFPmedia", SuppliesSplitLevel="SuppliesLevel2") %>% 
#   f_rkRegion(yearRef=2017, rankBy="Rev") %>%
#   f_rkSupplies(yearRef=2017, rankBy="Rev") %>% 
#   f_topSupplies(topN=10) %>%
#   f_keepDateYear(ditch="fYear") %>% 
#   f_summariseIFnum %>% 
#   f_summarise4lollipop(lollipop_measure=NULL, showFY="All") %>% 
#   f_summarise4ts(ts_measure=NULL) %>% 
#   f_summarise4mosaic(mosaic_measure=NULL, showFY="All") %>% 
#   f_summarise4RVP(facetY = "size") 
# -----
# lollipop_input <- data4plot_Lollipop(dat, RRL="RegionLevel2", RRV="EU5-NB", RSL="RegionLevel2",
#                                      SPL="SuppliesLevel1", SPV="aqueous", SSL="SuppliesLevel2",
#                                      yearRef=2017, rankBy="Rev", topN=10,
#                                      lollipop_measure="Rev")
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

data4plot <- function(data, keepSize, keepSource,
                      RRL, RRV, RSL,
                      SPL, SPV, SSL,
                      yearRef, rankBy, topN, ditch, showFY,
                      lollipop_measure, ts_measure, mosaic_measure, RVP_facetY) {
  
  data2 <- data %>%
    f_keepSize(keepSize) %>% 
    f_keepSource(keepSource) %>% 
    f_keepRegion(ReportRegionLevel=RRL, ReportRegionValue=RRV, RegionSplitLevel=RSL) %>% 
    f_keepSupplies(SuppliesPlotLevel=SPL, SuppliesPlotValue=SPV, SuppliesSplitLevel=SSL) %>%
    f_rkRegion(yearRef, rankBy) %>%
    f_rkSupplies(yearRef, rankBy) %>%
    f_topSupplies(topN) %>%
    f_keepDateYear(ditch) %>%
    f_summariseIFnum %>%
    f_summarise4lollipop(lollipop_measure, showFY) %>%
    f_summarise4ts(ts_measure) %>%
    f_summarise4mosaic(mosaic_measure, showFY) %>%
    f_summarise4RVP(RVP_facetY)
}

data4plot_RVP <- function(data, keepSize, keepSource, RRL, RRV, RSL, 
                          SPL, SPV, SSL, 
                          yearRef, rankBy, topN,
                          RVP_facetY) {
  
  data2 <- data4plot(data, keepSize, keepSource,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch="fYear", showFY="All",
                     lollipop_measure=NULL, ts_measure=NULL, mosaic_measure=NULL, RVP_facetY)
  
  labs <- c(RVP_facetY=RVP_facetY)
  return(list(data=data2, labs=labs))
}

data4plot_Lollipop <- function(data, RRL, RRV, RSL,
                               SPL, SPV, SSL,
                               yearRef, rankBy, topN, showFY,
                               lollipop_measure) {
  
  data2 <- data4plot(data, keepSize=F, keepSource=F,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch="fDate", showFY,
                     lollipop_measure, ts_measure=NULL, mosaic_measure=NULL, RVP_facetY=NULL)
  
  divid <- function(y) { signif(y,3) %>% log10 %>% `%/%`(3) %>% floor %>% (function(x) {10^(3*x)}) }
  div <- data2 %>% select(measure) %>% unlist %>% max(na.rm=T) %>% divid
  
  labs <- c(paste0("Volume (x", div," sqm or Liters) per FY. Rank according to FY", yearRef), 
            paste0("sell-in data for the top ", topN, " models in product group ", SPV, "."), 
            "% of total ", 
            "unit", 
            "Ink / Printer model",
            paste0(RRL, RRV, RSL,"_", SPL, SPV, SSL, "_", yearRef, lollipop_measure, collapse="_"),
            div)
  
  return(list(data=data2, labs=labs))
}

data4plot_Mosaic <- function(data, RRL, RRV, RSL, 
                             SPL, SPV, SSL, 
                             yearRef, rankBy, topN, showFY,
                             mosaic_measure) {
  
  data2 <- data4plot(data, keepSize=F, keepSource=T,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch="fDate", showFY,
                     lollipop_measure=NULL, ts_measure=NULL, mosaic_measure, RVP_facetY=NULL)
  
  labs <- c(paste0("Split of Volume (, unit, ), per categories (within ", SPV, ") and channel", RRV),
            paste0("Top", topN, " categories | QTs are QTs approved by EMD"),
            "plotNameMosaic")
  return(list(data=data2, labs=labs))
}

data4plot_ts <- function(data, RRL, RRV, RSL, 
                         SPL, SPV, SSL, 
                         yearRef, rankBy, topN,
                         ts_measure) {
  
  data2 <- data4plot(data, keepSize=F, keepSource=F,
                     RRL, RRV, RSL,
                     SPL, SPV, SSL,
                     yearRef, rankBy, topN, ditch="fYear", showFY="All",
                     lollipop_measure=NULL, ts_measure, mosaic_measure=NULL, RVP_facetY=NULL)
  
  labs <- c(SPV)
  return(list(data=data2, labs=labs))
}


plot_lollipop <- function(dat) {
  
  cols <- c("-1" = "red", "0" = "grey75", "1" = "black")
  
  p1 <- ggplot(dat$data, aes(percentage, SuppliesSplit, label = signif(measure,3) / as.numeric(dat$labs[7]))) + 
    geom_segment(aes(x = 0, y = SuppliesSplit, xend = percentage, yend = SuppliesSplit), color = "grey50") +
    geom_point(aes(colour = factor(sign(measure))), size = ifelse(length(levels(dat$data$RegionSplit)) == 1, 7, 5)) +
    geom_text(color = "white", fontface = "bold", size = ifelse(length(levels(dat$data$RegionSplit)) == 1, 2.7, 2.3)) +
    scale_colour_manual(values = cols) +
    scale_y_discrete(limits = rev(levels(dat$data$SuppliesSplit))) +
    theme_bw(base_size = 9) +
    theme(strip.text.x = element_text(size = rel(1.2)),
          strip.text.y = element_text(size = rel(1.2))) +
    labs(title = dat$labs[1],
         subtitle = dat$labs[2],
         caption = dat$labs[6],
         x = dat$labs[3], 
         y = dat$labs[4]) + 
    facet_grid(RegionSplit ~ fYear)
  return(p1)
}

# ggplotly(p)
# plotlyOutput("plot1", height = "400px", width = "600px")
# output$plot1 <- renderPlotly({
#   plot_ly(d, x = ~carat, y = ~price, color = ~carat,
#           size = ~carat, text = ~paste("Clarity: ", clarity))    
# })



plot_ts <- function(dat) {
  # dat is a list with a slot "data" and a slot "labs"
  # former name inkLiLj
  
  p <- ggplot(dat$data, aes(x = fDate, y = measure)) + 
    geom_line(aes(color = SuppliesSplit)) +
    ylim(c(0, NA)) +
    theme_bw(base_size = 9) +
    theme(strip.text.x = element_text(size = rel(1.2)),
          strip.text.y = element_text(size = rel(1.2)),
          plot.caption = element_text(size = rel(0.5)),
          legend.title = element_blank()) +
    labs(title = paste0("Sales (, unit, /month), 6 months moving average"), 
         subtitle = paste0("Product group: ", dat$labs[1]),
         caption = "plotName",
         x = "", y = paste0("unit, /month")) +
    facet_wrap(~ RegionSplit + SuppliesPlot, nrow=1, scales = "free_y") +
    scale_color_discrete(name = "SuppliesSplit")
  return(p)
}


plot_mosaic <- function(dat) {
  # dat is a list with a slot "data" and a slot "labs"
  
  p1 <- ggplot(data = dat$data) + 
    geom_mosaic(aes(weight = measure, x = product(source, SuppliesSplit), fill=factor(source)), na.rm=TRUE, offset=0.005) + 
    labs(x="", 
         y=paste0("% of , unit"), 
         title = dat$labs[1],
         subtitle = dat$labs[2],
         caption = dat$labs[3]) +
    geom_segment(data = tibble(x0=seq(0,1,0.1), y0=rep(-0.015, 11), x9=seq(0,1,0.1), y9=rep(0, 11)),
                 aes(x=x0 , y=y0, xend=x9, yend=y9), colour = "grey40", size=0.2) +
    geom_segment(data = tibble(x0=seq(0,1,0.1), y0=rep(1, 11), x9=seq(0,1,0.1), y9=rep(1.015, 11)),
                 aes(x=x0 , y=y0, xend=x9, yend=y9), colour = "grey40", size=0.2) +
    facet_grid(RegionSplit ~ fYear) +
    guides(fill = guide_legend(title = "channel", reverse = TRUE)) + 
    theme_bw(base_size = 9) +
    theme(plot.title = element_text(size = rel(1.5)),
          strip.text.x = element_text(size=rel(1.2))) + 
    coord_flip()
  
  # fails otherwise (length(levels(Region)) > 1)
  if (length(levels(dat$data$RegionSplit))==1 & length(unique(dat$data$fYear)) == 1) {
    p1 <- p1 + 
      annotate("text", x = seq(0.01,0.91,0.1), y = rep(1.03, 10), label = paste0(seq(0,90,10), "%"), size=2.7)
  }
  return(p1)
}

plot_RVP <- function(dat) {
  # dat is a list with a slot "data" and a slot "labs"
  
  ggplot(dat$data, aes(x = fDate, y = VALUE)) +
    geom_line(aes(color = SuppliesSplit)) +
    ylim(c(0, NA)) +
    theme_bw(base_size = 9) +
    theme(strip.text.x = element_text(size = rel(1.2)),
          strip.text.y = element_text(size = rel(1.2)),
          plot.caption = element_text(size = rel(0.5)),
          legend.title = element_blank()) +
    labs(title = paste0("Revenue (euros netnet/month), Volume (Liters or sqm/month), Price (euros netnet/month), 6 months moving average"), 
         subtitle = paste0("Product group: ", dat$labs[1]),
         caption = "plotName",
         x = "", y = paste0("unit/month")) +
    facet_grid(KEY ~ facetY + SuppliesPlot, scales = "free") +
    scale_color_discrete(name = "SuppliesSplit")
  
}

f_waterfall_input <- function(data, Driver, period) {
  
  # f_waterfall_input(dat$RVP, "fDate", c(ymd("2017-04-01"), ymd("2017-07-31")))
  # data columns: fDate, possible drivers (except month), Revenue
  # input border of the reference period: input$dateRange[1], input$dateRange[2] (need to convert to fiscal_date) period <- c(ymd("2017-04-01"), ymd("2017-07-31"))
  # filter years and months (definition of period1, input$day01, input$day99 , period2 is year+1)
  # create source (period1 and 2)
  # group_by year, driver
  # driver can be regionLevel4, regionLevel2, months, suppliesLevel3, suppliesLevel4
  # summarize
  
  # nbMonths <- period %>% { interval(.[[1]], .[[2]]) %/% months(1) + 1 }
  # periodRef_name <- period[[1]] %>% { paste("from", year(.), month(.), "for", nbMonths, "months", sep = "_") }
  periodRef_name <- period %>% { paste("from", year(.[[1]]), month(.[[1]]), "to",  year(.[[2]]), month(.[[2]]), sep = "_") }
  nbYears <- period %>% { interval(.[[1]], .[[2]]) %/% years(1)  }
  periodRef <- period %>% purrr::map(f_C2Fdate) %>% { interval(.[[1]], .[[2]]) }
  period2 <- period %>% `+`(years(nbYears + 1)) %>% purrr::map(f_C2Fdate)
  period2_name <- period2 %>% { paste("from", year(.[[1]]), month(.[[1]]), "to",  year(.[[2]]), month(.[[2]]), sep = "_") }
  period2 <- period2 %>% { interval(.[[1]], .[[2]]) }
  
  dt <- data %>% 
    mutate(period = case_when(
      fDate %within% periodRef ~ periodRef_name,
      fDate %within% period2 ~ period2_name,
      TRUE ~ ""
    )
    ) %>%
    filter(period == periodRef_name | period == period2_name) %>% 
    rename(driver = !!sym(Driver)) %>% 
    { if (Driver == "fDate")  mutate(., driver = month(driver)) else . } %>%
    # mutate(driver=ifelse(Driver == "fDate", month(driver), driver)) %>% 
    group_by(period, driver) %>%
    summarise(Rev = sum(Rev, na.rm=T)) %>%
    ungroup %>%
    spread(key=period, value=Rev) %>% 
    mutate(DeltaValue= .[[period2_name]] - .[[periodRef_name]],
           DeltaValue= ifelse(is.na(DeltaValue), 0, DeltaValue), 
           # if driver is numeric (Month for instance), don't work with simply is.numeric, hence add map
           driver = ifelse(purrr::map(.$driver, ~is.numeric(.x)), 
                           format(driver, trim=FALSE), driver))
  
  dt1 <- tibble(driver=c(periodRef_name), DeltaValue=c(sum(dt[[periodRef_name]], na.rm=TRUE)))
  dt2 <- tibble(driver=c(period2_name), DeltaValue=c(sum(dt[[period2_name]], na.rm=TRUE)))
  dt <- bind_rows(dt1, select(dt, driver, DeltaValue), dt2) %>%
    mutate(driver = forcats::as_factor(driver),
           DeltaValue = ifelse(row_number() == n(), -1, 1) * DeltaValue,
           balance = cumsum(lag(DeltaValue, default = 0)),
           time = row_number(),
           flow = ifelse(row_number() == n() | row_number() == 1, 2, sign(DeltaValue)),
           flow = forcats::as_factor(as.character(flow)))
  
  return(dt)
}

euro_f <- function(x) {
  # € works in Rstudio interface, not in Shiny
  paste0(format(round(x/1000,0), big.mark = " ", decimal.mark = ",", trim = TRUE, scientific = FALSE), " kE") 
}

plot_waterfall <- function(dt, inputDriver) {
  # if labs do the basic version (region, sap_model, months), else do the variance analysis
  
  # rescale the y-axis (start from ybottom)
  ybottom <- dt %>% 
    select(balance) %>% 
    arrange(balance) %>% 
    filter(row_number() == 2) %>% 
    transmute(ymin = round(balance * 0.98, 1-floor(log10(balance * 0.98)))) %>%
    unlist()
  
  dt$DeltaLabel <- dt$DeltaValue
  dt$DeltaLabel[nrow(dt)] <- dt$DeltaLabel[nrow(dt)] * -1
  dt$balance[1] <- ybottom
  dt$DeltaValue[1] <- dt$DeltaValue[1] - ybottom
  dt$DeltaValue[nrow(dt)] <- ybottom + dt$DeltaValue[nrow(dt)]
  yintercept <- 0 + ybottom
  
  labs = c(paste0(dt$driver[1], "  VS.  ", dt$driver[nrow(dt)]),
           paste0("Variation by ", inputDriver))
  labels = dt$driver
  
  p <- ggplot(dt) +
    geom_hline(yintercept = yintercept, colour = "grey", size = 2) +
    geom_rect(aes(xmin = time - 0.45, xmax = time + 0.45, 
                  ymin = balance, ymax = balance + DeltaValue, 
                  fill = flow)) +
    geom_text(aes(x = time, 
                  y = pmin(balance, balance + DeltaValue) - 50, 
                  label = euro_f(DeltaLabel)),
              hjust = 0.5, vjust = 1, size = 3) +
    scale_x_continuous("", breaks = dt$time, labels = labels) +
    scale_y_continuous("Balance", labels = euro_f) +
    scale_fill_manual(values = c("-1" = "red", "1" = "green", "0" = "white", "2" = "royalblue4"), guide = FALSE) +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          panel.grid.minor.x = element_blank()) +
    labs(title = labs[1],
         subtitle = labs[2])
  
  return(p)
}

# --- FR ---

# merge ink sales and MIF data by date, inkL4, RegionL4
# FR <- moving_avg(inksales) / MIF

f_fr_merge <- function(IS, MIF) {
  MIF1 <- MIF %>% group_by(SuppliesLevel4, region, fDate) %>% 
    # here we get rid of hwmodel, so this is the place apply the multiplicator coef for models like SCF9 or SCS6
    summarise(cumqty=sum(cumqty, na.rm = T),
              cumMIF=sum(cumMIF, na.rm=T))
  FR <- IS %>% 
    group_by(RegionLevel0, RegionLevel2, RegionLevel4, fDate, SuppliesLevel4) %>%
    summarise(Liters_sm=sum(Liters_sm, na.rm=T)) %>%
    ungroup %>%
    #inner_join, rather than left_join, no need to keep the product without corresponding HW
    inner_join(MIF1, by = c("SuppliesLevel4" = "SuppliesLevel4",
                            "fDate" = "fDate", 
                            "RegionLevel4" = "region"))
  return(FR)
}

f_fr_filter <- function(FR, regionValue, regionValueLevel, regionSplit, suppliesValue, suppliesValueLevel, max_fr) {
  #return a df with field region, regionSplit, supplies (suppliesValueLevel), fDate, ready for plot
  FR <- FR %>% 
    mutate(region = !!sym(regionValueLevel)) %>% # mutate rather than rename in case regionValueLevel and RegionSplit are equal
    filter(region == regionValue) %>%
    rename(regionSplit = !!sym(regionSplit)) %>%
    rename(supplies = !!sym(suppliesValueLevel)) %>%
    filter(supplies == suppliesValue) %>%
    group_by(region, regionSplit, supplies, fDate) %>%
    summarise_if(is.numeric, funs(sum(., na.rm=T))) %>%
    ungroup %>%
    mutate(fr = Liters_sm / cumMIF,
           fr = ifelse(fr > max_fr, max_fr, fr)) %>% #ifelse(is.numeric(cumMIF), Liters_sm / cumMIF, NA))
    tidyr::gather(key = KEY, value = VALUE, Liters_sm, cumqty, cumMIF, fr)
  return(FR)
}

plot_fr <- function(FR) {
  ggplot(FR, aes(x=fDate, y=VALUE, color=regionSplit)) + 
    facet_grid(KEY ~ ., scales = "free_y") +
    geom_line() + 
    theme_bw(base_size = 9) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title.x = element_blank()) +
    labs(title = "title",
         subtitle = "subtitle",
         caption = "caption",
         x = "x",
         y = "y")
  
}