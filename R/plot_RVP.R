#' Returns a ggplot2 object.
#'
#' \code{plot_lollipop} expect data from \code{\link{data4plot_Lollipop}}.
#'
#' @param dat A list including a dataframe, data, and labels, labs.
#'   data has columns: percentage, SuppliesSplit, measure, RegionSplit, fYear.
#' @return a ggplot2 object.
plot_lollipop <- function(dat) {

  cols <- c("-1" = "red", "0" = "grey75", "1" = "black")

  p1 <-
    ggplot2::ggplot(
      dat$data,
      ggplot2::aes(percentage,
                   SuppliesSplit,
                   label = signif(measure,2) / as.numeric(dat$labs[7]))) +
    ggplot2::geom_segment(
      ggplot2::aes(x = 0,
                   y = SuppliesSplit,
                   xend = percentage,
                   yend = SuppliesSplit),
      color = "grey50") +
    ggplot2::geom_point(
      ggplot2::aes(colour = factor(sign(measure))),
                   # size = ifelse(length(levels(dat$data$RegionSplit)) == 1, 8, 7)) +
                   size = 5) +
    # ggplot2::geom_text(color = "white",
    #                    fontface = "bold",
    #                    size = ifelse(length(levels(dat$data$RegionSplit)) == 1, 2.7, 2.5)) +
    ggplot2::geom_text(
      color = "black",
      # fontface = "bold",
      size = 4,
      nudge_x = 0.06,
      nudge_y = 0.3) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::scale_y_discrete(limits = rev(levels(dat$data$SuppliesSplit))) +
    ggplot2::scale_x_continuous(
      labels = function(x) scales::percent(x, accuracy = 1)) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.0)),
      strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.0)),
      axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.1)),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.1))) +
    ggplot2::labs(title = dat$labs$title,
                  subtitle = dat$labs$subtitle,
                  caption = dat$labs$caption,
                  x = dat$labs$x_lab,
                  y = dat$labs$y_lab) +
    ggplot2::facet_grid(RegionSplit ~ fYear)
  return(p1)
}

# ggplotly(p)
# plotlyOutput("plot1", height = "400px", width = "600px")
# output$plot1 <- renderPlotly({
#   plot_ly(d, x = ~carat, y = ~price, color = ~carat,
#           size = ~carat, text = ~paste("Clarity: ", clarity))
# })



#' Returns a ggplot2 object.
#'
#' \code{plot_ts} expects data from \code{\link{data4plot_ts}}.
#'
#' @param dat A list including a dataframe, data, and labels, labs.
#' @return a ggplot2 object.
plot_ts <- function(dat) {

  subtitle <- ifelse(dat$labs$SP_filter_part == "all",
                     dat$labs$SPV,
                     paste0(dat$labs$SP_filter_part, " in ", dat$labs$SPV)) %>%
    paste0("Product Group: ", .)
  vol_unit <-
    data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == dat$labs$SP_filter_part,
                   "unit"][[1]]
  title <- ifelse(stringr::str_detect(dat$labs$ts_measure, "Rev"),
                  "Sell-in Revenue in EurosNetNet/month",
                  paste0("Sell-in Volume in ", vol_unit, "/month")) %>%
    paste0(", 6 months moving average.")


  p <- ggplot2::ggplot(dat$data,
                       ggplot2::aes(x = fDate, y = measure)) +
    ggplot2::geom_line(ggplot2::aes(color = SuppliesSplit)) +
    ggplot2::ylim(c(0, NA)) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.2)),
      strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.2)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.5)),
      legend.title = ggplot2::element_blank()) +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  caption = "plotName",
                  x = "",
                  y = paste0(vol_unit, "/month")) +
    ggplot2::facet_wrap(~ RegionSplit + SuppliesPlot,
                        nrow = 1,
                        scales = dat$labs$free_y) +
    ggplot2::scale_color_discrete(name = "SuppliesSplit")
  return(p)
}


#' Returns a ggplot2 object.
#'
#' \code{plot_mosaic} expect data from \code{\link{data4plot_Mosaic}}.
#'
#' @param dat A list including a dataframe, data, and labels, labs.
#' @return a ggplot2 object.
plot_mosaic <- function(dat) {
  # dat is a list with a slot "data" and a slot "labs"

  # DEBUG
  # dat <- mosaic_input_R() <- data4plot_Mosaic(subsetData_RVP(),
                                              # RRL = RegLevD(),
                                              # RRV = input$top_RegionValue,
                                              # RSL = input$top_RegionSplit,
                                              # SPL = SupLevD(),
                                              # SPV = input$top_SuppliesValue,
                                              # SSL = input$top_SuppliesSplit,
                                              # yearRef = as.numeric(input$top_yearRef),
                                              # rankBy = "Rev",
                                              # topN = as.numeric(input$top_topN),
                                              # showFY = input$top_FYs,
                                              # mosaic_measure = "Rev",
                                              # SP_filter_part = input$filter_part)
  #
  #   pattern <- data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == input$filter_part, "regex"][[1]]
  # subsetData_RVP <- dat$RVP %>%
  #   dplyr::filter(stringr::str_detect(SuppliesLevel2, pattern)) %>%
  #   dplyr::filter((!!dplyr::sym(RegLevD())) == input$top_RegionValue,
  #                 (!!dplyr::sym(SupLevD())) == input$top_SuppliesValue)
  # dat <- suppliesdash_data(myData) [myData = data_ISenriched]

  #DEBUG2
  # load code in input_RVP.R
  # library(tidyverse)
  # library(ggmosaic)
  # load("./data/data_ISenriched.rda")
  # load("./R/sysdata.rda")
  # subsetData_RVP <- data_ISenriched$RVP %>%
  #   dplyr::filter((!!dplyr::sym("RegionLevel0")) == "EMEAR",
  #                 (!!dplyr::sym("SuppliesLevel1")) == "CommPhoto-Supplies")
  # dat <- data4plot_Mosaic(subsetData_RVP,
  #                         RRL = "RegionLevel0",
  #                         RRV = "EMEAR",
  #                         RSL = "RegionLevel0",
  #                         SPL = "SuppliesLevel1",
  #                         SPV = "CommPhoto-Supplies",
  #                         SSL = "SuppliesLevel3",
  #                         yearRef = 2018,
  #                         rankBy = "Rev",
  #                         topN = 7,
  #                         showFY = "All",
  #                         mosaic_measure = "Rev",
  #                         SP_filter_part = "all <|> .*")
  #END DEBUG2, now can run the code of plot_mosaic
  # check ooutput,  dat <- list() ; dat$data <- read_csv2("test_mosaic_jan18.csv")

  # dat$data <-
  #   dplyr::mutate_if(dplyr::mutate_if(dat$data, is.character, forcats::as_factor), is.integer, ~forcats::as_factor(as.character(.)))
  # ggmosaic::geom_mosaic(ggplot2::aes(weight = as.numeric(dat$data$measure),
  #                                    x = ggmosaic::product(dat$data$source, dat$data$SuppliesSplit),
  #                                    fill = dat$data$source),
  #                       na.rm = TRUE,
  #                       offset = 0.005)



  dat$data <-
    dat$data %>%
    dplyr::mutate_if(is.character, forcats::as_factor) %>%
    dplyr::mutate_if(is.integer, ~forcats::as_factor(as.character(.)))

  p1 <-
    ggplot2::ggplot(data = dat$data) +
    ggmosaic::geom_mosaic(
      ggplot2::aes(weight = as.numeric(measure),
                   x = ggmosaic::product(source, SuppliesSplit),
                   fill = source),
      na.rm = TRUE,
      offset = 0.005) +
    ggplot2::labs(# x = "",
                # y = paste0("% of , unit"),
                 title = dat$labs[1],
                 subtitle = dat$labs[2],
                 caption = dat$labs[3]) +
    # scale_?_productlist seen in
    # https://github.com/earowang/paper-tsibble/blob/master/R/flights.R
    # ggmosaic::scale_x_productlist(name = "") +
    # ggmosaic::scale_y_productlist(name = paste0("% of , unit")) +
    ggplot2::geom_segment(data = tibble::tibble(x0 = seq(0, 1, 0.1),
                                                y0 = rep(-0.015, 11),
                                                x9 = seq(0, 1, 0.1),
                                                y9 = rep(0, 11)),
                          ggplot2::aes(x = x0 , y = y0, xend = x9, yend = y9),
                          colour = "grey40",
                          size = 0.2) +
    ggplot2::geom_segment(data = tibble::tibble(x0 = seq(0, 1, 0.1),
                                                y0 = rep(1, 11),
                                                x9 = seq(0, 1, 0.1),
                                                y9 = rep(1.015, 11)),
                          ggplot2::aes(x = x0 , y = y0, xend = x9, yend = y9),
                          colour = "grey40",
                          size = 0.2) +
    ggplot2::geom_segment(data = tibble::tibble(x0 = rep(-0.015, 11),
                                                y0 = seq(0, 1, 0.1),
                                                x9 = rep(0, 11),
                                                y9 = seq(0, 1, 0.1)),
                          ggplot2::aes(x = x0 , y = y0, xend = x9, yend = y9),
                          colour = "grey40",
                          size = 0.2) +
    # ggplot2::scale_x_continuous() +
    ggplot2::facet_grid(RegionSplit ~ fYear) +
    ggplot2::guides(
      fill = ggplot2::guide_legend(title = "channel", reverse = TRUE)) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = ggplot2::rel(1.5)),
      strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.4)),
      strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.4)),
      axis.text.y = ggplot2::element_text(size = ggplot2::rel(1.2))) +
      # axis.text.x = ggplot2::element_text(size = ggplot2::rel(1.0),
      #                    angle = 45,
      #                    vjust = 0.5, hjust = 0.7)) +
    ggplot2::coord_flip()

  # fails otherwise (length(levels(Region)) > 1)
  if (length(levels(dat$data$RegionSplit)) == 1 &
      length(unique(dat$data$fYear)) == 1) {
    p1 <-
      p1 +
      ggplot2::annotate("text",
                        x = seq(0.01,0.91,0.1),
                        y = rep(1.03, 10),
                        label = paste0(seq(0,90,10), "%"),
                        size = 2.7)
  }
  return(p1)
}


#' Returns a ggplot2 object
#'
#' \code{plot_RVP} expect data from \code{\link{data4plot_RVP}}.
#'
#' @param dat A list including a dataframe, data, and labels, labs.
#' @return a ggplot2 object.
plot_RVP <- function(dat) {
  # dat is a list with a slot "data" and a slot "labs"

  subtitle <-
    ifelse(dat$labs$SP_filter_part == "all",
           dat$labs$SPV,
           paste0(dat$labs$SP_filter_part, " in ", dat$labs$SPV)) %>%
    paste0("Product Group: ", .)
  vol_unit <-
    data_SL2_regex[data_SL2_regex$SuppliesLevel2_type == dat$labs$SP_filter_part,
                   "unit"][[1]]
  title <-
    paste0("Revenue (euros netnet/month), Volume (", vol_unit, "/month), ",
           "Price (euros netnet/", vol_unit, "/month), 6 months moving average.")

  #NEW dat$data$label

  ggplot2::ggplot(dat$data, ggplot2::aes(x = fDate, y = VALUE)) +
    ggplot2::geom_line(ggplot2::aes(color = SuppliesSplit)) +
    ggplot2::ylim(c(0, NA)) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      strip.text.x = ggplot2::element_text(size = ggplot2::rel(1.2)),
      strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.2)),
      plot.caption = ggplot2::element_text(size = ggplot2::rel(0.5)),
      legend.title = ggplot2::element_blank()) +
    ggplot2::labs(title = title,
                  subtitle = subtitle,
                  caption = "plotName",
                  x = "",
                  y = paste0("unit/month")) +
    # ggplot2::facet_grid(KEY ~ facetY + SuppliesPlot, scales = "free") +
    # ggplot2::facet_wrap(KEY ~ facetY + SuppliesPlot, nrow = 3, scales = dat$labs$free_y) +
    ggplot2::facet_wrap(~label, nrow = 3, scales = dat$labs$free_y) +
    ggplot2::scale_color_discrete(name = "SuppliesSplit")

}
