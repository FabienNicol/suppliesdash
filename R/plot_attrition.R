# consider there can be more than 1 attrition profile per SuppliesLevel4
# data_plot_attrition will give one plot per row (usually one row)
# /!\ current version only te first line is shown

#data_attrition is loaded from rda

# SuppliesLevel4_id <- "T782_SLD700"
# data_plot_attrition(SuppliesLevel4_id, data_attrition) %>%
#   select(plot) %>%
#   pull(1)

#MANAGE 2 plots ... (ideally facet)
# par(mfrow = c(1,2))
# data_plot_attrition$plot[[1]]
# data_plot_attrition$plot[[2]]
# par(mfrow = c(1,1))



#' Return a dataframe, with 1 ggplot2 object in column plot.
#'
#' \code{data_plot_attrition} uses \code{\link{plot_3segAttrition}}.
#'
#' @param SuppliesLevel4_id a string, select the model to plot.
#' @param data_attrition a dataframe with columns: SuppliesLevel4, hwRatio,
#'   attritionProfile, hwmodel2, segX_percent, segX_begin, segX_end.
#' @return a dataframe with columns, SuppliesLevel4, plot.
data_plot_attrition <- function(SuppliesLevel4_id, data_attrition) {

  data_attrition %>%
    dplyr::filter(SuppliesLevel4 == SuppliesLevel4_id) %>%
    # hwRatio not used for now
    dplyr::select(-hwRatio) %>%
    dplyr::distinct() %>%
    dplyr::mutate(plot = purrr::pmap(list(SuppliesLevel4, attritionProfile, hwmodel2,
                                          segA_percent, segA_begin, segA_end,
                                          segB_percent, segB_begin, segB_end,
                                          segC_percent, segC_begin, segC_end),
                                     plot_3segAttrition)) %>%
    return()
}


#' Returns the (1-attrition) profile, month by month.
#'
#' \code{f_basicAttrition}
#'
#' @param month_at_100 an integer, month when decline starts.
#' @param month_0_reached an integer, month when decline ends.
#' @return a double vector, with continuous declining values from 1 to 0.
f_basicAttrition <- function(month_at_100 = 24, month_0_reached = 48) {
  c(rep(1, month_at_100),
    seq(1, 0, length.out = month_0_reached - month_at_100),
    rep(0, length.out = 12*20 - month_0_reached))
}



#' Return a ggplot2 object displaying the 3 segment attrition profile.
#'
#' \code{plot_3segAttrition} uses \code{\link{f_basicAttrition}}.
#' grey dashed lines shows the half-life.
#'
#' @param SuppliesLevel4 a string
#' @param attritionProfile astrin TBC
#' @param hwmodel2 a string TBC
#' @param segA_percent a double, the share of the population in segment A.
#' @param segB_percent a double, the share of the population in segment B.
#' @param segC_percent a double, the share of the population in segment C.
#' @param segA_begin an integer, month where decline starts for segment A.
#' @param segB_begin an integer, month where decline starts for segment B.
#' @param segC_begin an integer, month where decline starts for segment C.
#' @param segA_end an integer, month where decline ends for segment A.
#' @param segB_end an integer, month where decline ends for segment B.
#' @param segC_end an integer, month where decline ends for segment C.
#' @return a ggplot2 object.
plot_3segAttrition <- function(
  SuppliesLevel4, attritionProfile, hwmodel2,
  segA_percent = c(0.1, 0.3), segA_begin = 24, segA_end = 48,
  segB_percent = c(0.7, 0.4), segB_begin = 36, segB_end = 120,
  segC_percent = c(0.2, 0.3), segC_begin = 60, segC_end = 240) {


  # pmap_df(list(segA_begin, segA_end), f_basicAttrition)
  # ... operation by row (the output is the dataframe, result column must have the name of the rows)

  data_plot <-
    segA_percent * f_basicAttrition(segA_begin, segA_end) +
    segB_percent * f_basicAttrition(segB_begin, segB_end) +
    segC_percent * f_basicAttrition(segC_begin, segC_end)

  data_plot <-
    tibble::tibble(attrition = data_plot) %>%
    dplyr::mutate(time = 1:dplyr::n())

  find_time <-
    function(attrition) { which.min(abs(data_plot$attrition - attrition)) }

  arrows <-
    data_plot %>%
    dplyr::filter(time %in% purrr::map_int(c(0.25, 0.5, 0.75), find_time))

  ggplot2::theme_set(ggplot2::theme_bw())

  data_plot %>%
    ggplot2::ggplot(ggplot2::aes(x = time, y = attrition)) +
    ggplot2::geom_line() +
    ggplot2::geom_segment(data = arrows,
                          ggplot2::aes(x = time, y = attrition, xend = time, yend = 0),
                          color = c("gray60", "blue", "gray60"),
                          linetype = "dashed",
                          arrow = ggplot2::arrow(length = ggplot2::unit(0.3, "cm"))) +
    ggplot2::geom_segment(data = arrows,
                          ggplot2::aes(x = time, y = attrition, xend = 0, yend = attrition),
                          color = c("gray60", "blue", "gray60"),
                          linetype = "dashed") +
    ggplot2::geom_text(data = arrows,
                       ggplot2::aes(x = time, y = 0, label = time),
                       color = c("gray60", "blue", "gray60"),
                       vjust = 0, nudge_y = -0.04) +
    ggplot2::labs(title = paste0("attrition rate for ", SuppliesLevel4, " with ", attritionProfile, " profile"),
                  subtitle = paste0("attrition rate and half-life for ", hwmodel2),
                  x = "time (months)",
                  y = "1 - attrition") %>%
    return()
}
