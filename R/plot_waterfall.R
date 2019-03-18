#' Format a double in kEuros.
#'
#' \code{euro_f} is used in \code{\link{plot_waterfall}}.
#'
#' @param x A double.
euro_f <- function(x) {
  # € works in Rstudio interface, not in Shiny
  paste0(format(round(x/1000,0), big.mark = " ",
                decimal.mark = ",", trim = TRUE, scientific = FALSE), " kE")
}



#' Plot waterfall chart.
#'
#' \code{plot_waterfall} get input from \code{\link{f_waterfall_input}}.
#'
#' @param dt A dataframe with fields balance, DeltaValue, driver.
#' @param inputDriver A string.
#' @return a ggplot object.
plot_waterfall <- function(dt, inputDriver) {
  # if labs do the basic version (region, sap_model, months), else do the variance analysis

  # rescale the y-axis (start from ybottom)
  ybottom <-
    dt %>%
    dplyr::select(balance) %>%
    dplyr::arrange(balance) %>%
    dplyr::filter(dplyr::row_number() == 2) %>%
    dplyr::transmute(ymin = round(balance * 0.98, 1 - floor(log10(balance * 0.98)))) %>%
    unlist()

  dt$DeltaLabel <- dt$DeltaValue
  dt$DeltaLabel[nrow(dt)] <- dt$DeltaLabel[nrow(dt)] * -1
  dt$balance[1] <- ybottom
  dt$DeltaValue[1] <- dt$DeltaValue[1] - ybottom
  dt$DeltaValue[nrow(dt)] <- ybottom + dt$DeltaValue[nrow(dt)]
  yintercept <- 0 + ybottom

  # DEBUG (do not work placed here)
  # cat(file=stderr(), "plot_waterfall> ", names(dat), "\n")
  # print(dt)
  # write.csv(dt, "dt_waterfall_in_plot.csv")

  labs <- c(paste0(dt$driver[1], "  VS.  ", dt$driver[nrow(dt)]),
           paste0("Variation by ", inputDriver))
  labels <- dt$driver

  p <-
    ggplot2::ggplot(dt) +
    ggplot2::geom_hline(yintercept = yintercept, colour = "grey", size = 2) +
    ggplot2::geom_rect(ggplot2::aes(xmin = time - 0.45, xmax = time + 0.45,
                  ymin = balance, ymax = balance + DeltaValue,
                  fill = flow)) +
    ggplot2::geom_text(
      ggplot2::aes(x = time,
                   y = pmin(balance, balance + DeltaValue) - 50,
                   label = euro_f(DeltaLabel)),
                   hjust = 0.5, vjust = 1, size = 5) +
    ggplot2::scale_x_continuous("", breaks = dt$time, labels = labels) +
    ggplot2::scale_y_continuous("Balance", labels = euro_f) +
    ggplot2::scale_fill_manual(values = c("-1" = "red",
                                          "1" = "green",
                                          "0" = "white",
                                          "2" = "royalblue4"), guide = FALSE) +
    ggplot2::theme_bw(base_size = 13) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                       hjust = 1,
                                                       size = ggplot2::rel(1.2)),
                   panel.grid.minor.x = ggplot2::element_blank()) +
    ggplot2::labs(title = labs[1],
                  subtitle = labs[2])

  return(p)
}
