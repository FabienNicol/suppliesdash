#' Build a ggplot2 object with geom_line and facet on KEY.
#'
#' \code{plot_fr} expects input data from \code{\link{f_fr_filter}}.
#' One unique supplies (SuppliesLevel4) is selected for this plot.
#'
#' @param FR a dataframe with columns: fDate, VALUE, regionSplit, KEY, supplies.
#' @return a ggplot2 object.
plot_fr <- function(FR) {

  title <- paste0(FR$supplies, "   |   Fit rate in Liters/months/unit, Liters_sm in Liters/month")

  ggplot2::ggplot(FR, ggplot2::aes(x = fDate, y = VALUE, color = regionSplit)) +
    ggplot2::facet_grid(KEY ~ ., scales = "free_y") +
    ggplot2::geom_line() +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
                   axis.title.x = ggplot2::element_blank(),
                   strip.text.y = ggplot2::element_text(size = ggplot2::rel(1.5))) +
    ggplot2::labs(title = title)
                  # , subtitle = FR$supplies,
                  # caption = "caption",
                  # x = "x",
                  # y = "y")

}
