#' Plot a treemap.
#'
#' \code{plotTree} takes input from \code{\link{dataTree}}.
#'
#' @param dat A dataframe with columns category, subcategory and total.
#' @return a ggplot object.
plotTree <- function(dat) {

  # colors <- c(
  #   income = "#975c72",
  #   expenses = "#724678",
  #   savings = "#545294"
  # )

  ggplot2::ggplot(
    dat,
    ggplot2::aes(
      area = total, fill = category, label = subcategory,
      subgroup = subcategory
  )) +
    treemapify::geom_treemap() +
    treemapify::geom_treemap_text(color = "white", fontface = 2) +
    # scale_fill_manual(values = colors) +
    ggplot2::scale_fill_brewer() +
    ggplot2::theme(
      legend.key.width = ggplot2::unit(2, "cm"),
      legend.key.size = ggplot2::unit(1, "cm"),
      legend.background = ggplot2::element_rect(fill = "gray90", size = 0.5, linetype = "dotted"),
      legend.position = "bottom",
      legend.title = ggplot2::element_text(
        face = "bold",
        inherit.blank = TRUE)
    )
}
