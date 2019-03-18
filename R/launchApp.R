#' Launches the suppliesdash app.
#'
#' Display a shiny user interface that display graphs and filters.
#'
#' @export launchApp
#' @importFrom shiny shinyApp
#'
#' @return shiny application object
#'
# @example \dontrun {launchApp()}
launchApp <- function() {
  # option for debugging
  # HERE: options(shiny.error = browser)
  # WITHIN THE CODE: cat(file=stderr(), "drawing histogram with", input$bins, "bins", "\n")

  data(data_ISenriched, package = "suppliesLFPefs")
  shiny::addResourcePath("www", system.file("dash_app", "www", package = "suppliesLFPefs"))

  shiny::shinyApp(
    suppliesdash_ui(data_ISenriched),
    suppliesdash_server(data_ISenriched)
  )

}
