#' Return the following finer level.
#'
#' \code{finer} is used in the treemap.
#'
#' @param x A string, an item from vector Vx.
#' @param vx A vector.
#' @return A string.
finer <- function(x, vx) {

  # take any of names(sLeveln())/names(rLeveln())
  vx <- vx[order(vx)]
  pos <- match(x, vx)
  pos <- ifelse(pos < length(vx), pos + 1, pos)
  vx[pos]
}

#' Dummy function in order to have import to namespace done.
#'
#' The comments of the function get imported or the operator aren't imported
#' otherwise.
#'
# @importFrom magrittr %>%
# @importFrom rlang !! !!! :=
# @importFrom lubridate %m+% %within%
#'
#' @param dummy A dummy variable.
dah <- function(dummy) {
  "dah"
}
