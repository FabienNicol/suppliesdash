#' Prepare data for plotting treemap.
#'
#' \code{dataTree}
#'
#' @param dat A dataframe with columns evaluate(RevVol), fDate, evaluate(cat),
#'   evaluate(subcat).
#' @param cat A string.
#' @param subcat A string.
#' @param RevVol A string.
#' @param FY An integer, fiscal year.
#' @return a dataframe ready for plotting by plotTree(), columns category,
#'   subcategory and total.
dataTree <- function(dat, cat, subcat, RevVol, FY) {

  # need to find category and sub cat
  # summarize the data
  # cat <- enquo(cat)
  # subcat <- enquo(subcat)
  RevVol <- dplyr::sym(RevVol)
  dat %>%
    dplyr::filter(lubridate::year(fDate) == FY) %>%
    # rename(category := !!rlang::sym(cat), DO NOT WORK WHEN CAT==SUBCAT
    #        subcategory := !!rlang::sym(subcat)) %>%
    dplyr::mutate(category = !!dplyr::sym(cat),
                  subcategory = !!dplyr::sym(subcat)) %>%
    # group_by_at(vars(cat, subcat)) %>%
    dplyr::group_by(category, subcategory) %>%
    dplyr::summarise(total = sum(!!RevVol, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(category = tidyr::replace_na(category, "NA"),
                  subcategory = tidyr::replace_na(subcategory, "NA"))
}
