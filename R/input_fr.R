# --- FR ---

# merge ink sales and MIF data by date, inkL4, RegionL4
# FR <- moving_avg(inksales) / MIF


#' Join Inksales and MIF data, with prior relevant summary.
#'
#' \code{f_fr_merge} used only in Supplies Project not in suppliesdash.
#'
#' @param IS A dataframe, ink sales
#' @param MIF A dataframe, Machine In Field
#' @return a dataframe with columns: RL0, 2, 4, fDate, SL4, cumqty, cumMIF,
#'   Liters_sm
f_fr_merge <- function(IS, MIF) {
  MIF1 <- MIF %>%
    dplyr::group_by(SuppliesLevel4, region, fDate) %>%
    # here we get rid of hwmodel, so this is the place apply the multiplicator coef for models like SCF9 or SCS6
    dplyr::summarise(cumqty = sum(cumqty, na.rm = T),
                     cumMIF = sum(cumMIF, na.rm = T))
  FR <- IS %>%
    dplyr::group_by(RegionLevel0, RegionLevel2, RegionLevel4, fDate, SuppliesLevel4) %>%
    dplyr::summarise(Liters_sm = sum(Liters_sm, na.rm = T)) %>%
    dplyr::ungroup() %>%
    #inner_join, rather than left_join, no need to keep the product without corresponding HW
    dplyr::inner_join(MIF1, by = c("SuppliesLevel4" = "SuppliesLevel4",
                                   "fDate" = "fDate",
                                   "RegionLevel4" = "region"))
  return(FR)
}



#' Filter, and summarise, the dataframe containing the fit rate information.
#'
#' \code{f_fr_filter} used in shiny::eventReactive subsetData_FR(), which is
#' the input to \code{\link{plot_fr}}.
#'
#' @param FR A dataframe, Fit Rate resulting from MIF and IS.
#' @param regionValue A string.
#' @param regionValueLevel A string.
#' @param regionSplit A string.
#' @param suppliesValue A string.
#' @param suppliesValueLevel A string.
#' @param max_fr A double.
#' @return a dataframe with columns: region, regionSplit, supplies, fDate,
#'   KEY, VALUE. Key lists Liters_sm, fr, MIF, cumqty.
f_fr_filter <- function(FR, regionValue, regionValueLevel, regionSplit, suppliesValue, suppliesValueLevel) {
  # SIMPLIFICATION remove max_fr
  #return a df with field region, regionSplit, supplies (suppliesValueLevel), fDate, ready for plot
  FR <- FR %>%
    # mutate rather than rename in case regionValueLevel and RegionSplit are equal
    dplyr::mutate(region = !!dplyr::sym(regionValueLevel)) %>%
    dplyr::filter(region == regionValue) %>%
    dplyr::rename(regionSplit = !!dplyr::sym(regionSplit)) %>%
    dplyr::rename(supplies = !!dplyr::sym(suppliesValueLevel)) %>%
    dplyr::filter(supplies == suppliesValue) %>%
    dplyr::group_by(region, regionSplit, supplies, fDate) %>%
    dplyr::summarise_if(is.numeric, dplyr::funs(sum(., na.rm = T))) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(fr = Liters_sm / cumMIF
                  #ifelse(is.numeric(cumMIF), Liters_sm / cumMIF, NA))
                  # ,fr = ifelse(fr > max_fr, max_fr, fr) # SIMPLIFICATION
                  ) %>%
    # Warning: Error in tidyr::gather: object 'Liters_sm' not found
    # cumMIF doesn't mean anything, MIF is the proper name, correction made here, at the end of the process
    dplyr::rename(MIF = cumMIF) %>%
    tidyr::gather(key = KEY, value = VALUE, Liters_sm, cumqty, MIF, fr) %>%
    dplyr::mutate(KEY = factor(KEY, c("Liters_sm", "fr", "MIF", "cumqty")))
  return(FR)
}
