# <in launchApp.R or in server.R>
suppliesdash_PreviousMonth <- function(PreviousMonth) {
  lubridate::day(PreviousMonth) <- 1
  PreviousMonth <- PreviousMonth + lubridate::years(-1) + lubridate::days(-1)
  return(PreviousMonth)
}

suppliesInitValue <- function() {"ink"}
suppliesInitLevel <- function() {"SuppliesLevel0"}

regionInitValue <- function() {"EMEAR"}
regionInitLevel <- function() {"RegionLevel0"}


#' Clean up app input data.
#'
#' \code{suppliesdash_data}
#'
#' @param myData a list, containing 2 dataframe RVP and V.
#' @return a list, myData with the 2 dataframe RVP and V amended.
suppliesdash_data <- function(myData) {

  # dat <- list()
  # dat <- dataFROMdata

  myData$RVP <- myData$RVP %>%
    # should be done in the process that generate the data
    dplyr::filter(!is.na(SuppliesLevel0))

  myData$V <- myData$V %>%
    # should be done in the process that generate the data
    dplyr::filter(!is.na(SuppliesLevel0))
  return(myData)
}


#' function returning the sLevel, supplies hierarchy.
#'
#' \code{suppliesdash_sLevel}
#'
#' @param myData a dataframe with columns SuppliesLeveli.
#' @return a dataframe with only the columns SuppliesLeveli and the unique
#'   combinations.
suppliesdash_sLevel <- function(myData) {
  # sLevel
  myData$RVP %>%
    dplyr::select(dplyr::matches("SuppliesLevel[0-4]")) %>%
    dplyr::distinct() %>%
    dplyr::arrange(SuppliesLevel0, SuppliesLevel1, SuppliesLevel2,
                   SuppliesLevel3, SuppliesLevel4) %>%
    #sort columns alphabetically
    dplyr::select(order(colnames(.)))
}


#' function returning the rLevel, region hierarchy.
#'
#' \code{suppliesdash_rLevel}
#'
#' @param myData a dataframe with columns RegionLeveli.
#' @return a dataframe with only the columns RegionLeveli and the unique
#'   combinations.
suppliesdash_rLevel <- function(dat) {
  # rLevel
  dat$RVP %>%
    dplyr::select(dplyr::matches("RegionLevel[0-4]")) %>%
    dplyr::bind_rows(dplyr::tibble(RegionLevel0 = "EMEAR",
                                   RegionLevel1 = "Europe",
                                   RegionLevel2 = "CEE",
                                   RegionLevel4 = "EE_Balk&Gr_TR")) %>%
    dplyr::distinct() %>%
    dplyr::filter(RegionLevel0 == 'EMEAR') %>%
    dplyr::arrange(RegionLevel1, RegionLevel2, RegionLevel4) %>%
    #sort columns alphabetically
    dplyr::select(order(colnames(.)))
}


#' Was used for populating top_supplies value (suppliesInitValues).
#'
#' \code{unlist_unique} used to find the SuppliesInitValue from sLevel.
#' \code{unlist_unique} consume the result from suppliesdash_sLevel.
#'
#' @param level1 a dataframe.
#' @return a vector of strings with distinct items.
unlist_unique <- function(level1) {
  # regionInitValues
  level1 %>%
    unlist(use.names = F) %>%
    unique()
}

# <... in launchApp.R or in server.R>
