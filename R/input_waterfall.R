#' Prepare the data for f_waterfall_input depending on the Driver value.
#'
#' \code{order_driver} is used in \code{\link{f_waterfall_input}}.
#' No modification are done to the data in the case the driver if fDate.
#'
#' @param df dataframe, column are driver and DeltaValue.
#' @param Driver A string.
#' @param max_driver_count Integer, same as topN_driver.
#' @return a dataframe with the same column as the input.
order_driver <- function(df, Driver, max_driver_count) {
  # in f_waterfal_input

  if (Driver != "fDate") {
    df %>%
      dplyr::mutate(.,
                    signDelta = sign(DeltaValue),
                    signDelta = ifelse(signDelta == 0, 1, signDelta)) %>%
      dplyr::arrange(dplyr::desc(signDelta),
                     dplyr::desc(abs(DeltaValue))) %>%
      dplyr::group_by(signDelta) %>%
      dplyr::mutate(n = dplyr::row_number(),
                    driver = ifelse(n > as.numeric(max_driver_count),
                                    paste0("OTHER", ifelse(signDelta == 1,
                                                           "_positive",
                                                           "_negative")),
                                    driver),
                    n = ifelse(n > as.numeric(max_driver_count),
                               as.numeric(max_driver_count) + 1,
                               n)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(signDelta, driver, n) %>%
      dplyr::summarise(DeltaValue = sum(DeltaValue)) %>%
      dplyr::ungroup() %>%
      dplyr::arrange(dplyr::desc(signDelta),
                     n) %>%
      dplyr::select(-signDelta, -n) %>%
      return()
  } else return(df)
}

#' Make a name (string) for a time interval.
#'
#' \code{label_FYdate} is used in \code{\link{f_waterfall_input}}.
#'
#' @param x a time interval.
#' @return a string representing the time interval in input.
label_FYdate <- function(x) {
  x %>%
    { paste("from",
            lubridate::year(.[[1]]) %>% paste0("FY", .),
            lubridate::month(.[[1]]),
            "to",
            lubridate::year(.[[2]]) %>% paste0("FY", .),
            lubridate::month(.[[2]]),
            sep = "_")
    }
}


#' Prepare the data for the waterfal chart.
#'
#' \code{f_waterfall_input} use \code{\link{label_FYdate}} and
#' \code{\link{order_driver}}.
#'
#' @param data A dataframe.
#' @param Driver A string.
#' @param cal_period A date range.
#' @param topN_driver Integer, max number of positive drivers positive, and
#'   max number of negative drivers.
#' @return a dataframe with columns: driver, DeltaValue, balance, time, flow.
f_waterfall_input <- function(data, Driver, cal_period, topN_driver = NULL) {

  # f_waterfall_input(dat$RVP, "fDate", c(ymd("2017-04-01"), ymd("2017-07-31")))
  # data columns: fDate, possible drivers (except month), Revenue
  # input border of the reference period: input$dateRange[1], input$dateRange[2]
  # ( /!\ need to convert to fiscal_date) cal_period <- c(ymd("2017-04-01"), ymd("2017-07-31"))
  # filter years and months (definition of period1, input$day01, input$day99 ,
  # period2 is year+1)
  # create source (period1 and 2)
  # group_by year, driver
  # driver can be regionLevel4, regionLevel2, months, suppliesLevel3, suppliesLevel4
  # summarize

  topN_driver <- ifelse(is.null(topN_driver), 7, topN_driver)

  nbYears <-
    cal_period %>%
    { lubridate::interval(.[[1]], .[[2]]) %/% lubridate::years(1)  }

  periodRef <- cal_period %>% purrr::map(f_C2Fdate)
  periodRef_name <- periodRef %>% label_FYdate()

  period2 <-
    cal_period %>%
    `+`(lubridate::years(nbYears + 1)) %>%
    purrr::map(f_C2Fdate)
  period2_name <- period2 %>% label_FYdate()

  periodRef <- periodRef %>% { lubridate::interval(.[[1]], .[[2]]) }
  period2 <- period2 %>% { lubridate::interval(.[[1]], .[[2]]) }

  dt <-
    data %>%
    dplyr::mutate(period =
      dplyr::case_when(
        fDate %within% periodRef ~ periodRef_name,
        fDate %within% period2 ~ period2_name,
        TRUE ~ ""
      )) %>%
    dplyr::filter(period == periodRef_name | period == period2_name) %>%
    dplyr::rename(driver = !!dplyr::sym(Driver)) %>%
    { if (Driver == "fDate")  dplyr::mutate(., driver = lubridate::month(driver)) else . } %>%
    # mutate(driver=ifelse(Driver == "fDate", month(driver), driver)) %>%
    dplyr::group_by(period, driver) %>%
    dplyr::summarise(Rev = sum(Rev, na.rm=T)) %>%
    dplyr::ungroup() %>%
    tidyr::spread(key = period, value = Rev) %>%
    dplyr::mutate(DeltaValue = .[[period2_name]] - .[[periodRef_name]],
                  DeltaValue = ifelse(is.na(DeltaValue), 0, DeltaValue),
                  # if driver is numeric (Month for instance),
                  # don't work with simply is.numeric, hence add map
                  driver = ifelse(purrr::map(.$driver, ~is.numeric(.x)),
                                  format(driver, trim=FALSE), driver))



  dt1 <- dplyr::tibble(driver = c(periodRef_name),
                       DeltaValue = c(sum(dt[[periodRef_name]], na.rm = TRUE)))
  dt3 <- dplyr::tibble(driver = c(period2_name),
                       DeltaValue = c(sum(dt[[period2_name]], na.rm = TRUE)))
  dt2 <-
    dt %>%
    dplyr::select(driver, DeltaValue) %>%
    order_driver(.,
                 Driver = Driver,
                 max_driver_count = topN_driver)

  dt <-
    dplyr::bind_rows(dt1, dt2, dt3) %>%
    dplyr::mutate(driver = forcats::as_factor(driver),
                  DeltaValue = ifelse(dplyr::row_number() == dplyr::n(), -1, 1) * DeltaValue,
                  balance = cumsum(dplyr::lag(DeltaValue, default = 0)),
                  time = dplyr::row_number(),
                  flow = ifelse(dplyr::row_number() == dplyr::n() |
                                 dplyr::row_number() == 1, 2, sign(DeltaValue)),
                  flow = forcats::as_factor(as.character(flow)))

  # write.csv(dt, "dt_waterfall_in_waterfall_input.csv")
  return(dt)
}
