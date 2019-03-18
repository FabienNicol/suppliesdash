# --------------
# --- GLOBAL ---
# --------------
options("scipen" = 50, "digits" = 4)
# 999 prevent scientific notation, 0 is the default value


#' Compute the moving average of window x.
#'
#' \code{rollmean} take a vector as input returns the moving average vector.
#' The non computable data (the fist window-1 items) are replaced by NAs.
#'
#' @param x a numeric vector.
#' @param window integer, representing the width of the window.
#' @return a numeric vector
rollmean <- function(x, window) {
  t <- tibbletime::rollify(.f = ~mean(.x, na.rm = TRUE),
                           window = window,
                           na_value = NA)
  return(t(x))
}



#' Transform a 2-columns df into a vector with named items.
#'
#' \code{f_df2v} transform a 2-columns dataframe into a vector.
#' Used for instance with date convertions (calendat to/from fiscal).
#'
#' @param df a 2-columns dataframe. Column1 is the name, column2 the value.
#' @return a vecto of named items.
f_df2v <- function(df) {
  # use isTRUE in R3.5.0
  if (isTRUE(ncol(df) != 2)) print("f_df2v. Error, the input must be a 2 columns dataframe")
  v <- NULL
  v <- unlist(df[,2])
  names(v) <- unlist(df[,1])
  return(v)
}

#' Clean dataframe column names.
#'
#' \code{f_clean_names} removes characters ([, ], space, *, newline, U+0096)
#' and replaces - by _.
#'
#' @param df a dataframe.
#' @return a dataframe with clean column names
f_clean_names <- function(df) {

  names(df) <-
    names(df) %>%
    stringr::str_replace_all(pattern = "\\[|\\]| |\\*|<U\\+0096>|\\n", replacement = "") %>%
    stringr::str_replace_all(pattern = "-", replacement = "_")
  return(df)
}


#' Format continuous axis in ggplot2.
#'
#' \code{fmt_decimals} is rounding and forcing non-scientific numeric format.
#'
#' @param decimals number of decimals to keep
#' @return a formated numeric
fmt_decimals <- function(decimals=0){

  function(x) format(x, nsmall = decimals, scientific = FALSE)
}

# --- convert ymd date CY/FY ---

date_convert <-
  tibble::tibble(
    cm = 1:12,                                      # calendar month
    cm2 = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
            "Oct", "Nov", "Dec"),
    cm3 = c("January", "February", "March", "April", "May", "June", "July",
            "August", "September", "October", "November", "December"),
    fm = c(10:12, 1:9),                             # fiscal month
    cy2fy = c(rep(-1,3), rep(0,9)),                 # fiscal year
    fy2cy = c(rep(1,3), rep(0,9)),
    fq = c(rep(4,3), rep(1,3), rep(2,3), rep(3,3)), # fiscal quarter
    cut1 = rep("fc", 12),
    cut2 = rep("fc", 12),
    FC1 = c(rep("fc", 3), rep("act", 2), rep("fc", 7)),
    FC2 = c(rep("fc", 3), rep("act", 5), rep("fc", 4)),
    FC3 = c(rep("fc", 3), rep("act", 8), rep("fc", 1))
)

#' Convert calendar date to fiscal date.
#'
#' \code{f_C2Fdate} uses date convert dataframe (in global environment) to
#' convert the calendar date input in fiscal date output.
#'
#' @param cDate a calendar date.
#' @param table A tibble representing months.
#' @return calendar date
f_C2Fdate <- function(cDate, table = date_convert) {
  #condition exists in order to avoid to recompute the vector
  # if (!exists(Yconv))
    Yconv <- f_df2v(table %>% dplyr::select(cm, cy2fy))
  # if (!exists(Mconv))
    Mconv <- f_df2v(table %>% dplyr::select(cm, fm))

  fYear <- Yconv[as.character(lubridate::month(cDate))] + lubridate::year(cDate)
  fMonth <- Mconv[as.character(lubridate::month(cDate))]
  fDate <- lubridate::ymd(paste(fYear, fMonth, "01", sep = "/"))
  return(fDate)
}



#' Convert fiscal date to calendar date.
#'
#' \code{f_F2Cdate} uses date convert dataframe (in global environment) to
#' convert the fiscal date input in calendar date output.
#'
#' @param fDate a fiscal date.
#' @param table A tibble representing months.
#' @return calendar date
f_F2Cdate <- function(fDate, table = date_convert) {
  # if (!exists(Yconv2))
    Yconv2 <- f_df2v(table %>% dplyr::select(fm, fy2cy))
  # if (!exists(Mconv2))
    Mconv2 <- f_df2v(table %>% dplyr::select(fm, cm))
  # fDate in ymd format (lubridate)
  cYear <- Yconv2[as.character(lubridate::month(fDate))] + lubridate::year(fDate)
  cMonth <- Mconv2[as.character(lubridate::month(fDate))]
  cDate <- lubridate::ymd(paste(cYear, cMonth, "01", sep = "/"))
  return(cDate)
}


#' Wrap text of a vector into a string of width "width".
#'
#' \code{wrap_text}
#'
#' @param x a vector of strings.
#' @param width the maximum width of the string result
#' @return a string of maximum width "width".
wrap_text <- function(x, width = 19) {

  wrapped <- lapply(x, function(char) strwrap(char, width))
  wrapped <- lapply(wrapped, paste, collapse = '\n')
  if (is.atomic(x))
    wrapped <- unlist(wrapped)
  return(wrapped)
}


#' Make a named list from nested dataframe.
#'
#' \code{table2list} creates a nested list from a nested dataframe ndf.
#' the nested data frame has at least 2 columns describing a tree (named
#' ".*Leveli" colums), no other columns. From coarser to finer, from left
#' to right
#'
#' Initially planned for use in parameter CHOICES in shiny::selectInput
#' but SHINY::inputSelect does not work as expected (the titles aren't clickable).
#'
#' @param ndf a nested dataframe (hierarchy).
#' @return a nested list.
table2list <- function(ndf) {

  col <- names(ndf)
  ndf <- ndf %>% dplyr::mutate(v1 = !! rlang::sym(col[length(col)]))

  for (i in 1:(length(col) - 1)) {
    var4 <- rlang::sym(col[length(col) - i ])
    var6 <- rlang::syms(as.list(col[1:(length(col) - i)]))

    ndf <-
      ndf %>%
      dplyr::group_by(!!! var6) %>%
      # summarize(v1 = paste(v1, collapse=",")) %>%
      dplyr::summarise(v1 = list(v1)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(v1 = as.list(v1)) %>%
      dplyr::mutate(nw = setNames(v1, 1))
    # %>% mutate(v1=paste0("(", v1,")", !! var4))
    # names(ndf)[["v1"]] <- ndf[[length(col) - i ]]
    # set_names(ndf[["v1"]], ndf[[length(col) - i ]])
  }

  return(ndf)
}


#' Spread a tidyr nested dataframe to a named list.
#'
#' \code{spread_to_list} is used in \code{\link{multinest}}.
#'
#' @param t a tidyr nested dataframe.
#' @return a list
spread_to_list <- function(t) {
  setNames(as.list(t[[2]]), t[[1]])
}

# Now I want to make multinest() recursive so
# that it factors out the inner tables
# in the same way. I'll assume all the name
# columns are at the front of the table. I
# do have to give an extra argument saying
# how many there are.

#' function found on the web.
#'
#' \code{multinest} recursive function. not used.
#'
#' @param t what is it already
#' @param no_of_name_cols an integer.
#' return a list.
multinest <- function(t, no_of_name_cols) {
  if (t[[1,1]] == '')
    return(t[[1, ncol(t)]])
  else {
    colname_1 <- as.name(names(t)[1]) # rlang::sym

    if (no_of_name_cols > 1)
      listed <-
        t %>%
        dplyr::group_by(!!colname_1) %>%
        tidyr::nest %>%
        spread_to_list %>%
        purrr::map(multinest, no_of_name_cols - 1)
    else if (no_of_name_cols == 1)
      listed <-
        t %>%
        dplyr::group_by(!!colname_1) %>%
        tidyr::nest %>%
        spread_to_list %>%
        purrr::map(unlist, use.names = FALSE)

    return(listed)
  }
}

# data_ISenriched$RVP%>% dplyr::filter(!is.na(SuppliesLevel0)) %>%
#      dplyr::select(dplyr::matches("SuppliesLevel[0-4]")) %>%
#      dplyr::distinct() %>% dplyr::select(order(colnames(.))) %>%
#      dplyr::arrange_all() %>% multinest(dim(.)[2]-2)
#
# multinest_4(t, dim(t)[2]-1)


#' Check in hierarchy nested tables there is no rep of leveli within levelj.
#'
#' \code{repetions_between_col} not used.
#'
#' @param df a dataframe.
#' @return the vector of the repeated values.
repetitions_between_col <- function(df) {
  identical <- NULL
  n <- dim(df)[2]
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      identical <- c(identical, df[[i]][df[[i]]  %in% df[[j]]]) %>% unique()
    }
  }
  return(identical)
}


#' Add leading dots to the items of an ordered hierarchical dataframe.
#'
#' \code{leadingDots} is used in \code{\link{set_names_vect}}.
#' Uses function \code{\link{dotsVector}}.
#'
#' @param df a dataframe representing a hierarchy.
#' @return a dataframe.
leadingDots <- function(df) {
  dotsVector <-
    df %>%
    {purrr::map2(". ", seq(0, dim(.)[2] - 1), function(x, y) paste0(rep(x, y), collapse = ""))}
  df %>% purrr::map2_dfc(dotsVector, ., paste0)
}



#' Convert a dataframe representing a hierarchy into a 1D-list.
#'
#' \code{set_names_vect} uses \code{\link{leadingDots}} and
#' \code{\link{flatten_in_order}}.
#' \code{set_names_vect} is used in UI to defined the list of top_suppliesvalue
#' dropdown box.
#'
#' @param df a dataframe representing a hierarchy.
#' @return a named list to be used in a shiny::selectInput.
set_names_vect <- function(df) {
  values <- df %>% flatten_in_order()
  labels <- df %>% leadingDots(.) %>% flatten_in_order()
  # unlist(use.names = F) %>% unique() replaced by flatten_in_order
  # return(paste(values, labels, sep="      "))
  return(purrr::set_names(values, labels))
}

# data_ISenriched$RVP %>% dplyr::filter(!is.na(SuppliesLevel0)) %>%
#   dplyr::select(dplyr::matches("SuppliesLevel[0-4]")) %>%
#   dplyr::distinct() %>% dplyr::select(order(colnames(.))) %>%
#   dplyr::arrange_all() %>% set_names_vect() %>% View


# keep order of a nested tree in order to benefit from the dots
# flatten_in_order(tibble(vect0 = c("A","A","A","A", "B"), vect1=c("a", "a", "a", "b", "c"), vect2 = c("u", "v", "44", "34", "w")))


#' Convert a dataframe representing a hierarchy into a vector.
#'
#' \code{flatten_in_order} is recursive.
#' \code{flatten_in_order} is used in \code{\link{set_names_vect}}.
#'
#' @param df a dataframe representing a hierarchy.
#' @return a vector.
flatten_in_order <- function(df) {

  col <- names(df)
  if (length(col) > 2) {

    var5 <- rlang::sym(col[length(col)])
    var4 <- rlang::sym(col[length(col) - 1])
    var1 <- rlang::syms(col[1:(length(col) - 1)])

    df %>%
      dplyr::group_by(!!!var1) %>%
      dplyr::summarise(!!var5 := paste(!!var5, collapse = "|")) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!var4 := paste(!!var4, !!var5,  sep = "|")) %>%
      dplyr::select(-!!var5) %>%
      flatten_in_order()

  } else if (length(col) == 2) {
    df %>%
      dplyr::group_by_at(1) %>%
      # change from dplyr 0.8, summarise_at() excludes the grouping variables
      # dplyr::summarise_at(dplyr::vars(2), paste, collapse = "|") %>%
      dplyr::summarise_at(dplyr::vars(colnames(.)[2]), paste, collapse = "|") %>%
      dplyr::ungroup() %>%
      dplyr::mutate(new = paste(.[[1]], .[[2]],  sep = "|")) %>%
      dplyr::select(new) %>%
      dplyr::summarise(new = paste(new, collapse = "|")) %>%
      flatten_in_order()

  } else {
    return(df %>%
             `[[`(1,1) %>%
             stringr::str_split(., "\\|") %>%
             unlist()
    )
  }
}
