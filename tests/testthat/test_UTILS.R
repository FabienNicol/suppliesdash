context("utils-context")
library(tidyverse)
library(lubridate)
# source("./R/UTILS.R")
# source("./R/UTILS_common.R")

test_that("rollmean", {
  expect_equal(rollmean(x = 1:30, window = 6), c(rep(NA,5), 3:27 + 0.5))
})

test_that("dataframe to named vector, f_df2v", {
  x <- f_df2v(tibble::tibble(n = c("Albert", "Romain", "Pierre"), v = c(1, 2, 3)))
  expect_equal(x, c(Albert = 1, Romain = 2, Pierre = 3))
})

test_that("clean dataframe column names, f_clean_names", {
  x <- f_clean_names(tibble::tibble(`n-me1` = 1:3, `N*me2` = 4:6,
                                    `n[me3` = 7:9, `n me4` = 10:12))
  expect_equal(names(x), c("n_me1", "Nme2", "nme3", "nme4"))
})

test_that("format decimals, fmt_decimals", {
  expect_equal(fmt_decimals(1)(38.24), "38.24")
  expect_equal(fmt_decimals(10)(38.24), "38.2400000000")
})


test_that("calendar date to Fiscal date, f_C2Fdate", {
  expect_equal(f_C2Fdate(lubridate::ymd("2018-9-23")),
               lubridate::ymd("2018-06-01"))
  expect_equal(f_C2Fdate(lubridate::ymd("2018-1-23")),
               lubridate::ymd("2017-10-01"))
})

test_that("fiscal date to calendar date", {
  expect_equal(f_F2Cdate(lubridate::ymd("2018-9-23")),
               lubridate::ymd("2018-12-01"))
  expect_equal(f_F2Cdate(lubridate::ymd("2018-1-23")),
               lubridate::ymd("2018-04-01"))
})

test_that("wrap_text", {
  x1 <- paste0("ce texte est très long, vraiment très long, ",
               "va-t-il rentrer dans l'espace qui lui est dédié ?")
  x2 <- paste0("ce texte\nest très\nlong,\nvraiment\ntrès long,\nva-t-il\n",
               "rentrer\ndans\nl'espace\nqui lui est\ndédié ?")
  expect_equal(wrap_text(x = x1, width = 12), x2)
})

test_that("flatten a list, table2list", {
  x <-
    table2list(tibble::tibble(
      h0 = rep("genre", 4),
      h1 = c(rep("famille1", 2), rep("famille2", 2)),
      h2 = c("espece1", "espece2", "espece3", "espece4")))
  expect_equal(purrr::pluck(x, 2, 1, 1), c("espece1", "espece2"))
  #"genre"
  expect_equal(dplyr::pull(x, 1), "genre")
})



test_that("list repeated value in different columns, repetitions_between_col", {
  x <- tibble::tibble(c1 = LETTERS[1:5],
                      c2 = LETTERS[4:8],
                      c3 = c("A", "E", "U", "V", "W"))
  expect_equal(repetitions_between_col(x), c("D", "E", "A"))
})

test_that("add leading dots to datafram items, leadingDots", {
  x <-
    tibble::tibble(
      h0 = rep("genre", 4),
      h1 = c(rep("famille1", 2), rep("famille2", 2)),
      h2 = c("espece1", "espece2", "espece3", "espece4"))
  expect_equal(names(leadingDots(x)), c("V1", "V2", "V3"))
  expect_equal(leadingDots(x)[[1, 2]], ". famille1")
  expect_equal(leadingDots(x)[[3, 3]], ". . espece3")
})

test_that("set_names_vector", {
  x1 <- tibble::tibble(
    h0 = rep("genre", 4),
    h1 = c(rep("famille1", 2), rep("famille2", 2)),
    h2 = c("espece1", "espece2", "espece3", "espece4"))
  x2 <-
    c(genre = "genre", `. famille1` = "famille1", `. . espece1` = "espece1",
    `. . espece2` = "espece2", `. famille2` = "famille2",
    `. . espece3` = "espece3", `. . espece4` = "espece4")
  expect_equal(set_names_vect(x1), x2)
})

test_that("flatten_in_order", {
  x1 <- tibble::tibble(
    h0 = rep("genre", 4),
    h1 = c(rep("famille1", 2), rep("famille2", 2)),
    h2 = c("espece1", "espece2", "espece3", "espece4"))
  x2 <- c("genre", "famille1", "espece1", "espece2", "famille2", "espece3",
    "espece4")
  expect_equal(flatten_in_order(x1), x2)
})
