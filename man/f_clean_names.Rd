% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/UTILS_common.R
\name{f_clean_names}
\alias{f_clean_names}
\title{Clean dataframe column names.}
\usage{
f_clean_names(df)
}
\arguments{
\item{df}{a dataframe.}
}
\value{
a dataframe with clean column names
}
\description{
\code{f_clean_names} removes characters ([, ], space, *, newline, U+0096)
and replaces - by _.
}
