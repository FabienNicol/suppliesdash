% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_RVP.R
\name{f_rkSupplies}
\alias{f_rkSupplies}
\title{Rank supplies according to revenue (or volume) within the reference year.}
\usage{
f_rkSupplies(data, yearRef = 2017, rankBy = "Rev")
}
\arguments{
\item{data}{a dataframe with columns: fYear, SuppliesSplit, SuppliesPlot,
Rev, Vol.}

\item{yearRef}{an Integer representing the reference year.}

\item{rankBy}{a string representing the measure on which the Region are
ranked, "Rev" or "Vol".}
}
\value{
a dataframe with an ordered factor for column SuppliesSplit
  and fYear.
}
\description{
\code{f_rkSupplies} is used in \code{\link{data4plot}}.
}
