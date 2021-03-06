% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_fr.R
\name{f_fr_filter}
\alias{f_fr_filter}
\title{Filter, and summarise, the dataframe containing the fit rate information.}
\usage{
f_fr_filter(FR, regionValue, regionValueLevel, regionSplit, suppliesValue,
  suppliesValueLevel)
}
\arguments{
\item{FR}{A dataframe, Fit Rate resulting from MIF and IS.}

\item{regionValue}{A string.}

\item{regionValueLevel}{A string.}

\item{regionSplit}{A string.}

\item{suppliesValue}{A string.}

\item{suppliesValueLevel}{A string.}

\item{max_fr}{A double.}
}
\value{
a dataframe with columns: region, regionSplit, supplies, fDate,
  KEY, VALUE. Key lists Liters_sm, fr, MIF, cumqty.
}
\description{
\code{f_fr_filter} used in shiny::eventReactive subsetData_FR(), which is
the input to \code{\link{plot_fr}}.
}
