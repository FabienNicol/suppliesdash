% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/input_RVP.R
\name{f_summarise4mosaic}
\alias{f_summarise4mosaic}
\title{Prepare data for mosaic plots.}
\usage{
f_summarise4mosaic(data, mosaic_measure, showFY)
}
\arguments{
\item{data}{a dataframe with columns source, RegionSplit, RegionReport,
SuppliesSplit, SuppliesPlot, fYear, mosaic_measure.}

\item{mosaic_measure}{a string, such as "Rev" or "Liters". NULL if the
plot isn't a mosaic.}

\item{showFY}{a string vector, such as "All" or c("2017", "2018")}
}
\value{
a dataframe with columns source, RegionSplit, RegionReport,
  SuppliesSplit, SuppliesPlot, fYear, measure.
}
\description{
\code{f_summarise4mosaic} filters the fiscal years, identifies the measure
column.
Used in \code{\link{data4plot}}.
}
