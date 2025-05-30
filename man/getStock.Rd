% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStock.R
\name{getStock}
\alias{getStock}
\title{Download historical stock prices as a time-series from AV}
\usage{
getStock(
  symbol,
  period = "daily",
  interval = NULL,
  datatype = "json",
  key = "premium"
)
}
\arguments{
\item{symbol}{stock ticker to look up}

\item{period}{period: "intraday", "daily", "weekly", "monthly"}

\item{interval}{NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"}

\item{datatype}{"json" or "csv"}

\item{key}{"premium" or "free"; the type of API key obtained from AV}
}
\value{
parsed json or csv file
}
\description{
{Wrapper to AV API. Requires free or premium key.}
}
\details{
{The argument 'interval' is only to be specified for period = 'intraday'.}
}
