% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getAssets.R
\name{timescale}
\alias{timescale}
\title{Get the correct timescale factor for periodic data}
\usage{
timescale(period = "daily", interval = NULL, crypto = FALSE)
}
\arguments{
\item{period}{period: "intraday", "daily", "weekly", "monthly"}

\item{interval}{NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"}

\item{crypto}{boolean for cryptomarkets (24hrs)}
}
\value{
numeric
}
\description{
{Return the correct timescale for daily,weekly,monthly, etc.}
}
