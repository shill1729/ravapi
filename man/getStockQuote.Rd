% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getStockQuote.R
\name{getStockQuote}
\alias{getStockQuote}
\title{Global instrument quote via Alpha-Vantage}
\usage{
getStockQuote(symbol, datatype = "json", key = "premium")
}
\arguments{
\item{symbol}{symbol to look up}

\item{datatype}{"json" or "csv" return type from AV}

\item{key}{"premium" or "free"; the type of API key obtained from AV}
}
\value{
data.frame containing columns
\code{symbol}, \code{open}, \code{high}, \code{low}, \code{price}, \code{volume},
\code{latest_trading_day}, \code{previous_close}, \code{change}, \code{change_percent}.
}
\description{
{Look up global quote via AV.}
}
