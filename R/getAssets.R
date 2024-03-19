#' Download asset data from Alpha-Vantage
#'
#' @param symbols vector of stock ticker or coin symbols to download
#' @param period period: "intraday", "daily", "weekly", "monthly"
#' @param interval NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"
#' @param market only needed for crypto: use "USD", "CNY", "EUR", etc
#' @param crypto boolean for downloading crypto
#' @param adjusted boolean for adjusted prices
#' @param datatype "json" or "csv"
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {A wrapper to Alpha Vantage's API. This function can download either stocks or crypto-currencies. Adjusted close prices
#' are returned when pulling multiple stocks in a single xts object with columns for each stock. Otherwise, for a single asset
#' the entire OHLCV object is returned.}
#' @details {This is a wrapper to non-exported functions that separately pull either stock or coin data.}
#'
#' @return xts object
#' @export getAssets
getAssets <- function(symbols, period = "daily", interval = NULL, market="USD", crypto=FALSE, adjusted=TRUE, datatype = "json", key = "premium")
{
  coins <- c("BTC", "ETH", "ETC", "LTC", "DOGE")
  num_assets <- length(symbols)
  if(num_assets == 1)
  {
    if(crypto)
    {
      return(getCoin(symbols, period, interval, market = market, datatype, key))
    } else
    {
      return(getStock(symbols, period, interval, datatype, key))
    }
  } else if(num_assets > 1)
  {
    if(crypto)
    {
      return(getCoins(symbols, period, interval, market, key))
    } else
    {
      return(getStocks(symbols, period, interval, adjusted, key))
    }
  } else{
    stop("Must past a non-zero list of symbols")
  }
}


#' Get the correct timescale factor for periodic data
#'
#' @param period period: "intraday", "daily", "weekly", "monthly"
#' @param interval NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"
#' @param crypto boolean for cryptomarkets (24hrs)
#'
#' @description {Return the correct timescale for daily,weekly,monthly, etc.}
#' @return numeric
#' @export timescale
timescale <- function(period = "daily", interval = NULL, crypto = FALSE)
{
  intraday_length <- 7.5
  num_weekdays <- 5
  num_monthdays<- 21
  year_length <- 252
  if(crypto)
  {
    intraday_length <- 24
    num_weekdays <- 7
    year_length <- 365
  }

  else if(!crypto)
  {
    intraday_length <- 7.5
    num_weekdays <- 5
    num_monthdays <- 30
    year_length <- 252
  }

  # For intraday, w=e convert to daily figures
  if(period == "intraday")
  {
    if( interval == "1min")
    {
      time_scale = 1/(intraday_length*60)
    } else if(interval == "5min")
    {
      time_scale = 1/(intraday_length*12)
    } else if(interval == "15min")
    {
      time_scale = 1/(intraday_length*4)
    } else if(interval == "30min")
    {
      time_scale = 1/(intraday_length*2)
    } else if(interval == "60min")
    {
      time_scale = 1/intraday_length
    }


  } else if(period == "daily")
  {  # For daily/weekly, we convert to annual figures
    time_scale = 1/year_length # Crypto trades year-round.
  } else if(period == "weekly")
  {
    time_scale = 1/(year_length/num_weekdays)
  } else if(period == "monthly")
  {
    time_scale <- 1/(year_length/num_monthdays)
  }
  return(time_scale)
}
