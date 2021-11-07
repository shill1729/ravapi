#' Download asset data from Alpha-Vantage
#'
#' @param symbols vector of stock ticker or coin symbols to download
#' @param period period: "intraday", "daily", "weekly", "monthly"
#' @param interval NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"
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
getAssets <- function(symbols, period = "daily", interval = NULL, datatype = "json", key = "premium")
{
  coins <- c("BTC", "ETH", "ETC", "LTC", "DOGE")
  num_assets <- length(symbols)
  if(num_assets == 1)
  {
    if(symbols %in% c(coins))
    {
      return(getCoin(symbols, period, interval, market = "USD", datatype, key))
    } else
    {
      return(getStock(symbols, period, interval, datatype, key))
    }
  } else if(num_assets > 1)
  {
    if(symbols[1] %in% coins)
    {
      return(getCoins(symbols, period, interval, key))
    } else
    {
      return(getStocks(symbols, period, interval, key))
    }
  } else{
    stop("Must past a non-zero list of symbols")
  }
}
