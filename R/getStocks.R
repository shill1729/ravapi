#' Load multiple stocks into a single time-series/matrix
#'
#' @param symbols list of stock symbols to look up
#' @param period resolution between prices: "daily", "weekly", "monthly"
#' @param interval NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"
#' @param adjusted boolean for adjusted prices
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Returns common sample of historical prices of given portfolio.}
#' @return xts
#' @importFrom stats complete.cases
getStocks <- function(symbols, period = "daily", interval = NULL, adjusted=TRUE, key = "premium")
{
  if(key == "premium")
  {
    callLimit <- 30
  } else if(key == "free")
  {
    callLimit <- 5
  }
  tickers <- symbols
  stocks <- list()
  for(i in 1:length(tickers))
  {
    print(tickers[i])
    if(period != "intraday" && adjusted)
    {
      stocks[[i]] <- getStock(symbol = tickers[i], period = period, interval = interval, key = key)$adj_close
    } else if(period == "intraday" || !adjusted)
    {
      stocks[[i]] <- getStock(symbol = tickers[i], period = period, interval = interval, key = key)$close
    }

    if(i%%callLimit==0)
    {
      Sys.sleep(60)
    }
  }
  stocks <- do.call(cbind, stocks)
  stocks <- stocks[complete.cases(stocks), ]
  colnames(stocks) <- tickers
  return(stocks)
}
