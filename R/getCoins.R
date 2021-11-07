#' Load multiple cryptocurrencies
#'
#' @param symbols list of cryptocurrency symbols to look up
#' @param period "daily", "weekly", "monthly", "yearly", or "intraday"
#' @param interval NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Returns common sample of historical prices of given portfolio of cryptocurrencies.}
#' @return xts
#' @importFrom stats complete.cases
getCoins <- function(symbols, period = "daily", interval = NULL, key = "premium")
{
  if(key == "premium")
  {
    callLimit <- 30
  } else if(key == "free")
  {
    callLimit <- 5
  }
  tickers <- symbols
  coins <- list()
  for(i in 1:length(tickers))
  {
    print(tickers[i])
    coins[[i]] <- getCoin(symbol = tickers[i], period = period, interval = interval, key = key)$close
    if(i%%callLimit==0)
    {
      Sys.sleep(60)
    }
  }
  coins <- do.call(cbind, coins)
  coins <- coins[complete.cases(coins), ]
  colnames(coins) <- tickers
  return(coins)
}
