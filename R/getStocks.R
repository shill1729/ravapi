#' Load multiple stocks into a single time-series/matrix
#'
#' @param symbols list of stock symbols to look up
#' @param period resolution between prices: "daily", "weekly", "monthly"
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Returns common sample of historical prices of given portfolio.}
#' @return xts
#' @importFrom stats complete.cases
#' @export getStocks
getStocks <- function(symbols, period = "daily", key = "premium")
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
    if(period != "intraday")
    {
      stocks[[i]] <- getStock(symbol = tickers[i], period = period, key = key)$adj_close
    } else
    {
      stocks[[i]] <- getStock(symbol = tickers[i], period = period, key = key)$close
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
