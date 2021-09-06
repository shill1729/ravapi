#' Load multiple cryptocurrencies
#'
#' @param symbols list of cryptocurrency symbols to look up
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Returns common sample of historical prices of given portfolio of cryptocurrencies.}
#' @return xts
#' @importFrom stats complete.cases
#' @export getCoins
getCoins <- function(symbols, key = "premium")
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
    coins[[i]] <- getCoin(symbol = tickers[i], key = key)$close
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
