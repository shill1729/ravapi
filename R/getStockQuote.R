#' Global instrument quote via Alpha-Vantage
#'
#' @param symbol symbol to look up
#' @param datatype "json" or "csv" return type from AV
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Look up global quote via AV.}
#'
#' @return data.frame containing columns
#' \code{symbol}, \code{open}, \code{high}, \code{low}, \code{price}, \code{volume},
#' \code{latest_trading_day}, \code{previous_close}, \code{change}, \code{change_percent}.
#' @export getStockQuote
getStockQuote <- function(symbol, datatype = "json", key = "premium")
{
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  apikey <- paste(key, "_api_key", sep = "")
  payload <- list("function" = "GLOBAL_QUOTE",
                  symbol = symbol,
                  apikey = Sys.getenv(apikey),
                  datatype = datatype
  )
  request <- httr::GET(url = avEndpoint(), query = payload)
  print(unlist(httr::http_status(request)))
  if(datatype == "json")
  {
    dat <- jsonlite::fromJSON(txt = httr::content(x = request, type = "text", encoding = "UTF-8"))
    # Clean up the response
    gquote <- data.frame(dat$`Global Quote`)
    names(gquote) <- substring(names(dat$`Global Quote`), 5)
    names(gquote) <- gsub(" ", "_", names(gquote))
    # Convert change_percent separately to divide by 100
    gquote$change_percent <- as.numeric(gsub("%", "", gquote$change_percent))/100
    columns <- c("open", "high", "low", "price", "volume", "previous_close", "change")
    # Convert the rest of the revelant columns to numeric
    gquote[, columns] <- lapply(columns, function(x) as.numeric(gquote[[x]]))
    return(gquote)
  } else if(datatype == "csv")
  {
    dat <- readr::read_csv(httr::content(x = request, type = "text", encoding = "UTF-8"))
    return(dat)
  }

}
