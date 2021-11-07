#' Download daily time series of cryptocurrency from Alpha-Vantage
#'
#' @param symbol cryptocurrency symbol, i.e. "BTC" (string)
#' @param period "daily", "weekly", "monthly", "yearly", or "intraday"
#' @param interval NULL for anything but intraday otherwise "1min", "5min", "15min", "30min", "60min"
#' @param market physical currency symbol for quoting i.e. "USD", "CNY", etc.
#' @param datatype "json" or "csv" for return type to be parsed
#' @param key string for the API key to use Alpha Vantage
#'
#' @description {Download cryptocurrency daily time series from Alpha vantage using
#' either a free or premium API key. Includes open high low close, volume and market cap.}
#' @return xts object
getCoin <- function(symbol = "BTC", period = "daily", interval = NULL, market = "USD", datatype = "json", key = "premium")
{
  # TODO add symbol check in digital_currency_list.csv from AV
  # TODO add market check in physical_currency_list.csv from AV
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  if(datatype != "json" && datatype != "csv")
  {
    stop("argument 'datatype' must be either 'json' or 'csv'")
  }
  end_point <- ""
  if(period != "intraday")
  {
    end_point <- "DIGITAL_CURRENCY_"
  } else{
    end_point <- "CRYPTO_"
  }
  apikey <- paste(key, "_api_key", sep = "")
  payload <- list("function" = paste(end_point, toupper(period), sep = ""),
                  symbol = symbol,
                  market = market,
                  datatype = datatype,
                  apikey = Sys.getenv(apikey)
  )
  if(period == "intraday")
  {
    if(is.null(interval))
    {
      stop("interval must be non-null for intraday")
    }
    if(!interval %in% c("1min", "5min" , "15min", "30min", "60min"))
    {
      stop("interval must be one of '1min', '5min', '15min', '30min', or '60min'")
    }
    payload$interval <- interval
    payload$outputsize <- "full"
  }
  request <- httr::GET(url = avEndpoint(), query = payload)
  checkRequest(request)
  if(datatype == "json")
  {
    # Parse the json response and then format
    response <- httr::content(x = request, type = "application/json")
    checkResponse(response)
    # Rbind open high low close volume and unlist columns
    dat <- do.call(what = rbind, response[[2]])
    dat <- apply(dat, 2, unlist)

    # Extract time points, convert columns to numeric, matrix to data.frame
    time_points <- rownames(dat)
    dat <- apply(dat, 2, as.numeric)
    dat <- data.frame(time = time_points, dat)
    if(period != "intraday")
    {
      dat <- dat[, c(1, (1:5)*2, 11)]
      colnames(dat) <- c("time", "open", "high", "low", "close", "volume", "market_cap")
    } else{
      colnames(dat) <- c("time", "open", "high", "low", "close", "volume")
    }


    # Finally convert to xts
    dat <- xts::xts(dat[, -1], order.by = as.POSIXct(x = dat[, 1]))
    return(dat)
  } else if(datatype == "csv")
  {
    # read_csv will do the heavy lifting for us in terms of formatting.
    dat <- readr::read_csv(httr::content(x = request, type = "text", encoding = "UTF-8"))
    return(dat)
  }

}
