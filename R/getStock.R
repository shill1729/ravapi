#' Download historical stock prices as a time-series from AV
#'
#' @param symbol stock ticker to look up
#' @param period period: "intraday", "daily", "weekly", "monthly"
#' @param datatype "json" or "csv"
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Wrapper to AV API. Requires free or premium key.}
#' @details {Default 1 min for intraday, full output size, and adjusted.}
#'
#' @return parsed json or csv file
#' @export getStock
getStock <- function(symbol, period = "daily", datatype = "json", key = "premium")
{
  # Input-error handling
  if(key != "premium" && key != "free")
  {
    stop("argument 'key' must be either 'premium' or 'free'")
  }
  apikey <- paste(key, "_api_key", sep = "")

  # For daily, weekly, monthly, we append need to pass x_adjusted
  if(period == "daily" || period == "weekly" || period == "monthly")
  {
    per <- paste(period, "_adjusted", sep = "")
  } else if(period == "intraday")
  {
    per <- period
  }
  # Payload base
  payload <- list("function" = paste("TIME_SERIES_", toupper(per), sep = ""),
                  symbol = symbol,
                  outputsize = "full",
                  datatype = datatype,
                  apikey = Sys.getenv(apikey)
  )
  # For intraday add interval resolution, for else add adjusted boolean
  if(period == "intraday")
  {
    payload$interval = "1min"

  } else if(period %in% c("daily", "weekly", "monthly"))
  {
    payload$adjusted = TRUE
  }
  # GET request
  request <- httr::GET(url = avEndpoint(), query = payload)
  checkRequest(request)
  # Depending on requested data type we have different parsing and formatting
  if(datatype == "json")
  {
    # Parse the json response and then format
    response <- jsonlite::fromJSON(txt = httr::content(x = request, type = "text", encoding = "UTF-8"))
    checkResponse(response)
    # Rbind open high low close volume and unlist columns
    dat <- do.call(what = rbind, response[[2]])
    dat <- apply(dat, 2, unlist)
    # Extract time points, convert columns to numeric, matrix to data.frame
    time_points <- rownames(dat)
    dat <- apply(dat, 2, as.numeric)
    dat <- data.frame(time = time_points, dat)
    # Rename, add time row labels: column names change depending on period
    if(period == "intraday")
    {
      names(dat) <- c("time", "open", "high", "low", "close", "volume")
    } else if(period == "daily")
    {
      names(dat) <- c("time", "open", "high", "low", "close", "adj_close", "volume", "div_amt", "split_coef")
    } else if(period == "weekly" || period == "monthly")
    {
      names(dat) <- c("time", "open", "high", "low", "close", "adj_close", "volume", "div_amt")
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
