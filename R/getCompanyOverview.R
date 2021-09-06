#' Get company overview
#'
#' @param symbol stock ticker to look up
#' @param key "premium" or "free"; the type of API key obtained from AV
#'
#' @description {Return company financial overview plus basic metrics.}
#'
#' @return list
#' @export getCompanyOverview
getCompanyOverview <- function(symbol, key = "premium")
{
  if(key != "premium" && key != "free")
  {
    stop("The argument 'key' must be either 'premium' or 'free'")
  }
  apikey <- paste(key, "_api_key", sep = "")
  payload <- list("function" = "OVERVIEW",
                  symbol = symbol,
                  apikey = Sys.getenv(apikey)
  )

  z <- httr::GET(url = avEndpoint(), query = payload)
  print(unlist(httr::http_status(z)))
  dat <- jsonlite::fromJSON(txt = httr::content(x = z, type = "text", encoding = "UTF-8"))
  return(dat)
}
