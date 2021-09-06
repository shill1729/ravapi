#' Backend function for Alpha-vantage endpoints
#'
#' @description {Returns the endpoint url for Alpha-Vantage's API.}
#' @return string of the url "https://www.alphavantage.co/query"
avEndpoint <- function()
{
  av_url <- "https://www.alphavantage.co/query"
  return(av_url)
}


#' Check Get/Post calls for success messages.
#'
#' @param request the returned object from a get or post call
#'
#' @description {Check post calls for success messages and print the error otherwise.}
#' @return either null or error message
#' @importFrom assertthat assert_that
checkRequest <- function(request)
{
  response <- httr::http_status(request)
  assertthat::assert_that(response$category == "Success", msg = response$message)
}

#' Check Get/Post calls for success messages.
#'
#' @param response the returned object from a json-parsed get or post call
#'
#' @description {Check post calls for AV messages and print the error otherwise.}
#' @return either null or error message
checkResponse <- function(response)
{
  if(names(response)[1] == "Error Message")
  {
    stop(response)
  }
}

