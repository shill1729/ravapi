
# ravapi

<!-- badges: start -->
<!-- badges: end -->

The package ravapi provides a library/wrapper to the Alpha Vantage API if the user has a free or premium API key (see below).

## Installation

You can install the released version of ravapi from [CRAN](https://CRAN.R-project.org) with:

``` r
devtools::install_github("shill1729/ravapi")
```

## Setting up your API key in .Renviron
Register with **[Alpha Vantage](https://www.alphavantage.co/support/#api-key)** to obtain a free API key or subscribe for a premium API key. Save these in your .Renviron file with
```r
usethis::edit_r_environ()
```
and simply set in the .Renviron file the following variables:
```r
free_api_key = "YOUR_KEY_CODE1"
premium_api_key = "YOUR_KEY_CODE2"
```
where the RHS is the actual key obtained from AV after registering for either the free or premium access. The second line is not required obviously if you only plan on registering a free API key.

By default a premium key is assumed in every function that calls the AV API. In the future we will add an option to set defaults.

