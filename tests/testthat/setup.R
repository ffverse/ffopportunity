suppressPackageStartupMessages({
  library(testthat)
})

gha_online <- !is.null(curl::nslookup("github.com", error = FALSE))

skip <- !gha_online

if(gha_online){
  tryCatch(
    expr = {
      ep_cache_models(version = "v1.0.0")
      pbp_2020 <- nflreadr::load_pbp(2020)
    },
    warning = function(e) skip <<- TRUE,
    error = function(e) skip <<- TRUE
  )
}

skippy <- function() NULL
if (skip) skippy <- function() testthat::skip(message = "Unable to connect to GHA")

