#' Model versioning
#'
#' This function checks the cache for a previously downloaded model and then (optionally) tries to download the model from GitHub release.
#'
#' @param version one of "latest" or "v1.0.0" - currently these refer to the same thing
#' @param force TRUE or FALSE - forces download regardless of currently existing
#' @param ask TRUE or FALSE - ask before downloading - `force` will skip this.
#'
#' @return a status message after attempting to download the model.
#'
#' @export
ep_cache_models <- function(version = c("latest","v1.0.0"), force = FALSE, ask = interactive()){
  version <- rlang::arg_match0(version, c("latest","v1.0.0"))

  cache_dir <- rappdirs::user_cache_dir(appname = "ffexpectedpoints", appauthor = "ffverse")

  if(!dir.exists(cache_dir)) dir.create(cache_dir)

  if(!force){
    if(file.exists(file.path(cache_dir,version))) return(invisible(NULL))

    cli::cli_alert_info("Could not find ep model version {.val {version}} in cache directory: \n {.path {cache_dir}} \n")

    proceed <- 1L

    if(ask){
      proceed <- utils::menu(title = glue::glue('Would you like to download model version: "{version}"? \n This file may be >100MB.'),
                      c("Yes","No"))
    }

    if(proceed != 1L) {
      cli::cli_alert_danger("Did not download model.")
      return(invisible(NULL))
    }

    if(!ask){
      cli::cli_alert_info("Attempting download")
    }
  }

  url <- glue::glue("https://github.com/ffverse/ffexpectedpoints/releases/download/{version}-model/{version}.zip")

  download_model <- try({
    utils::download.file(url = url, destfile = file.path(cache_dir, paste0(version,".zip")))
    utils::unzip(zipfile = file.path(cache_dir,paste0(version,".zip")),
          exdir = file.path(cache_dir,version),
          junkpaths = TRUE)
    unlink(file.path(cache_dir, paste0(version,".zip")))
  })

  if(inherits(download_model,"try-error")) {
    cli::cli_abort("Failed to download model from {.url {url}}")
  }

  cli::cli_alert_success("Successfully downloaded ep model version {.val {version}} to cache!")
}
