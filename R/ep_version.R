#' Model versioning
#' @export
ep_cache_models <- function(version = c("latest","v1.0.0"), force = FALSE, ask = interactive()){
  version <- rlang::arg_match0(version, c("latest","v1.0.0"))

  cache_dir <- rappdirs::user_cache_dir(appname = "ffexpectedpoints", appauthor = "ffverse")

  if(!dir.exists(cache_dir)) dir.create(cache_dir)

  if(file.exists(file.path(cache_dir,version))) return(invisible(NULL))

  if(!force){

    cli::cli_alert_info("Could not find ep model version {.val {version}} in cache directory: \n {.path {cache_dir}} \n")

    proceed <- 1L

    if(ask){
      proceed <- menu(title = glue::glue('Would you like to download model version: "{version}"? \n This file may be >100MB.'),
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
    download.file(url = url, destfile = file.path(cache_dir, paste0(version,".zip")))
    unzip(zipfile = file.path(cache_dir,paste0(version,".zip")),
          exdir = file.path(cache_dir,version),
          junkpaths = TRUE)
    unlink(file.path(cache_dir, paste0(version,".zip")))
  })

  if(inherits(download_model,"try-error")) {
    cli::cli_abort("Failed to download model from {.url {url}}")
  }

  cli::cli_alert_success("Successfully downloaded ep model version {.val {version}} to cache!")
}
