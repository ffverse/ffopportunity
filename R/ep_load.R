#' Load Expected Points data
#'
#' This function downloads precomputed expected points data from the ffopportunity automated releases.
#'
#' @param season A numeric vector of four digit years associated with given NFL seasons - defaults to latest season.
#' @param type Data type - one of `"weekly"`, `"pbp_pass"`, or `"pbp_rush"`
#' @param version EP model version: one of "latest" (default) or "v1.0.0" - these are currently identical.
#'
#' @return a dataframe identical to what would be returned by `ffopportunity::ep_build()` for a given season.
#'
#' @examples
#' \donttest{
#' try({
#'   ep_load() %>% head(10)
#'   ep_load(2020:2021) %>% head(10)
#'   ep_load(2021, type = "pbp_pass") %>% head(10)
#'   ep_load(2006, type = "pbp_rush", version = "v1.0.0") %>% head(10)
#' })
#' }
#'
#' @family main
#'
#' @export
ep_load <- function(season = nflreadr:::most_recent_season(),
                    type = c("weekly","pbp_pass","pbp_rush"),
                    version = c("latest","v1.0.0")){

  version <- rlang::arg_match0(version, c("latest","v1.0.0"))
  type <- rlang::arg_match0(type, c("weekly","pbp_pass","pbp_rush"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0(
    "https://github.com/ffverse/ffopportunity/releases/download/",
    paste0(version,"-data/ep_"), type, "_", season, ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)
  out <- purrr::map_dfr(urls, nflreadr::progressively(rds_from_url, p))
  ts <- nflreadr::raw_from_url(glue::glue("https://github.com/ffverse/ffopportunity/releases/download/{version}-data/timestamp.txt")) %>%
    rawToChar() %>%
    as.POSIXct()

  class(out) <- c("ffopps_load","tbl_df", "tbl", "data.frame")
  attr(out, "ep_version") <- version
  attr(out, "ep_timestamp") <- ts
  attr(out, "ep_type") <- type
  return(out)
}

#' @export
#' @noRd
print.ffopps_load <- function(x, ...) {
  cli::cli_alert("<ffopportunity predictions>")
  cli::cli_alert("Generated {.val {attr(x,'ep_timestamp')}} with ep model version {.val {attr(x,'ep_version')}}")
  NextMethod(print,x)
  invisible(x)
}
