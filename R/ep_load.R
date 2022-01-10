#' Load EP Pass Play By Play
#'
#' @description Loads multiple seasons from the
#' [ffexpectedpoints data repository](https://github.com/ffverse/ffexpectedpoints-data)
#'
#' @param season A numeric vector of 4-digit years associated with given NFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 1999.
#' @param version EP model version: one of "latest" or "v1.0.0" (these are identical at the moment)
#'
#' @return The complete ffexpectedpoints dataset as returned by `ffexpectedpoints::build_nflfastR_pbp()`
#' (see below) for all given `seasons`
#'
#' @examples
#' \donttest{
#'   try({ep_load(2019:2020)})
#' }
#'
#' @export
ep_load_pbp_pass <- function(season = nflreadr:::most_recent_season(), version = c("latest","v1.0.0")) {

  version <- rlang::arg_match0(version, c("latest","v1.0.0"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0(
    "https://github.com/ffverse/ffexpectedpoints/releases/download/",
    paste0(version,"-data/"),
    "ep_pbp_pass_",
    season,
    ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)
  out <- purrr::map_dfr(urls, nflreadr::progressively(rds_from_url, p))
  out
}

#' Load EP Rush Play By Play
#'
#' @description Loads multiple seasons from the
#' [ffexpectedpoints data repository](https://github.com/ffverse/ffexpectedpoints-data)
#'
#' @param season A numeric vector of 4-digit years associated with given NFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 1999.
#' @param version EP model version: one of "latest" or "v1.0.0" (these are identical at the moment)
#'
#' @return The complete ffexpectedpoints dataset as returned by `ffexpectedpoints::build_nflfastR_pbp()`
#' (see below) for all given `seasons`
#'
#' @examples
#' \donttest{
#'   try({ep_load_pbp_rush(2019:2020)})
#' }
#'
#' @export
ep_load_pbp_rush <- function(season = nflreadr:::most_recent_season(),
                             version = "v1.0.0") {

  version <- rlang::arg_match0(version, c("latest","v1.0.0"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0(
    "https://github.com/ffverse/ffexpectedpoints/releases/download/",
    paste0(version,"-data/"),
    "ep_pbp_rush_",
    season,
    ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)
  out <- purrr::map_dfr(urls, nflreadr::progressively(rds_from_url, p))
  out
}

#' Load Player Level Weekly EP Stats
#'
#' @param season A numeric vector of 4-digit years associated with given NFL
#' seasons - defaults to latest season. If set to `TRUE`,
#' returns all available data since 1999.
#' @param version EP model version: one of "latest" or "v1.0.0" (these are identical at the moment)
#'
#' @examples
#' \donttest{
#'   try({ep_load_player_stats()})
#' }
#'
#' @return A tibble of week-level player stats with corresponding EP variables
#'
#' @export
ep_load_player_stats <- function(season = nflreadr:::most_recent_season(),
                                 version = c("latest","v1.0.0")) {

  version <- rlang::arg_match0(version, c("latest","v1.0.0"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0(
    "https://github.com/ffverse/ffexpectedpoints/releases/download/",
    paste0(version,"-data/"),
    "ep_weekly_",
    season,
    ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)
  out <- purrr::map_dfr(urls, nflreadr::progressively(rds_from_url, p))
  out
}
