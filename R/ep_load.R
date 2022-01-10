#' Load EP Pass Play By Play
#'
#' @description Loads multiple seasons from the
#' [ffexpectedpoints data repository](https://github.com/ffverse/ffexpectedpoints-data)
#'
#' @param season A numeric vector of 4-digit years associated with given NFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 1999.
#' @param file_type One of `"rds"`, `"parquet"`, or `"csv"`.
#'
#' @return The complete ffexpectedpoints dataset as returned by `ffexpectedpoints::build_nflfastR_pbp()`
#' (see below) for all given `seasons`
#'
#' @examples
#' \donttest{
#'   try({load_ep_pbp_pass(2019:2020)})
#' }
#'
#' @export
load_ep_pbp_pass <- function(season = nflreadr:::most_recent_season(),
                             file_type = "rds") {

  file_type <- rlang::arg_match0(file_type, c("rds"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0(
    "https://github.com/ffverse/ffexpectedpoints-data/raw/main/data/pbp/ep_pbp_pass_",
    season,
    ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)

  out <- lapply(urls, nflreadr::progressively(rds_from_url, p))
  out <- data.table::rbindlist(out, use.names = TRUE)
  class(out) <- c("tbl_df","tbl","data.table","data.frame")
  out
}

#' Load EP Rush Play By Play
#'
#' @description Loads multiple seasons from the
#' [ffexpectedpoints data repository](https://github.com/ffverse/ffexpectedpoints-data)
#'
#' @param season A numeric vector of 4-digit years associated with given NFL seasons - defaults to latest season. If set to `TRUE`, returns all available data since 1999.
#' @param file_type One of `"rds"`, `"parquet"`, or `"csv"`.
#'
#' @return The complete ffexpectedpoints dataset as returned by `ffexpectedpoints::build_nflfastR_pbp()`
#' (see below) for all given `seasons`
#'
#' @examples
#' \donttest{
#'   try({load_ep_pbp_rush(2019:2020)})
#' }
#'
#' @export
load_ep_pbp_rush <- function(season = nflreadr:::most_recent_season(),
                             file_type = "rds") {

  file_type <- rlang::arg_match0(file_type, c("rds"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0(
  "https://github.com/ffverse/ffexpectedpoints-data/raw/main/data/pbp/ep_pbp_rush_",
  season,
  ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)

  out <- lapply(urls, nflreadr::progressively(rds_from_url, p))
  out <- data.table::rbindlist(out, use.names = TRUE)
  class(out) <- c("tbl_df","tbl","data.table","data.frame")
  out
}

#' Load Player Level Weekly EP Stats
#'
#' @param season A numeric vector of 4-digit years associated with given NFL
#' seasons - defaults to latest season. If set to `TRUE`,
#' returns all available data since 1999.
#' @param file_type One of `"rds"`, `"parquet"`, or `"csv"`.
#'
#' @examples
#' \donttest{
#'   try({load_player_ep_stats()})
#' }
#'
#' @return A tibble of week-level player stats with corresponding EP variables
#'
#' @export
load_player_ep_stats <- function(season = nflreadr:::most_recent_season(),
                                 file_type = "rds") {

  file_type <- rlang::arg_match0(file_type, c("rds"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  urls <- paste0("https://github.com/ffverse/ffexpectedpoints-data/raw/main/data/weekly/ep_weekly_",
                season,
                ".rds")

  p <- NULL
  if (is_installed("progressr")) p <- progressr::progressor(along = season)

  out <- lapply(urls, nflreadr::progressively(rds_from_url, p))
  out <- data.table::rbindlist(out, use.names = TRUE)
  class(out) <- c("tbl_df","tbl","data.table","data.frame")
  out
}
