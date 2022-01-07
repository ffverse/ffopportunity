#' Preprocess Data
#'
#' This function performs the necesssary pre-processing steps to make
#' expected points predictions on `nflreadr` data
#'
#' @param season a numeric vector of seasons that defaults to most recent season. Must be later than 2006.
#'
#' @examples
#' \donttest{
#' try({ # prevents cran-related errors
#'   ep_build(season = 2021)
#' })
#' }
#'
#' @return a list containing two dataframes: pbp for play-level ep predictions and weekly for game-level summaries.
#'
#' @export
ep_build <- function(season = nflreadr:::most_recent_season()){

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  vcli_rule("Starting ep build for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  vcli_alert("Loading pbp {Sys.time()}")
  pbp <- nflreadr::load_pbp(season)

  vcli_alert("Preprocessing pbp {Sys.time()}")
  pbp_preprocessed <- ep_preprocess(pbp)

  pbp_preds <- ep_predict(pbp_preprocessed)
  vcli_alert("Generating predictions {Sys.time()}")

  weekly_ep <- ep_summarize(pbp_preds)
  vcli_alert("Summarizing data {Sys.time()}")

  vcli_rule("Finished building ep for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  out <- structure(
    list(
      ep_pbp_pass = pbp_preds$pass_df,
      ep_pbp_rush = pbp_preds$rush_df,
      ep_weekly = weekly_ep,
      ep_version = "1.0.0",
      timestamp = Sys.time()
    ),
    class = "ffep_output"
  )
}

#' @export
#' @noRd
print.ffep_output <- function(x, ...) {
  cli::cli_alert("<ffexpectedpoints predictions>")
  cli::cli_alert("Generated {x$timestamp} with model version {x$ep_version}")
  utils::str(x, max.level = 2, give.attr = FALSE)
  invisible(x)
}
