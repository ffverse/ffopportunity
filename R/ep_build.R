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

  vcli_start(msg = "Loading pbp...")
  pbp <- nflreadr::load_pbp(season)
  vcli_end(msg_done = "Loading pbp...done! {Sys.time()}")

  vcli_start(msg = "Preprocessing pbp...")
  pbp_preprocessed <- ep_preprocess(pbp)
  vcli_end(msg_done = "Preprocessing pbp...done! {Sys.time()}")

  vcli_start(msg = "Generating predictions...")
  pbp_preds <- ep_predict(pbp_preprocessed)
  vcli_end(msg_done = "Generating predictions...done! {Sys.time()}")

  vcli_start(msg = "Summarizing data...")
  weekly_ep <- ep_summarize(pbp_preds)
  vcli_end(msg_done = "Summarizing data...done! {Sys.time()}")

  vcli_rule("Finished building ep for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  out <- structure(
    list(
      pbp = pbp_preds,
      weekly = weekly_ep,
      model_version = "0.1",
      timestamp = Sys.time()
    ),
    class = "ffep_output"
  )
}

#' @export
#' @noRd
print.ffep_output <- function(x, ...) {
  cli::cli_alert("<ffexpectedpoints predictions>")
  cli::cli_alert("Generated {x$timestamp} with version {x$model_version}")
  str(x, max.level = 1, give.attr = FALSE)
  invisible(x)
}
