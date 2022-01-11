#' Build EP
#'
#' This function builds Expected Fantasy Points predictions by downloading the xgboost models and play-by-play data, applying the model, and summarizing to player level.
#'
#' @param season a numeric vector of seasons that defaults to most recent season. Must be later than 2006.
#' @param version an EP model version - one of "latest" (default) or "v1.0.0" (these are currently identical)
#'
#' @examples
#' \donttest{
#' try({ # prevents cran-related errors
#'   ep_build(season = 2021)
#' })
#' }
#'
#' @return a list containing three dataframes: `ep_weekly` provides a game-level summary by player, `ep_pbp_pass` provides EP data on pass plays, and `ep_pbp_rush` provides EP data on rush plays.
#'
#' @family main
#'
#' @export
ep_build <- function(season = nflreadr:::most_recent_season(), version = "latest"){

  version <- rlang::arg_match0(version, c("latest", "v1.0.0"))

  stopifnot(
    length(season) > 0,
    is.numeric(season),
    all(!is.na(season)),
    min(season) >= 2006
  )

  vcli_rule("Starting ep build for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  vcli_alert("Downloading EP models, if required {Sys.time()}")
  ep_cache_models(version)

  vcli_alert("Loading pbp {Sys.time()}")
  pbp <- nflreadr::load_pbp(season)

  vcli_alert("Preprocessing pbp {Sys.time()}")
  pbp_preprocessed <- ep_preprocess(pbp)

  pbp_preds <- ep_predict(pbp_preprocessed, version = version)
  vcli_alert("Generating predictions {Sys.time()}")

  weekly_ep <- ep_summarize(pbp_preds)
  vcli_alert("Summarizing data {Sys.time()}")

  vcli_rule("Finished building ep for {paste(unique(range(season)),collapse = '-')} season(s)! {Sys.time()}")

  out <- structure(
    list(
      ep_weekly = weekly_ep,
      ep_pbp_pass = pbp_preds$pass_df,
      ep_pbp_rush = pbp_preds$rush_df,
      ep_version = version,
      timestamp = Sys.time()
    ),
    class = "ffep_output"
  )
  return(out)
}

#' @export
#' @noRd
print.ffep_output <- function(x, ...) {
  cli::cli_alert("<ffexpectedpoints predictions>")
  cli::cli_alert("Generated {x$timestamp} with model version {x$ep_version}")
  utils::str(x, max.level = 2, give.attr = FALSE)
  invisible(x)
}
