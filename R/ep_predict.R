#' Predict EP
#'
#' This function runs the prediction functions over preprocessed data.
#'
#' @param preprocessed_pbp list with dataframes created by `ep_preprocess`
#' @param version ep model version - available is `"latest"` or `"v1.0.0"` (these are currently the same thing)
#'
#' @examples
#' \donttest{
#'   try({
#'   preprocessed <- readRDS(system.file("ep_preprocessed.rds",package = "ffopportunity"))
#'   # this file is equivalent to nflreadr::load_pbp(2021) %>% head(1000) %>% ep_preprocess()
#'   ep_predict(preprocessed)
#'   })
#' }
#'
#' @return a dataframe with the expected fields added
#'
#' @import xgboost
#' @import recipes
#'
#' @export
ep_predict <- function(preprocessed_pbp, version = c("latest", "v1.0.0")) {

  version <- rlang::arg_match0(version, c("latest", "v1.0.0"))
  ep_cache_models(version = version)

  rush_df <-
    preprocessed_pbp$rush_df %>%
    .forge_and_predict("rushing_yards", version) %>%
    .forge_and_predict("rushing_td", version) %>%
    .forge_and_predict("rushing_fd", version) %>%
    dplyr::mutate(
      two_point_conv_exp = dplyr::if_else(
        .data$two_point_attempt == 1, .data$rushing_td_exp, 0),

      rush_yards_exp = dplyr::case_when(grepl("kneel", .data$desc) ~ -1,
                                        grepl("Aborted", .data$desc) ~ 0,
                                        TRUE ~ .data$rushing_yards_exp),

      rush_touchdown_exp = dplyr::case_when(
        grepl("kneel|Aborted", .data$desc) ~ 0,
        .data$two_point_attempt == 1 ~ 0,
        TRUE ~ .data$rushing_td_exp),

      rush_first_down_exp = dplyr::if_else(grepl("kneel|Aborted", .data$desc),
                                      0,
                                      .data$rushing_fd_exp)
      )

  pass_df <-
    preprocessed_pbp$pass_df %>%
    .forge_and_predict("pass_completion", version) %>%
    .forge_and_predict("yards_after_catch", version) %>%
    dplyr::mutate(yardline_exp =
                    .data$yardline_100 -
                    .data$air_yards -
                    .data$yards_after_catch_exp) %>%
    .forge_and_predict("pass_touchdown", version) %>%
    dplyr::mutate(pass_touchdown_exp = dplyr::if_else(
      .data$air_yards == .data$yardline_100,
      .data$pass_completion_exp,
      .data$pass_touchdown_exp)) %>%
    .forge_and_predict("pass_first_down", version) %>%
    .forge_and_predict("passing_int", version) %>%
    dplyr::rename(pass_interception_exp = .data$passing_int_exp) %>%
    dplyr::mutate(
      two_point_conv_exp = dplyr::if_else(
        .data$two_point_attempt == 1, .data$pass_touchdown_exp, 0),

      pass_completion_exp = dplyr::if_else(grepl("spike", .data$desc), 0,
                                           .data$pass_completion_exp),
      yards_after_catch_exp = dplyr::if_else(grepl("spike", .data$desc), 0,
                                             .data$yards_after_catch_exp),
      pass_first_down_exp = dplyr::if_else(grepl("spike", .data$desc), 0,
                                             .data$pass_first_down_exp),
      pass_interception_exp = dplyr::if_else(grepl("spike", .data$desc), 0,
                                           .data$pass_interception_exp),

      pass_touchdown_exp = dplyr::case_when(
        .data$two_point_attempt == 1 ~ 0,
        grepl("spike", .data$desc) ~ 0,
        TRUE ~ .data$pass_touchdown_exp)
    )

  list_df <- list(
    rush_df = rush_df,
    pass_df = pass_df
  )

  return(list_df)
}
#' @keywords internal
.forge_and_predict <- function(df, variable, version) {

  silencer <- if(getOption("ffopportunity.verbose", default = FALSE)) force else suppressWarnings
  # could probably call .load_model_obj here based on the variable name?
  model_obj <- .load_model_objs(variable, version)

  silencer({
    df[[paste0(variable, "_exp")]] <-
      stats::predict(
        object = model_obj$model,
        newdata = hardhat::forge(new_data = df,
                                 blueprint = model_obj$blueprint)$predictors %>%
          as.matrix()
      )
  })

  return(df)
}

# future: reroute system.file() to user's cache folder and
# automatically/prompt-for download if file not found?
# future: add some kind of version selector (as package option?)
#' @keywords internal
.load_model_objs <- function(variable, version) {

  cache_dir <- rappdirs::user_cache_dir("ffopportunity", "ffverse")

  folder_path <- file.path(cache_dir,version)

  model_path <- file.path(folder_path, paste0(variable,".xgb"))
  blueprint_path <- file.path(folder_path, paste0(variable,".rds"))

  stopifnot(file.exists(model_path), file.exists(blueprint_path))

  model <- xgboost::xgb.load(model_path)
  blueprint <- readRDS(blueprint_path)

  return(list(model = model, blueprint = blueprint))
}
