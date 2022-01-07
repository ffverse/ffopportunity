#' Predict EP
#'
#' This function calls the prediction objects on the pre-processed data
#'
#' @param preprocessed_pbp list with dataframes created by `ep_preprocess`
#'
#' @examples
#' \donttest{
#' try({
#' nflreadr::load_pbp(2021) %>%
#' head(100) %>%
#' ep_preprocess() %>%
#' ep_predict()
#' })
#' }
#'
#' @return a dataframe with the expected fields added
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @import xgboost
ep_predict <- function(preprocessed_pbp) {

  rush_df <-
    preprocessed_pbp$rush_df %>%
    .forge_and_predict("rushing_yards") %>%
    .forge_and_predict("rushing_td") %>%
    .forge_and_predict("rushing_fd") %>%
    dplyr::mutate(
      two_point_conv_exp = dplyr::if_else(
        .data$two_point_attempt == 1, .data$rushing_td_exp, 0),
      rushing_td_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$rushing_td_exp)) %>%
    dplyr::rename(
      rush_yards_exp = .data$rushing_yards_exp,
      rush_touchdown_exp = .data$rushing_td_exp,
      rush_first_down_exp = .data$rushing_fd_exp
    )

  pass_df <-
    preprocessed_pbp$pass_df %>%
    .forge_and_predict("pass_completion") %>%
    .forge_and_predict("yards_after_catch") %>%
    dplyr::mutate(yardline_exp =
                    .data$yardline_100 -
                    .data$air_yards -
                    .data$yards_after_catch_exp) %>%
    .forge_and_predict("pass_touchdown") %>%
    dplyr::mutate(pass_touchdown_exp = dplyr::if_else(
      .data$air_yards == .data$yardline_100,
      .data$pass_completion_exp,
      .data$pass_touchdown_exp)) %>%
    .forge_and_predict("pass_first_down") %>%
    .forge_and_predict("passing_int") %>%
    dplyr::rename(pass_interception_exp = .data$passing_int_exp) %>%
    dplyr::mutate(
      two_point_conv_exp = dplyr::if_else(
        .data$two_point_attempt == 1, .data$pass_touchdown_exp, 0),
      pass_touchdown_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$pass_touchdown_exp)
    )

  list_df <- list(
    rush_df = rush_df,
    pass_df = pass_df
  )

  return(list_df)
}
#' @keywords internal
.forge_and_predict <- function(df, variable) {

  # could probably call .load_model_obj here based on the variable name?

  model_obj <- .load_model_objs(variable)

  df[[paste0(variable, "_exp")]] <-
    stats::predict(
      object = model_obj$model,
      newdata = hardhat::forge(new_data = df,
                               blueprint = model_obj$blueprint)$predictors %>%
        as.matrix()
    )

  return(df)
}

# future: reroute system.file() to user's cache folder and
# automatically/prompt-for download if file not found?
# future: add some kind of version selector (as package option?)
#' @keywords internal
.load_model_objs <- function(variable) {

  folder_path <- system.file("extdata", package = "ffexpectedpoints")
  model_path <- file.path(folder_path, paste0(variable,".xgb"))
  blueprint_path <- file.path(folder_path, paste0(variable,".rds"))

  stopifnot(file.exists(model_path), file.exists(blueprint_path))

  model <- xgboost::xgb.load(model_path)
  blueprint <- readRDS(blueprint_path)

  return(list(model = model, blueprint = blueprint))
}
