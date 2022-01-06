#' Predict EP
#'
#' This function calls the prediction objects on the pre-processed data
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' preprocessed_pbp <- ep_preprocess(pbp)
#' pred_obj <- ep_predict(preprocessed_pbp)
#'
#' }
#'
#' @return a dataframe with the expected fields added
#'
#' @seealso `vignette("basic")` for example usage
#'

ep_predict <- function(preprocessed_pbp){

  models <- .load_model_obj("models")
  blueprints <- .load_model_obj("blueprints")
#' @import xgboost

  rush_df <-
    preprocessed_pbp$rush_df %>%
    dplyr::mutate(rushing_yards_exp = stats::predict(models$rush_yard,
                                                     newdata = hardhat::forge(new_data = .,
                                                                              blueprints$rush_yard)$predictors %>% as.matrix())) %>%
    dplyr::mutate(rushing_td_exp = stats::predict(models$rush_td,
                                                  newdata = hardhat::forge(new_data = .,
                                                                           blueprints$rush_td)$predictors %>% as.matrix())) %>%
    dplyr::mutate(rushing_fd_exp = stats::predict(models$rush_fd,
                                                  newdata = hardhat::forge(new_data = .,
                                                                           blueprints$rush_fd)$predictors %>% as.matrix())) %>%
    dplyr::mutate(rush_touchdown_exp = dplyr::if_else(two_point_attempt == 1, 0, rushing_td_exp),
                  two_point_conv_exp = dplyr::if_else(two_point_attempt == 1, rushing_td_exp, 0))

  pass_df <-
    preprocessed_pbp$pass_df %>%
    dplyr::mutate(pass_completion_exp = stats::predict(models$pass_completion,
                                                       newdata = hardhat::forge(new_data = .,
                                                                                blueprints$pass_completion)$predictors %>% as.matrix())) %>%
    dplyr::mutate(yards_after_catch_exp = stats::predict(models$pass_yac,
                                                         newdata = hardhat::forge(new_data = .,
                                                                                  blueprints$pass_yac)$predictors %>% as.matrix()),
                  yardline_exp = yardline_100 - air_yards - yards_after_catch_exp) %>%
    dplyr::mutate(pass_touchdown_exp = stats::predict(models$pass_td,
                                                         newdata = hardhat::forge(new_data = .,
                                                                                  blueprints$pass_td)$predictors %>% as.matrix()),
                  pass_touchdown_exp = dplyr::if_else(air_yards == yardline_100, pass_completion_exp, pass_touchdown_exp)) %>%
    dplyr::mutate(pass_first_down_exp = stats::predict(models$pass_fd,
                                                       newdata = hardhat::forge(new_data = .,
                                                                                blueprints$pass_fd)$predictors %>% as.matrix())) %>%
    dplyr::mutate(passing_int_exp = stats::predict(models$pass_int,
                                                       newdata = hardhat::forge(new_data = .,
                                                                                blueprints$pass_int)$predictors %>% as.matrix())) %>%
    dplyr::mutate(two_point_conv_exp = dplyr::if_else(two_point_attempt == 1, pass_touchdown_exp, 0),
                  pass_touchdown_exp = dplyr::if_else(two_point_attempt == 1, 0, pass_touchdown_exp))

  list_df <- list(rush_df = rush_df,
                  pass_df = pass_df)

  return(list_df)
}

.load_model_obj <- function(type = c("models", "blueprints")){

  if (type == "models") {
    load_fn <-  xgboost::xgb.load
    load_pattern <- "xgb" }
  else {
    load_fn <-  readRDS
    load_pattern <- "rds"
  }

  folder_path <- system.file("extdata", package = "ffexpectedpoints")
  file_names <- list.files(folder_path, pattern = load_pattern, full.names = TRUE)
  obj_names <- stringr::str_remove_all(file_names, paste0(folder_path,"/|.xgb|.rds"))
  models <- purrr::map(file_names, load_fn) %>% rlang::set_names(obj_names)

  return(models)

}

# .load_models <- function(){
#
#   list(fit_rush_yards = ffexpectedpoints::fit_rush_yards,
#        fit_rush_tds = ffexpectedpoints::fit_rush_tds,
#        fit_rush_fds = ffexpectedpoints::fit_rush_fds,
#        fit_pass_yac = ffexpectedpoints::fit_pass_yac,
#        fit_pass_td = ffexpectedpoints::fit_pass_td,
#        fit_pass_fd = ffexpectedpoints::fit_pass_fd,
#        fit_pass_completion = ffexpectedpoints::fit_pass_completion,
#        fit_pass_int = ffexpectedpoints::fit_pass_int)
#
# }
