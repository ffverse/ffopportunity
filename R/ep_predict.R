#' Predict EP
#'
#' This function calls the prediction objects on the pre-processed data
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' temp <- ep_preprocess(pbp)
#' pred_obj <- ep_predict(temp)
#'
#' }
#'
#' @return a dataframe with the expected fields added
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @export
#'

ep_predict <- function(preprocessed_pbp){

  library(workflows)

  models <- .load_models()

  rush_df <-
    preprocessed_pbp$rush_df %>%
    dplyr::bind_cols(stats::predict(models$fit_rush_yards, new_data = .)) %>%
    dplyr::rename(rushing_yards_exp = .pred) %>%
    dplyr::bind_cols(stats::predict(models$fit_rush_tds, new_data = ., type = "prob")) %>%
    dplyr::rename(rushing_td_exp = .pred_1) %>%
    dplyr::select(-.pred_0) %>%
    dplyr::bind_cols(stats::predict(models$fit_rush_fds, new_data = ., type = "prob")) %>%
    dplyr::select(-.pred_0) %>%
    dplyr::rename(rushing_fd_exp = .pred_1) %>%
    dplyr::mutate(season = substr(game_id, 1, 4),
                  touchdown_exp = dplyr::if_else(two_point_attempt == 1, 0, rushing_td_exp),
                  two_point_conv_exp = dplyr::if_else(two_point_attempt == 1, rushing_td_exp, 0))


  pass_df <-
    preprocessed_pbp$pass_df %>%
    dplyr::bind_cols(stats::predict(models$fit_pass_completion, new_data = ., type = "prob")) %>%
    dplyr::rename(pass_completion_exp = .pred_complete) %>%
    dplyr::select(-.pred_incomplete) %>%
    dplyr::bind_cols(stats::predict(models$fit_pass_yards, new_data = .)) %>%
    dplyr::rename(receiving_yards_exp = .pred) %>%
    dplyr::bind_cols(stats::predict(models$fit_pass_td, new_data = ., type = "prob")) %>%
    dplyr::rename(rec_td_exp = .pred_1) %>%
    # Cap TD probability at catch probability in the end zone
    dplyr::mutate(rec_td_exp = if_else(air_yards == yardline_100, pass_completion_exp, rec_td_exp)) %>%
    dplyr::select(-.pred_0) %>%
    dplyr::bind_cols(stats::predict(models$fit_pass_fd, new_data = ., type = "prob")) %>%
    dplyr::rename(rec_fd_exp = .pred_1) %>%
    dplyr::select(-.pred_0) %>%
    dplyr::bind_cols(stats::predict(models$fit_pass_int, new_data = ., type = "prob")) %>%
    dplyr::rename(passing_int_exp = .pred_1) %>%
    dplyr::select(-.pred_0) %>%
    dplyr::mutate(season = substr(game_id, 1, 4),
                  touchdown_exp = dplyr::if_else(two_point_attempt == 1, 0, rec_td_exp),
                  two_point_conv_exp = dplyr::if_else(two_point_attempt == 1, rec_td_exp, 0))

  list_df <- list(rush_df,
                  pass_df)

  return(list_df)

}


.load_models <- function(){

  filenames <- list.files("./models", pattern="fit", full.names=TRUE)
  obj_names <- stringr::str_remove_all(filenames,"./models/|.RDS")
  models <- purrr::map(filenames, readRDS) %>% rlang::set_names(obj_names)

  return(models)

}
