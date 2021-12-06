#' Summarize EP
#'
#' This function summarizes the EP data up to the game level
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' temp <- ep_preprocess(pbp)
#' predicted_pbp <- ep_predict(temp)
#' cleaned <- ep_calculate_player_stats(pred_obj)
#' }
#'
#' @return a dataframe with the expected fields added
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @export
#'

ep_calculate_player_stats <- function(predicted_pbp){

  rush_df <-
    predicted_pbp$rush_df %>%
    dplyr::transmute(season = substr(game_id, 1, 4),
                     week,
                     game_id,
                     play_id = as.factor(play_id),
                     play_description = desc,
                     player_id = fantasy_player_id,
                     full_name,
                     position,
                     posteam,
                     player_type = "rush",
                     attempt = 1,
                     yards_gained = rushing_yards,
                     yards_gained_exp = rushing_yards_exp,
                     touchdown = dplyr::if_else(rush_touchdown == "1", 1L, 0L),
                     touchdown_exp = dplyr::if_else(two_point_attempt == 1, 0, rush_touchdown_exp),
                     two_point_conv = two_point_converted,
                     two_point_conv_exp = dplyr::if_else(two_point_attempt == 1, rush_touchdown_exp, 0),
                     first_down = dplyr::if_else(first_down == "1", 1L, 0L),
                     first_down_exp = rushing_fd_exp,
                     fantasy_points = 6*touchdown + 2*two_point_converted + 0.1*rushing_yards - 2*fumble_lost,
                     fantasy_points_exp = 0.1*rushing_yards_exp + dplyr::if_else(two_point_attempt == 1,
                                                                                 2*rush_touchdown_exp,
                                                                                 6*rush_touchdown_exp),
                     fumble_lost)

  pass_df <-
    predicted_pbp$pass_df %>%
    dplyr::transmute(season = substr(game_id, 1, 4),
                     week,
                     game_id,
                     play_id = as.factor(play_id),
                     play_description = desc,
                     posteam,

                     pass.player_id = passer_player_id,
                     pass.full_name = passer_full_name,
                     pass.position = passer_position,

                     rec.player_id = receiver_player_id,
                     rec.full_name = receiver_full_name,
                     rec.position = receiver_position,

                     posteam,
                     attempt = 1,
                     air_yards,
                     complete_pass = dplyr::if_else(complete_pass == "1", 1L, 0L),
                     complete_pass_exp = pass_completion_exp,

                     yards_gained = dplyr::if_else(is.na(receiving_yards), 0, receiving_yards),
                     yards_gained_exp = pass_completion_exp * (yards_after_catch_exp + air_yards),

                     touchdown = dplyr::if_else(pass_touchdown == "1", 1L, 0L),
                     touchdown_exp = dplyr::if_else(two_point_attempt == 1, 0, pass_touchdown_exp),
                     two_point_conv = two_point_converted,
                     two_point_conv_exp = dplyr::if_else(two_point_attempt == 1, pass_touchdown_exp, 0),
                     first_down = dplyr::if_else(first_down == "1", 1L, 0L),
                     first_down_exp = pass_first_down_exp,
                     interception = dplyr::if_else(interception == "1", 1L, 0L),
                     interception_exp = passing_int_exp,
                     fumble_lost) %>%
    tidyr::pivot_longer(cols = c(pass.player_id, pass.full_name, pass.position,
                                 rec.player_id, rec.full_name, rec.position),
                        names_to = c("player_type", ".value"),
                        names_sep = "\\.") %>%
    dplyr::mutate(fantasy_points_exp =
                    dplyr::if_else(player_type == "rec",
                                   0.1*yards_gained_exp + complete_pass_exp + 6*touchdown_exp + 2*two_point_conv_exp,
                                   0.04*yards_gained_exp - 2*interception_exp + 4*touchdown_exp + 2*two_point_conv_exp),
                  fantasy_points =
                    dplyr::if_else(player_type == "rec",
                                   6*touchdown + 2*two_point_conv  + 0.1*yards_gained - 2*fumble_lost + complete_pass,
                                   4*touchdown + 2*two_point_conv  + 0.04*yards_gained - 2*fumble_lost - 2*interception))

  combined_df <-
    pass_df %>%
    dplyr::bind_rows(rush_df) %>%
    tidyr::pivot_wider(id_cols = c(season, posteam, week, game_id, player_id, full_name, position),
                       names_from = player_type,
                       names_glue = "{player_type}_{.value}",
                       values_fn = sum,
                       values_from = c(where(is.numeric), -week)
    ) %>%
    janitor::remove_empty(which = "cols") %>%
    dplyr::mutate(dplyr::across(.cols = where(is.numeric),
                                .fns =  ~tidyr::replace_na(.x, 0)),
                  dplyr::across(.cols = where(is.numeric),
                                .fns =  ~round(.x, 2))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total_yards_gained = sum(dplyr::across(ends_with("yards_gained"), sum)),
                  total_yards_gained_exp = sum(dplyr::across(ends_with("yards_gained_exp"), sum)),
                  total_touchdown = sum(dplyr::across(ends_with("touchdown"), sum)),
                  total_touchdown_exp = sum(dplyr::across(ends_with("touchdown_exp"), sum)),
                  total_first_down = sum(dplyr::across(ends_with("first_down"), sum)),
                  total_first_down_exp = sum(dplyr::across(ends_with("first_down_exp"), sum)),
                  total_fantasy_points = sum(dplyr::across(ends_with("fantasy_points"), sum)),
                  total_fantasy_points_exp = sum(dplyr::across(ends_with("fantasy_points_exp"), sum)))

  exp_fields <-
    combined_df %>%
    dplyr::select(contains("exp")) %>%
    colnames() %>%
    stringr::str_remove_all("_exp")

  for(f in exp_fields) {
    combined_df[paste0(f,"_diff")] <- combined_df[f]-combined_df[paste0(f,"_exp")]
  }

  return(combined_df)
}
