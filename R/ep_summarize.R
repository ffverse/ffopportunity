#' Summarize EP
#'
#' This function summarizes the EP data up to the game level
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' temp <- ep_preprocess(pbp)
#' predicted_pbp <- ep_predict(temp)
#' cleaned <- ep_calculate_player_stats(predicted_pbp)
#'
#' predicted_pbp$pass_df %>%
#'   dplyr::filter(game_id == "2021_01_ARI_TEN", posteam == "ARI", first_down == 1, penalty == 1)
#' predicted_pbp$rush_df %>% dplyr::filter(week == 5, two_point_attempt == 1)
#' }
#'
#' @return a dataframe with the expected fields added
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @export


ep_summarize <- function(
  predicted_pbp,
  stat_type = c("expected_points", "team_stats")){

  rush_df <-
    predicted_pbp$rush_df %>%
    dplyr::transmute(
      season = substr(.data$game_id, 1, 4),
      .data$week,
      .data$game_id,
      play_id = as.factor(.data$play_id),
      play_description = .data$desc,
      player_id = .data$rusher_player_id,
      .data$full_name,
      .data$position,
      .data$posteam,
      player_type = "rush",
      attempt = dplyr::if_else(.data$two_point_attempt == 1, 0, rush_attempt),
      #already 0 for 2pt attempts
      yards_gained = .data$rushing_yards,
      yards_gained_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, rush_yards_exp),
      touchdown = dplyr::if_else(.data$rush_touchdown == "1", 1L, 0L),
      touchdown_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, rush_touchdown_exp),
      two_point_conv = .data$two_point_converted,
      two_point_conv_exp = dplyr::if_else(
        .data$two_point_attempt == 1, .data$two_point_conv_exp, 0),
      first_down = .data$first_down_rush,
      first_down_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$rush_first_down_exp),
      fantasy_points =
        6.0*.data$touchdown +
        2.0*.data$two_point_converted +
        0.1*.data$rushing_yards -
        2.0*.data$fumble_lost,
      fantasy_points_exp =
        0.1*.data$rush_yards_exp +
        dplyr::if_else(
          .data$two_point_attempt == 1,
          2*.data$two_point_conv_exp,
          6*.data$rush_touchdown_exp),
      .data$fumble_lost)

  pass_df <-
    predicted_pbp$pass_df %>%
    dplyr::transmute(
      season = substr(.data$game_id, 1, 4),
      .data$week,
      .data$game_id,
      play_id = as.factor(.data$play_id),
      play_description = .data$desc,
      .data$posteam,
      pass.player_id = .data$passer_player_id,
      pass.full_name = .data$passer_full_name,
      pass.position = .data$passer_position,
      rec.player_id = .data$receiver_player_id,
      rec.full_name = .data$receiver_full_name,
      rec.position = .data$receiver_position,
      .data$posteam,
      attempt = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$pass_attempt),
      air_yards = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$air_yards),
      complete_pass = dplyr::if_else(.data$complete_pass == "1", 1L, 0L),
      complete_pass_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$pass_completion_exp),
      #already 0 for 2pt attempts
      yards_gained = dplyr::if_else(
        is.na(.data$receiving_yards), 0, .data$receiving_yards),
      yards_gained_exp =
        .data$complete_pass_exp *
        (.data$yards_after_catch_exp + .data$air_yards),
      touchdown = dplyr::if_else(.data$pass_touchdown == "1", 1L, 0L),
      touchdown_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$pass_touchdown_exp),
      two_point_conv = .data$two_point_converted,
      two_point_conv_exp = dplyr::if_else(
        .data$two_point_attempt == 1, .data$two_point_conv_exp, 0),
      first_down = .data$first_down_pass,
      first_down_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$pass_first_down_exp),
      interception = dplyr::if_else(.data$interception == "1", 1L, 0L),
      interception_exp = dplyr::if_else(
        .data$two_point_attempt == 1, 0, .data$pass_interception_exp),
      .data$fumble_lost) %>%
    tidyr::pivot_longer(cols = c(.data$pass.player_id,
                                 .data$pass.full_name,
                                 .data$pass.position,
                                 .data$rec.player_id,
                                 .data$rec.full_name,
                                 .data$rec.position),
                        names_to = c("player_type", ".value"),
                        names_sep = "\\.") %>%
    dplyr::mutate(fantasy_points_exp =
                    dplyr::if_else(.data$player_type == "rec",
                                   0.1*.data$yards_gained_exp +
                                     .data$complete_pass_exp +
                                     6*.data$touchdown_exp +
                                     2*.data$two_point_conv_exp,
                                   0.04*.data$yards_gained_exp -
                                     2*.data$interception_exp +
                                     4*.data$touchdown_exp +
                                     2*.data$two_point_conv_exp),
                  fantasy_points =
                    dplyr::if_else(.data$player_type == "rec",
                                   6*.data$touchdown +
                                     2*.data$two_point_conv  +
                                     0.1*.data$yards_gained -
                                     2*.data$fumble_lost +
                                     .data$complete_pass,
                                   4*.data$touchdown +
                                     2*.data$two_point_conv  +
                                     0.04*.data$yards_gained -
                                     2*.data$fumble_lost -
                                     2*.data$interception))

  combined_df <-
    pass_df %>%
    dplyr::bind_rows(rush_df) %>%
    tidyr::pivot_wider(id_cols = c(.data$season,
                                   .data$posteam,
                                   .data$week,
                                   .data$game_id,
                                   .data$player_id,
                                   .data$full_name,
                                   .data$position),
                       names_from = player_type,
                       names_glue = "{player_type}_{.value}",
                       values_fn = sum,
                       values_from = c(where(is.numeric), -.data$week)) %>%
    janitor::remove_empty(which = "cols") %>%
    dplyr::mutate(dplyr::across(.cols = where(is.numeric),
                                .fns =  ~tidyr::replace_na(.x, 0)),
                  dplyr::across(.cols = where(is.numeric),
                                .fns =  ~round(.x, 2))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      total_yards_gained =
        sum(dplyr::across(ends_with("yards_gained"), sum)),
      total_yards_gained_exp =
        sum(dplyr::across(ends_with("yards_gained_exp"), sum)),
      total_touchdown =
        sum(dplyr::across(ends_with("touchdown"), sum)),
      total_touchdown_exp =
        sum(dplyr::across(ends_with("touchdown_exp"), sum)),
      total_first_down =
        sum(dplyr::across(ends_with("first_down"), sum)),
      total_first_down_exp =
        sum(dplyr::across(ends_with("first_down_exp"), sum)),
      total_fantasy_points =
        sum(dplyr::across(ends_with("fantasy_points"), sum)),
      total_fantasy_points_exp =
      sum(dplyr::across(ends_with("fantasy_points_exp"), sum)))

  exp_fields <-
    combined_df %>%
    dplyr::select(contains("exp")) %>%
    colnames() %>%
    stringr::str_remove_all("_exp")

  for(f in exp_fields) {
    combined_df[paste0(f,"_diff")] <-
      combined_df[f]-combined_df[paste0(f,"_exp")]
  }

  team_df <-
    combined_df %>%
    dplyr::group_by(.data$season, .data$posteam, .data$week, .data$game_id) %>%
    dplyr::summarise(
      dplyr::across(
        .cols = where(is.numeric),
        .fns = sum,
        .names = "{col}_team")
      ) %>%
    dplyr::ungroup()

  player_team_df <-
    combined_df %>%
    dplyr::left_join(team_df, by = c("season", "posteam", "week", "game_id"))

  if(any("expected_points" == stat_type) & any("team_stats" == stat_type))
    return(player_team_df)
  else if(stat_type == "team_stats") return(team_df)
  else return(combined_df)
}
