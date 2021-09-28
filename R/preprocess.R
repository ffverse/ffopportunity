#' Preprocess Data
#'
#' This function performs the necesssary pre-processing steps to make
#' expected points predictions on `nflreadr` data
#'
#' @examples
#' \donttest{
#' pbp <- nflreadr::load_pbp(2021)
#' .preprocess_common_fields(pbp) %>% dplyr::glimpse()
#' temp <- ep_preprocess(pbp) %>% dplyr::glimpse()
#' }
#'
#' @return a list of two dataframes (one for passes and one for rushes)
#' of nflreadr data with the expectedpoints columns transformed for prediction
#'
#' @seealso `vignette("basic")` for example usage
#'
#' @export
#'

ep_preprocess <- function(pbp){

  rosters <- .get_rosters(2021)

  prep_pbp <- .preprocess_common_fields(pbp)

  list_df <- list(rush_df = .preprocess_rush_df(prep_pbp, rosters),
                  pass_df = .preprocess_pass_df(prep_pbp,rosters))

  return(list_df)
}

#' @keywords internal
.preprocess_common_fields <- function(pbp){

  pbp <- pbp %>%
    dplyr::mutate(
      #Create New Fields
      game_month = lubridate::month(game_date),
      game_month = dplyr::if_else(game_month < 3, 12, game_month),
      game_week = lubridate::week(game_date),
      game_week = dplyr::if_else(game_week <= 30, 53, game_week),
      game_wday = as.character(lubridate::wday(game_date, label = TRUE)),
      game_wday = dplyr::case_when(
        game_wday %in% c("Tue","Wed","Fri","Sat") ~ "Other",
        TRUE ~ game_wday),
      game_time = lubridate::hour(lubridate::hms(start_time)),
      implied_total = dplyr::case_when(
        posteam_type == "away" & spread_line<=0 ~ (total_line+spread_line)/2 - spread_line,
        posteam_type == "away" & spread_line>0 ~ (total_line-spread_line)/2,
        posteam_type == "home" & spread_line>0 ~ (total_line+spread_line)/2 - spread_line,
        posteam_type == "home" & spread_line<=0 ~ (total_line-spread_line)/2),

      #Data Cleaning
      two_point_converted = dplyr::case_when(two_point_conv_result == "success" ~ 1,
                                             is.na(two_point_conv_result) &
                                               grepl( "ATTEMPT SUCCEEDS", desc) ~ 1,
                                             TRUE ~ 0),
      down = dplyr::if_else(two_point_attempt == 1, 4, down),
      surface = dplyr::if_else(surface == "grass", "grass", "turf"),
      temp = dplyr::case_when(roof %in% c("closed", "dome") ~ 68L,
                              is.na(temp) ~ 60L,
                              TRUE ~ temp),
      wind = dplyr::case_when(roof %in% c("closed", "dome") ~ 0L,
                              is.na(wind) ~ 8L,
                              TRUE ~ wind),

      #Set ordered factor variables
      week = factor(week, levels = as.character(c(1:21)), ordered = TRUE),
      game_month = factor(game_month, levels = as.character(c(9:12)), ordered = TRUE),
      game_week = factor(game_week, levels = as.character(c(36:53)), ordered = TRUE),
      game_time = factor(game_time, levels = as.character(c(9:23)), ordered = TRUE),
      qtr = factor(qtr, levels = as.character(c(1:6)), ordered = TRUE),
      down = factor(down, levels = as.character(c(1:4)), ordered = TRUE),
      goal_to_go = factor(goal_to_go, levels = as.character(c(0,1))),
      shotgun = factor(shotgun, levels = as.character(c(0,1))),
      no_huddle = factor(no_huddle, levels = as.character(c(0,1))),
      two_point_attempt = factor(two_point_attempt, levels = as.character(c(0,1))),
      qb_dropback = factor(qb_dropback, levels = as.character(c(0,1))),
      qb_scramble = factor(qb_scramble, levels = as.character(c(0,1))),
      first_down = factor(first_down, levels = as.character(c(0,1)))
    )
  return(pbp)
}

#' @keywords internal
.preprocess_rush_df <- function(prep_pbp, rosters){

  rush_df <- prep_pbp %>%
    dplyr::filter(play_type == "run", !grepl("kneel|Aborted", desc)) %>%
    dplyr::left_join(rosters, by = c("fantasy_player_id" = "gsis_id", "season"),
                     na_matches = "never") %>%
    # dplyr::filter(position %in% c("QB","RB","WR","TE")) %>%
    dplyr::mutate(

      #Data Cleaning
      score = dplyr::if_else(rush_touchdown == 1 | two_point_converted == 1, 1, 0),
      rushing_yards = dplyr::case_when(
        is.na(rushing_yards) & two_point_attempt == 1 & two_point_converted == 1 ~ yardline_100,
        is.na(rushing_yards) & two_point_attempt == 1 & two_point_converted == 0 ~ 0,
        TRUE ~ rushing_yards),
      run_location = dplyr::case_when(!is.na(run_location) ~ run_location,
                                      grepl(" left", desc) ~ "left",
                                      grepl(" right", desc) ~ "right",
                                      grepl(" middle", desc) ~ "middle",
                                      TRUE ~ "unk"),
      run_gap = dplyr::case_when(!is.na(run_gap) ~ run_gap,
                                 run_location == "middle" ~ "guard",
                                 grepl(" end", desc) ~ "end",
                                 grepl(" tackle", desc) ~ "tackle",
                                 grepl(" guard", desc) ~ "guard",
                                 grepl(" middle", desc) ~ "guard",
                                 TRUE ~ "unk"),
      run_gap_dir = paste(run_location, run_gap, sep = "_"),

      #Create New Fields
      rusher_age = .get_age(birth_date, game_date, dec = TRUE),
      rushing_fantasy_points = 6*rush_touchdown + 2*two_point_converted + 0.1*rushing_yards - 2*fumble_lost,

      #Set ordered factor variables
      season = factor(season, levels = as.character(c(2001:2020)), ordered = TRUE),

      score = factor(score, levels = as.character(c(0,1))))
  # dplyr::filter(run_gap_dir %in% c("left_end", "left_tackle", "left_guard", "middle_guard",
  #                                  "right_guard", "right_tackle", "right_end"))

  return(rush_df)

}

#' @keywords internal
.preprocess_pass_df <- function(prep_pbp, rosters){

  pass_df <- prep_pbp %>%
    dplyr::filter(play_type == "pass", !grepl("Aborted", desc)) %>%
    dplyr::left_join(rosters, by = c("passer_player_id" = "gsis_id", "season"),
                     na_matches = "never") %>%
    dplyr::rename(passer_position = position, passer_birth_date = birth_date) %>%
    dplyr::left_join(rosters, by = c("receiver_player_id" = "gsis_id", "season"),
                     na_matches = "never") %>%
    dplyr::rename(receiver_position = position, receiver_birth_date = birth_date) %>%

    # dplyr::filter(passer_position %in% c("QB","RB","WR","TE")) %>%
    # dplyr::filter(receiver_position %in% c("QB","RB","WR","TE")) %>%
    dplyr::mutate(

      #Data Cleaning
      score = dplyr::if_else(pass_touchdown == 1 | two_point_converted == 1, 1, 0),
      receiving_yards = dplyr::case_when(
        is.na(receiving_yards) & two_point_attempt == 1 & two_point_converted == 1 ~ yardline_100,
        is.na(receiving_yards) & two_point_attempt == 1 & two_point_converted == 0 ~ 0,
        complete_pass == 0 ~ 0,
        TRUE ~ receiving_yards),
      air_yards = dplyr::if_else(two_point_attempt == 1, yardline_100, air_yards),
      complete_pass = dplyr::if_else(two_point_attempt == 1 & grepl("is complete", desc), 1, complete_pass),
      pass_complete = dplyr::if_else(complete_pass == 1, "complete", "incomplete"),
      xpass = dplyr::if_else(two_point_attempt == 1, 0.75, xpass),
      distance_to_sticks = air_yards - ydstogo,
      pass_location = dplyr::case_when(
        !is.na(pass_location) ~ pass_location,
        grepl(" left", desc) ~ "left",
        grepl(" right", desc) ~ "right",
        grepl(" middle", desc) ~ "middle",
        TRUE ~ "unk"),

      #Create New Fields
      passer_age = .get_age(passer_birth_date, game_date, dec = TRUE),
      receiver_age = .get_age(receiver_birth_date, game_date, dec = TRUE),
      passer_position = dplyr::if_else(passer_position != "QB", "non-QB", passer_position),
      receiving_fantasy_points = 6*pass_touchdown + 2*two_point_converted  + 0.1*receiving_yards - 2*fumble_lost + complete_pass,
      passing_fantasy_points =  4*pass_touchdown + 2*two_point_converted  + 0.04*receiving_yards - 2*fumble_lost - 2*interception,

      #Set ordered factor variables
      season = factor(season, levels = as.character(c(2001:2020)), ordered = TRUE),
      score = factor(score, levels = as.character(c(0,1))),
      pass_complete = factor(pass_complete, levels = c("complete", "incomplete"), ordered = TRUE),
      interception = factor(interception, levels = as.character(c(0,1))),
      qb_hit = factor(qb_hit, levels = as.character(c(0,1))))
  # dplyr::filter(!is.na(air_yards))

  return(pass_df)
}
