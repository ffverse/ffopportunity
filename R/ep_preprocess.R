#' Preprocess Data
#'
#' This function performs the necesssary pre-processing steps to make
#' expected points predictions on `nflreadr` data
#'
#' @param pbp pbp dataframe from `nflreadr::load_pbp()`
#'
#' @examples
#' \donttest{
#' try({ # catch failures for CRAN purposes
#'   nflreadr::load_pbp(2021) %>%
#'   head(100) %>%
#'   ep_preprocess()
#'   })
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

  rosters <- .get_rosters(unique(pbp$season))

  prep_pbp <- .preprocess_common_fields(pbp)

  list_df <- list(rush_df = .preprocess_rush_df(prep_pbp, rosters),
                  pass_df = .preprocess_pass_df(prep_pbp, rosters))

  return(list_df)
}

#' @keywords internal
.preprocess_common_fields <- function(pbp){

  pbp <- pbp %>%
    dplyr::mutate(

      # New Calculated Columns
      implied_total = dplyr::case_when(
        .data$posteam_type == "away" & .data$spread_line<=0 ~
          (.data$total_line+.data$spread_line)/2 - .data$spread_line,
        .data$posteam_type == "away" & .data$spread_line>0 ~
          (.data$total_line-.data$spread_line)/2,
        .data$posteam_type == "home" & .data$spread_line>0 ~
          (.data$total_line+.data$spread_line)/2 - .data$spread_line,
        .data$posteam_type == "home" & .data$spread_line<=0 ~
          (.data$total_line-.data$spread_line)/2),

      # New Categorical Columns
      surface = dplyr::if_else(.data$surface == "grass", "grass", "turf"),
      roof = dplyr::if_else(
        .data$roof %in% c("dome","closed"), "indoors", "outdoors"),
      temp = dplyr::case_when(.data$roof %in% c("closed", "dome") ~ 68L,
                              is.na(.data$temp) ~ 60L,
                              TRUE ~ .data$temp),
      wind = dplyr::case_when(.data$roof %in% c("closed", "dome") ~ 0L,
                              is.na(.data$wind) ~ 8L,
                              TRUE ~ .data$wind),
      era = dplyr::if_else(.data$season >= 2018, "post2018", "pre2018"),

      # Cleaning 2pt attempts
      down = dplyr::if_else(.data$two_point_attempt == 1, 4, .data$down),
      rushing_yards = dplyr::if_else(.data$two_point_attempt == 1,
                                     0, .data$rushing_yards),
      xpass = dplyr::if_else(.data$two_point_attempt == 1, 0.75, .data$xpass),
      pass_location = dplyr::case_when(
        !is.na(.data$pass_location) ~ .data$pass_location,
        stringr::str_detect(.data$desc, " left") ~ "left",
        stringr::str_detect(.data$desc, " right") ~ "right",
        stringr::str_detect(.data$desc, " middle") ~ "middle",
        TRUE ~ "unk"),
      yards_after_catch = dplyr::if_else(
        .data$two_point_attempt == 1,0, .data$xpass),
      air_yards = dplyr::if_else(
        .data$two_point_attempt == 1, .data$yardline_100, .data$air_yards),
      two_point_converted = dplyr::case_when(
        .data$two_point_conv_result == "success" ~ 1,
        is.na(.data$two_point_conv_result) &
          stringr::str_detect(.data$desc, "ATTEMPT SUCCEEDS") ~ 1,
        TRUE ~ 0),

      # Categorical Variables
      dplyr::across(
        .cols = c(.data$goal_to_go,
                  .data$shotgun,
                  .data$no_huddle,
                  .data$qb_hit,
                  .data$down,
                  .data$qtr,
                  .data$qb_dropback,
                  .data$qb_scramble),
        .fns = as.factor)
    )
  return(pbp)
}

#' @keywords internal
.preprocess_rush_df <- function(prep_pbp, rosters){

  rush_df <- prep_pbp %>%
    dplyr::filter(.data$play_type == "run",
                  !grepl("kneel|Aborted", .data$desc)) %>%
    dplyr::left_join(rosters, by = c("rusher_player_id" = "gsis_id", "season"),
                     na_matches = "never") %>%
    dplyr::mutate(

      # Categorical Variables
      run_location = dplyr::case_when(
        !is.na(.data$run_location) ~ .data$run_location,
        stringr::str_detect(.data$desc, " left") ~ "left",
        stringr::str_detect(.data$desc, " right") ~ "right",
        stringr::str_detect(.data$desc, " middle") ~ "middle",
        TRUE ~ "unk"),
      run_gap = dplyr::case_when(
        !is.na(.data$run_gap) ~ .data$run_gap,
        .data$run_location == "middle" ~ "guard",
        stringr::str_detect(.data$desc, " end") ~ "end",
        stringr::str_detect(.data$desc, " tackle") ~ "tackle",
        stringr::str_detect(.data$desc, " guard") ~ "guard",
        stringr::str_detect(.data$desc, " middle") ~ "guard",
        TRUE ~ "unk"),
      run_gap_dir = paste(.data$run_location, .data$run_gap, sep = "_"),

      # Outcome Variables
      rush_touchdown = factor(
        dplyr::if_else(.data$rush_touchdown == 1, "1", "0"),
        levels = c("1","0")),
      first_down = factor(
        dplyr::if_else(.data$first_down == 1, "1", "0"),
        levels = c("1","0"))) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$desc,
      .data$rusher_player_id,
      .data$full_name,
      .data$posteam,
      .data$two_point_attempt,
      .data$two_point_converted,
      .data$rush_attempt,
      .data$first_down_rush,
      .data$fumble_lost,
      .data$season,
      .data$week,
      .data$rushing_yards,
      .data$rush_touchdown,
      .data$first_down,
      .data$posteam_type,
      .data$run_location,
      .data$run_gap,
      .data$run_gap_dir,
      .data$surface,
      .data$wind,
      .data$temp,
      .data$roof,
      .data$position,
      .data$yardline_100,
      .data$half_seconds_remaining,
      .data$game_seconds_remaining,
      .data$fixed_drive,
      .data$era,
      .data$xpass,
      .data$qtr,
      .data$down,
      .data$goal_to_go,
      .data$ydstogo,
      .data$shotgun,
      .data$no_huddle,
      .data$qb_dropback,
      .data$qb_scramble,
      .data$score_differential,
      .data$ep,
      .data$vegas_wp,
      .data$implied_total
    )
  return(rush_df)
}

#' @keywords internal
.preprocess_pass_df <- function(prep_pbp, rosters){

  pass_df <- prep_pbp %>%
    dplyr::filter(.data$play_type == "pass", !grepl("Aborted", .data$desc)) %>%
    dplyr::left_join(rosters, by = c("passer_player_id" = "gsis_id", "season"),
                     na_matches = "never") %>%
    dplyr::rename(passer_position = .data$position,
                  passer_birth_date = .data$birth_date,
                  passer_full_name = .data$full_name) %>%
    dplyr::left_join(rosters,by = c("receiver_player_id" = "gsis_id", "season"),
                     na_matches = "never") %>%
    dplyr::rename(receiver_position = .data$position,
                  receiver_birth_date = .data$birth_date,
                  receiver_full_name = .data$full_name) %>%
    dplyr::mutate(

      # New Calculated Columns
      relative_to_sticks = .data$air_yards - .data$ydstogo,
      relative_to_endzone = .data$air_yards - .data$yardline_100,

      # Categorical Variables
      complete_pass = factor(
        dplyr::if_else(.data$complete_pass == 1, "1", "0"),
        levels = c("1","0")),
      pass_touchdown = factor(
        dplyr::if_else(.data$pass_touchdown == 1, "1", "0"),
        levels = c("1","0")),
      first_down = factor(
        dplyr::if_else(.data$first_down == 1, "1", "0"),
        levels = c("1","0")),
      interception = factor(
        dplyr::if_else(.data$interception == 1, "1", "0"),
        levels = c("1","0"))) %>%
    dplyr::filter(!is.na(.data$air_yards)) %>%
    dplyr::select(
      .data$game_id,
      .data$play_id,
      .data$desc,
      .data$passer_player_id,
      .data$passer_full_name,
      .data$passer_position,
      .data$receiver_player_id,
      .data$receiver_full_name,
      .data$receiver_position,
      .data$posteam,
      .data$two_point_attempt,
      .data$two_point_converted,
      .data$pass_attempt,
      .data$receiving_yards,
      .data$first_down_pass,
      .data$fumble_lost,
      .data$season,
      .data$week,
      .data$season,
      .data$complete_pass,
      .data$yards_after_catch,
      .data$pass_touchdown,
      .data$first_down,
      .data$interception,
      .data$relative_to_endzone,
      .data$wind,
      .data$score_differential,
      .data$xpass,
      .data$vegas_wp,
      .data$total_line,
      .data$implied_total,
      .data$relative_to_sticks,
      .data$air_yards,
      .data$yardline_100,
      .data$half_seconds_remaining,
      .data$game_seconds_remaining,
      .data$ep,
      .data$fixed_drive,
      .data$ydstogo,
      .data$temp,
      .data$era,
      .data$qb_hit,
      .data$posteam_type,
      .data$pass_location,
      .data$surface,
      .data$roof,
      .data$passer_position,
      .data$receiver_position,
      .data$qtr,
      .data$down,
      .data$goal_to_go,
      .data$shotgun,
      .data$no_huddle
    )
  return(pass_df)
}
