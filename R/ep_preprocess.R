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
      # New Calculated Columns
      implied_total = dplyr::case_when(
        posteam_type == "away" & spread_line<=0 ~ (total_line+spread_line)/2 - spread_line,
        posteam_type == "away" & spread_line>0 ~ (total_line-spread_line)/2,
        posteam_type == "home" & spread_line>0 ~ (total_line+spread_line)/2 - spread_line,
        posteam_type == "home" & spread_line<=0 ~ (total_line-spread_line)/2),

      # New Categorical Columns
      surface = dplyr::if_else(surface == "grass", "grass", "turf"),
      roof = dplyr::if_else(roof %in% c("dome","closed"), "indoors", "outdoors"),
      temp = dplyr::case_when(roof %in% c("closed", "dome") ~ 68L, is.na(temp) ~ 60L, TRUE ~ temp),
      wind = dplyr::case_when(roof %in% c("closed", "dome") ~ 0L, is.na(wind) ~ 8L, TRUE ~ wind),
      era = dplyr::if_else(season >= 2018, "post2018", "pre2018"),

      # Cleaning 2pt attempts
      down = dplyr::if_else(two_point_attempt == 1, 4, down),
      rushing_yards = dplyr::if_else(two_point_attempt == 1, 0, rushing_yards),
      xpass = dplyr::if_else(two_point_attempt == 1, 0.75, xpass),
      pass_location = dplyr::case_when(!is.na(pass_location) ~ pass_location,
                                       stringr::str_detect(desc, " left") ~ "left",
                                       stringr::str_detect(desc, " right") ~ "right",
                                       stringr::str_detect(desc, " middle") ~ "middle",
                                       TRUE ~ "unk"),
      yards_after_catch = dplyr::if_else(two_point_attempt == 1, 0, xpass),
      air_yards = dplyr::if_else(two_point_attempt == 1, yardline_100, air_yards),
      two_point_converted = dplyr::case_when(two_point_conv_result == "success" ~ 1,
                                             is.na(two_point_conv_result) & stringr::str_detect(desc, "ATTEMPT SUCCEEDS") ~ 1,
                                             TRUE ~ 0),

      # Categorical Variables
      dplyr::across(
        .cols = c(goal_to_go, shotgun, no_huddle, qb_hit, down, qtr, qb_dropback, qb_scramble),
        .fns = as.factor)

    )
  return(pbp)
}

#' @keywords internal
.preprocess_rush_df <- function(prep_pbp, rosters){

  rush_df <- prep_pbp %>%
    dplyr::filter(play_type == "run", !grepl("kneel|Aborted", desc)) %>%
    dplyr::left_join(rosters, by = c("fantasy_player_id" = "gsis_id", "season"), na_matches = "never") %>%
    # dplyr::rename(rusher_full_name = full_name) %>%
    # dplyr::filter(position %in% c("QB","RB","WR","TE")) %>%
    dplyr::mutate(
      # Categorical Variables
      run_location = dplyr::case_when(
        !is.na(run_location) ~ run_location,
        stringr::str_detect(desc, " left") ~ "left",
        stringr::str_detect(desc, " right") ~ "right",
        stringr::str_detect(desc, " middle") ~ "middle",
        TRUE ~ "unk"),
      run_gap = dplyr::case_when(
        !is.na(run_gap) ~ run_gap,
        run_location == "middle" ~ "guard",
        stringr::str_detect(desc, " end") ~ "end",
        stringr::str_detect(desc, " tackle") ~ "tackle",
        stringr::str_detect(desc, " guard") ~ "guard",
        stringr::str_detect(desc, " middle") ~ "guard",
        TRUE ~ "unk"),
      run_gap_dir = paste(run_location, run_gap, sep = "_"),

      # Outcome Variables
      rush_touchdown = factor(dplyr::if_else(rush_touchdown == 1, "1", "0"), levels = c("1","0")),
      first_down = factor(dplyr::if_else(first_down == 1, "1", "0"), levels = c("1","0"))

      )

  # dplyr::filter(run_gap_dir %in% c("left_end", "left_tackle", "left_guard", "middle_guard",
  #                                  "right_guard", "right_tackle", "right_end"))

  return(rush_df)

}

#' @keywords internal
.preprocess_pass_df <- function(prep_pbp, rosters){

  pass_df <- prep_pbp %>%
    dplyr::filter(play_type == "pass", !grepl("Aborted", desc)) %>%
    dplyr::left_join(rosters, by = c("passer_player_id" = "gsis_id", "season"), na_matches = "never") %>%
    dplyr::rename(passer_position = position, passer_birth_date = birth_date, passer_full_name = full_name) %>%
    dplyr::left_join(rosters, by = c("receiver_player_id" = "gsis_id", "season"), na_matches = "never") %>%
    dplyr::rename(receiver_position = position, receiver_birth_date = birth_date, receiver_full_name = full_name) %>%
    # dplyr::filter(passer_position %in% c("QB","RB","WR","TE")) %>%
    # dplyr::filter(receiver_position %in% c("QB","RB","WR","TE")) %>%
    dplyr::mutate(
      # New Calculated Columns
      relative_to_sticks = air_yards - ydstogo,
      relative_to_endzone = air_yards - yardline_100,

      # Categorical Variables
      complete_pass = factor(dplyr::if_else(complete_pass == 1, "1", "0"), levels = c("1","0")),
      pass_touchdown = factor(dplyr::if_else(pass_touchdown == 1, "1", "0"), levels = c("1","0")),
      first_down = factor(dplyr::if_else(first_down == 1, "1", "0"), levels = c("1","0")),
      interception = factor(dplyr::if_else(interception == 1, "1", "0"), levels = c("1","0"))) %>%
    dplyr::filter(!is.na(air_yards))

  return(pass_df)
}
