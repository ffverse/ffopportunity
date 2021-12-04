#### UTILS ###
# External functions imported and sometimes re-exported

#' Pull `nflreadr` rosters
#'
#' @param seasons choose season or range of seasons
#' @export

.get_rosters <- function(seasons){
  nflreadr::load_rosters(seasons) %>%
    dplyr::transmute(season,
                     gsis_id,
                     full_name,
                     position = dplyr::if_else(position %in% c("HB","FB"), "RB", position),
                     birth_date)
}
