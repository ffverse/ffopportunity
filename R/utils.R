#### UTILS ###
# External functions imported and sometimes re-exported

#' Calculate age
#'
#' @param from_date from date
#' @param to_date to date
#' @param dec decimal age
#' @export
.get_age <- function(from_date,to_date = lubridate::now(),dec = FALSE){
  if(is.character(from_date)) from_date <- lubridate::as_date(from_date)
  if(is.character(to_date))   to_date   <- lubridate::as_date(to_date)
  if (dec) { age <- lubridate::interval(start = from_date, end = to_date)/(lubridate::days(365)+lubridate::hours(6))
  } else   { age <- lubridate::year(lubridate::as.period(lubridate::interval(start = from_date, end = to_date)))}
  round(age,2)
}


#' Pull `nflreadr` rosters
#'
#' @param seasons choose season or range of seasons
#' @export

.get_rosters <- function(seasons){
  nflreadr::load_rosters(seasons) %>%
    dplyr::transmute(season,
                     gsis_id,
                     position = dplyr::if_else(position %in% c("HB","FB"), "RB", position),
                     birth_date)
}
