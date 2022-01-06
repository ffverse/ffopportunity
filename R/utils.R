#### UTILS ###
# External functions imported and sometimes re-exported
#' Pull `nflreadr` rosters
#' @param seasons choose season or range of seasons
#' @export

.get_rosters <- function(seasons){
  nflreadr::load_rosters(seasons) %>%
    dplyr::transmute(.data$season,
                     .data$gsis_id,
                     .data$full_name,
                     position = dplyr::if_else(.data$position %in% c("HB","FB"), "RB", .data$position),
                     .data$birth_date)
}

#' Tidy eval helpers
#'
#' @description
#'
#' To learn more about tidy eval and how to use these tools, visit
#' \url{https://tidyeval.tidyverse.org} and the
#' \href{https://adv-r.hadley.nz/metaprogramming.html}{Metaprogramming
#' section} of \href{https://adv-r.hadley.nz}{Advanced R}.
#'
#' @md
#' @name tidyeval
#' @keywords internal
#' @importFrom rlang .data .env
#' @aliases .data .env
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

vcli_start <- function(...){
  v <- getOption("ffexpectedpoints.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_process_start(...,.envir = parent.frame(2))
}
vcli_end <- function(...){
  v <- getOption("ffexpectedpoints.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_process_done(..., .envir = parent.frame(2))
}
vcli_rule <- function(...){
  v <- getOption("ffexpectedpoints.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_rule(..., .envir = parent.frame())
}
