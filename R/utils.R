#### UTILS ###
# External functions imported and sometimes re-exported
#' Pull `nflreadr` rosters
#' @param seasons choose season or range of seasons
#' @export
#' @keywords internal
.get_rosters <- function(seasons){
  nflreadr::load_rosters(seasons) %>%
    dplyr::transmute(.data$season,
                     .data$gsis_id,
                     .data$full_name,
                     position = dplyr::if_else(.data$position %in% c("HB","FB"), "RB", .data$position),
                     .data$birth_date) %>%
    dplyr::distinct()
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

#' @keywords internal
vcli_alert <- function(...){
  v <- getOption("ffopportunity.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_alert(...,.envir = parent.frame())
}

#' @keywords internal
vcli_rule <- function(...){
  v <- getOption("ffopportunity.verbose", default = TRUE)

  if(!v) return(NULL)

  cli::cli_rule(..., .envir = parent.frame())
}

#' Load .rds file from a remote connection
#'
#' @param url a character url
#'
#' @return a dataframe as created by [`readRDS()`]
#'
#' @examples
#' \donttest{
#' try({ rds_from_url("https://github.com/nflverse/nfldata/raw/master/data/games.rds") })
#' }
#' @keywords internal
rds_from_url <- function(url) {
  con <- url(url)
  on.exit(close(con))
  load <- try(readRDS(con), silent = TRUE)

  if (inherits(load, "try-error")) {
    warning(paste0("Failed to readRDS from <", url, ">"), call. = FALSE)
    return(tibble::tibble())
  }

  return(tibble::tibble(load))
}

#' @keywords internal
is_installed <- function(pkg) requireNamespace(pkg, quietly = TRUE)

utils::globalVariables("where")

