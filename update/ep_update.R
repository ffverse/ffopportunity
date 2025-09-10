pkgload::load_all()

save_ep_data <- function(season, folder_path, version){

  ep_object <- ffopportunity::ep_build(season,version = version)

  # rds
  saveRDS(ep_object$ep_weekly, file.path(folder_path, glue::glue('ep_weekly_{season}.rds')))
  saveRDS(ep_object$ep_pbp_rush, file.path(folder_path, glue::glue('ep_pbp_rush_{season}.rds')))
  saveRDS(ep_object$ep_pbp_pass, file.path(folder_path, glue::glue('ep_pbp_pass_{season}.rds')))

  # csv
  readr::write_csv(ep_object$ep_weekly, file = file.path(folder_path, glue::glue('ep_weekly_{season}.csv')))
  readr::write_csv(ep_object$ep_pbp_rush, file = file.path(folder_path, glue::glue('ep_pbp_rush_{season}.csv')))
  readr::write_csv(ep_object$ep_pbp_pass, file = file.path(folder_path, glue::glue('ep_pbp_pass_{season}.csv')))

  # .parquet
  arrow::write_parquet(ep_object$ep_weekly, file.path(folder_path, glue::glue('ep_weekly_{season}.parquet')))
  arrow::write_parquet(ep_object$ep_pbp_rush, file.path(folder_path, glue::glue('ep_pbp_rush_{season}.parquet')))
  arrow::write_parquet(ep_object$ep_pbp_pass, file.path(folder_path, glue::glue('ep_pbp_pass_{season}.parquet')))

  writeLines(as.character(Sys.time()), file.path(folder_path,"timestamp.txt"))
  writeLines(as.character(version), file.path(folder_path,"version.txt"))
}

upload_ep_data <- function(folder_path, version){
  list.files(folder_path, pattern = "csv$|rds$|parquet$|txt$", full.names = TRUE) %>%
    piggyback::pb_upload(repo = "ffverse/ffopportunity", tag = "latest-data", overwrite = TRUE)

  list.files(folder_path, pattern = "csv$|rds$|parquet$|txt$", full.names = TRUE) %>%
    piggyback::pb_upload(repo = "ffverse/ffopportunity", tag = glue::glue("{version}-data"), overwrite = TRUE)

  cli::cli_alert_success("Completed ep upload! {Sys.time()}")
}

update_ep <- function(season, version = "v1.0.0", folder_path){
  purrr::walk(season, save_ep_data, folder_path = folder_path, version = version)
  upload_ep_data(folder_path, version)
  invisible(NULL)
}

version <- "v1.0.0"
temp <- file.path(tempdir(), "build")
dir.create(temp, showWarnings = FALSE, recursive = TRUE)

try(piggyback::pb_new_release(repo = "ffverse/ffopportunity", tag = "latest-data"))
try(piggyback::pb_new_release(repo = "ffverse/ffopportunity", tag = glue::glue("{version}-data")))

update_ep(nflreadr::most_recent_season(), version = "latest", folder_path = temp)
update_ep(nflreadr::most_recent_season(), version = "v1.0.0", folder_path = temp)

unlink(temp, recursive = TRUE, force = TRUE)
