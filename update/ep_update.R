pkgload::load_all()

try(piggyback::pb_new_release(repo = "ffverse/ffexpectedpoints", tag = "latest-data"))
try(piggyback::pb_new_release(repo = "ffverse/ffexpectedpoints", tag = "v1.0.0-data"))

save_ep_data <- function(season){

  ep_object <- ffexpectedpoints::ep_build(season)

  temp_dir <- tempdir()
  on.exit(unlink(temp_dir,recursive = TRUE,force = TRUE))
  # rds
  saveRDS(ep_object$ep_weekly, file.path(temp_dir, glue::glue('ep_weekly_{season}.rds')))
  saveRDS(ep_object$ep_pbp_rush, file.path(temp_dir, glue::glue('ep_pbp_rush_{season}.rds')))
  saveRDS(ep_object$ep_pbp_pass, file.path(temp_dir, glue::glue('ep_pbp_pass_{season}.rds')))

  # csv
  readr::write_csv(ep_object$ep_weekly, file = file.path(temp_dir, glue::glue('ep_weekly_{season}.csv')))
  readr::write_csv(ep_object$ep_pbp_rush, file = file.path(temp_dir, glue::glue('ep_pbp_rush_{season}.csv')))
  readr::write_csv(ep_object$ep_pbp_pass, file = file.path(temp_dir, glue::glue('ep_pbp_pass_{season}.csv')))

  # .parquet
  arrow::write_parquet(ep_object$ep_weekly, file.path(temp_dir, glue::glue('ep_weekly_{season}.parquet')))
  arrow::write_parquet(ep_object$ep_pbp_rush, file.path(temp_dir, glue::glue('ep_pbp_rush_{season}.parquet')))
  arrow::write_parquet(ep_object$ep_pbp_pass, file.path(temp_dir, glue::glue('ep_pbp_pass_{season}.parquet')))

  writeLines(as.character(Sys.time()), file.path(temp_dir,"timestamp.txt"))

  list.files(temp_dir, pattern = "csv$|rds$|parquet$|txt$", full.names = TRUE) %>%
    purrr::walk(piggyback::pb_upload, repo = "ffverse/ffexpectedpoints", tag = "latest-data", overwrite = TRUE) %>%
    purrr::walk(piggyback::pb_upload, repo = "ffverse/ffexpectedpoints", tag = "v1.0.0-data", overwrite = TRUE)

  cli::cli_alert_success("Completed {season} at {Sys.time()}!")
}

purrr::walk(2006:2021, save_ep_data)
