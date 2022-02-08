# version <- "v1.0.0"
# folder_path <- tempdir()
# wd <- setwd(here::here("models"))
#
# zip(file.path(folder_path,paste0(version,".zip")),files = file.path(version))
# zip(file.path(folder_path,"latest.zip"), files = file.path(version))
#
# try(piggyback::pb_new_release(repo = "ffverse/ffopportunity",tag = paste0(version, "-model")))
# piggyback::pb_upload(file = file.path(folder_path,paste0(version,".zip")),
#                      repo = "ffverse/ffopportunity",
#                      tag = paste0(version, "-model"))
#
# try(piggyback::pb_new_release(repo = "ffverse/ffopportunity",tag = "latest-model"))
# piggyback::pb_upload(file = file.path(folder_path,"latest.zip"),
#                      repo = "ffverse/ffopportunity",
#                      tag = "latest-model")
#
# setwd(wd)
