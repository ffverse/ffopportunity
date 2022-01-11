library(dpylr)
library(DALEXtra)
library(here)
library(tidymodels)

setwd(here())

rush_yards_tidymodel <- readRDS("./modelling/fit_rush_yards.RDS")

preprocessed <- ffopportunity::ep_preprocess(nflreadr::load_pbp(2006:2021))

debug(DALEXtra::explain_tidymodels)
rush_yards_explainer <-
  DALEXtra::explain_tidymodels(
    rush_yards_tidymodel,
    data = dplyr::select(preprocessed$rush_df, -rushing_yards),
    y =  preprocessed$rush_df$rushing_yards)

plot(feature_importance(rush_yards_explainer))

pdp_time <-
  model_profile(
    rush_yards_explainer,
    variables = "rusher_age",
    groups = "position"
  )

plot(pdp_time)


# testing xgb dalex -------------------------------------------------------

.load_model_objs <- function(variable, version) {

  cache_dir <- rappdirs::user_cache_dir("ffopportunity", "ffverse")

  folder_path <- file.path(cache_dir,version)

  model_path <- file.path(folder_path, paste0(variable,".xgb"))
  blueprint_path <- file.path(folder_path, paste0(variable,".rds"))

  stopifnot(file.exists(model_path), file.exists(blueprint_path))

  model <- xgboost::xgb.load(model_path)
  blueprint <- readRDS(blueprint_path)

  return(list(model = model, blueprint = blueprint))
}



model_obj <- .load_model_objs("rushing_yards", version = "1.0.0")

model_obj$model$params$objective <- "reg"

preprocessed_pbp <- ep_preprocess(nflreadr::load_pbp(2021))

rush_df <-
  preprocessed_pbp$rush_df %>%
  hardhat::forge(new_data = ., blueprint = model_obj$blueprint)

ep_load <- rush_df$predictors %>%
  dplyr::mutate(rushing_yards = preprocessed_pbp$rush_df$rushing_yards) %>%
  as.matrix()

ep_load <- load_ep_pbp_rush(2018:2021) %>%
  as.matrix()

rush_yards_explainer <-
  DALEXtra::explain_xgboost(
    model = model_obj$model,
    data = ep_load,
    y =  ep_load[,14])
