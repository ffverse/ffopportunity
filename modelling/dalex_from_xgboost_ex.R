# testing xgb dalex -------------------------------------------------------
#If you ever want to run these DALEX off of the xgboost instead
model_obj <- ffopportunity:::.load_model_objs("rushing_yards", version = "latest")

model_obj$model$params$objective <- "reg"

preprocessed_pbp <- ep_preprocess(nflreadr::load_pbp(2021))

rush_df <-
  preprocessed_pbp$rush_df %>%
  hardhat::forge(new_data = ., blueprint = model_obj$blueprint)

ep_load <- rush_df$predictors %>%
  as.matrix()

rush_yards_explainer <-
  DALEXtra::explain_xgboost(
    model = model_obj$model,
    data = ep_load,
    # data = preprocessed_pbp$rush_df %>% as.matrix(),
    y = preprocessed_pbp$rush_df$rushing_yards)
