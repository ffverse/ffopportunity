library(DALEXtra)
library(here)
library(tidymodels)
library(tidyverse)

setwd(here())

source("./modelling/dalex_plot_helpers.R")

rush_tds_tidymodel <- readRDS("./modelling/fit_rush_tds.RDS")

preprocessed <- ffopportunity::ep_preprocess(nflreadr::load_pbp(2015:2021))

rush_yards_explainer <-
  DALEXtra::explain_tidymodels(
    rush_yards_tidymodel,
    data = dplyr::select(preprocessed$rush_df, -rushing),
    y =  preprocessed$rush_df$rushing_yards)

new_observation <- preprocessed$rush_df %>%
  mutate(across(where(is.numeric), ~round(.x,3))) %>%
  filter(play_id == 3553, game_id == "2021_18_SEA_ARI")

# make a studio for the model
studio <- modelStudio::modelStudio(rush_yards_explainer, new_observation)

save_html(studio, './vignettes/html_modelStudio_files_rushing_yards_modelStudio.html')
