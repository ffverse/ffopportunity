library(DALEXtra)
library(here)
library(tidymodels)
library(tidyverse)
library(ragg)

setwd(here())

source("./modelling/dalex_plot_helpers.R")

rush_yards_tidymodel <- readRDS("./modelling/fit_rush_yards.RDS")

preprocessed <- ffopportunity::ep_preprocess(nflreadr::load_pbp(2021:2021))

rush_yards_explainer <-
  DALEXtra::explain_tidymodels(
    rush_yards_tidymodel,
    data = dplyr::select(preprocessed$rush_df, -rushing_yards),
    y =  preprocessed$rush_df$rushing_yards)

mod_parts <- DALEX::model_parts(rush_yards_explainer)

mod_parts %>%
  group_by(variable) %>%
  summarise(mean_dropout_loss = mean(dropout_loss, na.rm = TRUE)) %>%
  ungroup() %>%
  view()

agg_png('./vignettes/plots/rush_yards_feat_imp.png', width = 1000, height = 592)
mod_parts %>%
  filter(variable %in% c("_baseline_", "_full_model_", "yardline_100", "run_gap",
                         "xpass", "position", "qb_dropback", "ydstogo", "half_seconds_remaining",
                         "vegas_wp", "game_seconds_remaining", "implied_total")) |>
  ggplot_imp() +
  tantastic::theme_uv() +
  theme(legend.position = "none",
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.title.x = element_text(size = unit(10,"pt")),
        plot.subtitle = element_text(size = unit(10,"pt"))) +
  scale_x_continuous(expand = c(0.1,0.1)) +
  labs(
    title = "Feature Importance for Expected Rushing Yards",
    subtitle = "Distance to Endzone, Position, Expected Pass Rate, and Run Gap are the most important factors"
  )
dev.off()

pdp_yds <- DALEX::model_profile(
  rush_yards_explainer,
  groups = "run_gap",
  N = 500,
  variables = "yardline_100")

agg_png('./vignettes/plots/rush_yards_pdp_yards.png', width = 1000, height = 592)
ggplot_pdp(pdp_yds, yardline_100) +
  tantastic::theme_uv() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        plot.subtitle = element_text(size = unit(12,"pt"))) +
  labs(title = "Distance to Endzone",
       subtitle = "Outside rushes have higher expected yardage until you get inside the 5",
       x = "Yards from End Zone",
       y = "Expected Yardage",
       color = "Run Gap") +
  scale_x_continuous(breaks = seq(0,100,10), trans = "reverse") +
  scale_y_continuous(breaks = seq(0,10,2))
dev.off()

pdp_xpass <- DALEX::model_profile(
  rush_yards_explainer,
  groups = "position",
  N = 500,
  variables = "xpass")

agg_png('./vignettes/plots/rush_yards_pdp_xpass.png', width = 1000, height = 592)
ggplot_pdp(pdp_xpass, xpass) +
  tantastic::theme_uv() +
  theme(plot.title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
        panel.grid.minor = element_blank()
        ) +
  labs(title = "How do Expected Yards change with likelihood to pass?",
       subtitle = "All positions can expect higher rushing yards when the offense is more likely to pass",
       x = "Expected Pass Rate",
       y = "Expected Yardage",
       color = "Position") +
  scale_x_continuous(breaks = seq(0,1,.2),labels = percent) +
  scale_y_continuous(breaks = seq(0,10,2))
dev.off()

example_rush <- DALEX::predict_parts(
  rush_yards_explainer,
  new_observation =
    preprocessed$rush_df %>%
    mutate(across(where(is.numeric), ~round(.x,3))) %>%
    filter(play_id == 3553, game_id == "2021_18_SEA_ARI"))

# new_observation =
#   preprocessed$rush_df %>%
#   mutate(across(where(is.numeric), ~round(.x,3))) %>%
#   slice_sample(n = 1))

agg_svg('./vignettes/plots/rush_yards_breakdown.png', width = 1000, height = 592)
plot(example_rush,
     digits = 2,
     vcolors = c("purple","darkgreen","black"),
     max_features = 10) +
  tantastic::theme_uv() +
  theme(legend.position = "none",
        plot.subtitle = element_text(size = 12),
        panel.grid.major.y = element_line(colour = "#eeeeee"),
        panel.grid.major.x = element_line(colour = "#eeeeee"),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.text = element_blank()) +
  labs(title = "How does each component affect the predicted yards?",
       # subtitle = example_rush %>% filter(variable_name == "desc") %>% pull(variable_value),
       # caption = example_rush %>% filter(variable_name == "game_id") %>% pull(variable_value),
       subtitle = paste(
         example_rush %>% filter(variable_name == "game_id") %>% pull(variable_value),
         example_rush %>% filter(variable_name == "desc") %>% pull(variable_value))) +
  scale_y_continuous(name =  "Expected Yards", breaks = seq(4.2,4.9,0.1), expand = c(0.1,0.1)) +
  scale_color_manual("black")
dev.off()

# model_perf <- DALEX::model_diagnostics(rush_yards_explainer)
#
# plot(model_perf, variable = "y", yvariable = "y_hat") +
#   geom_abline(colour = "red", intercept = 0, slope = 1) +
#   xlim(0,20)
#
# plot(model_perf, variable = "y_hat", yvariable = "residuals")
