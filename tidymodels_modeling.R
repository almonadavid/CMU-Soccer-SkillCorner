library(tidyverse)
library(data.table)
library(ranger)
library(tidymodels)
library(tictoc)


#### Load Data ####
pressing_data <- fread("results/all_games_pressing_sequences.csv") |>
  filter(
    between(ball_carrier_x, -52.5, 52.5),
    between(ball_carrier_y, -34, 34)
  ) |> 
  select(-match_id:-player_in_possession_name, -game_id, -period, -is_home) |>
  mutate(
    start_type = case_when(
      str_detect(start_type, "interception") ~ "interception",
      str_detect(start_type, "reception") ~ "reception",
      start_type == "keep_possession" ~ "keep_possession",
      start_type == "recovery" ~ "recovery",
      TRUE ~ "unknown"
    )
  ) |> 
  mutate_if(is.character, as.factor) |> 
  mutate_if(is.logical, as.factor)


# Create training and testing sets
set.seed(123)
pressing_split <- initial_split(pressing_data, strata = forced_turnover_within_5s)
pressing_train <- training(pressing_split)
pressing_test <- testing(pressing_split)


# Recipe
pressing_rec <- recipe(forced_turnover_within_5s ~ ., data = pressing_train) |>
  step_unknown(all_nominal_predictors(), new_level = "missing") |>
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors())

# Folds
pressing_folds <- vfold_cv(pressing_train, v = 10, strata = forced_turnover_within_5s)

pressing_folds

#### XGBOOST ###################################################################################################

xgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(), 
  min_n = tune(), 
  loss_reduction = tune(),
  sample_size = tune(), 
  mtry = tune(), 
  learn_rate = tune()
) |> 
  set_engine("xgboost", 
             nthread = parallel::detectCores(),
             early_stopping_rounds = 10,
             validation = 0.2) |>
  set_mode("classification")

#xgb_spec
#_______________________________________

xgb_grid <- grid_space_filling(
  trees(range = c(100, 1000)),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), pressing_train),
  learn_rate(),
  size = 20
  
)

#xgb_grid
#______________________________________

xgb_wf <- workflow() |> 
  add_recipe(pressing_rec) |> 
  add_model(xgb_spec)

#xgb_wf
#_____________________________________

library(future)
plan(multisession, workers = parallel::detectCores() - 1)

set.seed(456)
tic("XGBoost Tuning")
xgb_res <- tune_race_anova(
  xgb_wf,
  resamples = pressing_folds,
  grid = xgb_grid,
  control = control_race(
    save_pred = TRUE,
    verbose_elim = TRUE)
)
#xgb_res
toc()
gc()
#__________________________________________

#### Explore Results ####
xgb_res |> 
  collect_metrics() |> 
  filter(.metric == "roc_auc") |> 
  select(mean, mtry:sample_size) |> 
  pivot_longer(mtry:sample_size, 
               names_to = "parameter",
               values_to = "value") |> 
  ggplot(aes(value, mean, color = parameter)) +
  geom_point(show.legend = FALSE) +
  facet_wrap(~parameter, scales = "free_x")
#____________________________________________

show_best(xgb_res, metric = "roc_auc")

best_xgb <- select_best(xgb_res, metric = "roc_auc")

final_xgb <- finalize_workflow(xgb_wf, best_xgb)
#final_xgb
#____________________________________

library(vip)
final_xgb |> 
  fit(data = pressing_train) |> 
  extract_fit_parsnip() |> 
  vip()
#___________________________________

final_xgb_res <- last_fit(final_xgb, pressing_split)

final_xgb_res |> 
  collect_metrics() 
#__________________________________

# confusion matrix
final_xgb_res |> 
  collect_predictions() |> 
  conf_mat(forced_turnover_within_5s, .pred_class)

# roc curve
final_xgb_res |> 
  collect_predictions() |> 
  roc_curve(forced_turnover_within_5s, .pred_FALSE) |> 
  autoplot()


#### LIGHT GBM #############################################################
library(bonsai)

# Model specification
lgb_spec <- boost_tree(
  trees = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  learn_rate = tune(),
  mtry = tune(),
  sample_size = tune()
) |> 
  set_engine("lightgbm",
             num_threads = parallel::detectCores(),
             early_stopping_round = 10,
             validation = 0.2) |>
  set_mode("classification")

#lgb_spec
#_____________________________________________________

#hyperparameter grid
lgb_grid <- grid_space_filling(
  trees(),
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), pressing_train),
  learn_rate(),
  size = 20
)

#lgb_grid

#___________________________________________________

lgb_wf <- workflow() |> 
  add_recipe(pressing_rec) |> 
  add_model(lgb_spec)

#lgb_wf
#___________________________________________________
library(future)
plan(multisession, workers = parallel::detectCores() - 1)

set.seed(456)
tic("LightGBM Tuning")
lgb_res <- tune_race_anova(
  lgb_wf,
  resamples = pressing_folds,
  grid = lgb_grid,
  control = control_race(
    save_pred = TRUE,
    verbose_elim = TRUE)
)
toc()
#lgb_res

#__________________________________________________

show_best(lgb_res, metric = "roc_auc")

best_lgb <- select_best(lgb_res, metric = "roc_auc")

final_lgb <- finalize_workflow(lgb_wf, best_lgb)
#final_lgb
#____________________________________

library(vip)
final_lgb |> 
  fit(data = pressing_train) |> 
  extract_fit_parsnip() |> 
  vip()
#___________________________________

final_lgb_res <- last_fit(final_lgb, pressing_split)

final_lgb_res |> 
  collect_metrics() 
#__________________________________

# confusion matrix
final_lgb_res |> 
  collect_predictions() |> 
  conf_mat(forced_turnover_within_5s, .pred_class)

# roc curve
final_lgb_res |> 
  collect_predictions() |> 
  roc_curve(forced_turnover_within_5s, .pred_FALSE) |> 
  autoplot()

#### SAVE ######################################################################

dir.create("model_results", showWarnings = FALSE)

# final model results
saveRDS(final_xgb_res, "model_results/final_xgb_res.rds")
saveRDS(final_lgb_res, "model_results/final_lgb_res.rds")

# tuning results
saveRDS(xgb_res, "model_results/xgb_tuning_res.rds")
saveRDS(lgb_res, "model_results/lgb_tuning_res.rds")

# hyperparameters
saveRDS(best_xgb, "model_results/best_xgb_params.rds")
saveRDS(best_lgb, "model_results/best_lgb_params.rds")

# training/test data
saveRDS(pressing_split, "model_results/data_split.rds")

#### MODEL COMPARISON ##########################################################

model_comparison <- bind_rows(
  final_xgb_res |> 
    collect_metrics() |> 
    mutate(model = "XGBoost"),
  final_lgb_res |> 
    collect_metrics() |> 
    mutate(model = "LightGBM")
)

model_comparison |> 
  select(model, .metric, .estimate) |> 
  pivot_wider(names_from = .metric, values_from = .estimate) |> 
  arrange(desc(roc_auc))


#### COMBINED ROC CURVE ########################################################

all_predictions <- bind_rows(
  final_xgb_res |> 
    collect_predictions() |> 
    mutate(model = "XGBoost"),
  final_lgb_res |> 
    collect_predictions() |> 
    mutate(model = "LightGBM")
)


all_predictions |> 
  group_by(model) |> 
  roc_curve(forced_turnover_within_5s, .pred_FALSE) |> 
  autoplot(size = 15) +
  labs(
    title = "ROC Curves: Model Comparison",
    subtitle = "Pressing Sequence Turnover Prediction"
  ) +
  theme_light()



