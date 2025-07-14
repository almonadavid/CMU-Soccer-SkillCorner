library(tidyverse)


library(stacks)
library(xgboost)
library(caret)
library(glmnet)

library(tictoc)


#### Load data ####

# # single-game
# pressing_data <- read.csv("results/game_1274085_pressing_analysis.csv")

# multi-game
pressing_data <- as.data.table(read.csv("results/all_games_pressing_sequences.csv"))


#### DATA FOR MODELING ####

# exclude pressing sequence that last just 1 frame (0.1s), could be noise
pressing_data <- pressing_sequences[sequence_duration_frames > 1]

# removing non-essential columns for modeling
cols_to_remove <- which(names(pressing_data) == "sequence_id"):which(names(pressing_data) == "player_in_possession_name")
pressing_data <- pressing_data[, -cols_to_remove, with = FALSE]




#### Elastic Net ####


#### Random forest #### no tuning
library(ranger)
pressing_rf <- ranger(forced_turnover_within_5s ~ ., 
                      num.trees = 1000,
                      classification = TRUE,
                      probability = TRUE,
                      importance = "impurity", # try "permutation"???
                      data = pressing_data)
library(vip)
vip(pressing_rf)

pressing_data |> 
  mutate(pred = pressing_rf$predictions) |> 
  summarize(rmse = sqrt(mean((forced_turnover_within_5s - pred) ^ 2))) # RMSE

#### tidymodels ####
library(tidymodels)

set.seed(123)
pressing_split <- initial_split(pressing_data, strata = forced_turnover_within_5s)
pressing_train <- training(pressing_split)
pressing_test <- testing(pressing_split)


set.seed(456)
pressing_folds <- vfold_cv(pressing_train, strata = forced_turnover_within_5s)
pressing_folds


usemodels::use_ranger(forced_turnover_within_5s ~ ., data = pressing_train)
# Copy code from console
ranger_recipe <- 
  recipe(formula = forced_turnover_within_5s ~ ., data = pressing_train) 

ranger_spec <- 
  rand_forest(trees = 1000) %>% 
  set_mode("classification") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

doParallel::registerDoParallel()
set.seed(98631)
ranger_tune <-
  fit_resamples(ranger_workflow, 
            resamples = pressing_folds,
            control = control_resamples(save_pred = TRUE))


#### XGBoost ####


