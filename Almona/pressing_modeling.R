library(tidyverse)
library(stacks)
library(xgboost)
library(caret)
library(glmnet)
library(tictoc)

# install.packages("tidymodels")
library(tidymodels)


## YouTube tutorial

#### Load Data ####
pressing_data <- read_csv("results/all_games_pressing_sequences.csv") |>
  select(-match_id:-player_in_possession_name, -game_id) |>
  mutate_if(is.character, as.factor) |> 
  mutate_if(is.logical, as.factor) |> 
  as.data.table()

# exclude pressing sequence that last just 1 frame (0.1s), could be noise
# pressing_data <- pressing_data[sequence_duration_frames > 1]


#### Exploratory Data Analysis ####
ggplot(pressing_data, aes(x = avg_approach_velocity, y = dist_to_attacking_goal, color = forced_turnover_within_5s)) +
  geom_point()


ggplot(pressing_data, aes(start_type, fill = forced_turnover_within_5s)) +
  geom_bar(position = "fill") +
  coord_flip() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(pressing_data, aes(poss_third_start, fill = forced_turnover_within_5s)) +
  geom_bar(position = "fill") +
  coord_flip() +
  labs(
    y = "Proportion", 
    x = "Possession Third", 
    title = "Turnover Rate by Starting Possession Third",
    fill = "Turnover"
  )


ggplot(pressing_data, aes(forced_turnover_within_5s, dist_to_attacking_goal)) +
  geom_boxplot() +
  labs(
    x = "Forced Turnover", 
    y = "Distance to Attacking Goal (m)", 
    title = "Distance to Goal When Turnovers Occur"
  ) +
  coord_flip()



#### Build a Model ####
# Create training and testing sets
set.seed(123)
pressing_split <- initial_split(pressing_data, strata = forced_turnover_within_5s)
pressing_train <- training(pressing_split)
pressing_test <- testing(pressing_split)
#________________________________________

xgb_spec <- boost_tree(
  trees = 1000,
  tree_depth = tune(), min_n = tune(), loss_reduction = tune(),
  sample_size = tune(), mtry = tune(), learn_rate = tune()
) |> 
  set_engine("xgboost") |> 
  set_mode("classification")
xgb_spec
#_______________________________________

xgb_grid <- grid_space_filling(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), pressing_train),
  learn_rate(),
  size = 20
  
)
xgb_grid
#______________________________________

xgb_wf <- workflow() |> 
  add_formula(forced_turnover_within_5s ~ .) |> 
  add_model(xgb_spec)
xgb_wf
#_____________________________________

pressing_folds <- vfold_cv(pressing_train, strata = forced_turnover_within_5s)
pressing_folds
#_____________________________________

doParallel::registerDoParallel()

set.seed(456)
xgb_res <- tune_grid(
  xgb_wf,
  resamples = pressing_folds,
  grid = xgb_grid,
  control = control_grid(save_pred = TRUE) # save for roc curve
)
xgb_res
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

best_auc <- select_best(xgb_res, metric = "roc_auc")
best_auc


final_xgb <- finalize_workflow(xgb_wf, best_auc )
final_xgb
#____________________________________

library(vip)
final_xgb |> 
  fit(data = pressing_train) |> 
  extract_fit_parsnip() |> 
  vip()
#___________________________________

final_res <- last_fit(final_xgb, pressing_split)

final_res |> 
  collect_metrics() 
#__________________________________

# confusion matrix
final_res |> 
  collect_predictions() |> 
  conf_mat(forced_turnover_within_5s, .pred_class)

# roc curve
final_res |> 
  collect_predictions() |> 
  roc_curve(forced_turnover_within_5s, .pred_FALSE) |> 
  autoplot()


#### Next Steps
# try these engines...ranger, glm, lightgbm, catboost, rpart, kknn, 






















#### Load data ####

pressing_data <- as.data.table(read.csv("results/all_games_pressing_sequences.csv"))


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



