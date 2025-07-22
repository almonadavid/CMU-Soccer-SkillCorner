
library(tidyverse)
library(data.table)


#### LOAD DATA ####

pressing_data <- fread("results/all_games_pressing_sequences.csv") |>
  filter(
    between(ball_carrier_x, -52.5, 52.5),
    between(ball_carrier_y, -34, 34)
  ) |> 
  select(-match_id:-player_in_possession_name, -period, -is_home) |>
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


pressing_data <- pressing_data |>
  mutate(forced_turnover_within_5s = as.factor(ifelse(forced_turnover_within_5s == TRUE, "Yes", "No")))


# handling NAs
pressing_data <- pressing_data |>
  mutate(
    incoming_pass_range_received_missing = is.na(incoming_pass_range_received),
    incoming_high_pass_missing = is.na(incoming_high_pass),
    incoming_pass_distance_received_missing = is.na(incoming_pass_distance_received),
    ball_carrier_speed_missing = is.na(ball_carrier_speed),
    incoming_pass_range_received = factor(ifelse(is.na(incoming_pass_range_received), 
                                                 "unknown", as.character(incoming_pass_range_received))),
    incoming_high_pass = factor(ifelse(is.na(incoming_high_pass), 
                                       "unknown", as.character(incoming_high_pass))),
    incoming_pass_distance_received = ifelse(is.na(incoming_pass_distance_received), 
                                             -1, incoming_pass_distance_received),
    ball_carrier_speed = ifelse(is.na(ball_carrier_speed), 
                                -1, ball_carrier_speed),
    incoming_pass_range_received_missing = factor(incoming_pass_range_received_missing),
    incoming_high_pass_missing = factor(incoming_high_pass_missing),
    incoming_pass_distance_received_missing = factor(incoming_pass_distance_received_missing),
    ball_carrier_speed_missing = factor(ball_carrier_speed_missing),
    minutes_remaining_half = ifelse(is.na(minutes_remaining_half), median(minutes_remaining_half, na.rm = TRUE), minutes_remaining_half),
    minutes_remaining_game = ifelse(is.na(minutes_remaining_game), median(minutes_remaining_game, na.rm = TRUE), minutes_remaining_game)
  )


## LOGIT & XGBOOST ###################################################################################
library(xgboost)
library(caret)

create_match_index <- function(pressing_data, N_FOLDS) {
  unique_games <- pressing_data |> 
    distinct(game_id) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  pressing_data <- pressing_data |> 
    left_join(unique_games, by = c("game_id"))
  
  return(pressing_data)
}

N_FOLDS <- 10
pressing_data <- create_match_index(pressing_data, N_FOLDS)


# create hyperparameter grid
xg_grid <- crossing(nrounds = seq(20, 150, 10),
                    eta = c(0.01, 0.05, 0.1),
                    gamma = 0,
                    max_depth = seq(3, 6, 1),
                    colsample_bytree = 1,
                    min_child_weight = 1,
                    subsample = 1)

# tuning
set.seed(1234)

# tuning sample, I don't have enough RAM to tune on entire dataset
tuning_sample <- pressing_data |> 
  group_by(fold) |>
  slice_sample(prop = 0.1) |>
  ungroup()

xg_tune <- train(forced_turnover_within_5s ~ ., 
                 data = tuning_sample |> select(-fold),
                 tuneGrid = xg_grid,
                 trControl = trainControl(
                   method = "cv", 
                   number = max(tuning_sample$fold),
                   index = split(1:nrow(tuning_sample), tuning_sample$fold),
                   savePredictions = "final",
                   classProbs = TRUE,
                   summaryFunction = twoClassSummary
                 ),
                 method = "xgbTree",
                 metric = "ROC" )

xg_best <- xg_tune$bestTune


test_diff_models <- function(x){
  
  test_data <- pressing_data |> filter(fold == x) 
  train_data <- pressing_data |> filter(fold != x)
  
  train_x <- model.matrix(~ . - 1, data = train_data |> select(-forced_turnover_within_5s, -fold))
  test_x <- model.matrix(~ . - 1, data = test_data |> select(-forced_turnover_within_5s, -fold))
  
  train_y <- as.numeric(train_data$forced_turnover_within_5s) - 1 
  test_y <- as.numeric(test_data$forced_turnover_within_5s) - 1
  
  # Fit models
  logit_fit <- glm(forced_turnover_within_5s ~ . - fold, data = train_data, family = "binomial")
  
  xg_fit <- xgboost(
    data = train_x,
    label = train_y,
    objective = "binary:logistic",
    nrounds = xg_best$nrounds,
    eta = xg_best$eta, 
    max_depth = xg_best$max_depth,
    gamma = xg_best$gamma,
    colsample_bytree = xg_best$colsample_bytree,
    min_child_weight = xg_best$min_child_weight,
    subsample = xg_best$subsample,
    verbose = 0
  )

  out <- tibble(
    logit_pred = predict(logit_fit, newdata = test_data, type = "response"),
    xg_pred = predict(xg_fit, newdata = test_x),
    test_actual = test_y,
    test_fold = x
  )
  
  return(out)
}

test_pred_all <- map(1:N_FOLDS, test_diff_models) |> 
  bind_rows()


# performance evaluation
test_pred_all |>
  pivot_longer(logit_pred:xg_pred, 
               names_to = "type", 
               values_to = "test_pred") |>
  group_by(type, test_fold) |>
  summarize(
    rmse = sqrt(mean((test_actual - test_pred)^2)),
    log_loss = -mean(test_actual * log(test_pred + 1e-15) + (1 - test_actual) * log(1 - test_pred + 1e-15)),
    accuracy = mean((test_pred > 0.5) == test_actual),
    .groups = "drop"
  ) |> 
  ggplot(aes(x = type, y = log_loss)) + 
  geom_point(size = 4) +
  stat_summary(fun = mean, geom = "point", 
               color = "red", size = 4) + 
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               color = "red", width = 0.2) +
  theme_bw()


# variable importance
importance <- xgb.importance(model = xg_tune$finalModel)
print(head(importance, 10))

importance |>
  filter(startsWith(Feature, "start_type")) |>
  summarise(total_gain = sum(Importance))

pressing_data |>
  group_by(start_type) |>
  summarise(
    total_possessions = n(),
    turnovers = sum(turnover_actual),
    turnover_prop = turnovers / total_possessions
  ) |>
  arrange(desc(turnover_prop))


# confusion matrix
test_pred_classes <- test_pred_all |>
  mutate(
    logit_class = ifelse(logit_pred > 0.5, 1, 0),
    xg_class = ifelse(xg_pred > 0.5, 1, 0)
  )

# logit matrix
table(Actual = test_pred_classes$test_actual, 
      Predicted = test_pred_classes$logit_class)

# xgb matrix
table(Actual = test_pred_classes$test_actual,
      Predicted = test_pred_classes$xg_class)

# sum stats
library(pROC)

logit_roc <- roc(test_pred_all$test_actual, test_pred_all$logit_pred)
xg_roc <- roc(test_pred_all$test_actual, test_pred_all$xg_pred)

model_summary <- tibble(
  Model = c("Logistic Regression", "XGBoost"),
  AUC = c(round(auc(logit_roc), 3), round(auc(xg_roc), 3)),
  Accuracy = c(
    mean(test_pred_classes$logit_class == test_pred_classes$test_actual),
    mean(test_pred_classes$xg_class == test_pred_classes$test_actual)
  ),
  Log_Loss = c(
    mean(-test_pred_all$test_actual * log(test_pred_all$logit_pred + 1e-15) - 
           (1 - test_pred_all$test_actual) * log(1 - test_pred_all$logit_pred + 1e-15)),
    mean(-test_pred_all$test_actual * log(test_pred_all$xg_pred + 1e-15) - 
           (1 - test_pred_all$test_actual) * log(1 - test_pred_all$xg_pred + 1e-15))
  )) |>
  mutate(across(where(is.numeric), ~round(.x, 3)))

model_summary


# roc curve
roc_data <- bind_rows(
  tibble(
    fpr = 1 - logit_roc$specificities,
    tpr = logit_roc$sensitivities,
    model = "Logistic"
  ),
  tibble(
    fpr = 1 - xg_roc$specificities,
    tpr = xg_roc$sensitivities,
    model = "XGBoost"
  )
)


roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(
    values = c("Logistic" = "blue", "XGBoost" = "red"),
    labels = c(paste("Logistic (AUC =", round(auc(logit_roc), 3), ")"),
               paste("XGBoost (AUC =", round(auc(xg_roc), 3), ")"))
  ) +
  labs(
    x = "(1 - Specificity)",
    y = "(Sensitivity)",
    color = "Model"
  ) +
  theme_bw() +
  coord_fixed()

roc_plot