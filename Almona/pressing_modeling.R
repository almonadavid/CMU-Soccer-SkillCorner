
library(tidyverse)
library(data.table)

set.seed(1234)

#### LOAD DATA ####

pressing_data <- fread("results/all_games_pressing_sequences.csv") |>
  filter(
    between(ball_carrier_x, -52.5, 52.5),
    between(ball_carrier_y, -34, 34)
  ) |> 
  select(-pressed_team_name:-player_in_possession_name, -period, -is_home) |>
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
  ) |> 
  mutate(
    start_type = fct_relevel(start_type, "unknown"),
    poss_third_start = fct_relevel(poss_third_start, "defensive_third"),
    incoming_high_pass = fct_relevel(incoming_high_pass, "unknown"),
    incoming_pass_range_received = fct_relevel(incoming_pass_range_received, "unknown")
  )


## LOGIT, XGBOOST and other models ###################################################################################
library(xgboost)
library(caret)
library(glmnet)

create_match_index <- function(pressing_data, N_FOLDS) {
  unique_games <- pressing_data |> 
    distinct(game_id) |> 
    mutate(fold = sample(rep(1:N_FOLDS, length.out = n())))
  
  pressing_data <- pressing_data |> 
    left_join(unique_games, by = c("game_id"))
  
  return(pressing_data)
}

N_FOLDS <- 10
pressing_data <- create_match_index(pressing_data, N_FOLDS) |> select(-game_id)

# # tuning
# # create hyperparameter grid
# xg_grid <- crossing(nrounds = seq(20, 150, 10),
#                     eta = c(0.01, 0.05, 0.1),
#                     gamma = 0,
#                     max_depth = seq(3, 6, 1),
#                     colsample_bytree = 1,
#                     min_child_weight = 1,
#                     subsample = 1)
# 
# # tuning sample, I don't have enough RAM to tune on entire dataset
# set.seed(1234)
# tuning_sample <- pressing_data |>
#   group_by(fold) |>
#   slice_sample(prop = 0.5) |>
#   ungroup()
# 
# xg_tune <- train(forced_turnover_within_5s ~ .,
#                  data = tuning_sample |> select(-fold, -match_id, -sequence_id),
#                  tuneGrid = xg_grid,
#                  trControl = trainControl(
#                    method = "cv",
#                    number = max(tuning_sample$fold),
#                    index = split(1:nrow(tuning_sample), tuning_sample$fold),
#                    savePredictions = "final",
#                    classProbs = TRUE,
#                    summaryFunction = twoClassSummary
#                  ),
#                  method = "xgbTree",
#                  metric = "ROC")
# 
# # save and re-load
# saveRDS(xg_tune, "model_results/xg_tune_results.rds")

xg_tune <- readRDS("model_results/xg_tune_results.rds")
xg_best <- xg_tune$bestTune


test_diff_models <- function(x){
  
  test_data <- pressing_data |> filter(fold == x) 
  train_data <- pressing_data |> filter(fold != x)
  
  test_ids <- test_data |> select(match_id, sequence_id)
  
  train_x <- model.matrix(~ . - 1, data = train_data |> select(-forced_turnover_within_5s, -fold, -match_id, -sequence_id))
  test_x <- model.matrix(~ . - 1, data = test_data |> select(-forced_turnover_within_5s, -fold, -match_id, -sequence_id))
  
  train_y <- as.numeric(train_data$forced_turnover_within_5s) - 1 
  test_y <- as.numeric(test_data$forced_turnover_within_5s) - 1
  
  # Fit models
  logit_fit <- glm(forced_turnover_within_5s ~ . - fold, data = train_data, family = "binomial")
  ridge_fit <- cv.glmnet(train_x, train_y, alpha = 0, family = "binomial")
  lasso_fit <- cv.glmnet(train_x, train_y, alpha = 1, family = "binomial")
  enet_fit <- cv.glmnet(train_x, train_y, alpha = 0.5, family = "binomial")
  
  
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
    seed = 1234,
    verbose = 0
  )

  out <- tibble(
    match_id = test_ids$match_id,
    sequence_id = test_ids$sequence_id,
    logit_pred = predict(logit_fit, newdata = test_data, type = "response"),
    ridge_pred = as.numeric(predict(ridge_fit, newx = test_x, type = "response", s = "lambda.min")),
    lasso_pred = as.numeric(predict(lasso_fit, newx = test_x, type = "response", s = "lambda.min")),
    enet_pred = as.numeric(predict(enet_fit, newx = test_x, type = "response", s = "lambda.min")),
    xg_pred = predict(xg_fit, newdata = test_x),
    test_actual = test_y,
    test_fold = x
  )
  
  return(out)
}


library(tictoc)
library(furrr)
plan(multisession, workers = 4)


tic("Modeling run time")
test_pred_all <- future_map(1:N_FOLDS, test_diff_models, .options = furrr_options(seed = TRUE)) |> 
  bind_rows()
toc()

# fwrite(test_pred_all, "model_results/test_pred_all.csv")
# test_pred_all <- fread("model_results/test_pred_all.csv") |> as_tibble()

# calibration plot
test_pred_all |>
  pivot_longer(logit_pred:xg_pred, 
               names_to = "model", 
               values_to = "predicted_prob") |>
  mutate(
    prob_bin = cut(predicted_prob, breaks = seq(0, 1, by = 0.05), include.lowest = TRUE),
    model = factor(model,
                   levels = c("logit_pred", "lasso_pred", "ridge_pred", "enet_pred", "xg_pred"),
                   labels = c("Logistic", "Lasso", "Ridge", "Elastic Net", "XGBoost"))
  ) |>
  group_by(model, prob_bin) |>
  summarize(
    n = n(),
    mean_predicted = mean(predicted_prob),
    mean_actual = mean(test_actual),
    .groups = "drop"
  ) |>
  filter(n >= 10) |> 
  ggplot(aes(x = mean_predicted, y = mean_actual)) +
  geom_point(aes(size = n), alpha = 0.6) +
  geom_line() +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  facet_wrap(~model) +
  scale_size_continuous(range = c(1, 4)) +
  labs(
    x = "Predicted Probability",
    y = "Actual Turnover Rate"
  ) +
  coord_fixed(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_bw() # base_size = 16



# performance evaluation
summary <- test_pred_all |>
  pivot_longer(logit_pred:xg_pred, 
               names_to = "type", 
               values_to = "test_pred") |>
  group_by(type, test_fold) |>
  summarize(
    log_loss = -mean(test_actual * log(test_pred + 1e-15) + (1 - test_actual) * log(1 - test_pred)),
    accuracy = mean((test_pred > 0.5) == test_actual),
    TP = sum((test_pred > 0.5) & test_actual == 1),
    FP = sum((test_pred > 0.5) & test_actual == 0),
    FN = sum((test_pred <= 0.5) & test_actual == 1),
    precision = TP / (TP + FP),
    recall = TP / (TP + FN),
    F1 = 2 * precision * recall / (precision + recall),
    .groups = "drop"
  ) |>
  select(-c(TP, FP, FN)) |>
  group_by(type) |>
  summarise(
    log_loss_mean = mean(log_loss),
    log_loss_se = sd(log_loss) / sqrt(n()),
    accuracy_mean = mean(accuracy),
    accuracy_se = sd(accuracy) / sqrt(n()),
    precision_mean = mean(precision),
    precision_se = sd(precision) / sqrt(n()),
    recall_mean = mean(recall),
    recall_se = sd(recall) / sqrt(n()),
    F1_mean = mean(F1),
    F1_se = sd(F1) / sqrt(n()),
    .groups = "drop"
  )


# cleaned version of summary
cleaned_summary <- summary |> 
  mutate(
    log_loss = paste0(round(log_loss_mean,3), " ± ", round(log_loss_se, 5)),
    accuracy = paste0(round(accuracy_mean,3), " ± ", round(accuracy_se, 5)),
    precision = paste0(round(precision_mean,3), " ± ", round(precision_se, 5)),
    recall = paste0(round(recall_mean,3), " ± ", round(recall_se, 5)),
    F1 = paste0(round(F1_mean,3), " ± ", round(F1_se, 5)),
    type = recode(type,
                  "enet_pred" = "Elastic Net",
                  "lasso_pred" = "Lasso",
                  "logit_pred" = "Logistic",
                  "ridge_pred" = "Ridge",
                  "xg_pred" = "XGBoost")
  ) |> 
  mutate(type = factor(type, levels = c("Logistic", "Lasso", "Ridge", "Elastic Net", "XGBoost"))) |> 
  arrange(type) |> 
  select(type, log_loss, accuracy, precision, recall, F1)



# xg vs logit
folds_summary <- test_pred_all |>
  pivot_longer(logit_pred:xg_pred, 
               names_to = "type", 
               values_to = "test_pred") |>
  group_by(type, test_fold) |>
  summarize(
    log_loss = -mean(test_actual * log(test_pred + 1e-15) + (1 - test_actual) * log(1 - test_pred)),
    .groups = "drop"
  ) |> 
  pivot_wider(names_from = type, values_from = log_loss)

t.test(folds_summary$xg_pred, folds_summary$logit_pred, paired = TRUE) #confidence interval plot is in data_viz.R



# variable importance for xgb
importance <- xgb.importance(model = xg_tune$finalModel)
print(head(importance, 10))

importance |>
  filter(startsWith(Feature, "start_type")) |>
  summarise(total_gain = sum(Gain))
# start_type contributes > 70% to model (71.1% to be precise)

# vip plot
importance |>
  head(15) |>
  ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "black") +
  coord_flip() +
  labs(
    # title = "XGBoost Variable Importance (Top 15)",
    # subtitle = "start_type features contribute 71.1% to model",
    x = "Feature",
    y = "Gain"
  ) +
  theme_bw(base_size = 22)


