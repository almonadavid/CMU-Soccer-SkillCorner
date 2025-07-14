library(tidyverse)
library(ranger)
library(vip)
library(stacks)
library(xgboost)
library(caret)
library(glmnet)
library(tidymodels)
library(tictoc)


#### Load data ####

# # single-game
# pressing_data <- read.csv("results/game_1274085_pressing_analysis.csv")

# multi-game
pressing_data <- as.data.table(read.csv("results/all_games_pressing_sequences.csv"))


#### DATA FOR MODELING ####

# exclude pressing sequence that last just 1 frame (0.1s), could be noise
pressing_data <- pressing_data[sequence_duration_frames > 1]

# removing non-essential columns for modeling
pressing_data <- pressing_data[, !c(
  "game_id",
  "possession_team",
  "player_in_possession_id",
  "sequence_id",
  "sequence_start_frame",
  "sequence_end_frame",
  "sequence_duration_frames",
  "opponent_team_score",
  "team_score"
), with = FALSE]





#### Elastic Net ####


#### Random forest ####


# RMSE

# check tidymodels on slide 16 of boosting

#### XGBoost ####


