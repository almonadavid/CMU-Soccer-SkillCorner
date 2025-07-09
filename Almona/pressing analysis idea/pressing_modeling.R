library(ranger)
library(vip)


#### DATA FOR MODELING ####
pressing_data <- pressing_sequences[, !c(
  "possession_team",
  "player_in_possession_id",
  "sequence_id",
  "sequence_start_frame",
  "sequence_end_frame",
  "sequence_duration_frames",
  "opponent_team_score",
  "team_score"
), with = FALSE]

## Elastic Net


## Random forest


## XGBoost