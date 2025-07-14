library(tidyverse) # Data manipulation and visualization
library(data.table) # Handling large data, fast and memory efficient
library(jsonlite) # Loading JSON/JSONL files
library(sf) 
library(gganimate) # Plot animation
library(purrr)
library(deldir)



#### Read JSONL file ####  
tracking_data <- fromJSON("data/skillcorner/tracking/match_920975_tracking.json")
tracking_data <- tracking_data |> 
  unnest(cols = c(player_data)) |>
  as.data.table()

#### Get Player Information ####
match_info <- fromJSON("data/skillcorner/match_data/match_920975_data.json", simplifyDataFrame = FALSE)
match_info_df <- fromJSON("data/skillcorner/match_data/match_920975_data.json")

#### Get Team Information ####
home_team_name <- match_info$home_team$name
away_team_name <- match_info$away_team$name
home_team_id <- match_info_df$home_team$id
away_team_id <- match_info_df$away_team$id
players_info <- match_info_df$players

match_id <- match_info$id

#### Add player info and team names to tracking_data ####
# Add player information
tracking_data <- tracking_data |> 
  left_join(
    players_info |> select(id, team_id, first_name, last_name, short_name, number),
    join_by(player_id == id)
  )

# Create team mapping and add team names
team_mapping <- data.frame(
  team_id = c(home_team_id, away_team_id),
  team_name = c(home_team_name, away_team_name)
)

tracking_data <- tracking_data |> 
  left_join(team_mapping, by = "team_id")


#### Read event file ####
events <- read_csv("data/skillcorner/dynamic_events/match_920975_events.csv") |>
  mutate(
    player_in_possession_name = ifelse(event_type == "player_possession", player_name, player_in_possession_name)
  ) |> 
  left_join(
    players_info |> select(short_name, id, team_id),
    join_by(player_name == short_name, team_id == team_id)) |> 
  mutate(
    player_in_possession_id = ifelse(is.na(player_in_possession_id), id, player_in_possession_id)
  ) |> 
  select(-id) |> 
  as.data.table()


################################################################
##      PSA: Run pressing_functions.R before proceeding       ##
################################################################


################################################################
##              Pressing/Pressure in Soccer                   ##
################################################################

#### Calculate distance, direction, speed, acceleration ####
tracking_data <- calculate_metrics(tracking_data)


#### Find ball carrier at every frame ####
frames <- sort(unique(tracking_data$frame)) # can adjust to specific frames if needed
ball_carrier_df <- get_ball_carrier(frames, events)


#### Press Detection ####
pressing_results <- detect_pressing_action(tracking_data, ball_carrier_df)
pressing_sequences <- identify_pressing_sequences(pressing_results)


#### Create player-sequence mapping for attribution ####
# This captures all players involved in each pressing sequence

player_sequence_mapping <- pressing_results[is_pressing == TRUE][
  pressing_sequences,
  on = .(possession_team, 
         frame >= sequence_start_frame, 
         frame <= sequence_end_frame),
  nomatch = NULL,
  allow.cartesian = TRUE
  ]

# Aggregate by the sequence_id from pressing_sequences
player_sequence_mapping <- player_sequence_mapping[, .(
  avg_distance_in_sequence = mean(distance_to_ball_carrier),
  avg_approach_velocity_in_sequence = mean(approach_velocity),
  frames_involved = .N,
  pressing_team_id = first(team_id),
  pressing_team_name = first(team_name)
), by = .(sequence_id, possession_team, player_id, short_name)]


#### Tag player possession events that ended in forced turnovers ####
player_possession <- events[event_type == "player_possession"]
player_possession[, next_team_id := data.table::shift(team_id, type = "lead")] # helper column for next team_id
player_possession[, ended_with_turnover := 
                    start_type == "pass_interception" |
                    (pass_outcome == "unsuccessful" & 
                       is.na(game_interruption_after) & 
                       team_id != next_team_id)]


#### For each pressing sequence, check if a forced turnover occurred within 5 seconds ####
forced_turnovers <- player_possession[ended_with_turnover == TRUE]
frame_window <- 50 # 50 frames == 5 seconds

pressing_sequences[, forced_turnover_within_5s := FALSE]

for(i in 1:nrow(pressing_sequences)) {
  current_press <- pressing_sequences[i]
  
  relevant_forced_turnovers <- forced_turnovers[
    team_id == current_press$possession_team &
      frame_start >= current_press$sequence_start_frame &
      frame_start <= (current_press$sequence_start_frame + frame_window)
  ]
  
  if(nrow(relevant_forced_turnovers) > 0) {
    pressing_sequences[i, forced_turnover_within_5s := TRUE]
  }
}


#### Add game state to pressing sequence data ####
# First, get unique game state for each sequence
game_state_info <- events[
  event_type == "player_possession"
][pressing_sequences, 
  on = .(team_id == possession_team,
         frame_start <= sequence_end_frame,
         frame_end >= sequence_start_frame),
  nomatch = NULL
][, .(
  poss_third_start = first(third_start),
  game_state = first(game_state),
  team_score = first(team_score),
  opponent_team_score = first(opponent_team_score)
), by = sequence_id]

# Merge back by sequence_id (guaranteed 1:1)
pressing_sequences <- merge(
  pressing_sequences,
  game_state_info,
  by = "sequence_id",
  all.x = TRUE
)
pressing_sequences[, goal_diff := team_score - opponent_team_score]


#### Add variables from event data to pressing sequence data ####
for(i in 1:nrow(pressing_sequences)) {
  relevant_event <- events[event_type == "player_possession" &
                             frame_start <= pressing_sequences[i, sequence_end_frame] & 
                             frame_end >= pressing_sequences[i, sequence_start_frame]]
  
  if(nrow(relevant_event) > 0){
    pressing_sequences[i, max_passing_options := relevant_event$n_passing_options[1]]
    pressing_sequences[i, player_in_possession_id := relevant_event$player_in_possession_id[1]]
  }
}

# Add short_name of the player in possession
pressing_sequences <- pressing_sequences |>
  left_join(
    players_info |> select(id, short_name),
    by = c("player_in_possession_id" = "id")
  ) |>
  rename(player_in_possession_name = short_name)


#### Get incoming passing data ####
incoming_pass_data <- link_player_possessions_with_incoming_passes(events)
incoming_pass_data <- incoming_pass_data[,
                                         .(frame_start, frame_end, event_type, team_id, player_name, start_type,
                                           incoming_high_pass, incoming_pass_distance_received, incoming_pass_range_received)
]

incoming_info <- incoming_pass_data[
  event_type == "player_possession"
][pressing_sequences,
  on = .(team_id == possession_team,
         frame_start <= sequence_end_frame,
         frame_end >= sequence_start_frame),
  nomatch = NULL
][, .(
  start_type = first(start_type),
  incoming_high_pass = first(incoming_high_pass),
  incoming_pass_distance_received = first(incoming_pass_distance_received),
  incoming_pass_range_received = first(incoming_pass_range_received)
), by = sequence_id]

# Merge by sequence_id
pressing_sequences <- merge(
  pressing_sequences,
  incoming_info,
  by = "sequence_id",
  all.x = TRUE
)


#### Adding ball carrier coordinates to existing pressing_sequences ####
ball_carrier_positions <- tracking_data[, .(
  sequence_start_frame = frame, 
  player_in_possession_id = player_id,
  ball_carrier_x = x,
  ball_carrier_y = y
)]

pressing_sequences <- merge(
  pressing_sequences,
  ball_carrier_positions,
  by = c("sequence_start_frame", "player_in_possession_id"),
  all.x = TRUE
)


#### Adding attacking_side from events to pressing_sequences ####
pressing_sequences <- pressing_sequences[
  events[event_type == "player_possession", .(frame_start, frame_end, attacking_side, organised_defense)],
  on = .(sequence_start_frame >= frame_start, sequence_start_frame <= frame_end),
  nomatch = NULL
][, `:=`(
  attacking_side = attacking_side,
  organised_defense = organised_defense)][, !c("sequence_start_frame.1"), with = FALSE]


#### Calculating distances to sideline/endline ####

X_MIN <- -52.5  # Left goal line
X_MAX <- 52.5   # Right goal line  
Y_MIN <- -34    # Bottom sideline
Y_MAX <- 34     # Top sideline

pressing_sequences[, `:=`(
  # Distance to nearest sideline
  dist_to_nearest_sideline = pmin(abs(ball_carrier_y - Y_MIN), abs(ball_carrier_y - Y_MAX)),
  
  # Distance to nearest endline
  dist_to_nearest_endline = pmin(
    abs(ball_carrier_x - X_MIN), 
    abs(ball_carrier_x - X_MAX)
  ),
  
  # Distance to attacking endline
  dist_to_attacking_endline = ifelse(
    attacking_side == "left_to_right",
    abs(ball_carrier_x - X_MAX),
    abs(ball_carrier_x - X_MIN)
  ),
  
  # Distance to defending endline
  dist_to_defending_endline = ifelse(
    attacking_side == "left_to_right",
    abs(ball_carrier_x - X_MIN),
    abs(ball_carrier_x - X_MAX)
  )
)]


#### Other additions ####

# Does press start in defensive or offensive half
pressing_sequences[, is_defensive_half := ifelse((attacking_side == "left_to_right" & ball_carrier_x <= 0) | (attacking_side == "right_to_left" & ball_carrier_x > 0), 1, 0)]

# Corner trap situations
pressing_sequences[, is_corner_trap := (dist_to_nearest_sideline < 15) & (dist_to_attacking_endline < 15)]

# Distance of ball carrier to center of attacking goal
pressing_sequences[, dist_to_attacking_goal := ifelse(
  attacking_side == "left_to_right",
  sqrt((52.5 - ball_carrier_x)^2 + (0 - ball_carrier_y)^2),  # Attacking goal is at (52.5, 0)
  sqrt((-52.5 - ball_carrier_x)^2 + (0 - ball_carrier_y)^2)  # Attacking goal is at (-52.5, 0)
)]


## Calculate minutes remaining in half and full game

# Get all unique periods and their frame ranges
period_ranges <- tracking_data[, .(min_frame = min(frame), max_frame = max(frame)), by = period]

pressing_sequences[, period := NA_integer_]

# Assign periods based on frame ranges
for(p in period_ranges$period) {
  min_f <- period_ranges[period == p, min_frame]
  max_f <- period_ranges[period == p, max_frame]
  pressing_sequences[sequence_start_frame >= min_f & sequence_start_frame <= max_f, period := p]
}

# Initialize minutes remaining columns
pressing_sequences[, minutes_remaining_half := NA_real_]
pressing_sequences[, minutes_remaining_game := NA_real_]

# Calculate minutes remaining only for regular time (periods 1 and 2)
if (nrow(period_ranges[period == 1]) > 0) {
  period1_max <- period_ranges[period == 1, max_frame]
  pressing_sequences[period == 1, minutes_remaining_half := 
                       (period1_max - sequence_start_frame) / 10 / 60]
}

if (nrow(period_ranges[period == 2]) > 0) {
  period2_min <- period_ranges[period == 2, min_frame]
  period2_max <- period_ranges[period == 2, max_frame]
  pressing_sequences[period == 2, minutes_remaining_half := 
                       (period2_max - sequence_start_frame) / 10 / 60]
  
  # time remaining til full game from period 1
  if (nrow(period_ranges[period == 1]) > 0) {
    period1_max <- period_ranges[period == 1, max_frame]
    pressing_sequences[period == 1, minutes_remaining_game := 
                         ((period1_max - sequence_start_frame) + (period2_max - period2_min)) / 10 / 60]
  }
  
  # For period 2, minutes remaining in game equals minutes remaining in half
  pressing_sequences[period == 2, minutes_remaining_game := 
                       (period2_max - sequence_start_frame) / 10 / 60]
}

# Round to 2 d.p.
pressing_sequences[!is.na(minutes_remaining_half), 
                   minutes_remaining_half := round(minutes_remaining_half, 2)]
pressing_sequences[!is.na(minutes_remaining_game), 
                   minutes_remaining_game := round(minutes_remaining_game, 2)]


# Just adding team name
# The possession_team in pressing_sequences is the team being pressed (pressed_team)
pressing_sequences <- merge(
  pressing_sequences,
  team_mapping,
  by.x = "possession_team",
  by.y = "team_id",
  all.x = TRUE
)
setnames(pressing_sequences, "team_name", "pressed_team_name")

# Pressing team
pressing_sequences <- merge(
  pressing_sequences,
  team_mapping,
  by.x = "pressing_team_id",
  by.y = "team_id",
  all.x = TRUE,
  suffixes = c("", "_pressing")
)
setnames(pressing_sequences, "team_name", "pressing_team_name")
setnames(pressing_sequences, "possession_team", "pressed_team_id")


pressing_sequences[, match_id := match_id] # Add match id

#### Reorder Columns ####
setcolorder(pressing_sequences, c("match_id", "sequence_id", "pressed_team_name", "pressed_team_id", 
                                  "pressing_team_name", "pressing_team_id",
                                  "sequence_start_frame", "sequence_end_frame", "sequence_duration_frames", 
                                  "sequence_duration_seconds", "player_in_possession_id", "player_in_possession_name", "period",
                                  "ball_carrier_x", "ball_carrier_y", "forced_turnover_within_5s", "max_pressing_defenders", 
                                  "max_passing_options", "min_distance", "avg_approach_velocity",
                                  "team_score", "opponent_team_score", "goal_diff"))
setorder(pressing_sequences, sequence_id)


############################################
##        Variable Descriptions           ## UPDATE THIS !!!!!!!!!!!!!!!!!!!!!!
############################################

# max_pressing_defenders: This is the total count of unique defenders who were actively pressing at any point during the sequence.
# min_distance: This represents the absolute closest that any defender got to the ball carrier during the entire duration of the sequence.
# avg_approach_velocity: This is the average speed at which all pressing defenders closed down the ball carrier, averaged across all frames of the sequence.
# max_passing_options: The number of available passing options for the ball carrier at the moment the press begins.
# ball_carrier_x / ball_carrier_y: The on-pitch coordinates of the ball carrier specifically at the moment the press begins.
# dist_to_nearest_sideline / dist_to_nearest_endline / dist_to_attacking_endline: These distances are all calculated based on the ball carrier's position at the moment the press begins.
# dist_to_attacking_goal: Ball carrier's distance to center of attacking goal at the moment the press begins.
# poss_third_start: The area of the pitch (defensive, middle, attacking third) where the press sequence begins.
# start_type (e.g. is_pass_reception, is_recovery, ...): Describes how the player came to be in possession of the ball before or right as the press sequence started.
# incoming pass range received (e.g is_short_pass, is_medium_pass, ...): Information about the pass the player received (if any) right before the pressure started
# incoming high pass (e.g is_high_pass_true/false/na): Describes if the pass the player received (if any) was recorded as a high pass or not.
# organised defense (e.g is_organised_def_true/false/na): Describes the defensive structure at the moment the press begins (if recorded).


