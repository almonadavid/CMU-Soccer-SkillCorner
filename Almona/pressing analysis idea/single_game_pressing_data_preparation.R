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



#### Standardize tracking data so that home team always attacks left-to-right ####
home_team_directions <- events[!is.na(attacking_side) & team_id == home_team_id, 
                               .(home_attacking = first(attacking_side)), 
                               by = .(period)]

# Merge that attacking direction above to tracking data
tracking_data[home_team_directions, home_attacking := i.home_attacking, on = .(period)]

tracking_data[, ':='(
  x = ifelse(home_attacking == "right_to_left", -x, x),
  y = ifelse(home_attacking == "right_to_left", -y, y),
  ball_data.x = ifelse(home_attacking == "right_to_left", -ball_data.x, ball_data.x),
  ball_data.y = ifelse(home_attacking == "right_to_left", -ball_data.y, ball_data.y)
)]

tracking_data[, home_attacking := NULL]



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
pressing_sequences[
  events[event_type == "player_possession"], `:=`(
    poss_third_start = i.third_start,
    game_state = i.game_state,
    team_score = i.team_score,
    opponent_team_score = i.opponent_team_score),
  on = .(possession_team = team_id,
         sequence_start_frame >= frame_start,
         sequence_start_frame <= frame_end),
  mult = "first"
]
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
pressing_sequences[incoming_info,
                   `:=`(start_type = i.start_type,
                        incoming_high_pass = i.incoming_high_pass,
                        incoming_pass_distance_received = i.incoming_pass_distance_received,
                        incoming_pass_range_received = i.incoming_pass_range_received),
                   on = "sequence_id"]


#### Adding ball carrier coordinates to existing pressing_sequences ####
ball_carrier_positions <- tracking_data[, .(
  sequence_start_frame = frame, 
  player_in_possession_id = player_id,
  ball_carrier_x = x,
  ball_carrier_y = y
)]

pressing_sequences[ball_carrier_positions,
                   `:=`(ball_carrier_x = i.ball_carrier_x,
                        ball_carrier_y = i.ball_carrier_y),
                   on = .(sequence_start_frame = sequence_start_frame,
                          player_in_possession_id = player_in_possession_id)]


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


# Ball carrier direction and speed at sequence start
pressing_sequences[tracking_data[, .(frame, player_id, direction, speed_smooth)], 
                   `:=`(ball_carrier_direction = i.direction,
                        ball_carrier_speed = i.speed_smooth),
                   on = .(sequence_start_frame = frame, 
                          player_in_possession_id = player_id)]

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
pressing_sequences[team_mapping, 
                   pressed_team_name := i.team_name,
                   on = .(possession_team = team_id)]

# Pressing team
pressing_sequences[team_mapping,
                   pressing_team_name := i.team_name,
                   on = .(pressing_team_id = team_id)]

setnames(pressing_sequences, "possession_team", "pressed_team_id")


pressing_sequences[, match_id := match_id] # Add match id

#### Reorder Columns ####
setcolorder(pressing_sequences, c("match_id", "sequence_id", "pressed_team_name", "pressed_team_id", 
                                  "pressing_team_name", "pressing_team_id",
                                  "sequence_start_frame", "sequence_end_frame", "sequence_duration_frames", 
                                  "sequence_duration_seconds", "player_in_possession_id", "player_in_possession_name", "period",
                                  "ball_carrier_x", "ball_carrier_y", "forced_turnover_within_5s", "n_pressing_defenders", 
                                  "max_passing_options", "avg_approach_velocity",
                                  "team_score", "opponent_team_score", "goal_diff"))
setorder(pressing_sequences, sequence_id)




############################################
##        Variable Descriptions           ##
############################################

# forced_turnover_within_5s: TRUE if a forced turnover happened 5s after the pressing sequence began. Else FALSE.

# ball_carrier_x / ball_carrier_y: The on-pitch coordinates of the ball carrier specifically at the moment the press begins.
# n_pressing_defenders: This is the total count of unique defenders who were actively pressing at the moment the press begins.
# max_passing_options: The number of available passing options for the ball carrier at the moment the press begins.
# avg_approach_velocity: This is the average speed at which all pressing defenders closed down the ball carrier at the moment the press begins.
# poss_third_start: The area of the pitch (defensive, middle, attacking third) where the press sequence begins.
# game_state: winning, drawing or losing.
# start_type: Describes how the player came to be in possession of the ball.
# incoming_high_pass: Describes if the pass the player received (if any) was recorded as a high pass, above 1.8 meters, or not.
# incoming_pass_distance_received: Distance between the pass location to the location of the reception where the player started his possession.
# incoming_pass_range_received: Range of the pass received that led to the player possession.
# attacking_side: Attacking side of the team of the player in possession.
# organised_defense:  TRUE if the defense is considered as organised at the moment of the pass. Else FALSE.
# dist_to_nearest_sideline / dist_to_nearest_endline / dist_to_attacking_endline / dist_to_defensive_endline: These distances are all calculated based on the ball carrier's position at the moment the press begins.
# dist_to_attacking_goal: Ball carrier's distance to center of attacking goal at the moment the press begins.
# minutes_remaining_half: Minutes remaining until the end of current half (i.e til end of first or til end of second half)
# minutes_remaining_game: Minutes remaining until the end of the full game.
# ball_carrier_direction: Direction of ball carrier in degrees at the moment the press begins.
# ball_carrier_speed: Speed of ball carrier in m/s at the moment the press begins.




