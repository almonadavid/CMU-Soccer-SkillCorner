library(tidyverse) # Data manipulation and visualization
library(data.table) # Handling large data, fast and memory efficient
library(jsonlite) # Loading JSON/JSONL files
library(sf) 
library(gganimate) # Plot animation
library(purrr)
library(deldir)


#### Read JSONL file ####  
tracking_data <- stream_in(file("data/SkillCorner data/1274085_tracking_extrapolated.jsonl"))
tracking_data <- tracking_data |> 
  unnest(cols = c(player_data)) |>
  as.data.table()


#### Get Player Information ####
match_info <- fromJSON("data/SkillCorner data/1274085_match.json", simplifyDataFrame = FALSE)
match_info_df <- fromJSON("data/SkillCorner data/1274085_match.json")

players_info <- match_info_df$players |> 
  as.data.table()


#### Read event file ####
events <- read_csv("data/SkillCorner data/1274085_dynamic_events.csv") |>
  mutate(
    player_in_possession_name = ifelse(event_type == "player_possession", player_name, player_in_possession_name)
  ) |> 
  left_join(
    players_info |> select(short_name, id),
    join_by(player_name == short_name)) |> 
  mutate(
    player_in_possession_id = ifelse(is.na(player_in_possession_id), id, player_in_possession_id)
  ) |> 
  select(-id) |> 
  as.data.table()


tracking_data <- tracking_data |> 
  left_join(
    players_info |> select(id, team_id, number, short_name), 
    join_by(player_id == id))



################################################################
##      PSA: Run pressing_functions.R before proceeding       ##
################################################################


################################################################
##              Pressing/Pressure in Soccer                   ##
################################################################

#### Calculate distance, direction, speed, acceleration ####
tracking_data <- calculate_direction(tracking_data)


#### Find ball carrier at every frame ####
frames <- sort(unique(tracking_data$frame)) # can adjust to specific frames if needed
ball_carrier_df <- get_ball_carrier(frames, events)


#### Press Detection ####
pressing_results <- detect_pressing_action(tracking_data, ball_carrier_df)
pressing_sequences <- identify_pressing_sequences(pressing_results)

#### Tag player possession events that ended in forced turnovers ####
player_possession <- events[event_type == "player_possession"]
player_possession[, next_team_id := shift(team_id, type = "lead")] # helper column for next team_id
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
pressing_sequences <- events[event_type == "player_possession"][pressing_sequences, 
                                                                .(possession_team, sequence_id, sequence_start_frame, sequence_end_frame,
                                                                  sequence_duration_frames, sequence_duration_seconds, avg_pressing_defenders,
                                                                  max_pressing_defenders, min_distance, avg_approach_velocity,
                                                                  forced_turnover_within_5s, poss_third_start = third_start, game_state, team_score, opponent_team_score),
                                                                on = .(team_id == possession_team,
                                                                       frame_start <= sequence_end_frame,
                                                                       frame_end >= sequence_start_frame)
][, goal_diff := (team_score - opponent_team_score)]


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


#### Get incoming passing data ####
incoming_pass_data <- link_player_possessions_with_incoming_passes(events)
incoming_pass_data <- incoming_pass_data[,
                                         .(frame_start, frame_end, event_type, team_id, player_name, start_type,
                                           incoming_high_pass, incoming_pass_distance_received, incoming_pass_range_received)
]


# adding to pressing sequence
pressing_sequences <- incoming_pass_data[pressing_sequences,
                                         .(possession_team, player_in_possession_id, sequence_id, sequence_start_frame, sequence_end_frame,
                                           sequence_duration_frames, sequence_duration_seconds, avg_pressing_defenders,
                                           max_pressing_defenders, min_distance, avg_approach_velocity,
                                           forced_turnover_within_5s, poss_third_start, game_state, team_score, opponent_team_score,
                                           max_passing_options, start_type, incoming_high_pass, incoming_pass_distance_received, incoming_pass_range_received),
                                         on = .(team_id == possession_team,
                                                frame_start <= sequence_end_frame,
                                                frame_end >= sequence_start_frame)
]


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
  dist_to_sideline = pmin(abs(ball_carrier_y - Y_MIN), abs(ball_carrier_y - Y_MAX)),
  
  # Distance to nearest endline
  dist_to_endline = pmin(
    abs(ball_carrier_x - X_MIN), 
    abs(ball_carrier_x - X_MAX)
  ),
  
  # Distance to attacking endline
  dist_to_attacking_goal = ifelse(
    attacking_side == "left_to_right",
    abs(ball_carrier_x - X_MAX),
    abs(ball_carrier_x - X_MIN)
  ),
  
  # Distance to defending endline
  dist_to_defending_goal = ifelse(
    attacking_side == "left_to_right",
    abs(ball_carrier_x - X_MIN),
    abs(ball_carrier_x - X_MAX)
  )
)]


#### Clean data for modeling ####
## dummy encoding

# Game state ("drawing" become reference)
pressing_sequences$is_winning <- as.numeric(pressing_sequences$game_state == "winning")
pressing_sequences$is_losing <- as.numeric(pressing_sequences$game_state == "losing")

# Possession third start ("defensive_third" becomes reference)
pressing_sequences$is_middle_third <- as.numeric(pressing_sequences$poss_third_start == "middle_third")
pressing_sequences$is_attacking_third <- as.numeric(pressing_sequences$poss_third_start == "attacking_third")

# Start type
pressing_sequences$is_keep_possession <- as.numeric(pressing_sequences$start_type == "keep_possession")
pressing_sequences$is_pass_interception <- as.numeric(pressing_sequences$start_type == "pass_interception")
pressing_sequences$is_pass_reception <- as.numeric(pressing_sequences$start_type == "pass_reception")
pressing_sequences$is_recovery <- as.numeric(pressing_sequences$start_type == "recovery")
pressing_sequences$is_throw_in_reception <- as.numeric(pressing_sequences$start_type == "throw_in_reception")
pressing_sequences$is_free_kick_reception <- as.numeric(pressing_sequences$start_type == "free_kick_reception")
pressing_sequences$is_goal_kick_reception <- as.numeric(pressing_sequences$start_type == "goal_kick_reception")
pressing_sequences$is_throw_in_interception <- as.numeric(pressing_sequences$start_type == "throw_in_interception")
pressing_sequences$is_start_type_unknown <- as.numeric(pressing_sequences$start_type == "unknown")

# Attacking side ("right_to_left" becomes reference)
pressing_sequences$is_left_to_right <- as.numeric(pressing_sequences$attacking_side == "left_to_right")

# Incoming pass range
pressing_sequences$is_short_pass <- fifelse(is.na(pressing_sequences$incoming_pass_range_received), 0, 
                                            as.numeric(pressing_sequences$incoming_pass_range_received == "short"))
pressing_sequences$is_medium_pass <- fifelse(is.na(pressing_sequences$incoming_pass_range_received), 0,
                                             as.numeric(pressing_sequences$incoming_pass_range_received == "medium"))
pressing_sequences$is_long_pass <- fifelse(is.na(pressing_sequences$incoming_pass_range_received), 0,
                                           as.numeric(pressing_sequences$incoming_pass_range_received == "long"))
pressing_sequences$is_no_incoming_pass <- as.numeric(is.na(pressing_sequences$incoming_pass_range_received))

# Incoming high pass
pressing_sequences$is_high_pass_true <- fifelse(is.na(pressing_sequences$incoming_high_pass), 0,
                                                as.numeric(pressing_sequences$incoming_high_pass == TRUE))
pressing_sequences$is_high_pass_false <- fifelse(is.na(pressing_sequences$incoming_high_pass), 0,
                                                 as.numeric(pressing_sequences$incoming_high_pass == FALSE))
pressing_sequences$is_high_pass_na <- as.numeric(is.na(pressing_sequences$incoming_high_pass))

# Organised defense  
pressing_sequences$is_organised_def_true <- fifelse(is.na(pressing_sequences$organised_defense), 0,
                                                    as.numeric(pressing_sequences$organised_defense == TRUE))
pressing_sequences$is_organised_def_false <- fifelse(is.na(pressing_sequences$organised_defense), 0,
                                                     as.numeric(pressing_sequences$organised_defense == FALSE))
pressing_sequences$is_organised_def_na <- as.numeric(is.na(pressing_sequences$organised_defense))

# Corner trap situations
pressing_sequences[, is_corner_trap := (dist_to_sideline < 15) & (dist_to_attacking_goal < 15)]

# Forced turnover (convert to 0/1)
pressing_sequences$forced_turnover_within_5s <- as.numeric(pressing_sequences$forced_turnover_within_5s)

# Drop original categorical columns if you want
pressing_sequences <- pressing_sequences[, !c(
  "game_state", "poss_third_start",
  "start_type", "attacking_side",
  "incoming_pass_range_received",
  "incoming_high_pass", "organised_defense"
), with = FALSE]


#__________________________________________________________________________________
# # View results - Player pressing summary
# player_press_summary <- pressing_results$individual_pressing[, .(
#   team_id = first(team_id),
#   total_presses = .N,
#   avg_press_distance = round(mean(distance_to_ball_carrier, na.rm = TRUE), 2),
#   avg_approach_velocity = round(mean(approach_velocity, na.rm = TRUE), 2)
# ), by = .(player_id, short_name)][order(-total_presses)]






############################################
##            ROUGH WORK                 ##
############################################

#### Analyze specific frames with passing options for passing lane coverage ####
frame_sample <- sort(unique(tracking_data$frame))
passing_lane_analysis <- map_dfr(frame_sample, ~ {
  result <- analyze_passing_lane_coverage(tracking_data, events, .x)
  if(!is.null(result)) {
    result[, frame := .x]
  }
  result
})


test <- events |>
  select(event_id, frame_start, frame_end, event_type, player_name, team_id,
         player_in_possession_name, pass_angle, pass_angle_received, interplayer_angle)

