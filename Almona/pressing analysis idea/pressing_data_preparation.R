library(tidyverse) # Data manipulation and visualization
library(data.table) # Handling large data, fast and memory efficient
library(jsonlite) # Loading JSON/JSONL files
library(sf) 
library(gganimate) # Plot animation
library(purrr)
library(deldir)


#### Read JSONL file ####  
tracking_data <- stream_in(file("data/SkillCorner data/1832186_tracking_extrapolated.jsonl"))
tracking_data <- tracking_data |> 
  unnest(cols = c(player_data)) |>
  as.data.table()


#### Get Player Information ####
match_info <- fromJSON("data/SkillCorner data/1832186_match.json", simplifyDataFrame = FALSE)
match_info_df <- fromJSON("data/SkillCorner data/1832186_match.json")

players_info <- match_info_df$players |> 
  as.data.table()


#### Read event file ####
events <- read_csv("data/SkillCorner data/1832186_dynamic_events.csv") |>
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
tracking_data_update <- calculate_direction(tracking_data)


#### Find ball carrier at every frame ####
frames <- sort(unique(tracking_data_update$frame)) # can adjust to specific frames if needed
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


#### Add n_passing_option to pressing sequence data ####
for(i in 1:nrow(pressing_sequences)) {
  relevant_event <- events[event_type == "player_possession" &
                             frame_start <= pressing_sequences[i, sequence_end_frame] & 
                             frame_end >= pressing_sequences[i, sequence_start_frame]]
  
  if(nrow(relevant_event) > 0){
    pressing_sequences[i, max_passing_options := relevant_event$n_passing_options]
    pressing_sequences[i, player_in_possession_id := relevant_event$player_in_possession_id]
  }
}


#### Get incoming passing data ####
incoming_pass_data <- link_player_possessions_with_incoming_passes(events)
incoming_pass_data <- incoming_pass_data[,
  .(frame_start, frame_end, event_type, team_id, player_name, start_type,
    incoming_high_pass, incoming_pass_distance_received, incoming_pass_range_received)
]


# add to pressing sequence
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







# # View results - Player pressing summary
# player_press_summary <- pressing_results$individual_pressing[, .(
#   team_id = first(team_id),
#   total_presses = .N,
#   avg_press_distance = round(mean(distance_to_ball_carrier, na.rm = TRUE), 2),
#   avg_approach_velocity = round(mean(approach_velocity, na.rm = TRUE), 2)
# ), by = .(player_id, short_name)][order(-total_presses)]

#__________________________________________________________________________________

#### Analyze specific frames with passing options for passing lane coverage ####
frame_sample <- sort(unique(tracking_data_update$frame))
passing_lane_analysis <- map_dfr(frame_sample, ~ {
  result <- analyze_passing_lane_coverage(tracking_data_update, events, .x)
  if(!is.null(result)) {
    result[, frame := .x]
  }
  result
})



############################################
##              TESTING                   ##
############################################


test <- events |>
  select(event_id, frame_start, frame_end, event_type, player_name, team_id,
         player_in_possession_name, pass_angle, pass_angle_received, interplayer_angle)




