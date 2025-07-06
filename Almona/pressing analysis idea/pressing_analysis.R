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


#### Find ball carrier for any given frame ####
unique_frames <- sort(unique(tracking_data_update$frame))
ball_carrier_df <- map_dfr(unique_frames, ~ get_ball_carrier(.x, events)) # This will take a LONG while


#### Approach Velocity Calculation ####
approach_velocity_results <- calculate_approach_velocity(
  tracking_data = tracking_data_update,
  ball_carrier_df = ball_carrier_df,
  frame_rate = 10
)


#### Individual pressing stats ####
individual_pressing_stats <- approach_velocity_results %>%
  arrange(pressing_defender_id, frame) %>%
  group_by(pressing_defender_id, pressing_defender_name) %>%
  mutate(
    frame_diff = frame - lag(frame, default = first(frame) - 2),
    new_sequence = frame_diff > 1,  # Gap of more than 1 frame = new sequence
    sequence_id = cumsum(new_sequence)
  ) %>%
  summarise(
    total_pressing_sequences = max(sequence_id),
    total_pressing_frames = n(),
    avg_sequence_length = total_pressing_frames / total_pressing_sequences,
    avg_approach_velocity = mean(approach_velocity, na.rm = TRUE),
    max_approach_velocity = max(approach_velocity, na.rm = TRUE),
    avg_distance_when_pressing = mean(distance_to_ball_carrier, na.rm = TRUE),
    min_distance_achieved = min(distance_to_ball_carrier, na.rm = TRUE),
    pressing_time_seconds = total_pressing_frames / 10,
    pressing_time_minutes = pressing_time_seconds / 60,
    avg_sequence_duration_seconds = pressing_time_seconds / total_pressing_sequences,
    .groups = "drop"
  ) %>%
  arrange(desc(total_pressing_sequences)) %>%
  mutate(
    avg_approach_velocity = round(avg_approach_velocity, 2),
    max_approach_velocity = round(max_approach_velocity, 2),
    avg_distance_when_pressing = round(avg_distance_when_pressing, 2),
    min_distance_achieved = round(min_distance_achieved, 2),
    avg_sequence_length = round(avg_sequence_length, 1),
    pressing_time_minutes = round(pressing_time_minutes, 2),
    avg_sequence_duration_seconds = round(avg_sequence_duration_seconds, 1)
  )


#### Calculate Collective Pressing ####
collective_pressing <- calculate_collective_pressing(
  tracking_data_update, 
  ball_carrier_df
)

# Adding to existing approach velocity results
enhanced_results <- approach_velocity_results[collective_pressing, 
                                              on = "frame", nomatch = 0]


#### Calculate Defensive Compactness ####
defensive_compactness <- calculate_defensive_compactness(
  tracking_data_update, 
  ball_carrier_df
)

#### Calculate Defensive Compactness Using Area of Smallest Polygon ####
team_compactness <- calculate_defensive_area(
  tracking_data_update, 
  ball_carrier_df
)


#### Analyze specific frames with passing lane coverage ####
frame_sample <- unique(ball_carrier_df$frame)[1:100]  # Sample frames
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
  select(event_id, frame_start, frame_end, event_type, player_name, passing_option_at_pass_moment, team_id,
         player_in_possession_name, n_passing_options, associated_player_possession_event_id)



