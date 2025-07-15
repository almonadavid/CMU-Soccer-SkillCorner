library(tidyverse)


# Parameters are defined as arguments in function rather than hardcoded within for easier manipulation

#### Calculating metrics ####
calculate_metrics <- function(data, frame_rate = 10,
                              max_realistic_speed = 11.9,       # Usain Bolt max speed was 12 m/s
                              max_realistic_accel = 10.0,       # m/s²
                              outlier_percentile = 0.999,       # Remove top 0.001%
                              speed_smooth_window = 3,          # Window for speed smoothing
                              accel_smooth_window = 5) {        # Window for acceleration smoothing
  
  result <- copy(data)
  setorder(result, player_id, frame)
  
  result[, `:=`(
    dx = x - data.table::shift(x, type = "lag"),
    dy = y - data.table::shift(y, type = "lag")
  ), by = player_id]
  
  result[, `:=`(
    distance = sqrt(dx^2 + dy^2),
    direction = atan2(dy, dx) * 180 / pi,
    speed = sqrt(dx^2 + dy^2) * frame_rate
  )]
  
  result[, acceleration := (speed - data.table::shift(speed, type = "lag")) * frame_rate, 
         by = player_id]
  
  # Handle extreme values
  max_distance_per_frame <- max_realistic_speed / frame_rate
  
  # Remove physically impossible values
  result[distance > max_distance_per_frame, distance := NA]  # Remove "teleporting"
  result[speed > max_realistic_speed | speed < 0, speed := NA]  # Remove impossible speeds
  result[abs(acceleration) > max_realistic_accel, acceleration := NA]  # Remove impossible accelerations
  
  # Remove statistical outliers
  speed_999 <- quantile(result$speed, outlier_percentile, na.rm = TRUE)
  accel_999 <- quantile(abs(result$acceleration), outlier_percentile, na.rm = TRUE)
  
  result[speed > speed_999, speed := NA]
  result[abs(acceleration) > accel_999, acceleration := NA]
  
  result[is.na(speed), distance := NA]
  
  # Add smoothing
  result[, `:=`(
    speed_smooth = frollmean(speed, n = speed_smooth_window, align = "center"),
    acceleration_smooth = frollmean(acceleration, n = accel_smooth_window, align = "center")
  ), by = player_id]
  
  return(result)
}

#### Find ball carrier for any given frame ####
get_ball_carrier <- function(frames, events) {
  
  setDT(events)
  
  frames_dt <- data.table(frame = frames) # store all frames
  
  player_poss <- events[event_type == "player_possession"]
  
  ball_carriers <- player_poss[frames_dt, 
                               .(frame = i.frame,
                                 ball_carrier_id = player_in_possession_id,
                                 ball_carrier_name = player_in_possession_name,
                                 possession_team = team_id),
                               on = .(frame_start <= frame, frame_end >= frame),
                               mult = "last"
  ]
  
  return(ball_carriers)
}

#### Approach Velocity with Distance Weighting ####
calculate_approach_velocity <- function(tracking_data, ball_carrier_df, 
                                        frame_rate = 10,
                                        max_realistic_speed = 11.9,      # m/s
                                        max_press_distance = 15, #????        # meters
                                        approach_threshold = 1,          # m/s threshold for moving toward ball
                                        weight_min_distance = 2,         # Distance weight parameters
                                        weight_max_distance = 10) {      # Distance weight parameters
  
  setDT(tracking_data)
  setDT(ball_carrier_df)
  
  # Calculate velocities for each player # ## USE SMOOTHED SPEED CALCULATED FROM calculate_metrics #####################
  tracking_data <- tracking_data[order(player_id, frame)]
  tracking_data[, `:=`(
    vel_x = (x - data.table::shift(x, type = "lag")) * frame_rate,
    vel_y = (y - data.table::shift(y, type = "lag")) * frame_rate
  ), by = player_id]
  
  # Handle extreme values
  tracking_data[, `:=`(
    vel_magnitude = sqrt(vel_x^2 + vel_y^2)
  )]
  tracking_data[vel_magnitude > max_realistic_speed | is.na(vel_magnitude), `:=`(
    vel_x = NA,
    vel_y = NA
  )]
  tracking_data[, vel_magnitude := NULL] 
  
  # Merge ball carrier info to tracking data
  tracking_with_possession <- tracking_data[ball_carrier_df, on = "frame"]
  
  # Ball carrier position for each frame
  ball_carrier_positions <- tracking_with_possession[
    player_id == ball_carrier_id & !is.na(ball_carrier_id),
    .(frame, ball_carrier_id, ball_carrier_x = x, ball_carrier_y = y, 
      ball_carrier_vel_x = vel_x, ball_carrier_vel_y = vel_y, possession_team)
  ]
  
  # Find potential pressing defenders
  defenders <- tracking_with_possession[
    ball_carrier_positions, on = "frame"
  ][
    team_id != possession_team & !is.na(ball_carrier_id)
  ]
  
  # Calculate distances and filter close defenders
  defenders[, `:=`(
    dx = ball_carrier_x - x,
    dy = ball_carrier_y - y,
    distance_to_ball_carrier = sqrt((ball_carrier_x - x)^2 + (ball_carrier_y - y)^2)
  )]
  
  # Filter defenders within pressing distance
  close_defenders <- defenders[distance_to_ball_carrier <= max_press_distance]
  
  # Calculate approach velocity
  close_defenders[, `:=`(
    r_hat_x = dx / distance_to_ball_carrier,
    r_hat_y = dy / distance_to_ball_carrier,
    rel_vel_x = ball_carrier_vel_x - vel_x,
    rel_vel_y = ball_carrier_vel_y - vel_y
  )]
  
  close_defenders[, approach_velocity := -(rel_vel_x * r_hat_x + rel_vel_y * r_hat_y)]

  # Calculate angle from ball carrier to defender
  close_defenders[, angle_from_ball_carrier_to_defender := atan2(-dy, -dx) * 180 / pi]

  # Distance-weighted approach velocity, giving more weight to approach velocity when further away
  weight_range <- weight_max_distance - weight_min_distance
  close_defenders[, distance_weight := pmin(1, pmax(0, (distance_to_ball_carrier - weight_min_distance) / weight_range))]
  close_defenders[, weighted_approach_velocity := approach_velocity * distance_weight]
  close_defenders[, moving_toward_ball := approach_velocity > approach_threshold]
  
  return(close_defenders)
}

#### Criteria-Based Pressing Detection ####
detect_pressing_action <- function(tracking_data, ball_carrier_df,
                                   frame_rate = 10,
                                   press_distance = 6,
                                   initial_distance = 4,
                                   approach_vel_threshold = 1.0,
                                   close_approach_vel = 0.3,
                                   defender_speed_threshold = 3.5,
                                   approach_smooth_window = 3,        # Window for approach velocity smoothing
                                   accel_threshold = 0.5) {           # Approach acceleration threshold
  
  enhanced_data <- calculate_approach_velocity(tracking_data, ball_carrier_df, frame_rate)
  enhanced_data <- enhanced_data[order(player_id, frame)]
  
  # Look at movement patterns over multiple frames
  # first smooth 
  enhanced_data[, `:=`(
    approach_velocity_smooth = frollmean(approach_velocity, n = approach_smooth_window, align = "center")
  ), by = player_id]
  
  enhanced_data[, `:=`(
    approach_acceleration = (approach_velocity_smooth - data.table::shift(approach_velocity_smooth, type = "lag")) * frame_rate, # acceleration toward ball carrier
    defender_speed = sqrt(vel_x^2 + vel_y^2)  ## USE SMOOTHED SPEED CALCULATED FROM calculate_metrics #####################
  ), by = player_id]
  
  # Pressing criteria
  enhanced_data[, `:=`(
    within_press_distance = distance_to_ball_carrier <= press_distance,
    initial_approach = distance_to_ball_carrier > initial_distance & approach_velocity > approach_vel_threshold,
    close_engagement = distance_to_ball_carrier <= initial_distance & (
      approach_velocity > close_approach_vel | 
        defender_speed > defender_speed_threshold |
        approach_acceleration > accel_threshold
    )
  )]
  
  enhanced_data[, is_pressing := within_press_distance & (initial_approach | close_engagement)]
  
  enhanced_data[, pressing_team_id := team_id]
  
  return(enhanced_data)
}

#### Pressing Sequence Detection #### 
identify_pressing_sequences <- function(enhanced_data, frame_rate = 10, max_frame_gap = 15) {    # Max frames between presses to be same sequence
  
  # Get frames where pressing is happening
  pressing_frames <- enhanced_data[is_pressing == TRUE, .(
    n_pressers = .N
  ), by = .(frame, possession_team)]
  
  # Sort by frame and identify sequence breaks
  setorder(pressing_frames, possession_team, frame)
  pressing_frames[, frame_gap := frame - data.table::shift(frame, fill = frame[1]), by = possession_team]
  pressing_frames[, new_sequence := frame_gap > max_frame_gap]
  pressing_frames[, sequence_group := cumsum(new_sequence), by = possession_team]
  
  # Sequence summary
  sequences <- pressing_frames[, .(
    sequence_start_frame = min(frame),
    sequence_end_frame = max(frame),
    sequence_duration_frames = .N,
    sequence_duration_seconds = .N / frame_rate
  ), by = .(possession_team, sequence_group)]
  
  # Add sequence_id
  sequences[, sequence_id := .I]
  
  # For each sequence, get player information
  sequence_details <- list()
  
  for(i in 1:nrow(sequences)) {
    seq <- sequences[i]
    
    # Get all pressing action at the start of the sequence
    initial_metrics <- enhanced_data[
      is_pressing == TRUE & 
        frame == seq$sequence_start_frame &
        possession_team == seq$possession_team
    ]
    
    # Calculate metrics for this sequence
    details <- data.table(
      sequence_id = seq$sequence_id,
      n_pressing_defenders = uniqueN(initial_metrics$player_id),
      avg_approach_velocity = round(mean(initial_metrics$approach_velocity, na.rm = TRUE), 3),
      pressing_team_id = unique(initial_metrics$pressing_team_id)[1]
    )
    
    sequence_details[[i]] <- details
  }
  
  # Combine everything
  sequence_details_dt <- rbindlist(sequence_details)
  final_sequences <- merge(sequences, sequence_details_dt, by = "sequence_id")
  
  # Remove the temporary sequence_group column
  final_sequences[, sequence_group := NULL]
  
  return(final_sequences)
}

#### Get incoming passing data ####
link_player_possessions_with_incoming_passes <- function(events) {
  
  # Separate events by type
  player_possessions <- events[event_type == "player_possession"]
  
  passing_options <- events[event_type == "passing_option" & received == TRUE]
  
  passing_options_subset <- passing_options[, .(
    associated_player_possession_event_id,
    incoming_high_pass = high_pass,
    incoming_pass_distance_received = pass_distance_received,
    incoming_pass_range_received = pass_range_received,
    incoming_pass_angle_received = pass_angle_received,
    incoming_pass_direction_received = pass_direction_received,
    incoming_pass_outcome = pass_outcome,
    matched_passing_option_id = event_id
  )]
  
  enhanced_possessions <- merge(
    player_possessions,
    passing_options_subset,
    by.x = "event_id",
    by.y = "associated_player_possession_event_id",
    all.x = TRUE
  )
  
  enhanced_possessions[, has_incoming_pass_data := !is.na(matched_passing_option_id)]
  
  enhanced_possessions[, incoming_pass_distance_received := fifelse(
    is.na(incoming_pass_distance_received),
    pass_distance_received,
    incoming_pass_distance_received
  )]
  
  enhanced_possessions[, incoming_pass_range_received := fifelse(
    is.na(incoming_pass_range_received),
    pass_range_received,
    incoming_pass_range_received
  )]
  
  enhanced_possessions[, incoming_pass_angle_received := fifelse(
    is.na(incoming_pass_angle_received),
    pass_angle_received,
    incoming_pass_angle_received
  )]
  
  enhanced_possessions[, incoming_pass_direction_received := fifelse(
    is.na(incoming_pass_direction_received),
    pass_direction_received,
    incoming_pass_direction_received
  )]
  
  return(enhanced_possessions)
}

# #### Passing Lane Analysis ####
# analyze_passing_lane_coverage <- function(tracking_data, events, frame_number,
#                                           lane_coverage_distance = 4,      # Max distance from line to be "covering"
#                                           min_line_length = 0.1) {         # Minimum passing line length to analyze
#   
#   setDT(events)
#   
#   # Get current player possession
#   current_possession <- events[
#     frame_start <= frame_number & frame_end >= frame_number & 
#       event_type == "player_possession"
#   ]
#   
#   if(nrow(current_possession) == 0) {
#     return(NULL)
#   }
#   
#   # Get passing options associated with this player possession
#   possession_event_id <- current_possession$event_id[1]
#   
#   current_passing_options <- events[
#     frame_start <= frame_number & frame_end >= frame_number & 
#       event_type == "passing_option" &
#       associated_player_possession_event_id == possession_event_id
#   ]
#   
#   if(nrow(current_passing_options) == 0) {
#     return(NULL)
#   }
#   
#   # Get tracking data for this frame
#   frame_tracking <- tracking_data[frame == frame_number]
#   possession_team <- current_possession$team_id[1]
#   
#   # Ball carrier position
#   ball_carrier_id <- current_possession$player_id[1]
#   ball_carrier_pos <- frame_tracking[player_id == ball_carrier_id][1]
#   
#   if(nrow(ball_carrier_pos) == 0 || is.na(ball_carrier_pos$x)) {
#     return(NULL)
#   }
#   
#   # Analyze each passing option
#   passing_analysis <- current_passing_options[, {
#     # Target player position
#     target_player_id <- player_id
#     target_pos <- frame_tracking[player_id == target_player_id][1]
#     
#     if(nrow(target_pos) > 0 && !is.na(target_pos$x) && 
#        target_player_id != ball_carrier_id) {
#       
#       # Get defending team players
#       defenders <- frame_tracking[team_id != possession_team]
#       
#       # Calculate how many defenders are "covering" this passing lane
#       # (within some distance of the line between ball carrier and target)
#       
#       # Line ball carrier and target
#       line_dx <- target_pos$x - ball_carrier_pos$x
#       line_dy <- target_pos$y - ball_carrier_pos$y
#       line_length <- sqrt(line_dx^2 + line_dy^2)
#       
#       if(length(line_length) == 1 && !is.na(line_length) && line_length > min_line_length) {
#         
#         # For each defender, calculate distance to passing line
#         defender_distances <- defenders[, {
#           # Vector from ball carrier to defender
#           to_defender_x <- x - ball_carrier_pos$x
#           to_defender_y <- y - ball_carrier_pos$y
#           
#           # Project onto passing line
#           t <- (to_defender_x * line_dx + to_defender_y * line_dy) / (line_length^2)
#           t <- pmax(0, pmin(1, t))
#           
#           # Closest point on line
#           closest_x <- ball_carrier_pos$x + t * line_dx
#           closest_y <- ball_carrier_pos$y + t * line_dy
#           
#           distance_to_line <- sqrt((x - closest_x)^2 + (y - closest_y)^2)
#           
#           .(defender_id = player_id, distance_to_line, t)
#         }]
#         
#         # Count defenders "covering" this lane
#         covering_defenders <- sum(defender_distances$distance_to_line <= lane_coverage_distance, na.rm = TRUE)
#         
#         data.table(
#           ball_carrier_id = ball_carrier_id,
#           target_player_id = target_player_id,
#           passing_distance = line_length,
#           defenders_covering_lane = covering_defenders,
#           min_defender_distance_to_lane = min(defender_distances$distance_to_line, na.rm = TRUE),
#           passing_option_covered = covering_defenders > 0
#         )
#       }
#     }
#   }, by = .(player_id)]
#   
#   return(passing_analysis)
# }
