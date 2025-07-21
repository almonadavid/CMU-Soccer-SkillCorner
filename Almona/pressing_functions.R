library(tidyverse)


# Parameters are defined as arguments in function rather than hardcoded within

#### Calculating metrics ####
calculate_metrics <- function(data, frame_rate = 10,
                              max_realistic_speed = 11.9,       # Usain Bolt max speed was 12 m/s
                              max_realistic_accel = 10.0,       # m/s²
                              outlier_percentile = 0.999,    
                              speed_smooth_window = 3,         
                              accel_smooth_window = 5) {        
  
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
                                        approach_threshold = 1,          # m/s threshold for moving toward ball
                                        weight_min_distance = 2,      
                                        weight_max_distance = 10) {     
  
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
  
  # Calculate approach velocity
  defenders[, `:=`(
    r_hat_x = dx / distance_to_ball_carrier,
    r_hat_y = dy / distance_to_ball_carrier,
    rel_vel_x = ball_carrier_vel_x - vel_x,
    rel_vel_y = ball_carrier_vel_y - vel_y
  )]
  
  defenders[, approach_velocity := -(rel_vel_x * r_hat_x + rel_vel_y * r_hat_y)]

  # Calculate angle from ball carrier to defender
  defenders[, angle_from_ball_carrier_to_defender := atan2(-dy, -dx) * 180 / pi]

  # Distance-weighted approach velocity, giving more weight to approach velocity when further away
  weight_range <- weight_max_distance - weight_min_distance
  defenders[, distance_weight := pmin(1, pmax(0, (distance_to_ball_carrier - weight_min_distance) / weight_range))]
  defenders[, weighted_approach_velocity := approach_velocity * distance_weight]
  defenders[, moving_toward_ball := approach_velocity > approach_threshold]
  
  return(defenders)
}

#### Pressing Detection with Oval Pressure Zone ####
detect_pressing_action <- function(tracking_data, ball_carrier_df,
                                   frame_rate = 10,
                                   approach_vel_threshold = 1.0, # we want active pressing not just standing still in the pressure zone
                                   approach_smooth_window = 3) {
  
  enhanced_data <- calculate_approach_velocity(tracking_data, ball_carrier_df, frame_rate)
  enhanced_data <- enhanced_data[order(player_id, frame)]
  
  # Add goal center coordinates based on possession team
  # Home team attacks right (positive x), away team attacks left (negative x)
  enhanced_data[, `:=`(
    goal_center_x = fifelse(possession_team == home_team_id, 52.5, -52.5),
    goal_center_y = 0
  )]
  
  # Calculate threat direction vector (ball carrier to goal center)
  enhanced_data[, `:=`(
    threat_vector_x = goal_center_x - ball_carrier_x,
    threat_vector_y = goal_center_y - ball_carrier_y
  )]
  
  # Calculate target-to-presser vector (ball carrier to defender)
  enhanced_data[, `:=`(
    target_to_presser_x = x - ball_carrier_x,
    target_to_presser_y = y - ball_carrier_y
  )]
  
  # Calculate angle between threat direction and target-to-presser direction
  enhanced_data[, `:=`(
    threat_magnitude = sqrt(threat_vector_x^2 + threat_vector_y^2),
    presser_magnitude = sqrt(target_to_presser_x^2 + target_to_presser_y^2)
  )]
  
  # Dot product for angle calculation
  enhanced_data[, dot_product := threat_vector_x * target_to_presser_x + threat_vector_y * target_to_presser_y]
  
  # Calculate angle (in radians, then convert if needed)
  enhanced_data[, angle_theta := acos(pmax(-1, pmin(1, dot_product / (threat_magnitude * presser_magnitude))))]
  
  # Calculate z for oval formula
  enhanced_data[, z := (1 - cos(angle_theta)) / 2]
  
  # Calculate oval pressure zone boundary distance (Dfront = 9, Dback = 3)
  enhanced_data[, pressure_zone_boundary := 9 + (3 - 9) * (z^3 + 0.3 * z) / 1.3]
  
  # Check if defender is within oval pressure zone
  enhanced_data[, within_oval_pressure_zone := distance_to_ball_carrier <= pressure_zone_boundary]
  
  # Smooth approach velocity
  enhanced_data[, `:=`(
    approach_velocity_smooth = frollmean(approach_velocity, n = approach_smooth_window, align = "center")
  ), by = player_id]
  
  
  # Tag pressing
  enhanced_data[, is_pressing := within_oval_pressure_zone & approach_velocity > approach_vel_threshold]
  
  enhanced_data[, pressing_team_id := team_id]
  
  enhanced_data[, `:=`(
    threat_vector_x = NULL, threat_vector_y = NULL,
    target_to_presser_x = NULL, target_to_presser_y = NULL,
    threat_magnitude = NULL, presser_magnitude = NULL,
    dot_product = NULL, z = NULL
  )]
  
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
