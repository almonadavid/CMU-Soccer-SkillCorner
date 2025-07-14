library(tidyverse)


#### Calculating direction ####
calculate_direction <- function(data, frame_rate = 10) {
  
  result <- data[order(player_id, frame)][, `:=`(
    dx = x - shift(x, type = "lag"),
    dy = y - shift(y, type = "lag")
  ), by = player_id][, `:=`(
    distance = sqrt(dx^2 + dy^2),
    direction = atan2(dy, dx) * 180 / pi,
    speed = sqrt(dx^2 + dy^2) * frame_rate
  )][, `:=`(
    acceleration = (speed - shift(speed, type = "lag")) * frame_rate
  ), by = player_id]
  
  
  # Data cleaning for extreme values
  max_realistic_speed <- 11.9  # Usain Bolt max speed was 12 m/s
  max_realistic_accel <- 10.0  # m/s² 
  max_distance_per_frame <- max_realistic_speed / frame_rate
  
  result[, `:=`(
    distance = ifelse(distance > max_distance_per_frame, NA, distance), # Remove impossible distances ('teleporting')
    speed = ifelse(speed > max_realistic_speed | speed < 0, NA, speed), # Remove impossible speeds 
    acceleration = ifelse(abs(acceleration) > max_realistic_accel, NA, acceleration) # Remove impossible accelerations 
  )]
  
  speed_999 <- quantile(result$speed, 0.999, na.rm = TRUE)
  accel_999 <- quantile(abs(result$acceleration), 0.999, na.rm = TRUE)
  
  result[, `:=`(
    speed = ifelse(speed > speed_999, NA, speed),
    acceleration = ifelse(abs(acceleration) > accel_999, NA, acceleration)
  )]
  
  result[, distance := ifelse(is.na(speed), NA, distance)]
  
  return(result)
}

#### Find ball carrier for any given frame ####
get_ball_carrier <- function(frames, events) {
  
  setDT(events)
  
  frames_dt <- data.table(frame = frames) # store all frames with data.table
  
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
calculate_approach_velocity <- function(tracking_data, ball_carrier_df, frame_rate = 10) {
  
  setDT(tracking_data)
  setDT(ball_carrier_df)
  
  # Calculate velocities for each player
  tracking_data <- tracking_data[order(player_id, frame)]
  tracking_data[, `:=`(
    vel_x = (x - shift(x, type = "lag")) * frame_rate,
    vel_y = (y - shift(y, type = "lag")) * frame_rate
  ), by = player_id]
  
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
  close_defenders <- defenders[distance_to_ball_carrier <= 20]
  
  # Calculate approach velocity
  close_defenders[, `:=`(
    r_hat_x = dx / distance_to_ball_carrier,
    r_hat_y = dy / distance_to_ball_carrier,
    rel_vel_x = ball_carrier_vel_x - vel_x,
    rel_vel_y = ball_carrier_vel_y - vel_y
  )]
  
  close_defenders[, approach_velocity := -(rel_vel_x * r_hat_x + rel_vel_y * r_hat_y)]
  
  # Distance-weighted approach velocity, giving more weight to approach velocity when further away
  close_defenders[, distance_weight := pmin(1, pmax(0, (distance_to_ball_carrier - 2) / 8))]
  close_defenders[, weighted_approach_velocity := approach_velocity * distance_weight]
  close_defenders[, moving_toward_ball := approach_velocity > 0.5]
  
  return(close_defenders)
}

#### Criteria-Baed Pressing Detection ####
detect_pressing_action <- function(tracking_data, ball_carrier_df,
                                   frame_rate = 10,
                                   press_distance = 6,
                                   initial_distance = 4,
                                   approach_vel_threshold = 1.0,
                                   close_approach_vel = 0.3,
                                   defender_speed_threshold = 2.0) {
  
  enhanced_data <- calculate_approach_velocity(tracking_data, ball_carrier_df, frame_rate)
  enhanced_data <- enhanced_data[order(player_id, frame)]
  
  # Look at movement patterns over multiple frames
  enhanced_data[, `:=`(
    avg_approach_velocity_3f = frollmean(approach_velocity, n = 3, align = "right"),
    recent_approach_count = frollsum(moving_toward_ball, n = 5, align = "right"),
    approach_acceleration = (approach_velocity - shift(approach_velocity, type = "lag")) * frame_rate, # acceleration toward ball carrier
    defender_speed = sqrt(vel_x^2 + vel_y^2)
  ), by = player_id]
  
  # Pressing criteria
  enhanced_data[, `:=`(
    within_press_distance = distance_to_ball_carrier <= press_distance,
    initial_approach = distance_to_ball_carrier > initial_distance & approach_velocity > approach_vel_threshold,
    close_engagement = distance_to_ball_carrier <= initial_distance & (
      approach_velocity > close_approach_vel | 
        recent_approach_count >= 3 | 
        defender_speed > defender_speed_threshold |
        approach_acceleration > 0.5
    )
  )]
  
  enhanced_data[, is_pressing := within_press_distance & (initial_approach | close_engagement)]
  
  # Identify primary presser per frame (closest pressing defender)
  primary_pressers <- enhanced_data[
    is_pressing == TRUE
  ][, .SD[which.min(distance_to_ball_carrier)], by = frame]
  
  # Count total pressing defenders per frame
  pressing_summary <- enhanced_data[
    is_pressing == TRUE
  ][, .(
    total_pressing_defenders = .N,
    closest_pressing_distance = min(distance_to_ball_carrier),
    avg_approach_velocity = mean(approach_velocity, na.rm = TRUE),
    max_approach_velocity = max(approach_velocity, na.rm = TRUE)
  ), by = .(frame, possession_team)]
  
  return(list(
    individual_pressing = enhanced_data[is_pressing == TRUE],
    primary_pressers = primary_pressers,
    team_pressing_summary = pressing_summary
  ))
}

#### Pressing Sequence Detection ####
identify_pressing_sequences <- function(pressing_data, frame_rate = 10) {
  
  setDT(pressing_data$team_pressing_summary)
  
  pressing_sequences <- pressing_data$team_pressing_summary[order(frame)][, {
    
    # Find sequences where there's continuous pressing
    frame_diff <- c(1, diff(frame))
    sequence_break <- frame_diff > 1
    sequence_id <- cumsum(sequence_break)
    
    # Group by sequence and calculate metrics
    .SD[, .(
      sequence_start_frame = min(frame),
      sequence_end_frame = max(frame),
      sequence_duration_frames = .N,
      sequence_duration_seconds = .N / frame_rate,
      avg_pressing_defenders = round(mean(total_pressing_defenders), 2),
      max_pressing_defenders = max(total_pressing_defenders),
      min_distance = round(min(closest_pressing_distance), 3),
      avg_approach_velocity = round(mean(avg_approach_velocity, na.rm = TRUE), 3)
    ), by = sequence_id]
  }, by = possession_team]
  
  return(pressing_sequences)
}

#### Passing Lane Analysis ####
analyze_passing_lane_coverage <- function(tracking_data, events, frame_number) {
  
  setDT(events)
  
  # Get current player possession
  current_possession <- events[
    frame_start <= frame_number & frame_end >= frame_number & 
      event_type == "player_possession"
  ]
  
  if(nrow(current_possession) == 0) {
    return(NULL)
  }
  
  # Get passing options associated with this player possession
  possession_event_id <- current_possession$event_id[1]
  
  current_passing_options <- events[
    frame_start <= frame_number & frame_end >= frame_number & 
      event_type == "passing_option" &
      associated_player_possession_event_id == possession_event_id
  ]
  
  if(nrow(current_passing_options) == 0) {
    return(NULL)
  }
  
  # Get tracking data for this frame
  frame_tracking <- tracking_data[frame == frame_number]
  possession_team <- current_possession$team_id[1]
  
  # Ball carrier position
  ball_carrier_id <- current_possession$player_id[1]
  ball_carrier_pos <- frame_tracking[player_id == ball_carrier_id][1]
  
  if(nrow(ball_carrier_pos) == 0 || is.na(ball_carrier_pos$x)) {
    return(NULL)
  }
  
  # Analyze each passing option
  passing_analysis <- current_passing_options[, {
    # Target player position
    target_player_id <- player_id
    target_pos <- frame_tracking[player_id == target_player_id][1]
    
    if(nrow(target_pos) > 0 && !is.na(target_pos$x) && 
       target_player_id != ball_carrier_id) {
      
      # Get defending team players
      defenders <- frame_tracking[team_id != possession_team]
      
      # Calculate how many defenders are "covering" this passing lane
      # (within some distance of the line between ball carrier and target)
      
      # Line ball carrier and target
      line_dx <- target_pos$x - ball_carrier_pos$x
      line_dy <- target_pos$y - ball_carrier_pos$y
      line_length <- sqrt(line_dx^2 + line_dy^2)
      
      if(length(line_length) == 1 && !is.na(line_length) && line_length > 0.1) {
        
        # For each defender, calculate distance to passing line
        defender_distances <- defenders[, {
          # Vector from ball carrier to defender
          to_defender_x <- x - ball_carrier_pos$x
          to_defender_y <- y - ball_carrier_pos$y
          
          # Project onto passing line
          t <- (to_defender_x * line_dx + to_defender_y * line_dy) / (line_length^2)
          t <- pmax(0, pmin(1, t))
          
          # Closest point on line
          closest_x <- ball_carrier_pos$x + t * line_dx
          closest_y <- ball_carrier_pos$y + t * line_dy
          
          distance_to_line <- sqrt((x - closest_x)^2 + (y - closest_y)^2)
          
          .(defender_id = player_id, distance_to_line, t)
        }]
        
        # Count defenders "covering" this lane (within 3-5 meters of line)
        covering_defenders <- sum(defender_distances$distance_to_line <= 4, na.rm = TRUE)
        
        data.table(
          ball_carrier_id = ball_carrier_id,
          target_player_id = target_player_id,
          passing_distance = line_length,
          defenders_covering_lane = covering_defenders,
          min_defender_distance_to_lane = min(defender_distances$distance_to_line, na.rm = TRUE),
          passing_option_covered = covering_defenders > 0
        )
      }
    }
  }, by = .(player_id)]
  
  return(passing_analysis)
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