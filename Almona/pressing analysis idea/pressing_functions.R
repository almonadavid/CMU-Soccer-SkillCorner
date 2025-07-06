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
get_ball_carrier <- function(frame_number, data) {
  
  relevant_events <- data |>
    filter(frame_start <= frame_number & frame_end >= frame_number) |>
    filter(!is.na(player_in_possession_id)) |>
    arrange(desc(frame_start))
  
  if(nrow(relevant_events) > 0) {
    return(data.frame(
      frame = frame_number,
      ball_carrier_id = relevant_events$player_in_possession_id[1],
      ball_carrier_name = relevant_events$player_in_possession_name[1],
      possession_team = relevant_events$team_id[1],
      stringsAsFactors = FALSE
    ))
  } else {
    return(data.frame(
      frame = frame_number,
      ball_carrier_id = NA,
      ball_carrier_name = NA,
      possession_team = NA,
      stringsAsFactors = FALSE
    ))
  }
}

#### Approach Velocity Calculation ####
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
  defenders_with_ball_info <- tracking_with_possession[
    ball_carrier_positions, on = "frame"
  ][
    team_id != possession_team & !is.na(ball_carrier_id)  # Only opposing team
  ]
  
  # Calculate distances and filter close defenders
  defenders_with_ball_info[, `:=`(
    dx = ball_carrier_x - x,
    dy = ball_carrier_y - y,
    distance_to_ball_carrier = sqrt((ball_carrier_x - x)^2 + (ball_carrier_y - y)^2)
  )]
  
  # Filter defenders within pressing distance (say, 20 meters)
  close_defenders <- defenders_with_ball_info[distance_to_ball_carrier <= 20]
  
  # Calculate approach velocity
  close_defenders[, `:=`(
    # Unit direction vector from defender to ball carrier
    r_hat_x = dx / distance_to_ball_carrier,
    r_hat_y = dy / distance_to_ball_carrier,
    
    # Relative velocity (ball_carrier - defender)
    rel_vel_x = ball_carrier_vel_x - vel_x,
    rel_vel_y = ball_carrier_vel_y - vel_y
  )]
  
  # Calculate approach velocity: -(relative_velocity · unit_vector)
  close_defenders[, approach_velocity := -(rel_vel_x * r_hat_x + rel_vel_y * r_hat_y)]
  close_defenders[approach_velocity > 15, approach_velocity := NA]
  
  # Identify primary presser for each frame
  primary_pressers <- close_defenders[
    approach_velocity > 0  # Only defenders actually approaching
  ][, .SD[which.max(approach_velocity)], by = frame]  # Highest approach velocity per frame
  
  
  result <- primary_pressers[, .(
    frame,
    ball_carrier_id,
    ball_carrier_name,
    possession_team,
    pressing_defender_id = player_id,
    pressing_defender_name = short_name,
    approach_velocity,
    distance_to_ball_carrier,
    ball_carrier_x,
    ball_carrier_y,
    defender_x = x,
    defender_y = y
  )]
  
  return(result)
}

#### Team Pressing Intensity ####
calculate_collective_pressing <- function(tracking_data, ball_carrier_df, pressing_radius = 20) {
  
  setDT(tracking_data)
  setDT(ball_carrier_df)
  
  # Get ball carrier positions
  ball_carrier_positions <- tracking_data[ball_carrier_df, on = "frame"][
    player_id == ball_carrier_id & !is.na(ball_carrier_id),
    .(frame, ball_carrier_x = x, ball_carrier_y = y, possession_team)
  ]
  
  # Get all defenders with distances to ball carrier
  defenders_analysis <- tracking_data[ball_carrier_positions, on = "frame"][
    team_id != possession_team & !is.na(possession_team)
  ]
  
  defenders_analysis[, distance_to_ball := sqrt((ball_carrier_x - x)^2 + (ball_carrier_y - y)^2)]
  
  # Calculate collective pressing metrics per frame
  pressing_metrics <- defenders_analysis[, .(
    # Number of defenders in pressing zone
    defenders_in_press_zone = sum(distance_to_ball <= pressing_radius, na.rm = TRUE),
    
    # Average distance of closest 3 defenders
    avg_closest_3_distance = mean(sort(distance_to_ball)[1:3], na.rm = TRUE),
    
    # Defensive centroid distance to ball
    defensive_centroid_x = mean(x, na.rm = TRUE),
    defensive_centroid_y = mean(y, na.rm = TRUE)
  ), by = .(frame, possession_team)]
  
  # Add centroid distance to ball
  pressing_metrics[ball_carrier_positions, `:=`(
    centroid_to_ball_distance = sqrt((defensive_centroid_x - ball_carrier_x)^2 + 
                                       (defensive_centroid_y - ball_carrier_y)^2)
  ), on = "frame"]
  
  return(pressing_metrics)
}

#### Team Defensive Compactness Using Avg Distance ####
calculate_defensive_compactness <- function(tracking_data, ball_carrier_df) {
  
  setDT(tracking_data)
  setDT(ball_carrier_df)
  
  # Merge tracking data with ball carrier info
  tracking_with_possession <- tracking_data[ball_carrier_df, on = "frame"]
  
  # Filter out frames with no possession
  valid_frames <- tracking_with_possession[!is.na(possession_team)]
  
  # Calculate compactness for each frame
  team_compactness <- valid_frames[, {
    
    # Get unique teams in this frame
    teams_present <- unique(team_id)
    teams_present <- teams_present[!is.na(teams_present)]
    
    # Identify defending team (the one NOT in possession)
    defending_team <- teams_present[teams_present != possession_team[1]]
    
    if(length(defending_team) == 1) {
      # Get defending team players only
      defenders <- .SD[team_id == defending_team & !is.na(x) & !is.na(y)]
      
      if(nrow(defenders) > 1) {
        # Calculate pairwise distances
        distances <- as.matrix(dist(defenders[, .(x, y)]))
        avg_distance <- mean(distances[upper.tri(distances)], na.rm = TRUE)
        max_distance <- max(distances, na.rm = TRUE)
        
        data.table(
          defending_team = defending_team,
          defensive_compactness = avg_distance,
          defensive_spread = max_distance,
          num_defenders = nrow(defenders)
        )
      } else {
        NULL
      }
    } else {
      NULL
    }
  }, by = .(frame, possession_team)]
  
  return(team_compactness)
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

## Team Defensive Compactness Using Area Of Smallest Polygon ####
calculate_defensive_area <- function(tracking_data, ball_carrier_df) {
  
  setDT(tracking_data)
  setDT(ball_carrier_df)
  
  tracking_with_possession <- tracking_data[ball_carrier_df, on = "frame"]
  valid_frames <- tracking_with_possession[!is.na(possession_team)]
  
  team_compactness <- valid_frames[, {
    teams_present <- unique(team_id)
    teams_present <- teams_present[!is.na(teams_present)]
    defending_team <- teams_present[teams_present != possession_team[1]]
    
    if(length(defending_team) == 1) {
      defenders <- .SD[team_id == defending_team & !is.na(x) & !is.na(y)]
      
      if(nrow(defenders) >= 3) {  # Need at least 3 points for a polygon
        # Calculate convex hull area
        coords <- as.matrix(defenders[, .(x, y)])
        hull_indices <- chull(coords)
        hull_coords <- coords[hull_indices, ]
        
        # Calculate polygon area using shoelace formula
        n <- nrow(hull_coords)
        area <- 0.5 * abs(sum(hull_coords[1:(n-1), 1] * hull_coords[2:n, 2] - 
                                hull_coords[2:n, 1] * hull_coords[1:(n-1), 2]) +
                            hull_coords[n, 1] * hull_coords[1, 2] - 
                            hull_coords[1, 1] * hull_coords[n, 2])
        
        # Also calculate centroid for reference
        centroid_x <- mean(defenders$x)
        centroid_y <- mean(defenders$y)
        
        data.table(
          defending_team = defending_team,
          defensive_area = area,  # Square meters
          defensive_centroid_x = centroid_x,
          defensive_centroid_y = centroid_y,
          num_defenders = nrow(defenders)
        )
      }
    }
  }, by = .(frame, possession_team)]
  
  return(team_compactness)
}