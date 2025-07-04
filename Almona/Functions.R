# library(tidyverse)
# library(data.table)



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

#### Calculating minutes played (considering VAR stoppages and extra time, represents time spent on the pitch, not necessarily game time played) ####
calculate_minutes_played <- function(data, frame_rate = 10) {
  play_time <- data[, .(
    short_name = first(short_name),
    team_id = first(team_id),
    first_frame = min(frame),
    last_frame = max(frame),
    minutes_played = ((max(frame) - min(frame)) / frame_rate) / 60
  ), by = player_id][, .(player_id, minutes_played, short_name, team_id)]
  
  return(play_time)
}

#### Calculate distance covered by each player ####
calculate_distance_covered <- function(data) {
  distance_covered <- data[, .(
    short_name = first(short_name),
    team_id = first(team_id),
    total_distance = sum(distance, na.rm = TRUE),
    total_distance_km = sum(distance, na.rm = TRUE) / 1000
  ), by = player_id][, .(short_name, player_id, team_id, total_distance, total_distance_km)]
  
  return(distance_covered)
}

#### Distance covered walking, jogging, running and sprinting + number of sprints ####
calculate_speed_zones <- function(data, frame_rate = 10) {
  
  # Speed thresholds (m/s)
  walking_max <- 2.0
  jogging_max <- 4.0  
  running_max <- 5.5 # {not to self: adjust this to match SkillCorner metrics}
  
  # Minimum frames for sustained sprint (1.5 seconds = ~37 frames at 25Hz)
  min_sprint_frames <- round(1.5 * frame_rate)
  
  speed_analysis <- data[order(player_id, frame)][, `:=`(
    speed_zone = fcase(
      is.na(speed), "unknown",
      speed <= walking_max, "walking",
      speed <= jogging_max, "jogging", 
      speed <= running_max, "running",
      speed > running_max, "sprinting",
      default = "unknown"
    ), 
    is_sprinting = speed > running_max & !is.na(speed)
  ), by = player_id][, `:=`(
    sprint_group = cumsum(!is_sprinting | (is_sprinting & shift(is_sprinting, fill = FALSE) == FALSE))
  ), by = player_id][, `:=`(
    sprint_length = sum(is_sprinting),
    is_sustained_sprint = is_sprinting & sum(is_sprinting) >= min_sprint_frames
  ), by = .(player_id, sprint_group)][, .(
    short_name = first(short_name),       
    team_id = first(team_id), 
    distance_walking = sum(ifelse(speed_zone == "walking", distance, 0), na.rm = TRUE),
    distance_jogging = sum(ifelse(speed_zone == "jogging", distance, 0), na.rm = TRUE),
    distance_running = sum(ifelse(speed_zone == "running", distance, 0), na.rm = TRUE),
    distance_sprinting = sum(ifelse(speed_zone == "sprinting", distance, 0), na.rm = TRUE),
    total_distance = sum(distance, na.rm = TRUE),
    sustained_sprints = length(unique(sprint_group[is_sustained_sprint])),
    max_speed = max(speed, na.rm = TRUE),
    avg_speed = mean(speed, na.rm = TRUE)
  ), by = player_id][, .(short_name, player_id, team_id, distance_walking, distance_jogging, distance_running, 
                         distance_sprinting, total_distance, sustained_sprints, max_speed, avg_speed)]
  
  return(speed_analysis)
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
