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

#### Distance covered at different speed zones ####
calculate_speed_zones <- function(data, frame_rate = 10) {
  
  # Speed bands from SkillCorner in m/s (converted from km/h)
  jogging_max <- 4.16667 # ~ 15km/h
  running_max <- 5.55556 # ~ 20km/h  
  hsr_max <- 6.94444 # ~ 25km/h
  # sprinting > 25km/h
  
  
  # Minimum frames for sustained sprint (1 seconds = ~10 frames at 10Hz)
  min_sprint_frames <- round(1 * frame_rate)
  
  speed_analysis <- data[order(player_id, frame)][, `:=`(
    speed_zone = fcase(
      is.na(speed), "unknown",
      speed <= jogging_max, "jogging", 
      speed <= running_max, "running",
      speed <= hsr_max, "hsr",
      speed > hsr_max, "sprinting",
      default = "unknown"
    ), 
    is_sprinting = speed > hsr_max & !is.na(speed)
  ), by = player_id][, `:=`(
    sprint_group = cumsum(!is_sprinting | (is_sprinting & shift(is_sprinting, fill = FALSE) == FALSE))
  ), by = player_id][, `:=`(
    sprint_length = sum(is_sprinting),
    is_sustained_sprint = is_sprinting & sum(is_sprinting) >= min_sprint_frames
  ), by = .(player_id, sprint_group)][, .(
    short_name = first(short_name),       
    team_id = first(team_id), 
    distance_jogging = sum(ifelse(speed_zone == "jogging", distance, 0), na.rm = TRUE),
    distance_running = sum(ifelse(speed_zone == "running", distance, 0), na.rm = TRUE),
    distance_hsr = sum(ifelse(speed_zone == "hsr", distance, 0), na.rm = TRUE),
    distance_sprinting = sum(ifelse(speed_zone == "sprinting", distance, 0), na.rm = TRUE),
    total_distance = sum(distance, na.rm = TRUE),
    sustained_sprints = length(unique(sprint_group[is_sustained_sprint])),
    max_speed = max(speed, na.rm = TRUE),
    avg_speed = mean(speed, na.rm = TRUE)
  ), by = player_id][, .(short_name, player_id, team_id, distance_jogging, distance_running, 
                         distance_hsr, distance_sprinting, total_distance, sustained_sprints, max_speed, avg_speed)]
  
  return(speed_analysis)
}