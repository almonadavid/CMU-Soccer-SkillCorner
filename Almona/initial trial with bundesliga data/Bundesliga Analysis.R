# Load libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(gganimate)
library(deldir)
library(grid)
 

#### Run "xml to dataframe.rds" first ####


## Read data and convert to data.table
match_info = readRDS("J03WMX_match_info.rds") |> 
  setDT()

players = readRDS("J03WMX_players.rds") |> 
  setDT()

stadium = readRDS("J03WMX_stadium.rds") |> 
  setDT()

events = readRDS("J03WMX_events.rds") |> 
  setDT() |> 
  _[, event_time := ymd_hms(event_time)] # consistent timezone with positions df

positions = readRDS("J03WMX_positions.rds") |> 
  setDT() |> 
  _[, timestamp := ymd_hms(timestamp)] |>  # consistent timezone with events df
  _[, !c("speed", "distance", "acceleration", "m")] # remove these columns


## Calculating direction
calculate_direction <- function(data, frame_rate = 25) {
  
  # Calculate 99.9th percentile thresholds to clean extreme values of derived metrics
  thresholds <- data[, .(
    speed_threshold = quantile(sqrt((shift(x, type = "lead") - x)^2 + (shift(y, type = "lead") - y)^2) * frame_rate, 
                               0.999, na.rm = TRUE),
    accel_threshold = quantile(abs((shift(sqrt((shift(x, type = "lead") - x)^2 + (shift(y, type = "lead") - y)^2) * frame_rate, type = "lead") - 
                                      sqrt((shift(x, type = "lead") - x)^2 + (shift(y, type = "lead") - y)^2) * frame_rate) * frame_rate), 
                               0.999, na.rm = TRUE)
  ), by = team_id]
  
  
  result <- data[order(person_id, frame_n)][, `:=`(
    dx = shift(x, type = "lead") - x,
    dy = shift(y, type = "lead") - y
  ), by = person_id][, `:=`(
    distance = sqrt(dx^2 + dy^2),
    direction = atan2(dy, dx) * 180 / pi,
    speed = sqrt(dx^2 + dy^2) * frame_rate
  )][, acceleration := (shift(speed, type = "lead") - speed) * frame_rate, by = person_id]
  

  result <- thresholds[result, on = "team_id"][, `:=`(
    speed = ifelse(speed > speed_threshold, NA, speed),
    acceleration = ifelse(abs(acceleration) > accel_threshold, NA, acceleration),
    speed_threshold = NULL,
    accel_threshold = NULL
  )]
  
  return(result)
}


positions <- calculate_direction(positions)


## Calculating minutes played (considering VAR stoppages and extra time, represents time spent on the pitch, not necessarily game time played)
calculate_minutes_played <- function(data, frame_rate = 25) {
  play_time <- data[!team_id %in% c("BALL", "referee"), .(
    first_frame = min(frame_n),
    last_frame = max(frame_n),
    minutes_played = ((max(frame_n) - min(frame_n)) / frame_rate) / 60
  ), by = person_id][, .(person_id, minutes_played)]
  
  return(play_time)
}

game_time <- calculate_minutes_played(positions) |> 
  left_join(players) |> 
  select(first_name, last_name, team_name, minutes_played)


## Calculate distance covered by each player
calculate_distance_covered <- function(data) {
  distance_covered <- data[!team_id %in% c("BALL", "referee"), .(
    total_distance = sum(distance, na.rm = TRUE),
    total_distance_km = sum(distance, na.rm = TRUE) / 1000
  ), by = person_id][, .(person_id, total_distance, total_distance_km)]
  
  return(distance_covered)
}

player_distance <- calculate_distance_covered(positions) |> 
  left_join(players) |> 
  select(first_name, last_name, team_name, total_distance, total_distance_km)


## Distance covered walking, jogging, running and sprinting + number of sprints
calculate_speed_zones <- function(data, frame_rate = 25) {
  
  # Speed thresholds (m/s)
  walking_max <- 2.0
  jogging_max <- 4.0  
  running_max <- 5.5
  
  # Minimum frames for sustained sprint (1.5 seconds = ~37 frames at 25Hz)
  min_sprint_frames <- round(1.5 * frame_rate)
  
  speed_analysis <- data[!team_id %in% c("BALL", "referee")][order(person_id, frame_n)][, `:=`(
    speed_zone = fcase(
      is.na(speed), "unknown",
      speed <= walking_max, "walking",
      speed <= jogging_max, "jogging", 
      speed <= running_max, "running",
      speed > running_max, "sprinting",
      default = "unknown"
    ),
    is_sprinting = speed > running_max & !is.na(speed)
  ), by = person_id][, `:=`(
    sprint_group = cumsum(!is_sprinting | (is_sprinting & shift(is_sprinting, fill = FALSE) == FALSE))
  ), by = person_id][, `:=`(
    sprint_length = sum(is_sprinting),
    is_sustained_sprint = is_sprinting & sum(is_sprinting) >= min_sprint_frames
  ), by = .(person_id, sprint_group)][, .(
    distance_walking = sum(ifelse(speed_zone == "walking", distance, 0), na.rm = TRUE),
    distance_jogging = sum(ifelse(speed_zone == "jogging", distance, 0), na.rm = TRUE),
    distance_running = sum(ifelse(speed_zone == "running", distance, 0), na.rm = TRUE),
    distance_sprinting = sum(ifelse(speed_zone == "sprinting", distance, 0), na.rm = TRUE),
    total_distance = sum(distance, na.rm = TRUE),
    sustained_sprints = length(unique(sprint_group[is_sustained_sprint])),
    max_speed = max(speed, na.rm = TRUE),
    avg_speed = mean(speed, na.rm = TRUE)
  ), by = person_id][, .(person_id, distance_walking, distance_jogging, distance_running, 
                         distance_sprinting, total_distance, sustained_sprints, max_speed, avg_speed)]
  
  return(speed_analysis)
}

player_distance_category <- calculate_speed_zones(positions) |> 
  left_join(players) |> 
  select(first_name, last_name, team_name, distance_walking, distance_jogging, distance_running, 
         distance_sprinting, total_distance, sustained_sprints, max_speed, avg_speed)


# next steps:
# filter for corners
# plot corner positioning
# plot player movement trajectory plot
# https://squared2020.com/2017/10/25/the-art-of-sketching-trajectory-analysis/

corner_events <- events[event_type == "CornerKick"]
freekick_events <- events[event_type == "FreeKick"] # idea: look at freekicks in attacking half
event_time <- as.POSIXct("2023-05-27 13:45:16", tz = "UTC")

ggplot(data = positions[timestamp == event_time & frame_n == 32585]) +
  annotate("rect",xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = 0, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -36.75, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 36.75, xmax = 52.5, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 0, xend = 0, y = -34.5, yend = 34.5, colour = "black", size = 0.6)+
  annotate("segment", x = -52.5, xend = -52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  annotate("segment", x = 52.5, xend = 52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  theme(rect = element_blank(), line = element_blank()) +
  annotate("point", x = -42 , y = 0, colour = "black", size = 1.05) + # add penalty spot left
  annotate("point", x = 42 , y = 0, colour = "black", size = 1.05) + # add penalty spot right
  annotate("path", colour = "black", size = 0.6, x=0+8.75*cos(seq(0,2*pi,length.out=2000)),
           y=0+8.75*sin(seq(0,2*pi,length.out=2000))) + # add centre circle
  annotate("point", x = 0 , y = 0, colour = "black", size = 1.05) + # add centre spot
  annotate("path", x=-42+8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0+8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + # left penalty arc
  annotate("path", x=42-8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0-8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + # right penalty arc
  geom_point(data = anim_data, aes(x = x, 
                                   y = ifelse(team_id == "BALL" & y == -19.46, -34, y), 
                                   color = team_id), 
             size = 3, alpha = 0.9) + 
  coord_fixed() +
  scale_color_discrete(labels = c("DFL-CLU-000008" = "1. FC Köln", "DFL-CLU-00000G" = "FC Bayern München")) +
  theme_void()


#### GGANIMATE ####
event_time <- as.POSIXct("2023-05-27 13:45:16", tz = "UTC")

target_frame <- positions[timestamp == event_time, frame_n][1] # find first frame that matched event time
frames_before <- 0
frames_after <- 300
frame_range <- (target_frame - frames_before):(target_frame + frames_after)

anim_data <- positions[frame_n %in% frame_range]


# Create the animated plot
p <- ggplot(data = anim_data) +
  annotate("rect",xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = 0, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -36.75, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 36.75, xmax = 52.5, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 0, xend = 0, y = -34.5, yend = 34.5, colour = "black", size = 0.6)+
  annotate("segment", x = -52.5, xend = -52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  annotate("segment", x = 52.5, xend = 52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  annotate("point", x = -42 , y = 0, colour = "black", size = 1.05) +
  annotate("point", x = 42 , y = 0, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6, x=0+8.75*cos(seq(0,2*pi,length.out=2000)),
           y=0+8.75*sin(seq(0,2*pi,length.out=2000))) +
  annotate("point", x = 0 , y = 0, colour = "black", size = 1.05) +
  annotate("path", x=-42+8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0+8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=42-8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0-8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_spoke(data = anim_data[!team_id %in% c("BALL", "referee") & speed > 0.5], 
             aes(x = x, y = y, angle = direction * pi/180, 
                 radius = speed/3), 
             arrow = arrow(length = unit(1, "mm"))) +
  geom_point(data = anim_data, aes(x = x, 
                                   y = ifelse(team_id == "BALL" & y == -19.46, -34, y), 
                                   color = team_id), 
             size = 3, alpha = 0.9) +
  coord_fixed() +
  scale_color_discrete(labels = c("DFL-CLU-000008" = "1. FC Köln", "DFL-CLU-00000G" = "FC Bayern München")) +
  labs(
    #caption = "25 Hz",
    caption = "1. FC Köln vs FC Bayern München | Bundesliga Matchday 34, 2022/2023 Season | May 27, 2023 | Final Score: 1-2"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "black")
  ) +
  transition_time(frame_n)

# Render the animation
play <- animate(p, nframes = length(frame_range), fps = 25, width = 800, height = 600)

# Display the animation
play



anim <- animate(p, 
                width = 1920, height = 1080,
                res = 200,                   
                fps = 25,                     
                nframes = length(frame_range))

anim_save("corner_kick_presentation.gif", anim)



