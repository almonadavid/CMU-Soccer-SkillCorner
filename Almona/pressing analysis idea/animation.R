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

#### Get Team Information ####
home_team_name <- match_info$home_team$name
away_team_name <- match_info$away_team$name
home_team_id <- match_info_df$home_team$id
away_team_id <- match_info_df$away_team$id
players_info <- match_info_df$players

#### Add player info and team names to tracking_data ####
# Add player information
tracking_data <- tracking_data |> 
  left_join(
    players_info |> select(id, team_id, first_name, last_name, short_name, number),
    join_by(player_id == id)
  )

# Create team mapping and add team names
team_mapping <- data.frame(
  team_id = c(home_team_id, away_team_id),
  team_name = c(home_team_name, away_team_name)
)

tracking_data <- tracking_data |> 
  left_join(team_mapping, by = "team_id")


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

#### Calculate distance, direction, speed, acceleration ####
tracking_data <- calculate_direction(tracking_data)

#### GGANIMATE ####
event_time <- "00:33:42.00"
s
target_frame <- tracking_data[timestamp == event_time, frame][1]
frames_before <- 50
frames_after <- 200
frame_range <- 17050:17250 #(target_frame - frames_before):(target_frame + frames_after)

anim_data <- tracking_data[frame %in% frame_range]


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
  geom_spoke(data = anim_data[speed_smooth > 0.5],
             aes(x = x, y = y, angle = direction * pi/180,
                 radius = speed_smooth),
             arrow = arrow(length = unit(2, "mm"))) +
  geom_point(data = anim_data, aes(x = x, 
                                   y = y, 
                                   color = team_name), 
             size = 4, alpha = 0.9) +
  # geom_text(data = anim_data, aes(x = x, y = y, label = number), 
  #           size = 5, color = "black") + 
  geom_point(data = anim_data, aes(x = ball_data.x, 
                                   y = ball_data.y,
                                   color = "BALL"), 
             size = 3, alpha = 0.9) +
  coord_fixed() +
  # scale_color_discrete(...) +
  # labs(...) +
  theme_void() +
  # theme(...) +
  transition_time(frame)

# Render the animation
play <- animate(p, nframes = length(frame_range), fps = 20, width = 800, height = 600)

# Display the animation
play




ggplot(data = smoothed[frame >= 20760 & frame <= 20860 & number == 93, ]) +
  geom_line(aes(x = frame, y = acceleration, color = "Raw"), alpha = 0.7) +
  geom_line(aes(x = frame, y = acceleration_smooth, color = "Smoothed"), size = 1) +
  scale_color_manual(values = c("Raw" = "red", "Smoothed" = "blue")) +
  labs(title = "Acceleration: Raw vs Smoothed (Player 93)",
       y = "Acceleration (m/s²)",
       x = "Frame",
       color = "Data Type") +
  theme_minimal()

ggplot(data = smoothed[frame >= 20760 & frame <= 20860 & number == 93, ]) +
  geom_line(aes(x = frame, y = speed, color = "Raw"), alpha = 0.7) +
  geom_line(aes(x = frame, y = speed_smooth, color = "Smoothed"), size = 1) +
  scale_color_manual(values = c("Raw" = "red", "Smoothed" = "blue")) +
  labs(title = "Speed: Raw vs Smoothed (Player 93)",
       y = "Speed (m/s)",
       x = "Frame",
       color = "Data Type") +
  theme_minimal()  
  
  