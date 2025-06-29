library(tidyverse)
library(jsonlite)
library(tidyr)
library(sf)


tracking_data <- fromJSON("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/structured_data.json")
unnested_data <- unnest(tracking_data, cols = c(data))


match_info <- fromJSON("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/match_data.json", simplifyDataFrame = FALSE)
match_info_df <- fromJSON("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/match_data.json")


#### Get Team Information ####
home_team_name <- match_info$home_team$name
away_team_name <- match_info$away_team$name

print(paste("Home Team:", home_team_name))
print(paste("Away Team:", away_team_name))


#### Get Player Information ####
players_df <- match_info_df$players

#### Get the lineups for each team ####
home_team_id <- match_info_df$home_team$id
away_team_id <- match_info_df$away_team$id

home_players <- players_df[players_df$team_id == home_team_id, ]
away_players <- players_df[players_df$team_id == away_team_id, ]

# Show the home team lineup (player name and role)
# Note: player_role is a nested data frame, so we access its 'name' column
home_lineup <- data.frame(
  Name = home_players$first_name,
  Surname = home_players$last_name,
  Role = home_players$player_role$acronym
)

#home_lineup

# Show the away team lineup
away_lineup <- data.frame(
  Name = away_players$first_name,
  Surname = away_players$last_name,
  Role = away_players$player_role$acronym
)

#away_lineup


#### Get Referee and Pitch Information ####
referee_info <- match_info_df$referees

#referee_info

# Get pitch dimensions
pitch_length <- match_info_df$pitch_length
pitch_width <- match_info_df$pitch_width
print(paste("Pitch Dimensions:", pitch_length, "m x", pitch_width, "m"))


#### Merge Tracking Data with Player Information ####
player_details <- select(players_df, trackable_object, first_name, last_name, player_role)

full_tracking_data <- left_join(unnested_data, player_details, by = "trackable_object")

home_team_id <- match_info$home_team$id
away_team_id <- match_info$away_team$id

full_tracking_data <- full_tracking_data %>%
  mutate(team_name = case_when(
    group_name == "home team" ~ match_info$home_team$name,
    group_name == "away team" ~ match_info$away_team$name,
    TRUE ~ group_name # For ball, referees, etc.
  ))


#### Example Analysis 1: Calculating Player Speed and Distance Covered ####
player_movements <- full_tracking_data %>%
  filter(!is.na(track_id)) %>% # Focus on tracked players
  arrange(track_id, frame) %>%
  group_by(track_id) %>%
  mutate(
    distance = sqrt((x - lag(x))^2 + (y - lag(y))^2),
    time_diff = 0.1, # time between frames is 1/10th of a second (10 fps)
    speed_mps = distance / time_diff
  ) %>%
  ungroup()

# Calculate total distance covered by each player
total_distance_per_player <- player_movements %>%
  group_by(first_name, last_name) %>%
  summarise(total_distance_meters = sum(distance, na.rm = TRUE)) %>%
  arrange(desc(total_distance_meters))


#### Example Analysis 2: Ball Possession ####
possession_data <- tracking_data %>%
  select(frame, possession) %>%
  unnest(cols = c(possession))

# Merge with player details to get player names 
possession_with_names <- left_join(possession_data, player_details, by = c("trackable_object"))

# Calculate possession time for each team
possession_summary <- possession_with_names %>%
  filter(!is.na(group)) %>%
  group_by(group) %>%
  summarise(possession_seconds = n() * 0.1)

# Which player had the ball the most
player_possession <- possession_with_names %>%
  filter(!is.na(first_name)) %>%
  group_by(first_name, last_name) %>%
  summarise(possession_seconds = n() * 0.1) %>%
  arrange(desc(possession_seconds))


#=============================================================================