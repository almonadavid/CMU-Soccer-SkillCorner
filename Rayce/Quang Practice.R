library(tidyverse)
library(xml2)

#_____________________________________________________________
#### Load XML File ####
#_____________________________________________________________

xml_file <- "~/Downloads/Quang Practice/DFL_02_01_matchinformation_DFL-COM-000001_DFL-MAT-J03WMX.xml"
doc <- read_xml(xml_file)

#_____________________________________________________________
#### Match Information ####
#_____________________________________________________________

general <- xml_find_first(doc, "//General")
match_info <- tibble(
  match_id     = xml_attr(general, "MatchId"),
  competition  = xml_attr(general, "CompetitionName"),
  match_day    = xml_attr(general, "MatchDay"),
  season       = xml_attr(general, "Season"),
  kickoff_time = xml_attr(general, "KickoffTime"),
  home_team    = xml_attr(general, "HomeTeamName"),
  home_team_id = xml_attr(general, "HomeTeamId"),
  away_team    = xml_attr(general, "GuestTeamName"),
  away_team_id = xml_attr(general, "GuestTeamId"),
  result       = xml_attr(general, "Result")
)

#_____________________________________________________________
#### Player Information ####
#_____________________________________________________________

extract_team_players <- function(team_node) {
  team_id   <- xml_attr(team_node, "TeamId")
  team_name <- xml_attr(team_node, "TeamName")
  team_role <- xml_attr(team_node, "Role")
  
  players <- xml_find_all(team_node, ".//Player")
  
  if (length(players) == 0) return(tibble())
  
  tibble(
    team_id      = team_id,
    team_name    = team_name,
    team_role    = team_role,
    person_id    = xml_attr(players, "PersonId"),
    shirt_number = as.integer(xml_attr(players, "ShirtNumber")),
    first_name   = xml_attr(players, "FirstName"),
    last_name    = xml_attr(players, "LastName"),
    position     = xml_attr(players, "PlayingPosition"),
    starting     = xml_attr(players, "Starting") == "true",
    captain      = xml_attr(players, "TeamLeader") == "true"
  )
}

teams <- xml_find_all(doc, "//Team")
players_df <- map_df(teams, extract_team_players)

#_____________________________________________________________
#### Stadium Information ####
#_____________________________________________________________

env <- xml_find_first(doc, "//Environment")
stadium_df <- tibble(
  stadium_name     = xml_attr(env, "StadiumName"),
  stadium_capacity = as.integer(xml_attr(env, "StadiumCapacity")),
  attendance       = as.integer(xml_attr(env, "NumberOfSpectators")),
  temperature      = as.integer(xml_attr(env, "Temperature")),
  pitch_x          = as.numeric(xml_attr(env, "PitchX")),
  pitch_y          = as.numeric(xml_attr(env, "PitchY"))
)

#_____________________________________________________________
#### Events Data ####
#_____________________________________________________________

# Read events file
doc_events <- read_xml("~/Downloads/Quang Practice/DFL_03_02_events_raw_DFL-COM-000001_DFL-MAT-J03WMX.xml")

# Extract all events
events <- xml_find_all(doc_events, "//Event")


if (length(events) == 0) {
  warning("No event data found in XML.")
  events_df <- tibble()
  events_detailed_df <- tibble()
} else {
  events_df <- tibble(
    event_id   = xml_attr(events, "EventId"),
    event_time = xml_attr(events, "EventTime"),
    x_position = as.numeric(xml_attr(events, "X-Position")),
    y_position = as.numeric(xml_attr(events, "Y-Position")),
    match_id   = xml_attr(events, "MatchId")
  )
  
  events_df$event_type <- map_chr(events, function(event) {
    child <- xml_child(event)
    if (!is.na(child)) xml_name(child) else NA_character_
  })
  
  extract_all_event_details <- function(event) {
    event_type_node <- xml_child(event)
    if (length(event_type_node) == 0) return(tibble())
    
    attrs <- xml_attrs(event_type_node)
    play_node <- xml_find_first(event, ".//Play")
    
    if (!is.na(play_node)) {
      play_attrs <- xml_attrs(play_node)
      names(play_attrs) <- paste0("play_", tolower(names(play_attrs)))
      attrs <- c(attrs, play_attrs)
    }
    
    as_tibble(t(attrs))
  }
  
  event_details_list <- map(events, extract_all_event_details)
  all_columns <- unique(unlist(map(event_details_list, names)))
  
  event_details_standardized <- map(event_details_list, function(details) {
    for (col in setdiff(all_columns, names(details))) {
      details[[col]] <- NA_character_
    }
    details[all_columns]
  })
  
  event_details_df <- bind_rows(event_details_standardized)
  events_detailed_df <- bind_cols(events_df, event_details_df)
}

#_____________________________________________________________
#### Position Tracking Data ####
#_____________________________________________________________

framesets <- xml_find_all(doc, "//FrameSet")

process_frameset <- function(frameset) {
  team_id      <- xml_attr(frameset, "TeamId")
  person_id    <- xml_attr(frameset, "PersonId")
  game_section <- xml_attr(frameset, "GameSection")
  frames       <- xml_find_all(frameset, ".//Frame")
  
  if (length(frames) == 0) return(tibble())
  
  tibble(
    team_id    = team_id,
    person_id  = person_id,
    game_section = game_section,
    frame_n    = as.integer(xml_attr(frames, "N")),
    timestamp  = xml_attr(frames, "T"),
    x          = as.numeric(xml_attr(frames, "X")),
    y          = as.numeric(xml_attr(frames, "Y")),
    distance   = as.numeric(xml_attr(frames, "D")),
    speed      = as.numeric(xml_attr(frames, "S")),
    acceleration = as.numeric(xml_attr(frames, "A")),
    m          = as.integer(xml_attr(frames, "M"))
  )
}

process_all_positions <- function(doc, chunk_size = 10) {
  framesets <- xml_find_all(doc, "//FrameSet")
  n_chunks <- ceiling(length(framesets) / chunk_size)
  all_positions <- list()
  
  for (i in seq_len(n_chunks)) {
    message("Processing chunk ", i, " of ", n_chunks)
    start_idx <- (i - 1) * chunk_size + 1
    end_idx   <- min(i * chunk_size, length(framesets))
    
    chunk <- framesets[start_idx:end_idx]
    chunk_data <- map_df(chunk, process_frameset)
    all_positions[[i]] <- chunk_data
  }
  
  bind_rows(all_positions)
}

positions_df <- process_all_positions(doc)

#_____________________________________________________________
#### Save All ####
#_____________________________________________________________

save_match_data <- function(match_id, output_dir = "match_data") {
  dir.create(output_dir, showWarnings = FALSE)
  
  saveRDS(match_info, file.path(output_dir, paste0(match_id, "_match_info.rds")))
  saveRDS(players_df, file.path(output_dir, paste0(match_id, "_players.rds")))
  saveRDS(stadium_df, file.path(output_dir, paste0(match_id, "_stadium.rds")))
  saveRDS(events_detailed_df, file.path(output_dir, paste0(match_id, "_events.rds")))
  saveRDS(positions_df, file.path(output_dir, paste0(match_id, "_positions.rds")))
  
  message("All data saved to ", output_dir)
}

# Call save
save_match_data("J03WMX")

#Analysis--------------------------------------------------------------------------------------------

library(ggforce)
library(ggplot2)
library(tidyverse)


# 1. Identify the rows *after* each CornerKick
next_after_corners <- events_detailed_df %>%
  mutate(row_id = row_number()) %>%
  filter(event_type == "CornerKick") %>%
  mutate(next_row = row_id + 1) %>%
  select(next_row)

# 2. Extract those "next" rows and clean
next_events <- events_detailed_df %>%
  mutate(row_id = row_number()) %>%
  filter(row_id %in% next_after_corners$next_row) %>%
  mutate(
    x_position = as.numeric(x_position),
    y_position = as.numeric(y_position)
  ) %>%
  drop_na(x_position, y_position)

# 3. Define the soccer field plot function (same as before)
plot_soccer_field <- function() {
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = "palegreen4", color = "white") +
    geom_rect(aes(xmin = 0, xmax = 6, ymin = 36, ymax = 64), fill = NA, color = "white") +
    geom_rect(aes(xmin = 94, xmax = 100, ymin = 36, ymax = 64), fill = NA, color = "white") +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color = "white") +
    geom_circle(aes(x0 = 50, y0 = 50, r = 8), color = "white", fill = NA) +
    coord_fixed() +
    theme_void()
}

# 4. Plot those "next" events
plot_soccer_field() +
  geom_point(
    data = next_events,
    aes(x = x_position, y = y_position),
    color = "blue", size = 3, alpha = 0.7
  ) +
  labs(
    title = "Events Immediately After Corner Kicks"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16)
  )

#------------------------------------------------------------------


library(ggforce)
library(ggplot2)
library(tidyverse)



library(ggforce)
library(ggplot2)
library(tidyverse)

# 1. Add row numbers to preserve order
events_detailed_df <- events_detailed_df %>%
  arrange(event_time) %>%
  mutate(row_id = row_number())

# 2. Find row IDs for 1–4 frames after CornerKick
corner_rows <- events_detailed_df %>%
  filter(event_type == "CornerKick") %>%
  mutate(
    next_row_1 = row_id + 1,
    next_row_2 = row_id + 2,
    next_row_3 = row_id + 3,
    next_row_4 = row_id + 4
  ) %>%
  select(next_row_1, next_row_2, next_row_3, next_row_4)

# 3. Extract events for each frame
get_frame_events <- function(row_ids, frame_label) {
  events_detailed_df %>%
    filter(row_id %in% row_ids) %>%
    mutate(
      x_position = as.numeric(x_position),
      y_position = as.numeric(y_position),
      frame = frame_label
    ) %>%
    drop_na(x_position, y_position)
}

next_events_1 <- get_frame_events(corner_rows$next_row_1, "1_frame_after")
next_events_2 <- get_frame_events(corner_rows$next_row_2, "2_frames_after")
next_events_3 <- get_frame_events(corner_rows$next_row_3, "3_frames_after")
next_events_4 <- get_frame_events(corner_rows$next_row_4, "4_frames_after")

# 4. Combine all frames
all_events <- bind_rows(next_events_1, next_events_2, next_events_3, next_events_4)

# 5. Soccer field plot function
plot_soccer_field <- function() {
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = "palegreen4", color = "white") +
    geom_rect(aes(xmin = 0, xmax = 6, ymin = 36, ymax = 64), fill = NA, color = "white") +
    geom_rect(aes(xmin = 94, xmax = 100, ymin = 36, ymax = 64), fill = NA, color = "white") +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color = "white") +
    geom_circle(aes(x0 = 50, y0 = 50, r = 8), color = "white", fill = NA) +
    coord_fixed() +
    theme_void()
}

# 6. Plot events with frame-based color
plot_soccer_field() +
  geom_point(
    data = all_events,
    aes(x = x_position, y = y_position, color = frame),
    size = 3, alpha = 0.8
  ) +
  scale_color_manual(
    values = c(
      "1_frame_after" = "dodgerblue",
      "2_frames_after" = "orange",
      "3_frames_after" = "purple",
      "4_frames_after" = "deeppink"
    )
  ) +
  labs(
    title = "Events 1 to 4 Frames After Corner Kicks",
    color = "Frame"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  )

###############


library(ggplot2)
library(ggforce)
library(tidyverse)

# 1. Add row number and assign unique ID to each corner kick
events_detailed_df <- events_detailed_df %>%
  arrange(event_time) %>%
  mutate(row_id = row_number())

corner_kicks <- events_detailed_df %>%
  filter(event_type == "CornerKick") %>%
  mutate(
    corner_id = row_number(),  # Unique ID for each corner
    row_1 = row_id + 1,
    row_2 = row_id + 2,
    row_3 = row_id + 3,
    row_4 = row_id + 4
  )

# 2. Helper to extract frame rows with corner_id
get_frame_events <- function(corner_df, row_col, frame_label) {
  events_detailed_df %>%
    inner_join(
      corner_df %>% select(corner_id, row_id = {{row_col}}),
      by = "row_id"
    ) %>%
    mutate(
      x_position = as.numeric(x_position),
      y_position = as.numeric(y_position),
      frame = frame_label
    ) %>%
    drop_na(x_position, y_position)
}

# 3. Extract corner kick (frame 0) and frames 1–4 after
corner_kick_events <- events_detailed_df %>%
  inner_join(
    corner_kicks %>% select(corner_id, row_id),
    by = "row_id"
  ) %>%
  mutate(
    x_position = as.numeric(x_position),
    y_position = as.numeric(y_position),
    frame = "0_corner_kick"
  ) %>%
  drop_na(x_position, y_position)

next_events_1 <- get_frame_events(corner_kicks, row_1, "1_frame_after")
next_events_2 <- get_frame_events(corner_kicks, row_2, "2_frames_after")
next_events_3 <- get_frame_events(corner_kicks, row_3, "3_frames_after")
next_events_4 <- get_frame_events(corner_kicks, row_4, "4_frames_after")

# 4. Combine all events in frame order
all_events <- bind_rows(
  corner_kick_events,
  next_events_1,
  next_events_2,
  next_events_3,
  next_events_4
) %>%
  mutate(
    frame_order = case_when(
      frame == "corner_kick" ~ 0,
      frame == "1_frame" ~ 1,
      frame == "2_frames" ~ 2,
      frame == "3_frames" ~ 3,
      frame == "4_frames" ~ 4
    )
  )

# 5. Soccer field drawing function
library(ggplot2)
library(ggforce)  # for geom_circle()

plot_soccer_field <- function() {
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 105, ymin = 0, ymax = 68), fill = "palegreen4", color = "white") +
    geom_rect(aes(xmin = 0, xmax = 5.5, ymin = 24.84, ymax = 43.16), fill = NA, color = "white") +
    geom_rect(aes(xmin = 99.5, xmax = 105, ymin = 24.84, ymax = 43.16), fill = NA, color = "white") +
    geom_segment(aes(x = 52.5, xend = 52.5, y = 0, yend = 68), color = "white") +
    geom_circle(aes(x0 = 52.5, y0 = 34, r = 9.15), color = "white", fill = NA) +
    coord_fixed() +
    theme_void()
}


# 6. Plot points and connecting lines
plot_soccer_field() +
  # Connect points for each corner kick
  geom_path(
    data = all_events,
    aes(x = x_position, y = y_position, group = corner_id, color = factor(corner_id)),
    linewidth = 1.2,
    alpha = 0.6
  ) +
  # Show individual points for each frame
  geom_point(
    data = all_events,
    aes(x = x_position, y = y_position, color = factor(corner_id), shape = frame),
    size = 3, alpha = 0.9
  ) +
  labs(
    title = "Ball Trajectories Starting from Corner Kicks",
    color = "Corner Kick ID",
    shape = "Frame"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  )

################################################################

install.packages("gganimate")   # if not installed
library(gganimate)
library(ggplot2)
library(ggforce)
library(tidyverse)
library(gifski)

# --- 1. Prepare Event Data ---
events_detailed_df <- events_detailed_df %>%
  arrange(event_time) %>%
  mutate(row_id = row_number())

# Assign corner_id and define frame rows
corner_kicks <- events_detailed_df %>%
  filter(event_type == "CornerKick") %>%
  mutate(
    corner_id = row_number(),
    row_1 = row_id + 1,
    row_2 = row_id + 2,
    row_3 = row_id + 3,
    row_4 = row_id + 4
  )

# Function to extract frame events with frame label and corner ID
get_frame_events <- function(corner_df, row_col, frame_label) {
  events_detailed_df %>%
    inner_join(
      corner_df %>% select(corner_id, row_id = {{row_col}}),
      by = "row_id"
    ) %>%
    mutate(
      x_position = as.numeric(x_position),
      y_position = as.numeric(y_position),
      frame = frame_label
    ) %>%
    drop_na(x_position, y_position)
}

# Get frames 1–4
next_events_1 <- get_frame_events(corner_kicks, row_1, "1")
next_events_2 <- get_frame_events(corner_kicks, row_2, "2")
next_events_3 <- get_frame_events(corner_kicks, row_3, "3")
next_events_4 <- get_frame_events(corner_kicks, row_4, "4")

# Combine all frames
all_events <- bind_rows(next_events_1, next_events_2, next_events_3, next_events_4) %>%
  mutate(frame_number = as.integer(frame))

# --- 2. Soccer Field Plot Function ---
plot_soccer_field <- function() {
  ggplot() +
    geom_rect(aes(xmin = 0, xmax = 100, ymin = 0, ymax = 100), fill = "palegreen4", color = "white") +
    geom_rect(aes(xmin = 0, xmax = 6, ymin = 36, ymax = 64), fill = NA, color = "white") +
    geom_rect(aes(xmin = 94, xmax = 100, ymin = 36, ymax = 64), fill = NA, color = "white") +
    geom_segment(aes(x = 50, xend = 50, y = 0, yend = 100), color = "white") +
    geom_circle(aes(x0 = 50, y0 = 50, r = 8), color = "white", fill = NA) +
    coord_fixed() +
    theme_void()
}

# --- 3. Create Animated Plot ---
animated_plot <- plot_soccer_field() +
  geom_point(
    data = all_events,
    aes(x = x_position, y = y_position, group = corner_id, color = factor(corner_id)),
    size = 4
  ) +
  labs(
    title = "Ball Movement After Corner Kicks",
    subtitle = "Frame: {closest_state}",
    color = "Corner Kick ID"
  ) +
  transition_states(
    frame_number,
    transition_length = 2,
    state_length = 1
  ) +
  ease_aes('linear') +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, size = 14),
    legend.position = "right"
  )

# --- 4. Animate and Save as GIF ---
animate(
  animated_plot,
  renderer = gifski_renderer("corner_kick_animation.gif"),
  fps = 2,
  nframes = 8,
  width = 800,
  height = 500
)



 
