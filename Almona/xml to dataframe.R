library(tidyverse)
library(xml2)

#_____________________________________________________________
#### Match Information Data ####
#_____________________________________________________________

# Read match information
doc_match <- read_xml("C:/Users/almon/Downloads/28196177/DFL_02_01_matchinformation_DFL-COM-000001_DFL-MAT-J03WMX.xml")

# Extract match general info
general <- xml_find_first(doc_match, "//General")
match_info <- tibble(
  match_id = xml_attr(general, "MatchId"),
  competition = xml_attr(general, "CompetitionName"),
  match_day = xml_attr(general, "MatchDay"),
  season = xml_attr(general, "Season"),
  kickoff_time = xml_attr(general, "KickoffTime"),
  home_team = xml_attr(general, "HomeTeamName"),
  home_team_id = xml_attr(general, "HomeTeamId"),
  away_team = xml_attr(general, "GuestTeamName"),
  away_team_id = xml_attr(general, "GuestTeamId"),
  result = xml_attr(general, "Result")
)

# Extract player data
extract_team_players <- function(team_node) {
  team_id <- xml_attr(team_node, "TeamId")
  team_name <- xml_attr(team_node, "TeamName")
  team_role <- xml_attr(team_node, "Role")
  
  players <- xml_find_all(team_node, ".//Player")
  
  if(length(players) > 0) {
    tibble(
      team_id = team_id,
      team_name = team_name,
      team_role = team_role,
      person_id = xml_attr(players, "PersonId"),
      shirt_number = as.integer(xml_attr(players, "ShirtNumber")),
      first_name = xml_attr(players, "FirstName"),
      last_name = xml_attr(players, "LastName"),
      position = xml_attr(players, "PlayingPosition"),
      starting = xml_attr(players, "Starting") == "true",
      captain = xml_attr(players, "TeamLeader") == "true"
    )
  }
}

# Get all players
teams <- xml_find_all(doc_match, "//Team")
players_df <- map_df(teams, extract_team_players)

# Extract stadium info
env <- xml_find_first(doc_match, "//Environment")
stadium_df <- tibble(
  stadium_name = xml_attr(env, "StadiumName"),
  stadium_capacity = as.integer(xml_attr(env, "StadiumCapacity")),
  attendance = as.integer(xml_attr(env, "NumberOfSpectators")),
  temperature = as.integer(xml_attr(env, "Temperature")),
  pitch_x = as.numeric(xml_attr(env, "PitchX")),
  pitch_y = as.numeric(xml_attr(env, "PitchY"))
)


#_____________________________________________________________
#### Events Data ####
#_____________________________________________________________

# Read events file
doc_events <- read_xml("C:/Users/almon/Downloads/28196177/DFL_03_02_events_raw_DFL-COM-000001_DFL-MAT-J03WMX.xml")

# Extract all events
events <- xml_find_all(doc_events, "//Event")

# Basic event info
events_df <- tibble(
  event_id = xml_attr(events, "EventId"),
  event_time = xml_attr(events, "EventTime"),
  x_position = as.numeric(xml_attr(events, "X-Position")),
  y_position = as.numeric(xml_attr(events, "Y-Position")),
  match_id = xml_attr(events, "MatchId")
)

# Add event type
events_df$event_type <- map_chr(events, function(event) {
  child <- xml_child(event)
  if(length(child) > 0) {
    xml_name(child)
  } else {
    NA_character_
  }
})

# Let's see what event types we have
# event_types_summary <- events_df |>
#   count(event_type) |>
#   arrange(desc(n)) |> 
#   print(n = 30)

# Extraction
extract_all_event_details <- function(event) {
  event_type_node <- xml_child(event)
  if(length(event_type_node) == 0) return(tibble())
  
  event_type <- xml_name(event_type_node)
  
  # Get all attributes of the event type node
  attrs <- xml_attrs(event_type_node)
  
  # If there's a Play node, get its attributes too
  play_node <- xml_find_first(event, ".//Play")
  if(length(play_node) > 0) {
    play_attrs <- xml_attrs(play_node)
    names(play_attrs) <- paste0("play_", tolower(names(play_attrs)))
    attrs <- c(attrs, play_attrs)
  }
  
  # Convert to tibble
  as_tibble(t(attrs))
}

# Extract details for all events
event_details_list <- map(events, extract_all_event_details)

# Find all unique column names
all_columns <- unique(unlist(map(event_details_list, names)))

# Ensure all tibbles have all columns
event_details_standardized <- map(event_details_list, function(details) {
  missing_cols <- setdiff(all_columns, names(details))
  for(col in missing_cols) {
    details[[col]] <- NA_character_
  }
  details[all_columns]  # Ensure consistent column order
})

# Now combine
events_detailed_df <- bind_cols(
  events_df,
  bind_rows(event_details_standardized)
)


#_____________________________________________________________
#### Position Data  ####
#_____________________________________________________________

# Read positions file structure first
doc_position <- read_xml("C:/Users/almon/Downloads/28196177/DFL_04_03_positions_raw_observed_DFL-COM-000001_DFL-MAT-J03WMX.xml")

# Count framesets to estimate data size
framesets <- xml_find_all(doc_position, "//FrameSet")
cat("Number of FrameSets:", length(framesets), "\n")

# Process one frameset at a time to avoid memory issues
process_frameset <- function(frameset) {
  team_id <- xml_attr(frameset, "TeamId")
  person_id <- xml_attr(frameset, "PersonId")
  game_section <- xml_attr(frameset, "GameSection")
  
  # Get all frames in this frameset
  frames <- xml_find_all(frameset, ".//Frame")
  
  if(length(frames) > 0) {
    tibble(
      team_id = team_id,
      person_id = person_id,
      game_section = game_section,
      frame_n = as.integer(xml_attr(frames, "N")),
      timestamp = xml_attr(frames, "T"),
      x = as.numeric(xml_attr(frames, "X")),
      y = as.numeric(xml_attr(frames, "Y")),
      distance = as.numeric(xml_attr(frames, "D")),
      speed = as.numeric(xml_attr(frames, "S")),
      acceleration = as.numeric(xml_attr(frames, "A")),
      m = as.integer(xml_attr(frames, "M"))
    )
  }
}

# Process in chunks to handle large file
process_all_positions <- function(doc_position, chunk_size = 10) {
  framesets <- xml_find_all(doc_position, "//FrameSet")
  n_chunks <- ceiling(length(framesets) / chunk_size)

  all_positions <- list()

  for(i in 1:n_chunks) {
    cat("Processing chunk", i, "of", n_chunks, "\n")

    start_idx <- (i - 1) * chunk_size + 1
    end_idx <- min(i * chunk_size, length(framesets))

    chunk_framesets <- framesets[start_idx:end_idx]
    chunk_data <- map_df(chunk_framesets, process_frameset)

    all_positions[[i]] <- chunk_data

    # Optional: save intermediate results
    if(i %% 10 == 0) {
      temp_df <- bind_rows(all_positions)
      saveRDS(temp_df, paste0("positions_temp_", i, ".rds"))
    }
  }

  bind_rows(all_positions)
}

# Process entire file
positions_df <- process_all_positions(doc_position)


#_____________________________________________________________
#### Save All  ####
#_____________________________________________________________

# Function to save all data
save_match_data <- function(match_id, output_dir = "match_data") {
  dir.create(output_dir, showWarnings = FALSE)
  
  # Save each data frame
  saveRDS(match_info, file.path(output_dir, paste0(match_id, "_match_info.rds")))
  saveRDS(players_df, file.path(output_dir, paste0(match_id, "_players.rds")))
  saveRDS(stadium_df, file.path(output_dir, paste0(match_id, "_stadium.rds")))
  saveRDS(events_detailed_df, file.path(output_dir, paste0(match_id, "_events.rds")))
  saveRDS(positions_df, file.path(output_dir, paste0(match_id, "_positions.rds")))
  
  cat("Data saved to", output_dir, "\n")
}

save_match_data("J03WMX") # enter match_id
