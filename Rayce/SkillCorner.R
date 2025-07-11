#Almona reading data
library(jsonlite)
phlatltracking_data <- stream_in(file("~/Downloads/1141349_tracking_extrapolated.jsonl"))
phlatltracking_data <- phlatltracking_data |> 
  unnest(cols = c(player_data)) |>
  as.data.table()


#### Get Player Information ####
match_info <- fromJSON("data/SkillCorner data/1832186_match.json", simplifyDataFrame = FALSE)
match_info_df <- fromJSON("data/SkillCorner data/1832186_match.json")

players_info <- match_info_df$players |> 
  as.data.table()


#### Read event file ####
events <- read_csv("data/SkillCorner data/1832186_dynamic_events.csv") |>
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


tracking_data <- tracking_data |> 
  left_join(
    players_info |> select(id, team_id, number, short_name), 
    join_by(player_id == id))










#Miami/Charlotte

library(readr)
miamicharlotte <- read_csv("../Downloads/1186860_dynamic_events.csv")


##Philly/AtL
library(readr)
phillyatlanta <- read_csv("../Downloads/Philly:Atlanta/1141349_dynamic_events.csv")


##Separation created by different types of runs
library(dplyr)
runs<-PhillyAtlanta |>
  filter(event_type == "off_ball_run") |>
  select(frame_start, frame_end, event_type, event_subtype) 

possession<-PhillyAtlanta |>
  filter(event_type == "player_possession")|>
  mutate(sep_created = separation_end - separation_start) |>
  select(frame_start, frame_end, sep_created, n_simulatneous_runs)
  


library(dplyr)
library(fuzzyjoin)


runsjoin <- PhillyAtlanta %>%
  filter(event_type == "off_ball_run") %>%
  select(run_frame = frame_start, frame_end, event_type, event_subtype, n_simultaneous_runs)


possessionjoin <- PhillyAtlanta %>%
  filter(event_type == "player_possession") %>%
  mutate(sep_created = separation_end - separation_start) %>%
  select(possession_frame_start = frame_start,
         possession_frame_end = frame_end,
         sep_created)


possession_with_runs <- fuzzy_left_join(
  possessionjoin,
  runsjoin,
  by = c("possession_frame_start" = "run_frame",
         "possession_frame_end" = "run_frame"),
  match_fun = list(`<=`, `>=`)
)






bestruns <- PhillyAtlanta |>
  filter(event_type == "off_ball_run") |>
  group_by(event_subtype) |>
  mutate(sep_created = separation_end - separation_start) |>
  ungroup()

view(bestruns$sep_created)
library(ggplot2)

ggplot(bestruns, aes(x = event_subtype, y = sep_created, fill = event_subtype)) +
  geom_boxplot() +
  labs(
    title = "Separation Created by Off-Ball Run Subtype",
    x = "Run Subtype",
    y = "Separation Created (yards/meters/etc.)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")


#GGplot by player
ggplot(bestruns, aes(x = player_name, y = sep_created, fill = event_subtype)) +
  geom_col() +
  labs(
    title = "Total Separation Created per Player by Run Subtype",
    x = "Player",
    y = "Total Separation Created"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#GGplot by team

team_sep <- bestruns |>
  group_by(team_shortname, event_subtype) |>
  summarize(total_sep = sum(sep_created, na.rm = TRUE), .groups = "drop")


ggplot(team_sep, aes(x = team_shortname, y = total_sep, fill = event_subtype)) +
  geom_col(position = "stack", width = 0.6) +
  labs(
    title = "Total Separation Created per Team by Run Subtype",
    x = "Team",
    y = "Total Separation Created",
    fill = "Run Subtype"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    legend.position = "right"
  ) +
  scale_fill_brewer(palette = "Paired")

#rank player by sepeartion created
top_sep_players <- bestruns |>
  group_by(player_name) |>
  summarize(total_sep = sum(sep_created, na.rm = TRUE)) |>
  arrange(desc(total_sep))




# Run types compared to distance trvaeled
runtype_distance <- PhillyAtlanta |>
  filter(event_type == "off_ball_run", !is.na(distance_covered)) |>
  group_by(event_subtype) |>
  summarize(
    avg_distance = mean(distance_covered, na.rm = TRUE),
    total_distance = sum(distance_covered, na.rm = TRUE),
    count = n()
  ) |>
  arrange(desc(avg_distance))


ggplot(runtype_distance, aes(x = reorder(event_subtype, avg_distance), y = avg_distance)) +
  geom_col(aes(fill = event_subtype), show.legend = FALSE) +
  geom_text(aes(label = paste0("n=", count)), hjust = -0.1, size = 3.5) +
  coord_flip() +
  labs(
    title = "Average Distance Covered by Run Type",
    x = "Run Type",
    y = "Average Distance"
  ) +
  theme_minimal(base_size = 13)

#Speed v. Sepeartion
sep_speed_data <- bestruns |>
  filter(!is.na(sep_created), !is.na(speed_avg))


library(ggplot2)

ggplot(sep_speed_data, aes(x = speed_avg, y = sep_created)) +
  geom_point(alpha = 0.5, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred") +
  labs(
    title = "Relationship Between Average Speed and Separation Created",
    x = "Average Speed (m/s)",
    y = "Separation Created (absolute value)"
  ) +
  theme_minimal(base_size = 13)


#Try to plot where/how separation was created


