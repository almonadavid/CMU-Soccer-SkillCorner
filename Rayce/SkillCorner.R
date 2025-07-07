#Miami/Charlotte

library(readr)
miamicharlotte <- read_csv("../Downloads/1186860_dynamic_events.csv")

library(jsonlite)
tracking_data<-stream_in(file("~/Downloads/FC Charlotte: Inter Miami/1186860_tracking_extrapolated.jsonl"))
unested_data<-tracking_data |>
  unnest(cols=c(player_data))

##Philly/AtL
library(readr)
phillyatlanta <- read_csv("../Downloads/Philly:Atlanta/1141349_dynamic_events.csv")

library(jsonlite)
tracking_data<-stream_in(file("~/Downloads/Philly:Atlanta/1141349_tracking_extrapolated.jsonl.download"))
unested_data<-tracking_data |>
  unnest(cols=c(player_data))

##Separation created by different types of runs
library(dplyr)

bestruns <- PhillyAtlanta |>
  filter(event_type == "off_ball_run") |>
  group_by(event_subtype) |>
  mutate(sep_created = separation_end - separation_start) |>  # assuming 'end - start' means gain in separation
  ungroup()

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



ggplot(bestruns, aes(x = player_name, y = sep_created, fill = event_subtype)) +
  geom_col() +
  labs(
    title = "Total Separation Created per Player by Run Subtype",
    x = "Player",
    y = "Total Separation Created"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

