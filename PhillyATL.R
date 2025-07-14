library(dplyr)
library(fuzzyjoin)
library(tidyverse)

runsjoin <- PhillyAtlanta %>%
  filter(event_type == "off_ball_run") %>%
  select(match_id, frame_start, frame_end, event_type, event_subtype, n_simultaneous_runs, speed_avg, third_start, third_end, n_opponents_ahead_pass_reception, xthreat, x_start, y_start, x_end, y_end, carry)


possessionjoin <- PhillyAtlanta %>%
  filter(event_type == "player_possession") %>%
  mutate(sep_created = separation_end - separation_start) %>%
  select(match_id, frame_start, frame_end, sep_created)


possession_with_runs <- genome_left_join(
  possessionjoin,
  runsjoin,
  by = c("match_id", "frame_start", "frame_end"))
 # match_fun = list(`<=`, `>=`)

#carry == false to control for dribbbling
onlyruns<-possession_with_runs |>
  filter(event_type == "off_ball_run") 

which_runs_are_productive <- onlyruns |>
  group_by(event_subtype) %>%
  summarize(avg_sep_created = mean(sep_created, na.rm = TRUE)) %>%
  arrange(desc(avg_sep_created))

library(ggplot2)
ggplot(which_runs_are_productive, aes(x = reorder(event_subtype, avg_sep_created), 
                               y = avg_sep_created)) +
  geom_col(fill = "#0072B2") +
  coord_flip() +
  labs(
    title = "Average Separation Created by Run Subtype",
    x = "Run Subtype",
    y = "Average Separation Created (sep_created)"
  ) +
  theme_minimal()

final_third_runs <- onlyruns %>%
  filter(third_end == "attacking_third") %>%
  group_by(event_subtype) %>%
  summarize(avg_sep_created = mean(sep_created, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_sep_created))

middle_third_runs <- onlyruns %>%
  filter(third_end == "middle_third") %>%
  group_by(event_subtype) %>%
  summarize(avg_sep_created = mean(sep_created, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_sep_created))

defensive_third_runs <- onlyruns %>%
  filter(third_end == "defensive_third") %>%
  group_by(event_subtype) %>%
  summarize(avg_sep_created = mean(sep_created, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(avg_sep_created))


########SPEED##################################
which_runs_are_productive <- onlyruns %>%
  mutate(speed_bin = cut(speed_avg,
                         breaks = c(14, 18, 22, Inf),
                         labels = c("Slow", "Medium", "Fast"),
                         right = FALSE)) %>%
  group_by(event_subtype, speed_bin) %>%
  summarize(avg_sep_created = mean(sep_created, na.rm = TRUE), .groups = "drop") %>%
  arrange(event_subtype, desc(avg_sep_created))


library(ggplot2)

ggplot(which_runs_are_productive, aes(x = speed_bin, y = avg_sep_created, fill = speed_bin)) +
  geom_col() +
  facet_wrap(~ event_subtype) +
  labs(
    title = "Avg Separation by Run Subtype and Speed",
    x = "Speed Category",
    y = "Average Separation Created"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")
