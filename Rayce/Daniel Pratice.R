library(readr)
data <- read_csv("../data/1832186_dynamic_events (1).csv")

library(jsonlite)
tracking_data<-stream_in(file("~/Downloads/CMSAC Sample Data/1832186_tracking_extrapolated.jsonl"))
unested_data<-tracking_data |>
  unnest(cols=c(player_data))

#Calculate direction

library(dplyr)

# Function to compute direction in degrees
calculate_direction <- function(unnested_data) {
  unested_data %>%
    group_by(player_id) %>%
    arrange(player_id, row_number()) %>%
    mutate(
      dx = lead(x) - x,
      dy = lead(y) - y,
      direction = atan2(dy, dx) * 180 / pi
    ) %>%
    ungroup()
}

direction <- calculate_direction(unested_data)






##########
# 🔧 Choose player to animate
player_id_to_plot <- 36458

# 🧹 Prepare data (assumes x and y already centered with 0,0 at field center)
player_data_36458 <- unested_data %>%
  filter(player_id == 36458) %>%
  arrange(frame)

# ⚽ Function to draw a soccer pitch centered at (0, 0)
draw_pitch <- function() {
  list(
    # Full field background
    geom_rect(aes(xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34), fill = "palegreen4", color = NA),
    
    # Center line
    geom_segment(aes(x = 0, xend = 0, y = -34, yend = 34), color = "white"),
    
    # Center circle
    annotate("path",
             x = 0 + 9.15 * cos(seq(0, 2 * pi, length.out = 100)),
             y = 0 + 9.15 * sin(seq(0, 2 * pi, length.out = 100)),
             color = "white"),
    
    # Goals
    geom_segment(aes(x = -52.5, xend = -52.5, y = -3.66, yend = 3.66), color = "white", size = 1),
    geom_segment(aes(x = 52.5, xend = 52.5, y = -3.66, yend = 3.66), color = "white", size = 1)
  )
}

# 📈 Build animation plot
p <- ggplot(player_data, aes(x = x, y = y)) +
  draw_pitch() +
  geom_path(aes(group = 1), color = "gray70", alpha = 0.4) +
  geom_point(color = "blue", size = 4) +
  coord_fixed(xlim = c(-52.5, 52.5), ylim = c(-34, 34)) +
  labs(
    title = paste("Player", player_id_to_plot, "Movement on Soccer Field"),
    subtitle = "Frame: {frame_time}",
    x = "X (meters)",
    y = "Y (meters)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.background = element_rect(fill = "palegreen4"),
    plot.background = element_rect(fill = "palegreen4"),
    panel.grid = element_blank()
  ) +
  transition_time(frame) +
  shadow_mark(past = TRUE, future = FALSE, alpha = 0.3)

# ▶️ Animate and render
animate(p, fps = 20, duration = 5, width = 800, height = 520, renderer = gifski_renderer())


frame_rate <- 25  # frames per second
time_per_frame <- 1 / frame_rate  # seconds

# Calculate speed and direction
player_data_36458 <- player_data_36458 %>%
  arrange(frame) %>%
  mutate(
    dx = x - lag(x),
    dy = y - lag(y),
    dt = (frame - lag(frame)) * time_per_frame,
    speed_m_per_s = sqrt(dx^2 + dy^2) / dt,
    direction_deg = atan2(dy, dx) * 180 / pi
  )

##### Comparing run type to position
