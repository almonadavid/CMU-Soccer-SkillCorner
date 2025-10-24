#### Find ball carrier for any given frame ####
get_ball_carrier <- function(frames, events) {
  
  setDT(events)
  
  frames_dt <- data.table(frame = frames) # store all frames
  
  player_poss <- events[event_type == "player_possession"]
  
  ball_carriers <- player_poss[frames_dt, 
                               .(frame = i.frame,
                                 ball_carrier_id = player_in_possession_id,
                                 ball_carrier_name = player_in_possession_name,
                                 ball_carrier_team = team_id,
                                 ball_carrier_role = player_position,
                                 is_ball_carrier_gk = is_goalkeeper),
                               on = .(frame_start <= frame, frame_end >= frame),
                               mult = "last"
  ]
  
  return(ball_carriers)
}


#### Distances to ball carrier and angles relative to attacking goal ####
distance_and_angle <- function(tracking_data, ball_carrier_df, home_team_id, frame_rate = 10) {     
  
  setDT(tracking_data)
  setDT(ball_carrier_df)
  
  # Merge ball carrier info to tracking data
  tracking_with_possession <- tracking_data[ball_carrier_df, on = "frame"]
  
  # Ball carrier position for each frame
  ball_carrier_positions <- tracking_with_possession[
    player_id == ball_carrier_id & !is.na(ball_carrier_id),
    .(frame, ball_carrier_id, ball_carrier_x = x, ball_carrier_y = y, possession_team = ball_carrier_team, 
      ball_carrier_role, is_ball_carrier_gk, ball_carrier_name, ball_carrier_team)
  ]
  
  # Find potential pressing defenders
  defenders <- tracking_with_possession[
    ball_carrier_positions, on = "frame"
  ][
    team_id != possession_team & !is.na(ball_carrier_id)
  ]
  
  # Calculate defender distances to ball carrier
  defenders[, `:=`(
    dx = ball_carrier_x - x,
    dy = ball_carrier_y - y,
    distance_to_ball_carrier = sqrt((ball_carrier_x - x)^2 + (ball_carrier_y - y)^2)
  )]
  
  
  # Add goal center coordinates based on possession team
  # Home team attacks right (positive x), away team attacks left (negative x)
  defenders[, `:=`(
    goal_center_x = fifelse(possession_team == home_team_id, 52.5, -52.5),
    goal_center_y = 0
  )]
  
  # Calculate threat direction vector (ball carrier to goal center)
  defenders[, `:=`(
    threat_vector_x = goal_center_x - ball_carrier_x,
    threat_vector_y = goal_center_y - ball_carrier_y
  )]
  
  # Calculate target-to-presser vector (ball carrier to defender)
  defenders[, `:=`(
    target_to_presser_x = x - ball_carrier_x,
    target_to_presser_y = y - ball_carrier_y
  )]
  
  # Calculate angle between threat direction and target-to-presser direction
  defenders[, `:=`(
    threat_magnitude = sqrt(threat_vector_x^2 + threat_vector_y^2),
    presser_magnitude = sqrt(target_to_presser_x^2 + target_to_presser_y^2)
  )]
  
  # Dot product and cross product
  defenders[, `:=`(
    dot_product = threat_vector_x * target_to_presser_x + threat_vector_y * target_to_presser_y,
    cross_product = threat_vector_x * target_to_presser_y - threat_vector_y * target_to_presser_x
  )]
  
  
  defenders[, `:=`(
    angle_theta = atan2(cross_product, dot_product),
    angle_theta_degrees = atan2(cross_product, dot_product) * 180 / pi
  )]
  
  defenders <- defenders[, .(frame, player_id, team_id, x, y, ball_carrier_name, ball_carrier_id, 
                             ball_carrier_team, ball_carrier_role, is_ball_carrier_gk,
                             ball_carrier_x, ball_carrier_y, possession_team, distance_to_ball_carrier,
                             angle_theta, angle_theta_degrees)]
  
  return(defenders)
}



# closest_defender <- fread("defenders/all_defenders.csv") |>
#   group_by(game_id, frame, ball_carrier_id) |>
#   arrange(frame, distance_to_ball_carrier) |> 
#   slice_head() |> 
#   ungroup() |> 
#   filter(
#     between(ball_carrier_x, -52.5, 52.5),
#     between(ball_carrier_y, -34, 34)
#   ) |> 
#   mutate(
#     x_cart = distance_to_ball_carrier * cos(angle_theta_degrees * pi / 180),
#     y_cart = distance_to_ball_carrier * sin(angle_theta_degrees * pi / 180)
#   )
# 
# fwrite(closest_defender, "defenders/closest_defender.csv")

closest_defender <- fread("defenders/closest_defender.csv")

sample_data <- closest_defender |> 
  group_by(ball_carrier_role) |> 
  sample_n(min(50000, n())) |> 
  ungroup()


# points
ggplot(data = sample_data, aes(x = x_cart, y = y_cart)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 1, alpha = 0.009) +
  annotate("point", x = 0, y = 0, color = "red", size = 2) + 
  coord_fixed(xlim = c(-30, 30), ylim = c(-30, 30)) +
  labs(
    title = "Closest Defender to Ball Carrier (Origin) At Each Frame Of Ball Possession",
    subtitle = "Attack direction: Left-to-Right",
    caption = "Sample, n = 50,000. Alpha = 0.01",
    x = NULL, y = NULL
  ) +
  facet_wrap(~ ball_carrier_role, nrow = 3) +
  theme_bw(base_size = 14)


# hex bins
ggplot(data = sample_data, aes(x = x_cart, y = y_cart)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hex(bins = 100) +
  scale_fill_viridis_c() +
  annotate("point", x = 0, y = 0, color = "red", size = 2) + 
  coord_fixed(xlim = c(-20, 20), ylim = c(-20, 20)) +
  labs(
    title = "Three Closest Defenders Relative to Ball Carrier (Origin) At Each Frame Of Ball Possession",
    subtitle = "Attack direction: Left-to-Right",
    x = NULL, y = NULL
  ) +
  facet_wrap(~ ball_carrier_role, nrow = 3) +
  theme_bw()


# heatmap
heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = 1)

ggplot(data = sample_data, aes(x = x_cart, y = y_cart)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10), 
                         alpha = 0.4) +
  scale_fill_manual(values = heat_palette) +
  annotate("point", x = 0, y = 0, color = "red", size = 2) + 
  coord_fixed(xlim = c(-20, 20), ylim = c(-20, 20)) +
  labs(
    # title = "Pressure Zone Heatmap Around Ball Carrier",
    subtitle = "Attack direction: Left-to-Right",
    x = NULL, y = NULL,
    fill = "Density"
  ) +
  facet_wrap(~ ball_carrier_role, nrow = 3) +
  theme_bw() +
  theme(legend.position = "none")
