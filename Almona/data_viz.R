library(ggplot2)
library(data.table)
library(ggtext)
library(ggforce)
library(patchwork)



#### Load Data ####
pressing_data <- fread("results/all_games_pressing_sequences.csv") |>
  filter(
    between(ball_carrier_x, -52.5, 52.5),
    between(ball_carrier_y, -34, 34)
  ) |> 
  mutate(
    start_type = case_when(
      str_detect(start_type, "interception") ~ "interception",
      str_detect(start_type, "reception") ~ "reception",
      start_type == "keep_possession" ~ "keep_possession",
      start_type == "recovery" ~ "recovery",
      TRUE ~ "unknown"
    )
  ) |> 
  mutate_if(is.character, as.factor) |> 
  mutate_if(is.logical, as.factor)


## 1. Season Pressing Heatmap ####################################################################################################

heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = 1)

ggplot(data = test, aes(x = ball_carrier_x, y = ball_carrier_y)) +
  geom_point(size = 0.8, alpha = 0.4, color = "white") +
  geom_density_2d_filled(aes(fill = after_stat(level)),
                         contour_var = "ndensity", 
                         breaks = seq(0.1, 1.0, length.out = 10), 
                         alpha = 0.4) +
  annotate("rect",xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = -52.5, xmax = 0, ymin = -34, ymax = 34, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -36.75, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 36.75, xmax = 52.5, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", linewidth = 0.6) +
  annotate("segment", x = 0, xend = 0, y = -34.5, yend = 34.5, colour = "black", linewidth = 0.6)+
  annotate("segment", x = -52.5, xend = -52.5, y = -34, yend = 34, colour = "black", linewidth = 0.6)+
  annotate("segment", x = 52.5, xend = 52.5, y = -34, yend = 34, colour = "black", linewidth = 0.6)+
  annotate("point", x = -42 , y = 0, colour = "black", size = 1.05) +
  annotate("point", x = 42 , y = 0, colour = "black", size = 1.05) +
  annotate("path", colour = "black", linewidth = 0.6, x=0+8.75*cos(seq(0,2*pi,length.out=2000)),
           y=0+8.75*sin(seq(0,2*pi,length.out=2000))) +
  annotate("point", x = 0 , y = 0, colour = "black", size = 1.05) +
  annotate("path", x=-42+8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), linewidth = 0.6,
           y=0+8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=42-8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), linewidth = 0.6,
           y=0-8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  coord_fixed() +
  scale_x_continuous(limits = c(-52.5, 52.5)) +
  scale_y_continuous(limits = c(-34, 34)) +
  scale_fill_manual(values = heat_palette) +
  theme_void() +
  theme(legend.position = "none")


## 2. Andrienko et al., 2017 "pressure zone" #####################################################################################


Dfront <- 9
Dback <- 3
n_points <- 360

# Create oval coordinates
angles <- seq(0, 2*pi, length.out = n_points)
formula_angles <- angles + pi
z <- (1 - cos(formula_angles)) / 2
L <- Dback + (Dfront - Dback) * (z^3 + 0.3 * z) / 1.3
x <- L * cos(angles)
y <- L * sin(angles)

oval_coords <- data.table(x = x, y = y)

oval_plot <- ggplot() +
  geom_polygon(data = oval_coords, aes(x = x, y = y), 
               fill = "lightblue", alpha = 0.3, color = "blue", linewidth = 1) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 4) +
  geom_richtext(aes(x = 0, y = 0.5), 
                label = "Pressure target<br>(ball carrier)", 
                color = "red", size = 3.5, fontface = "bold", 
                fill = NA, label.color = NA, vjust = 0) +
  geom_segment(aes(x = 0, y = 0, xend = 12, yend = 0), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "darkgreen", linewidth = 1.2) +
  geom_text(aes(x = 12, y = 1), label = "To Goal", hjust = 1, size = 4, color = "darkgreen") +
  geom_segment(aes(x = 0, y = 0, xend = 9, yend = 0), color = "orange", linetype = "dashed", linewidth = 0.8) +
  geom_segment(aes(x = 0, y = 0, xend = -3, yend = 0), color = "purple", linetype = "dashed", linewidth = 0.8) +
  coord_equal() +
  xlim(-10, 12) +
  ylim(-7, 7) +
  labs(title = "Oval Pressure Zone (Andrienko et al. 2017)",
       subtitle = sprintf("Dfront = %dm (orange), Dback = %dm (purple)", Dfront, Dback),
       x = "Distance (m)", y = NULL) +
  theme_light()



circle_plot <- ggplot() +
  geom_circle(aes(x0 = 0, y0 = 0, r = 6), 
              fill = "lightblue", alpha = 0.3, color = "blue", linewidth = 1) +
  geom_point(aes(x = 0, y = 0), color = "red", size = 4) +
  geom_richtext(aes(x = 0, y = 0.5), 
                label = "Pressure target<br>(ball carrier)", 
                color = "red", size = 3.5, fontface = "bold", 
                fill = NA, label.color = NA, vjust = 0) +
  geom_segment(aes(x = 0, y = 0, xend = 12, yend = 0), 
               arrow = arrow(length = unit(0.3, "cm")), 
               color = "darkgreen", linewidth = 1.2) +
  geom_text(aes(x = 12, y = 1), label = "To Goal", hjust = 1, size = 4, color = "darkgreen") +
  coord_equal() +
  xlim(-10, 12) +
  ylim(-7, 7) +
  labs(title = "Our Initial Pressure Zone Idea (Circular)",
       subtitle = sprintf("Dfront = 6m, Dback = 6m"),
       x = "Distance (m)", y = "Distance (m)") +
  theme_light()


circle_plot + oval_plot + plot_layout(ncol = 2)


## 3. xxx #######################################################################################
  
lgb_predictions <- final_lgb |>
  fit(data = pressing_train) |>
  augment(new_data = pressing_data) |>
  select(.pred_TRUE) |>
  rename(xP_lgb = .pred_TRUE)


pressing_data <- cbind(pressing_data, lgb_predictions)
pressing_data[, `:=`(
  turnover_actual = as.numeric(forced_turnover_within_5s == "TRUE"),
  xP_diff_lgb = turnover_actual - xP_lgb
)]


pressing_team <- pressing_data[, .(
  games_played = uniqueN(match_id),
  actual_turnovers = sum(turnover_actual),
  expected_turnovers = sum(xP_lgb),
  xP_diff = sum(xP_diff_lgb),
  xp_diff_per_game = sum(xP_diff_lgb) / uniqueN(match_id)
), by = pressing_team_name][order(-xP_diff)]


pressed_team <- pressing_data[, .(
  games_played = uniqueN(match_id),
  actual_turnovers = sum(turnover_actual),
  expected_turnovers = sum(xP_lgb),
  xP_diff = sum(xP_diff_lgb),
  xp_diff_per_game = sum(xP_diff_lgb) / uniqueN(match_id)
), by = pressed_team_name][order(-xP_diff)]


# Diverging bar chart (teams that exceed/underperform pressing expectations)
pressing_team |>
  mutate(team = reorder(pressing_team_name, xp_diff_per_game)) |>
  ggplot(aes(x = team, y = xp_diff_per_game, fill = xp_diff_per_game > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"), 
                    labels = c("Below Expected", "Above Expected")) +
  labs(title = "Team Pressing Performance vs Expected (Per Game)",
       subtitle = "Positive values indicate teams forcing more turnovers than predicted",
       x = "", y = "Turnovers Above/Below Expected per Game",
       fill = "") +
  theme_light()

# Scatterplot - Actual vs Expected
pressing_team |> # replace points with team logos
  ggplot(aes(x = expected_turnovers, y = actual_turnovers)) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  ggrepel::geom_text_repel(
    aes(label = pressing_team_name)
  ) +
  labs(title = "Actual vs Expected Turnovers by Team",
       subtitle = "Points above the line exceed expectations",
       x = "Expected Turnovers (xP)",
       y = "Actual Turnovers") +
  theme_light()


# xP vs xP Diff
pressing_team |>
  mutate(
    xP_per_game = expected_turnovers / games_played,
    xP_diff_per_game = xp_diff_per_game
  ) |>
  ggplot(aes(x = xP_per_game, y = xP_diff_per_game)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = median(pressing_team$expected_turnovers/pressing_team$games_played), 
             linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.7) +
  ggrepel::geom_text_repel(
    aes(label = pressing_team_name),
    size = 3
  ) +
  labs(title = "MLS Team Pressing Profile: Volume vs Effectiveness",
       subtitle = "2023 Season",
       x = "Expected Turnovers per Game (Pressing Volume)",
       y = "Actual - Expected per Game (Pressing Effectiveness)") +
  theme_light() +
  annotate("text", x = Inf, y = Inf, label = "High Volume\nOverperforming", 
           hjust = 1.1, vjust = 1.1, alpha = 0.5) +
  annotate("text", x = -Inf, y = Inf, label = "Low Volume\nOverperforming", 
           hjust = -0.1, vjust = 1.1, alpha = 0.5)

