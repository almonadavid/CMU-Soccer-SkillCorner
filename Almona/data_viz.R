library(ggplot2)
library(data.table)



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


pressing_data <- pressing_data |>
  mutate(forced_turnover_within_5s = as.factor(ifelse(forced_turnover_within_5s == TRUE, "Yes", "No")))


# handling NAs
pressing_data <- pressing_data |>
  mutate(
    incoming_pass_range_received_missing = is.na(incoming_pass_range_received),
    incoming_high_pass_missing = is.na(incoming_high_pass),
    incoming_pass_distance_received_missing = is.na(incoming_pass_distance_received),
    ball_carrier_speed_missing = is.na(ball_carrier_speed),
    incoming_pass_range_received = factor(ifelse(is.na(incoming_pass_range_received), 
                                                 "unknown", as.character(incoming_pass_range_received))),
    incoming_high_pass = factor(ifelse(is.na(incoming_high_pass), 
                                       "unknown", as.character(incoming_high_pass))),
    incoming_pass_distance_received = ifelse(is.na(incoming_pass_distance_received), 
                                             -1, incoming_pass_distance_received),
    ball_carrier_speed = ifelse(is.na(ball_carrier_speed), 
                                -1, ball_carrier_speed),
    incoming_pass_range_received_missing = factor(incoming_pass_range_received_missing),
    incoming_high_pass_missing = factor(incoming_high_pass_missing),
    incoming_pass_distance_received_missing = factor(incoming_pass_distance_received_missing),
    ball_carrier_speed_missing = factor(ball_carrier_speed_missing),
    minutes_remaining_half = ifelse(is.na(minutes_remaining_half), median(minutes_remaining_half, na.rm = TRUE), minutes_remaining_half),
    minutes_remaining_game = ifelse(is.na(minutes_remaining_game), median(minutes_remaining_game, na.rm = TRUE), minutes_remaining_game)
  )


## 1. Season Pressing Heatmap ####################################################################################################

heat_palette <- paletteer::paletteer_d("RColorBrewer::YlOrRd", n = 9, direction = 1)

ggplot(data = pressing_data, aes(x = ball_carrier_x, y = ball_carrier_y)) +
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

library(ggtext)
library(ggforce)
library(patchwork)

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
  theme_bw()



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
  theme_bw()


circle_plot + oval_plot + plot_layout(ncol = 2)


## 3. TEAM ANALYSIS #######################################################################################
  
xg_predictions <- predict(xg_tune, newdata = pressing_data, type = "prob") |>
  select(Yes) |>
  rename(xP_xg = Yes)


pressing_data <- cbind(pressing_data, xg_predictions)
pressing_data[, turnover_actual := as.numeric(forced_turnover_within_5s == "Yes")]
pressing_data[, xP_diff_xg := turnover_actual - xP_xg]


pressing_team <- pressing_data[, .(
  games_played = uniqueN(match_id),
  actual_turnovers = sum(turnover_actual),
  expected_turnovers = sum(xP_xg),
  xP_diff = sum(xP_diff_xg),
  xp_diff_per_game = sum(xP_diff_xg) / uniqueN(match_id)
), by = pressing_team_name][order(-xP_diff)]


pressed_team <- pressing_data[, .(
  games_played = uniqueN(match_id),
  actual_turnovers = sum(turnover_actual),
  expected_turnovers = sum(xP_xg),
  xP_diff = sum(xP_diff_xg),
  xp_diff_per_game = sum(xP_diff_xg) / uniqueN(match_id)
), by = pressed_team_name][order(-xP_diff)]


# Diverging bar chart (teams that exceed/underperform pressing expectations)
pressing_team |>
  mutate(team = reorder(pressing_team_name, xp_diff_per_game)) |>
  ggplot(aes(x = team, y = xp_diff_per_game, fill = xp_diff_per_game > 0)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = c("#d7191c", "#2b83ba"), 
                    labels = c("Below Expected", "Above Expected")) +
  labs(x = "", y = "Turnovers Above/Below Expected per Game",
       fill = "") +
  theme_bw()


# Calibration plot: Actual vs Expected
ggplot(aes(x = xP_xg, y = turnover_actual), data = pressing_data) +
  geom_point(alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
  geom_smooth() +
  labs(x = "Expected Turnovers (xP)",
       y = "Actual Turnovers") +
  coord_fixed() +
  theme_bw()


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
  theme_bw()


library(ggimage)

# add logos
pressing_team <- pressing_team |>
  mutate(
    xP_per_game = expected_turnovers / games_played,
    xP_diff_per_game = xp_diff_per_game,
    logo_path = paste0("logos/", pressing_team_name, ".png")
  )

ggplot(pressing_team, aes(x = xP_per_game, y = xP_diff_per_game)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = median(pressing_team$xP_per_game), 
             linetype = "dashed", alpha = 0.5) +
  ggimage::geom_image(aes(image = logo_path), size = 0.07) +
  labs(
    x = "Expected Turnovers per Game (Pressing Volume)",
    y = "Actual - Expected per Game (Pressing Effectiveness)"
  ) +
  theme_bw()


## 4. Save Model SUm Stats table ###############################################################
library(gt)

model_summary_gt <- model_summary |>
  gt() |>
  tab_header(title = md("**Model Performance Summary**")) |>
  cols_label(
    AUC = "AUC",
    Accuracy = "Accuracy",
    Log_Loss = "Log Loss"
  ) |>
  fmt_percent(columns = Accuracy, decimals = 1) |>
  fmt_number(columns = c(AUC, Log_Loss), decimals = 3) |>
  tab_style(
    style = cell_borders(sides = c("left", "right"), color = "lightgrey", weight = px(1)),
    locations = list(
      cells_body(columns = everything()),
      cells_column_labels(columns = everything())
    )
  )

gtsave(model_summary_gt, "model_summary.png")


## 5. Turnover rate by start_type table #######################################################

pressing_summary_gt <- pressing_data |>
  group_by(start_type) |>
  summarise(
    total_possessions = n(),
    turnovers = sum(turnover_actual),
    turnover_prop = turnovers / total_possessions
  ) |>
  arrange(desc(turnover_prop)) |>
  gt() |>
  tab_header(title = md("**Turnover Rate by Start Type**")) |>
  cols_label(
    start_type = "Start Type",
    total_possessions = "Total Possessions", 
    turnovers = "Turnovers",
    turnover_prop = "Turnover Rate"
  ) |>
  fmt_number(columns = c(total_possessions, turnovers), decimals = 0) |>
  fmt_percent(columns = turnover_prop, decimals = 1) |>
  tab_style(
    style = cell_borders(sides = c("left", "right"), color = "lightgray", weight = px(1)),
    locations = list(
      cells_body(columns = everything()),
      cells_column_labels(columns = everything())
    )
  )

gtsave(pressing_summary_gt, "turnover_start_type.png")


## 6. ROC Curve ################################################################################

roc_data <- bind_rows(
  tibble(
    fpr = 1 - logit_roc$specificities,
    tpr = logit_roc$sensitivities,
    model = "Logistic"
  ),
  tibble(
    fpr = 1 - xg_roc$specificities,
    tpr = xg_roc$sensitivities,
    model = "XGBoost"
  )
)


roc_plot <- ggplot(roc_data, aes(x = fpr, y = tpr, color = model)) +
  geom_line(linewidth = 1) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "gray") +
  scale_color_manual(
    values = c("Logistic" = "blue", "XGBoost" = "red"),
    labels = c(paste("Logistic (AUC =", round(auc(logit_roc), 3), ")"),
               paste("XGBoost (AUC =", round(auc(xg_roc), 3), ")"))
  ) +
  labs(
    x = "(1 - Specificity)",
    y = "(Sensitivity)",
    color = "Model"
  ) +
  theme_bw() +
  coord_fixed()

## 7. Pressing vs when being pressed ###################################################

team_comparison <- merge(
  pressing_team[, .(team = pressing_team_name, pressing_xp_diff = xp_diff_per_game)],
  pressed_team[, .(team = pressed_team_name, pressed_xp_diff = xp_diff_per_game)],
  by = "team"
)

team_comparison |> 
  ggplot(aes(x = pressing_xp_diff, y = -pressed_xp_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.7) +
  ggrepel::geom_text_repel(aes(label = team), size = 3) +
  labs(
    title = "MLS Team Pressing Profile: Offensive vs Defensive",
    subtitle = "2023 Season Performance",
    x = "Pressing Effectiveness\n(Turnovers Above/Below Expected per Game)",
    y = "Press Resistance\n(Turnovers Prevented Above/Below Expected per Game)",
    caption = "Top-right quadrant: Elite at both pressing and resisting pressure"
  ) +
  theme_bw()


library(ggimage)

# add logos
team_comparison <- team_comparison |>
  mutate(logo_path = paste0("logos/", team, ".png"))


pressing_vs_pressed_plot <- team_comparison |> 
  ggplot(aes(x = pressing_xp_diff, y = -pressed_xp_diff)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  ggimage::geom_image(aes(image = logo_path), size = 0.07) +
  labs(
    x = "Pressing Effectiveness\n(Turnovers Above/Below Expected per Game)",
    y = "Press Resistance\n(Turnovers Prevented Above/Below Expected per Game)"
  ) +
  theme_bw()

pressing_vs_pressed_plot


