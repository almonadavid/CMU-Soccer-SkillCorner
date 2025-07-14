
############################################
##             ROUGH WORK                 ##
############################################

#### Analyze specific frames with passing options for passing lane coverage ####
frame_sample <- sort(unique(tracking_data$frame))
passing_lane_analysis <- map_dfr(frame_sample, ~ {
  result <- analyze_passing_lane_coverage(tracking_data, events, .x)
  if(!is.null(result)) {
    result[, frame := .x]
  }
  result
})


test <- events |>
  select(event_id, frame_start, frame_end, event_type, player_name, team_id,
         player_in_possession_name, pass_angle, pass_angle_received, interplayer_angle)