library(readr)
miamicharlotte <- read_csv("../Downloads/1186860_dynamic_events.csv")

runsjoin <- miamicharlotte %>%
  filter(event_type == "off_ball_run") %>%
  select(run_frame = frame_start, frame_end, event_type, event_subtype, n_simultaneous_runs, speed_avg, third_start, third_end, n_opponents_ahead_pass_reception, xthreat, x_start, y_start, x_end, y_end, player_position, channel_id_start)


possessionjoin <- miamicharlotte %>%
  filter(event_type == "player_possession") %>%
  mutate(sep_created = separation_end - separation_start) %>%
  select(possession_frame_start = frame_start,
         possession_frame_end = frame_end,
         sep_created, separation_start, separation_end)


possession_with_runs <- fuzzy_left_join(
  possessionjoin,
  runsjoin,
  by = c("possession_frame_start" = "run_frame",
         "possession_frame_end" = "run_frame"),
  match_fun = list(`<=`, `>=`)
)

onlyruns<-possession_with_runs |>
  filter(event_type == "off_ball_run")


#Variables: x_start, y_start, n_simultaneous_runs, third_start, player_position, channel_id_start with combined data

model_sep_created <- lm(sep_created ~ x_start + y_start + n_simultaneous_runs +
                         third_start + player_position + channel_id_start,
                       data = onlyruns)

summary(model_sep_created)


model_sep_start <- lm(separation_start ~ x_start + y_start + n_simultaneous_runs +
                        third_start + player_position + channel_id_start,
                      data = onlyruns)

summary(model_sep_start)


#All data

model_sep_start_all <- lm(separation_start ~ x_start + y_start + n_simultaneous_runs +
                        third_start + player_position + channel_id_start,
                      data = miamicharlotte)

summary(model_sep_start_all)





#Decision Tree
miamicharlotte$third_start <- as.factor(miamicharlotte$third_start)
miamicharlotte$player_position <- as.factor(miamicharlotte$player_position)
miamicharlotte$channel_id_start <- as.factor(miamicharlotte$channel_start)

library(caret)
library(ggplot2)

model_vars <- c("separation_start", "player_position", "third_start", "channel_id_start")
sum(complete.cases(miamicharlotte[, model_vars]))


miami_clean <- miamicharlotte %>%
  filter(complete.cases(across(all_of(model_vars))))

miami_possession<-miami_clean |>
  filter(event_type == "player_possession")

#What third
set.seed(10)

sep_start_tree <- train(
  separation_start ~  third_start,
  method = "rpart",
  tuneLength = 20,
  trControl = trainControl(method = "cv", number = 10),
  data = miami_possession
)


ggplot(sep_start_tree)

library(rpart.plot)
rpart.plot(sep_start_tree$finalModel)

#What Channel

set.seed(10)

sep_start_tree <- train(
  separation_start ~  channel_start,
  method = "rpart",
  tuneLength = 20,
  trControl = trainControl(method = "cv", number = 10),
  data = miami_possession
)


ggplot(sep_start_tree)

library(rpart.plot)
rpart.plot(sep_start_tree$finalModel)

##Players making runs 
miamicharlotte$associated_off_ball_run_subtype <- as.factor(miamicharlotte$associated_off_ball_run_subtype)
miami_runs<-miami_clean |>
  filter(event_type == "off_ball_run")


set.seed(10)

sep_start_tree <- train(
  separation_start ~  associated_off_ball_run_subtype,
  method = "rpart",
  tuneLength = 20,
  trControl = trainControl(method = "cv", number = 10),
  data = miami_runs
)


ggplot(sep_start_tree)

library(rpart.plot)
rpart.plot(sep_start_tree$finalModel)
