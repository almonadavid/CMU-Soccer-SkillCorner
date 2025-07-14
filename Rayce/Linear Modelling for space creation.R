library(readr)
miamicharlotte <- read_csv("../Downloads/1186860_dynamic_events.csv")


model_space_creation<- lm(separation_gain ~ x_start + y_start + n_simultaneous_runs +
                            third_start + player_position + channel_id_start,
                          data = miamicharlotte)

summary(model_space_creation)