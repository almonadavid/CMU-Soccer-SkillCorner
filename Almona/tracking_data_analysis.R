# Load libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(gganimate)


# Read data and convert to data.table
match_info = readRDS("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/CMU-Soccer-SkillCorner/Almona/match_data/J03WMX_match_info.rds") |> 
  setDT()

players = readRDS("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/CMU-Soccer-SkillCorner/Almona/match_data/J03WMX_players.rds") |> 
  setDT()

stadium = readRDS("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/CMU-Soccer-SkillCorner/Almona/match_data/J03WMX_stadium.rds") |> 
  setDT()

events = readRDS("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/CMU-Soccer-SkillCorner/Almona/match_data/J03WMX_events.rds") |> 
  setDT() |> 
  _[, event_time := ymd_hms(event_time)] # consistent timezone with positions df

positions = readRDS("C:/Users/almon/OneDrive - Centre College of Kentucky/Desktop/CMSACamp/Capstone/CMU-Soccer-SkillCorner/Almona/match_data/J03WMX_positions.rds") |> 
  setDT() |> 
  _[, timestamp := ymd_hms(timestamp)] # consistent timezone with events df



# next steps:
# filter for corners
# plot corner positioning
# plot player movement trajectory plot
# https://squared2020.com/2017/10/25/the-art-of-sketching-trajectory-analysis/

corner_events <- events[event_type == "CornerKick"]
event_time <- as.POSIXct("2023-05-27 13:45:16", tz = "UTC")

ggplot(data = positions[timestamp == event_time & frame_n == 32585]) +
  annotate("rect",xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = 0, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -36.75, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 36.75, xmax = 52.5, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 0, xend = 0, y = -34.5, yend = 34.5, colour = "black", size = 0.6)+
  annotate("segment", x = -52.5, xend = -52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  annotate("segment", x = 52.5, xend = 52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  theme(rect = element_blank(), line = element_blank()) +
  annotate("point", x = -42 , y = 0, colour = "black", size = 1.05) + # add penalty spot left
  annotate("point", x = 42 , y = 0, colour = "black", size = 1.05) + # add penalty spot right
  annotate("path", colour = "black", size = 0.6, x=0+8.75*cos(seq(0,2*pi,length.out=2000)),
           y=0+8.75*sin(seq(0,2*pi,length.out=2000))) + # add centre circle
  annotate("point", x = 0 , y = 0, colour = "black", size = 1.05) + # add centre spot
  annotate("path", x=-42+8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0+8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + # left penalty arc
  annotate("path", x=42-8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0-8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") + # right penalty arc
  geom_point(aes(x = x, y = y, color = team_id)) +
  coord_fixed() +
  scale_color_discrete(labels = c("DFL-CLU-000008" = "1. FC Köln", "DFL-CLU-00000G" = "FC Bayern München")) +
  theme_void()


#### GGANIMATE ####
event_time <- as.POSIXct("2023-05-27 13:45:16", tz = "UTC")

target_frame <- positions[timestamp == event_time, frame_n][1] # find first frame that matched event time
frames_before <- 80
frames_after <- 150
frame_range <- (target_frame - frames_before):(target_frame + frames_after)

anim_data <- positions[frame_n %in% frame_range]

# Create the animated plot
p <- ggplot(data = anim_data) +
  annotate("rect",xmin = -52.5, xmax = 52.5, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = 0, ymin = -34, ymax = 34, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -36.75, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 36.75, xmax = 52.5, ymin = -18.7, ymax = 18.7, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 47.25, ymin = -8.5, ymax = 8.5, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 52.5, xmax = 53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = -52.5, xmax = -53, ymin = -3.4, ymax = 3.4, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 0, xend = 0, y = -34.5, yend = 34.5, colour = "black", size = 0.6)+
  annotate("segment", x = -52.5, xend = -52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  annotate("segment", x = 52.5, xend = 52.5, y = -34, yend = 34, colour = "black", size = 0.6)+
  annotate("point", x = -42 , y = 0, colour = "black", size = 1.05) +
  annotate("point", x = 42 , y = 0, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6, x=0+8.75*cos(seq(0,2*pi,length.out=2000)),
           y=0+8.75*sin(seq(0,2*pi,length.out=2000))) +
  annotate("point", x = 0 , y = 0, colour = "black", size = 1.05) +
  annotate("path", x=-42+8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0+8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=42-8.75*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=0-8.75*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  geom_point(data = anim_data, aes(x = x, 
                                   y = ifelse(team_id == "BALL" & y == -19.46, -34, y), 
                                   color = team_id), 
             size = 3, alpha = 0.9) +
  coord_fixed() +
  scale_color_discrete(labels = c("DFL-CLU-000008" = "1. FC Köln", "DFL-CLU-00000G" = "FC Bayern München")) +
  labs(
    #caption = "25 Hz",
    caption = "1. FC Köln vs FC Bayern München | Bundesliga Matchday 34, 2022/2023 Season | May 27, 2023 | Final Score: 1-2"
  ) +
  theme_void() +
  theme(
    legend.position = "top",
    legend.title = element_blank(),
    plot.caption = element_text(hjust = 0.5, size = 10, color = "black")
  ) +
  transition_time(frame_n)

# Render the animation
play <- animate(p, nframes = length(frame_range), fps = 25, width = 800, height = 600)

# Display the animation
play


# For presentation slides
anim <- animate(p, 
                width = 1920, height = 1080,
                res = 200,                   
                fps = 25,                     
                nframes = length(frame_range))

anim_save("corner_kick_presentation.gif", anim)



