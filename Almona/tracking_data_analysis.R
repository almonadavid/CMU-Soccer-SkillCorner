# Load libraries
library(tidyverse)
library(lubridate)
library(data.table)


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
