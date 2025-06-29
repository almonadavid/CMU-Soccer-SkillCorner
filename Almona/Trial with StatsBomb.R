library(tidyverse)
library(StatsBombR)
library(sportyR)
library(ggplot)
library(grid)



wwc <- FreeCompetitions() |> 
  filter(competition_id == "72" & season_id == "107") |> 
  FreeMatches() |> 
  free_allevents() |> 
  allclean()


champions_league <- FreeCompetitions() |> 
  filter(competition_id == "16" & season_id %in% c(4,2,1, 22:27)) |> 
  FreeMatches() |> 
  free_allevents() |> 
  allclean()


corner <- champions_league |> 
  filter(pass.type.name == "Corner" & minute == 11 & type.name == "Pass")


geom_soccer(league = "EPL") +
  geom_point(data = corner, aes(x = location.x, y = location.y))


ggplot(data = corner, aes(x = location.x, y = location.y)) +
  geom_point()



defensiveactivitycolors <- c("#dc2429", "#dc2329", "#df272d", "#df3238", "#e14348", "#e44d51", "#e35256", "#e76266", "#e9777b", "#ec8589", "#ec898d", "#ef9195", "#ef9ea1", "#f0a6a9", "#f2abae", "#f4b9bc", "#f8d1d2", "#f9e0e2", "#f7e1e3", "#f5e2e4", "#d4d5d8", "#d1d3d8", "#cdd2d6", "#c8cdd3", "#c0c7cd", "#b9c0c8", "#b5bcc3", "#909ba5", "#8f9aa5", "#818c98", "#798590", "#697785", "#526173", "#435367", "#3a4b60", "#2e4257", "#1d3048", "#11263e", "#11273e", "#0d233a", "#020c16")

ggplot(data= corner, aes(x = location.x, y = location.y)) +
  annotate("rect",xmin = 0, xmax = 120, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 60, ymin = 0, ymax = 80, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 18, xmax = 0, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 102, xmax = 120, ymin = 18, ymax = 62, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = 6, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 114, ymin = 30, ymax = 50, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 120, xmax = 120.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("rect",xmin = 0, xmax = -0.5, ymin =36, ymax = 44, fill = NA, colour = "black", size = 0.6) +
  annotate("segment", x = 60, xend = 60, y = -0.5, yend = 80.5, colour = "black", size = 0.6)+
  annotate("segment", x = 0, xend = 0, y = 0, yend = 80, colour = "black", size = 0.6)+
  annotate("segment", x = 120, xend = 120, y = 0, yend = 80, colour = "black", size = 0.6)+
  theme(rect = element_blank(), line = element_blank()) +
  annotate("point", x = 12 , y = 40, colour = "black", size = 1.05) + # add penalty spot right
  annotate("point", x = 108 , y = 40, colour = "black", size = 1.05) +
  annotate("path", colour = "black", size = 0.6, x=60+10*cos(seq(0,2*pi,length.out=2000)),
           y=40+10*sin(seq(0,2*pi,length.out=2000)))+ # add centre spot
  annotate("point", x = 60 , y = 40, colour = "black", size = 1.05) +
  annotate("path", x=12+10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40+10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  annotate("path", x=108-10*cos(seq(-0.3*pi,0.3*pi,length.out=30)), size = 0.6,
           y=40-10*sin(seq(-0.3*pi,0.3*pi,length.out=30)), col="black") +
  # Add pass lines from start to end location
  geom_segment(aes(x = location.x, y = location.y, 
                   xend = pass.end_location.x, yend = pass.end_location.y),
               color = "red", size = 1, alpha = 0.7) +
  # Add starting points (corner kick locations)
  geom_point(color = "blue", size = 1, alpha = 0.8) +
  # Add ending points (where passes were received)
  geom_point(aes(x = pass.end_location.x, y = pass.end_location.y), 
             color = "green", size = 1, alpha = 0.8) +
  coord_fixed()
