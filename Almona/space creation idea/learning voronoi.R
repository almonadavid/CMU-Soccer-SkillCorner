library(deldir)

set.seed(123)
x <- runif(50)
y <- runif(50)

tesselation <- deldir(x,y)
tiles <- tile.list(tesselation)

plot(tiles, pch = 19, close = TRUE, fillcol = hcl.colors(50, "Blue-Red"))


library(ggvoronoi)
set.seed(123)

x <- sample(1:400, size = 100)
y <- sample(1:400, size = 100)

dist <- sqrt((x/200)^2 + (y/200)^2)

df <- data.frame(x, y, dist = dist)

ggplot(df, aes(x, y), fill = dist) +
  geom_voronoi() + 
  geom_point()



# space creation: 
# 1. how much time a player has on the ball
# 2. distance between ball carrier and nearest defender
# 3. distance between every player making a run and the nearest defender to them.

player_with_ball <- events[,.(frame_start, frame_end, duration, event_type, player_name)]
















