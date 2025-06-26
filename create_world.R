library(NetLogoR)
library(magrittr)


# create world
sea <- createWorld(0, 49, 0, 49) 
sea <- NLset(world = sea, 
  agents = patches(sea), 
  val = 1)


colony <- createWorld(
  0, 49, 0, 49,
  data = c(2, rep(NA, 50*50-1)) )

plot(colony)

prey <- createWorld(0, 49, 0, 49,
data = NA)

prey = NLset(prey, agents = patches, var )

prey_c <- of(prey, agents = patches, var = prey_centre)

# prey_centre = sample(1:(50*50), size = 1, replace = FALSE)

prey_centre = 20
prey[prey_centre:(prey_centre + 10), prey_centre:(prey_centre + 10)] <-3 # rep(3, 100)

plot(prey)

# prey_neighbors <- neighbors(prey, agents = patches(prey_centre), nNeighbors = 8)

nutrients <- createWorld(0, 49, 0, 49,
  data = 0)

world <- stackWorlds(sea, colony, prey, nutrients)

plot(world)
