##############################################
# Package used to build the initial population
library(NetLogoR)
##############################################

#######################################
# Initial population for our case study
# 10 seal, each having 
# a meal counter, set to 0 at first and can go up to 3
# 3 digestion time counters (one associated to each meal), set to 0 at first
# a poop counter, set to 0 and that'll allow to monitor no of poops
# a rest counter, set to 2 (animals are all rested when starting)

# Create a data frame of the individual wolf characteristics in the initial population
initPopseal_DF <- data.frame(
  ID = 1:10, # 10 individuals with id from 1 to 10
  meal_count = rep(0, 10), # from 1 to 3
  digestion_time1 = rep(0, 10), # from 0 to 5 (at 5, they'll poop)
  digestion_time2 = rep(0, 10), # from 0 to 5 (at 5, they'll poop)
  digestion_time3 = rep(0, 10), # from 0 to 5 (at 5, they'll poop)
  poop_count = rep(0, 10), 
  resting_time = rep(2, 10) # from 0 to 2, at 2 they go forage, otherwise they rest
)

# Create the initial population using the NetLogoR package
# Create an agentMatrix object
# Individual IDs are "who" in the agentMatrix and starts at 0 (automatically created when creating the individuals)
# init <- function(initPopseal_DF){
  
  # Initialize the model objects (i.e., land and wolves)
 # create a fictivesea
  
 sea <- createWorld(0, 49, 0, 49) 
 sea <- NLset(world = sea, 
   agents = patches(sea), 
   val = 1)
 
 
 colony <- createWorld(
   0, 49, 0, 49,
   data = c(2, rep(NA, 50*50-1)) )
  
   prey <- createWorld(0, 49, 0, 49,
    data = NA)
    
    # prey_centre = sample(1:(50*50), size = 1, replace = FALSE)
    
    prey_centre = 20
    prey[prey_centre:(prey_centre + 10), prey_centre:(prey_centre + 10)] <-3
  
    nutrients <- createWorld(0, 49, 0, 49,
      data = 0)
    
    sea <- stackWorlds(sea, colony, prey, nutrients)
  # create as many seals as nrow(initPopseal_DF), and place them in the colony
  seals <- createTurtles(n = nrow(initPopseal_DF), 
                         coords = cbind(xcor = 50, 
                                        ycor = 50)) 
  # define each individual characteristic
  seals <- turtlesOwn(turtles = seals, tVar = "meal_count", 
                      tVal = initPopseal_DF[, "meal_count"])
  seals <- turtlesOwn(turtles = seals, tVar = "digestion_time1", 
                      tVal = initPopseal_DF[, "digestion_time1"])
  seals <- turtlesOwn(turtles = seals, tVar = "digestion_time2", 
                      tVal = initPopseal_DF[, "digestion_time2"])
  seals <- turtlesOwn(turtles = seals, tVar = "digestion_time3", 
                      tVal = initPopseal_DF[, "digestion_time3"])
  seals <- turtlesOwn(turtles = seals, tVar = "poop_count", 
                      tVal = initPopseal_DF[, "poop_count"])
  seals <- turtlesOwn(turtles = seals, tVar = "resting_time", 
                      tVal = initPopseal_DF[, "resting_time"])
  
  # return(seals)
# }
#######################################



### processes

# foraging:

moveRandomly <- function(turtles, landscape, moveAngle) {
  # Move one step in the direction between (- moveAngle) and (+ moveAngle)
  turtles <- right(turtles = turtles, 
                   angle = runif(n = NLcount(turtles), 
                                 min = -moveAngle, 
                                 max = moveAngle))
  turtles <- fd(world = landscape, 
                turtles = turtles, 
                dist = 4, 
                torus = FALSE) # the world is "wrapped": e.g., an individual leaving the landscape
                              # on the right border reappears on the left border
  return(turtles)
}


seals1 <- moveRandomly(seals, sea, sample(150, 1, replace = FALSE))
seals2 <- moveRandomly(seals1, sea, sample(150, 1, replace = FALSE))


plot(sea$sea, col = "cyan")
points(seals, pch = 16,  
  col = "black")

plot(sea$colony, col = "brown", add=TRUE)
plot(sea$prey, col = "pink", add=TRUE)
points(seals, 
       pch = 16,  
       col = "black"
      )

points(seals1, 
        pch = 16,  
        col = "black")

points(seals2, 
          pch = 16,  
          col = "black")
