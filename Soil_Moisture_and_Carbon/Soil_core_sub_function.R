library(tidyverse)

depth0 <- c( 0, 5, 10, 15, 20 )
depth1 <- c( 5, 10, 15, 20, 25 )
amount <- c( 3, 15, 0, 10, 5)
bulk.density <- c( 100, 75, 82, 150, 90 )

cum.amount <- cumsum(amount)

plot(c(0,depth1), c(0,cumsum(amount)))

get.height <- Vectorize(function( depth0, depth1, amount, goal ){
  A <- cumsum(amount)
  goal.lwr <- goal - 5
  if( all( A < goal)){    # if the core isn't deep enough to reach the desired quantity of ash
    return(NA)
  }
  # calc height of core to goal ash content
  index.upr <- min( which( A >= goal ) )   # layer where we reach the desired quantity of ash
  start.ash <- A[index.upr] - amount[index.upr] # cumulative amount of ash to the layer below the layer where the goal is met
  need <- goal - start.ash # how much of the layer where the goal is met that is needed
  height <- (depth1[index.upr] - depth0[index.upr]) * (need / amount[index.upr]) # height of the layer where the goal is met that is needed
  tot.height <- depth0[index.upr] + height # add the core height to layer below layer where goal is met to the amount of current layer that is needed
  
  return(tot.height)
}, 'goal')

avg.soil.prop <- Vectorize(function( depth0, depth1, amount, soil.prop, goal ){
  A <- cumsum(amount)
  goal.lwr <- goal - 5
  if( all( A < goal)){    # if the core isn't deep enough to reach the desired quantity of ash
    return(NA)
  }
 
  # calc avg bulk density of core between last and current goals
  index.upr <- min( which( A >= goal ) )   # layer where we reach the desired quantity of ash
  start.ash <- A[index.upr] - amount[index.upr] # cumulative amount of ash to the layer below the layer where the goal is met
  need <- goal - start.ash # how much of the layer where the goal is met that is needed
  index.lwr <- min(which(A > goal.lwr))
  prior.bd <- soil.prop[index.lwr:index.upr]
  weights <- depth1[index.lwr:index.upr]
  weights[1] <- ( A[index.lwr] - goal.lwr )
  weights[length(weights)] <- need
  bd <- (prior.bd * weights) / 5
  
  return(bd)
}, 'goal')


f(depth0, depth1, amount, goal=1)
f(depth0, depth1, amount, goal=3)
f(depth0, depth1, amount, goal=8)
f(depth0, depth1, amount, goal=800)

f(depth0, depth1, amount, goal=c(5, 10, 15, 20, 25, 30), bulk.density)



depth0 <- c( 0, 5, 10, 15, 20, 0, 5, 10, 15, 20 )
depth1 <- c( 5, 10, 15, 20, 25, 5, 10, 15, 20, 25 )
amount <- c( 3, 15, 0, 10, 5, 6, 20, 30, 30, 10 )
bulk.density <- c( 3, 15, 0, 10, 5, 6, 20, 30, 30, 10 )
core <- c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2)
data <- data.frame( depth0 = depth0, depth1=depth1, amount=amount, core=core, bulk.density = bulk.density)

goal <- seq(5,100,5)

data

test <- data %>% group_by(core) %>%
  do({ data.frame( depth=get.height(.$depth0, .$depth1, .$amount, goal=goal),
                   bulk.density = avg.soil.prop(.$depth0, .$depth1, .$amount, .$bulk.density, goal=goal)) })
