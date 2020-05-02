library(dplyr)
library(ggplot2)
library(data.table)

# definitions
n = 1000 #number of games
comp_strat = c(0.6, 0.2, 0.2) #computer probabilities of rock/paper/scissors respectively 

weapons <- c('R', 'P', 'S')
num_actions <- length(weapons)
player_strategy <- vector(mode = 'numeric', length = num_actions)
regret_sum <- vector(mode = 'numeric', length = num_actions)
strategy_sum <- vector(mode = 'numeric', length = num_actions)

# rock-paper-scissors game 
rps <- function(comp, player){
  result <- if(identical(comp, player)){
    0
  } else if(player == 'R' & comp == 'S'|
            player == 'S' & comp == 'P' |
            player == 'P' & comp == 'R'){
    1
  } else{
    -1
  }
  return(result)
}

# regret-matched mixed strategy
updatePlayer <- function(player_strat, regret_sum, strategy_sum){
  normalising_sum = 0
  for(a in 1:num_actions){
    player_strat[a] = if_else(regret_sum[a] > 0, regret_sum[a], 0)
    normalising_sum = normalising_sum + player_strat[a]
  }
  
  for(a in 1:num_actions){
    if(normalising_sum > 0){
      player_strat[a] = player_strat[a] / normalising_sum
    } else {
      player_strat[a] = 1 / num_actions
    }
    strategy_sum[a] = strategy_sum[a] + player_strat[a]
  }
  
  out <- list('player_strat' = player_strat, 'strategy_sum' = strategy_sum)
  return(out)
}

# average strategy
avgStrat <- function(strategy_sum){
  avg_strat <- vector(mode = 'numeric', length = num_actions)
  normalising_sum = 0
  for(a in 1:num_actions){
    normalising_sum = normalising_sum + strategy_sum[a]
  }
  for(a in 1:num_actions){
    if(normalising_sum > 0){
      avg_strat[a] = strategy_sum[a] / normalising_sum
    } else{
      avg_strat[a] = 1 / num_actions
    }
  }
  return(avg_strat)
}

# get action
actions <- function(strat = strategy) {
  action <- sample(weapons, size = 1, prob = strat)
  return(action)
}

# accumulate regrets
regrets <- function(regret_sum, result, player){
  for(a in 1:num_actions){
    action_utility = rps(weapons[a], player)
    regret_sum[a] = regret_sum[a] + action_utility - result
  }
  return(regret_sum)
}



#player_strat = c(rep(1 / num_actions, num_actions)) # initialise player RPS probs evenly weighted

for (i in 1:10000){
  player_strategy <- updatePlayer(player_strategy, regret_sum, strategy_sum)
  
  strategy_sum <- player_strategy$strategy_sum
  player_strategy <- player_strategy$player_strat
  
  comp <- actions(comp_strat)
  player <- actions(player_strategy)

  result <- rps(comp, player)
  
  regret_sum <- regrets(regret_sum, result, player)
  
  player_strategy = avgStrat(strategy_sum)
  
  res = result + res
  print(c(res, player_strategy))
}


