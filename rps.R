library(dplyr)
library(ggplot2)
library(data.table)

# definitions
n = 1000 #number of games
comp_strat = c(0.4, 0.3, 0.3) #computer probabilities of rock/paper/scissors respectively 

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

# update strategy
updatePlayer <- function(player_strategy, regret_sum, strategy_sum){
  normalising_sum = 0
  for(a in 1:num_actions){
    player_strategy[a] = if_else(regret_sum[a] > 0, regret_sum[a], 0)
    normalising_sum = normalising_sum + player_strategy[a]
  }
  
  for(a in 1:num_actions){
    if(normalising_sum > 0){
      player_strategy[a] = player_strategy[a] / normalising_sum
    } else {
      player_strategy[a] = 1 / num_actions
    }
    strategy_sum[a] = strategy_sum[a] + player_strategy[a]
  }
  return(player_strategy)
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

for (i in 1:10){
  player_strategy <- updatePlayer(player_strategy, regret_sum, strategy_sum)
  
  comp <- actions(comp_strat)
  player <- actions(player_strategy)

  result <- rps(comp, player)
  
  regret_sum <- regrets(regret_sum, result, player)
  
  print(c(comp, player, result, player_strategy))
}

tmp <- as.data.table(tmp)
tmp$pl <- cumsum(tmp$tmp)
tail(tmp)

table(tmp)



