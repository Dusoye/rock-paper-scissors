library(dplyr)
library(ggplot2)
library(data.table)

n = 100000 #number of games
comp_prob = c(0.4, 0.3, 0.3) #computer probabilities of rock/paper/scissors respectively 

# game function
rps <- function(comp_prob = c(0.4, 0.3, 0.4), player_prob = c(1/3, 1/3, 1/3)){
  weapons <- c('R', 'P', 'S')
  
  comp <- sample(weapons, size = 1, prob = comp_prob) 
  player <- sample(weapons, size = 1, prob = player_prob)
  
  result <- if(identical(comp, player)){
    0
  } else if(player == 'R' & comp == 'S'|
            player == 'S' & comp == 'P' |
            player == 'P' & comp == 'R'){
    1
  } else{
    -1
  }
  output <- list('comp' = comp, 'player' = player, 'result' = result)
  return(output)
}


tmp <- c()

for (i in 1:n){
  out <- rps()
  tmp <- c(tmp, out$result)
  
}

tmp <- as.data.table(tmp)
tmp$pl <- cumsum(tmp$tmp)
tail(tmp)

table(tmp)



