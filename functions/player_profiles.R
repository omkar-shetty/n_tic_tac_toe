plr_random <- function(config,game,plr){
  allowed_play <- which(game == 3)
  if(length(allowed_play) > 1){
    move <- sample(allowed_play,1)
  } else{
    move <- allowed_play
  }
  
  return(move)
}


plr_minmax <- function(config,game,plr){
  
  allowed_play <- which(game == 3)
  if(length(allowed_play) > 1){
    move <- sample(allowed_play,1)
    return(move)
  } 
  
  n <- config$n
  pot_utility <- rep(0,n)
  
  for(i in allowed_play){
    game[i] <- plr
    game_result <- is_winner(config,game)
  }
  
    
  }
  
}