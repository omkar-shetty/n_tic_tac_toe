






plr_random <- function(config,game,plr){
  allowed_play <- which(game == 3)
  if(length(allowed_play) > 1){
    move <- sample(allowed_play,1)
  } else{
    move <- allowed_play
  }
  
  return(list(move = move, utility = 0))
}


plr_minmax <- function(config,game,plr){
  
  allowed_play <- which(game == 3)
  n <- config$n^2
  pot_score <- rep(0,n)
  
  if(length(allowed_play) == n){
    move <- sample(allowed_play,1)
    return(list(move = move
                ,utility = is_winner(config,game)$score))
  }
  
  if(length(allowed_play) == 1){
    move <- allowed_play
    game[move] <- plr
    return(list(move = move
                ,utility = is_winner(config,game)$score))
  } 
  
  n <- config$n^2
  pot_score <- rep(0,n)
  
  # Get scores if there are any terminal nodes in next step
  for(i in allowed_play){
    lgame <- game
    lgame[i] <- plr
    game_result <- is_winner(config,lgame)
    pot_score[i] <- game_result$score
  }
  
  if(plr == 1 & any(pot_score == 10)){
    move <- which.max(pot_score)
    game[move] <- plr
    return(list(move = move
                ,utility = is_winner(config,game)$score))
  }else if(plr == 2 & any(pot_score == -10)){
    move <- which.min(pot_score)
    game[move] <- plr
    return(list(move = move
                ,utility = is_winner(config,game)$score))
  }
    
  for(i in allowed_play){
    lgame <- game
    lgame[i] <- plr
    nxt_plr <- ifelse(plr == 1,2,1)
    pot_score[i] <- plr_minmax(config,lgame, plr = nxt_plr)$utility
  }
  
  pot_score[-allowed_play] <- ifelse(plr == 1,-1,1)*100
  if(plr == 1){
    move <- which.max(pot_score)
    return(list(move = move
                ,utility = pot_score[move]))
  }else if(plr == 2){
    move <- which.min(pot_score)
    game[move] <- plr
    return(list(move = move
                ,utility = pot_score[move]))
  }
  
}
