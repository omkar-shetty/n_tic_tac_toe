gc()
rm(list = ls())
cat('\014')


# Libraries ---------------------------------------------------------------



# Define Source Files -----------------------------------------------------
source('functions/player_profiles.R')
source('functions/utilities.R')


# Define Config -----------------------------------------------------------

config <- list(
    n = 10
  , window_size = 100
  ,player1 = plr_random
  ,player2 = plr_random
  ,draw_board = F
  ,silent = T
)

play_game <- function(config){
  
  game <- rep(3,config$n^2)
  if(config$draw_board){draw_game_board(config,game)}
  
  players <- c(config$player1,config$player2)
  player <- 1
  game_status <- 'in play'
  
  while(game_status == 'in play'){
    
    move <- players[[player]](game)
    game[move] <- player
    if(config$draw_board){draw_game_board(config,game)}
  
    player <- ifelse(player == 1,2,1)
    
    result <- is_winner(config,game)
    game_status <- result$status
    
  }
    
  game_outcome <- result$outcome
  game_winner <- result$winner
  if(!config$silent){
    cat(paste0('The game is a ',game_outcome,' and the winner is ',game_winner))}
  
  return(list(game_outcome,game_winner))
  
}

play_game(config)

p1_wins <- 0
p2_wins <- 0
tie <- 0
for(i in 1:10000){
  x <- play_game(config)
  if(x[[1]] == 'tie'){
   tie <- tie + 1 
  }else if(x[[2]] == 'player 2'){
    p2_wins <- p2_wins + 1
  }else if(x[[2]] == 'player 1'){
    p1_wins <- p1_wins + 1
  }
}


