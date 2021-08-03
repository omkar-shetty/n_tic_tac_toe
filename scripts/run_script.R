gc()
rm(list = ls())
cat('\014')

# Libraries ---------------------------------------------------------------
library(data.table)
library(glue)

# Define Source Files -----------------------------------------------------
source('functions/player_profiles.R')
source('functions/utilities.R')
source('functions/qlearn_tab.R')

# Define Config -----------------------------------------------------------

config <- list(
    n = 3
  , window_size = 100
  #,player1 = plr_minmax
  #,player2 = plr_random
  ,draw_board = T
  ,silent = F
  ,initialize_random_state = T
  ,rl_config = list(
    exploration = 0,      # % of times agent doesnt take the optimal action
    learning_rate = 0.8     
  )
)

play_game <- function(config){
  
  game <- initialize_game(config)
  if(config$draw_board){draw_game_board(config,game)}
  
  players <- c(config$player1,config$player2)
  player <- 1
  game_status <- 'in play'
  
  while(game_status == 'in play'){
    
    move <- players[[player]](config,game,plr = player)$move
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




