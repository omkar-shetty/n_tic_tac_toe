
initialize_game <- function(config){
  
  game <- NULL
  
  if(config$initialize_random_state){
    game <- intialize_random_state_game(config)
  }
  
  if(is.null(game)) game <- rep(3,config$n^2)
  
return(game)
}

intialize_random_state_game <- function(config){
  
  all_slots = config$n^2
  filled_slots_cnt <- sample(0:all_slots, 1)
  
  if(filled_slots_cnt == 0){
    return(NULL)
  }
  
  filled_slots <-  sample(1:all_slots, filled_slots_cnt, replace = F)
  p1_filled_slots <- filled_slots[1:round(filled_slots_cnt/2,0)]
  p2_filled_slots <- setdiff(filled_slots, p1_filled_slots)
  
  game <- rep(3,config$n^2)
  game[p1_filled_slots] <- 1  
  game[p2_filled_slots] <- 2  
  
  if(is_winner(config, game)$status == 'over') game = intialize_random_state_game(config)
  
  return(game)
}

draw_game_board <- function(config,game){
  
  #Identfy incorrect games
  if(length(game) != config$n^2){
    stop('Incorrect Game Dimensions !')
    }
 
  symbols <- c('X','O','')
  
  plot.new()
  plot.window(xlim = c(0,config$window_size), ylim = c(0,config$window_size))
  
  gap <- config$window_size/(config$n)
  
  cuts <- gap*(1:(config$n-1))
  abline(h = cuts, col="darkgrey", lwd = 2)
  abline(v = cuts, col="darkgrey", lwd = 2)
  
  x_coords <- (c(0,cuts) + gap/2)
  y_coords <- (c(0,cuts) + gap/2)
  y_coords <- sort(y_coords, decreasing = T)
  
  text(rep(x_coords,config$n),rep(y_coords, each = config$n),symbols[game])
  
}

is_winner <- function(config,game){
  
  #convert game constants
  game <- replace(game, game == 2, -1)
  game <- replace(game, game == 3, 0)
  
  grid <- t(matrix(game, nrow = config$n))
  
  if(any(abs(rowSums(grid)) == config$n)){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'row win'
    winner <- ifelse(unique(grid[which(abs(rowSums(grid)) == config$n),]) == 1,
                     'player 1', 'player 2')
    score <- ifelse(winner == 'player 1',10,-10)
    
  }else if(any(abs(colSums(grid)) == config$n)){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'column win'
    winner <- ifelse(unique(grid[,which(abs(colSums(grid)) == config$n)]) == 1,
                     'player 1', 'player 2')
    score <- ifelse(winner == 'player 1',10,-10)
    
  }else if(abs(sum(diag(grid))) == config$n){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'diagonal win'
    winner <- ifelse(sum(diag(grid))/config$n == 1,'player 1', 'player 2')
    score <- ifelse(winner == 'player 1',10,-10)
    
  } else if(abs(sum(diag(apply(grid,2,rev)))) == config$n){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'diagonal win'
    winner <- ifelse(sum(diag(apply(grid,2,rev)))/config$n == 1,'player 1', 'player 2')
    score <- ifelse(winner == 'player 1',10,-10)
  } else if(!(0 %in% game)){
    status <- 'over'
    outcome <- 'tie'
    sp_outcome <- NA
    winner <- NA
    score <- 0
  } else{
    status <- 'in play'
    outcome <- NA
    sp_outcome <- NA
    winner <- NA
    score <- 0
  }
  
  return(list(
    outcome = outcome
    ,status = status
    ,sp_outcome = sp_outcome
    ,winner = winner
    ,score = score
  ))
  
}

