
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
    
  }else if(any(abs(colSums(grid)) == config$n)){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'column win'
    winner <- ifelse(unique(grid[,which(abs(colSums(grid)) == config$n)]) == 1,
                     'player 1', 'player 2')
    
  }else if(abs(sum(diag(grid))) == config$n){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'diagonal win'
    winner <- ifelse(sum(diag(grid))/config$n == 1,'player 1', 'player 2')
    
  } else if(abs(sum(diag(apply(grid,2,rev)))) == config$n){
    status <- 'over'
    outcome <- 'win'
    sp_outcome <- 'diagonal win'
    winner <- ifelse(sum(diag(apply(grid,2,rev)))/config$n == 1,'player 1', 'player 2')
  } else if(!(0 %in% game)){
    status <- 'over'
    outcome <- 'tie'
    sp_outcome <- NA
    winner <- NA
  } else{
    status <- 'in play'
    outcome <- NA
    sp_outcome <- NA
    winner <- NA
  }
  
  return(list(
    outcome = outcome
    ,status = status
    ,sp_outcome = sp_outcome
    ,winner = winner
  ))
  
}

