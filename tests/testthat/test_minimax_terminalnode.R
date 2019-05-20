library(testthat)

source('functions/player_profiles.R')
source('functions/utilities.R')

#game <- c(1,1,2,2,1,3,3,2,3)

test_that("test terminal node",{
  game <-  c(1,1,2,2,1,1,3,2,2)
  config <- config <- list(
    n = 3
    , window_size = 100
  )

  move <- plr_minmax(config,game,plr = 1)$move
  game_score <- plr_minmax(config,game,plr = 1)$utility
  
  expect_equal(move,7)
  expect_equal(game_score,0)
    
})

test_that("test winning strategy - one step to win",{
  
  config <- config <- list(
    n = 3
    , window_size = 100
  )
  
  # Test Player One
  
  game <- game <- c(1,1,2,2,1,3,3,2,3)
  
  move <- plr_minmax(config,game,plr = 1)$move
  game_score <- plr_minmax(config,game,plr = 1)$utility
  
  expect_equal(move,9)
  expect_equal(game_score,10)
  
  # Test Player Two
  
  game <- c(3,1,2,3,2,1,3,1,3)
  
  move <- plr_minmax(config,game,plr = 2)$move
  game_score <- plr_minmax(config,game,plr = 2)$utility
  
  expect_equal(move,7)
  expect_equal(game_score,-10)
  
  # Multiple options to win
  
  game <- c(2,3,2,1,2,1,3,1,3)
  
  move <- plr_minmax(config,game,plr = 2)$move
  game_score <- plr_minmax(config,game,plr = 2)$utility
  
  expect_equal(move,2) #which.min should pick up the first instance
  expect_equal(game_score,-10)
  
})

