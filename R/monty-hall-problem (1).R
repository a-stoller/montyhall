#' @title
#'   Create a new Monty Hall Problem game.
#'
#' @description
#'   `create_game()` generates a new game that consists of two doors 
#'   with goats behind them, and one with a car.
#'
#' @details
#'   The game setup replicates the game on the TV show "Let's
#'   Make a Deal" where there are three doors for a contestant
#'   to choose from, one of which has a car behind it and two 
#'   have goats. The contestant selects a door, then the host
#'   opens a door to reveal a goat, and then the contestant is
#'   given an opportunity to stay with their original selection
#'   or switch to the other unopened door. There was a famous 
#'   debate about whether it was optimal to stay or switch when
#'   given the option to switch, so this simulation was created
#'   to test both strategies. 
#'
#' @param ... no arguments are used by the function.
#' 
#' @return The function returns a length 3 character vector
#'   indicating the positions of goats and the car.
#'
#' @examples
#'   create_game()
#'
#' @export
create_game <- function()
{
    a.game <- sample( x=c("goat","goat","car"), size=3, replace=F )
    return( a.game )
} 



#' @title
#'   The contestant makes their initial door selection.

#' @description
#'   `select_door()` Type: Integer -> randomly selects a door number from 1 to 3.
#' @details
#'   Selects door 1, 2, or 3. The door contains a goat or a car.
#' @param ... no arguments are used by the function.
#' @return The function returns the number of the randomly selected door, between 1 and 3. 
#' @examples
#'   select_door()
#' @export
select_door <- function( )
{
  doors <- c(1,2,3) 
  a.pick <- sample( doors, size=1 )
  return( a.pick )  # number between 1 and 3
}



#' @title
#'   The host opens one of the two goat doors, always revealing a goat. 
#' @description
#'   `open_goat_door()` Type: Integer -> Selects one of the goat doors, 
#'   generating a number between 1 and 3.  
#' @details
#'   The host will always open a goat door. This will never be the contestant's 
#'   initially selected door, or the car door.
#' @param ... Arguments: `game` (the name of the game) 
#'   and `a.pick` (its value is the initial selection door's number)
#' @return The function returns an integer representing the number
#'   of the goat door that the host opened.
#' @examples
#'   open_goat_door(game = newGame, a.pick = firstDoor)
#' @export
open_goat_door <- function( game, a.pick )
{
   doors <- c(1,2,3)
   # if contestant selected car,
   # randomly select one of two goats 
   if( game[ a.pick ] == "car" )
   { 
     goat.doors <- doors[ game != "car" ] 
     opened.door <- sample( goat.doors, size=1 )
   }
   if( game[ a.pick ] == "goat" )
   { 
     opened.door <- doors[ game != "car" & doors != a.pick ] 
   }
   return( opened.door ) # number between 1 and 3
}



#' @title
#'   The contestant can either stay with their initial selection, 
#'   or switch to the remaining unopened door.
#' @description
#'   `change_door()` Type: Integer -> Generates an integer of the contestant's final 
#'   selection door.
#' @details
#'   This allows the contestant to employ their chosen game strategy of either 
#'   staying with their first choice, or switching to the other unopened door. 
#' @param ... Arguments: `stay` (its value is TRUE if the contestant is staying 
#'   with their initial choice, or FALSE if they choose to switch to the other door.), 
#'   `opened.door` (its value is the door which the host opened in open_goat_door),
#'   and `a.pick` (its value is the contestant's initially selected door). 
#' @return The function returns an integer representing the contestant's final selection. 
#'   This is either the contestant's intitial pick, or the remaining unopened door. 
#' @examples
#'   change_door(stay = T, opened.door = openedDoor,
#'   a.pick = firstDoor)
#' @export
change_door <- function( stay=T, opened.door, a.pick )
{
   doors <- c(1,2,3) 
   
   if( stay )
   {
     final.pick <- a.pick
   }
   if( ! stay )
   {
     final.pick <- doors[ doors != opened.door & doors != a.pick ] 
   }
  
   return( final.pick )  # number between 1 and 3
}



#' @title
#'   Determine if the contestant has won or lost the game. 
#' @description
#'   `determine_winner()` Type: Character -> Returns 'WIN' if the final selection door 
#'   is a car door, or returns 'LOSE' if the final selection door is a goat door. 
#' @details
#'   This function determines if the contestant has won or lost. If their final selection
#'   was a Car door, they have won. If the final selection was a goat, they have lost.
#'   They do not get to keep the goat. 
#' @param ... Arguments: `final.pick` (its value is the number of the final selection door)
#'   and `game` (the name of the game)
#' @return Returns a character string of either "WIN" or "LOSE" depending if the 
#'   contestant's final selection was a car door. 
#' @examples
#'   determine_winner(final.pick = finalDoor, game = newGame)
#' @export
determine_winner <- function( final.pick, game )
{
   if( game[ final.pick ] == "car" )
   {
      return( "WIN" )
   }
   if( game[ final.pick ] == "goat" )
   {
      return( "LOSE" )
   }
}





#' @title
#'   Plays an entire game, from door setup to
#'   determinining if the contestant has won a car or not.
#' @description
#'   play_game() Combines all above functions to play an entire game at once. 
#' @details
#'   This function reports outcomes based on whether the contestant stayed or switched
#'   doors. The intent is to determine if staying or switching is the better strategy.
#' @param ... no arguments are used by the function.
#' @return This function returns a dataframe with two columns: "strategy" and "outcome". 
#'   "Strategy" contains the options "stay" and "switch", and "outcome" contains the 
#'   options"WIN" and "LOSE". This dataframe reports the outcome of the game
#'   as well as the contestant's strategy.
#' @examples
#'   play_game()
#' @export
play_game <- function( )
{
  new.game <- create_game()
  first.pick <- select_door()
  opened.door <- open_goat_door( new.game, first.pick )

  final.pick.stay <- change_door( stay=T, opened.door, first.pick )
  final.pick.switch <- change_door( stay=F, opened.door, first.pick )

  outcome.stay <- determine_winner( final.pick.stay, new.game  )
  outcome.switch <- determine_winner( final.pick.switch, new.game )
  
  strategy <- c("stay","switch")
  outcome <- c(outcome.stay,outcome.switch)
  game.results <- data.frame( strategy, outcome,
                              stringsAsFactors=F )
  return( game.results )
}






#' @title
#'   Play the Monty Hall game any number of times.
#' @description
#'   Use this to generate a data set to determine the best strategy for the game.
#' @details
#'   This function plays the game `n` numer of times (default n = 100), reporting 
#'   the outcome and strategy for all of the games in one data frame. 
#' @param ... Argument: `n` (the amount of times you want to play the game)
#' @return This function returns a dataframe reporting the outcomes and strategy for 
#'   each of the games it just played.
#' @examples
#'   play_n_games(100)
#'   play_n_games(5000000)
#'   play_n_games(10)
#' @export
play_n_games <- function( n=100 )
{
  
  library( dplyr )
  results.list <- list()   # collector
  loop.count <- 1

  for( i in 1:n )  # iterator
  {
    game.outcome <- play_game()
    results.list[[ loop.count ]] <- game.outcome 
    loop.count <- loop.count + 1
  }
  
  results.df <- dplyr::bind_rows( results.list )

  table( results.df ) %>% 
  prop.table( margin=1 ) %>%  # row proportions
  round( 2 ) %>% 
  print()
  
  return( results.df )

}
