install.packages("stringr")

library(stringr)

game <- function() {
  
  data <- c("Rock", "Paper", "Scissors")
  correct_input <- c("Rock", "Paper", "Scissors", "R", "P", "S", "1", "2", "3")
  user_data <- c("Rock", "Paper", "Scissors", "R", "P", "S", "1", "2", "3", "0", "Quit")
  
  # computer randomly choose => Rock / Paper / Scissors
  computer <- sample(data, 1)
  
  # user choose => Rock / Paper / Scissors
  print("Welcome to the rock-paper-scissors games. May the odds be ever in your favor")
  print("Please choose     : (Rock / Paper / Scissors / Quit)")
  print("You can also type : (R    / P     / S        / 0)")
  print("You can also type : (1    / 2     / 3        / 0)")
  user_play <- str_to_title(readline("Let's choose : "))
  
  # show number of round now
  count_round <- 0
  
  # summary variables
  rock_computer <- 0
  paper_computer <- 0
  scissors_computer <- 0
  
  rock_user <- 0
  paper_user <- 0
  scissors_user <- 0
  
  win_computer <- 0
  lost_computer <- 0
  
  win_user <- 0
  lost_user <- 0
  
  tie_round <- 0
  total_round <- 0
  
  while (TRUE) {
    # computer randomly choose => Rock / Paper / Scissors
    computer <- sample(data, 1)
    
    correct_input <-
      c("Rock", "Paper", "Scissors", "R", "P", "S", "1", "2", "3")
    if (user_play %in% correct_input) {
      count_round <- count_round + 1
      print(paste("This is round", count_round))
      
      # What did we choose in this round and who won the match?
      # [1] Rock
      if (user_play == "1" | user_play == "Rock" | user_play == "R") {
        print("You have chosen : Rock")
        print(paste("Computer have choose :", computer))
        rock_user <- rock_user + 1
        
        # [1-1]---[Rock & Rock]
        if (computer == "Rock") {
          print("The match is tied!")
          tie_round <- tie_round + 1
          rock_computer <- rock_computer + 1
          
          # [1-2]---[Rock & Paper]
        } else if (computer == "Paper") {
          print("You have lost this match!")
          lost_user <- lost_user + 1
          paper_computer <- paper_computer + 1
          
          # [1-3]---[Rock & Scissors]
        } else {
          print("You have won this match!")
          win_user <- win_user + 1
          scissors_computer <- scissors_computer + 1
        }
        
        # [2] Paper
      } else if (user_play == "2" | user_play == "Paper" | user_play == "P") {
        print("You have choose : Paper")
        print(paste("Computer have choose :", computer))
        paper_user <- paper_user + 1
        
        # [2-1]---[Paper & Rock]
        if (computer == "Rock") {
          print("You have won this match!")
          win_user <- win_user + 1
          rock_computer <- rock_computer + 1
          
          # [2-2]---[Paper & Paper]
        } else if (computer == "Paper") {
          print("The match is tied!")
          tie_round <- tie_round + 1
          paper_computer <- paper_computer + 1
          
          # [2-3]---[Paper & Scissors]
        } else {
          print("You have lost this match!")
          lost_user <- lost_user + 1
          scissors_computer <- scissors_computer + 1
        }
        
        # [3] Scissors
      } else {
        print("You have choose : Scissors")
        print(paste("Computer have choose :", computer))
        scissors_user <- scissors_user + 1
        
        # [3-1]---[Scissors & Rock]
        if (computer == "Rock") {
          print("You have lost this match!")
          lost_user <- lost_user + 1
          rock_computer <- rock_computer + 1
          
          # [3-2]---[Scissors & Paper]
        } else if (computer == "Paper") {
          print("You have won this match!")
          win_user <- win_user + 1
          paper_computer <- paper_computer + 1
          
          # [3-3]---[Scissors & Scissors]
        } else {
          print("The match is tied!")
          tie_round <- tie_round + 1
          scissors_computer <- scissors_computer + 1
        }
      }
      # [0] Quit
    } else if (user_play == "0" | user_play == "Quit") {
      print("You have exited the game. Let's see the score")
      print("-------------------------------------------")
      print(paste("You have played :", count_round, "round."))
      print(paste("You have won    :", win_user, "round."))
      print(paste("You have tied   :", tie_round, "round."))
      print(paste("You have lost   :", lost_user, "round."))
      print("-------------------------------------------")
      print(paste("You have chosen rock     :", rock_user, "round."))
      print(paste("You have chosen paper    :", paper_user, "round."))
      print(paste("You have chosen scissors :", scissors_user, "round."))
      print("-------------------------------------------")
      print(paste("Computer have chosen rock     :", rock_computer, "round."))
      print(paste("Computer have chosen paper    :", paper_computer, "round."))
      print(paste("Computer have chosen scissors :", scissors_computer, "round."))
      
      break
      
      # wrong input
    } else {
      print("Your input is not correct => This round is not count")
      print("Please choose     : (Rock / Paper / Scissors / Quit)")
      print("You can also type : (R    / P     / S        / 0)")
      print("You can also type : (1    / 2     / 3        / 0)")
      count_round <- count_round
    }
    
    # choose again
    user_play <- str_to_title(readline("Let's choose again : "))
  }
}


game()