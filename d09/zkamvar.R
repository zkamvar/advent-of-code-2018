#!/usr/bin/env Rscript
# --- Day 9: Marble Mania ---

# You talk to the Elves while you wait for your navigation system to
# initialize. To pass the time, they introduce you to their favorite marble
# game.

# The Elves play this game by taking turns arranging the marbles in a circle
# according to very particular rules. The marbles are numbered starting with 0
# and increasing by 1 until every marble has a number.

# First, the marble numbered 0 is placed in the circle. At this point, while it
# contains only a single marble, it is still a circle: the marble is both
# clockwise from itself and counter-clockwise from itself. This marble is
# designated the current marble.

# Then, each Elf takes a turn placing the lowest-numbered remaining marble into
# the circle between the marbles that are 1 and 2 marbles clockwise of the
# current marble. (When the circle is large enough, this means that there is
# one marble between the marble that was just placed and the current marble.)
# The marble that was just placed then becomes the current marble.

# However, if the marble that is about to be placed has a number which is a
# multiple of 23, something entirely different happens. First, the current
# player keeps the marble they would have placed, adding it to their score. In
# addition, the marble 7 marbles counter-clockwise from the current marble is
# removed from the circle and also added to the current player's score. The
# marble located immediately clockwise of the marble that was removed becomes
# the new current marble.

# For example, suppose there are 9 players. After the marble with value 0 is
# placed in the middle, each player (shown in square brackets) takes a turn.
# The result of each of those turns would produce circles of marbles like this,
# where clockwise is to the right and the resulting current marble is in
# parentheses:

# [-] (0)
# [1]  0 (1)
# [2]  0 (2) 1 
# [3]  0  2  1 (3)
# [4]  0 (4) 2  1  3 
# [5]  0  4  2 (5) 1  3 
# [6]  0  4  2  5  1 (6) 3 
# [7]  0  4  2  5  1  6  3 (7)
# [8]  0 (8) 4  2  5  1  6  3  7 
# [9]  0  8  4 (9) 2  5  1  6  3  7 
# [1]  0  8  4  9  2(10) 5  1  6  3  7 
# [2]  0  8  4  9  2 10  5(11) 1  6  3  7 
# [3]  0  8  4  9  2 10  5 11  1(12) 6  3  7 
# [4]  0  8  4  9  2 10  5 11  1 12  6(13) 3  7 
# [5]  0  8  4  9  2 10  5 11  1 12  6 13  3(14) 7 
# [6]  0  8  4  9  2 10  5 11  1 12  6 13  3 14  7(15)
# [7]  0(16) 8  4  9  2 10  5 11  1 12  6 13  3 14  7 15 
# [8]  0 16  8(17) 4  9  2 10  5 11  1 12  6 13  3 14  7 15 
# [9]  0 16  8 17  4(18) 9  2 10  5 11  1 12  6 13  3 14  7 15 
# [1]  0 16  8 17  4 18  9(19) 2 10  5 11  1 12  6 13  3 14  7 15 
# [2]  0 16  8 17  4 18  9 19  2(20)10  5 11  1 12  6 13  3 14  7 15 
# [3]  0 16  8 17  4 18  9 19  2 20 10(21) 5 11  1 12  6 13  3 14  7 15 
# [4]  0 16  8 17  4 18  9 19  2 20 10 21  5(22)11  1 12  6 13  3 14  7 15 
# [5]  0 16  8 17  4 18(19) 2 20 10 21  5 22 11  1 12  6 13  3 14  7 15 
# [6]  0 16  8 17  4 18 19  2(24)20 10 21  5 22 11  1 12  6 13  3 14  7 15 
# [7]  0 16  8 17  4 18 19  2 24 20(25)10 21  5 22 11  1 12  6 13  3 14  7 15

# The goal is to be the player with the highest score after the last marble is
# used up. Assuming the example above ends after the marble numbered 25, the
# winning score is 23+9=32 (because player 5 kept marble 23 and removed marble
# 9, while no other player got any points in this very short example game).

# Here are a few more examples:

# -   10 players; last marble is worth 1618 points: high score is 8317
# -   13 players; last marble is worth 7999 points: high score is 146373
# -   17 players; last marble is worth 1104 points: high score is 2764
# -   21 players; last marble is worth 6111 points: high score is 54718
# -   30 players; last marble is worth 5807 points: high score is 37305

# What is the winning Elf's score?
msg <- "Usage: Rscript zkamvar.R [<players> <rounds>] [-f FILE] [-e]

        Arguments:
        <players>  an integer specifying the number of players in the game
        <rounds>   an integer specifying the number of points the last marble
                   is worth
        -f FILE    an optional input file that specifies the number of players
                   and rounds in the form of:
                     '<n> players; last marble is worth <m> points'
        -e         Run an example game

        Examples:

        Rscript zkamvar.R 10 1618 # high score: 8317 points to player 7
        Rscript zkamvar.R 13 7999 # high score: 146373 points
        Rscript zkamvar.R 17 1104 # high score: 2764 points
        Rscript zkamvar.R 21 6111 # high score: 54718 points
        Rscript zkamvar.R 30 5807 # high score: 37350 points
        Rscript zkamvar.R -f zkamvar-input.txt
        Rscript zkamvar.R 476 7143100 # note: this uses a lot of memory
"
args <- commandArgs(trailingOnly = TRUE)
ex <- FALSE
if (length(args) == 1 && args == "-e") {
  ex <- TRUE
} else if (length(args) == 2) {
  if (args[1] == "-f") {
    suppressWarnings({
      params <- scan(args[2], what = character())
      params <- as.integer(params)
      params <- params[!is.na(params)]
    })
  } else if (!anyNA(args <- as.integer(args))) {
    params <- args
  } else {
    stop(msg)
  }
} else {
  stop(msg)
}
insert <- function(marble = 0, circle) {
  e <- new.env(hash = TRUE, parent = circle, size = 3)
  e$marble <- marble
  i <- sprintf("m%d", marble)
  if (marble == 0) {
    e$left      <- e
    e$right     <- e
  } else {
    left <- circle[[circle$current]]$right
    # (left) -- marble i
    e$left <- left
    # marble i -> (right)
    e$right <- left$right
    # left -- (marble i) 
    left$right$left <- e
    # (marble i) -- right
    left$right <- e
  }
  circle[[i]] <- e
  circle$current <- i
}

delete <- function(marble = 23, circle) {
  i <- sprintf("m%d", marble)
  m <- circle[[i]]
  r <- m$right
  l <- m$left
  r$left  <- l
  l$right <- r
  m$right <- NULL
  m$left  <- NULL
}

show_circle <- function(circle) {
  zero <- circle[["m0"]]
  right <- zero$right
  cat(zero$marble, " ")
  while (!identical(zero, right)) {
    if (sprintf("m%d", right$marble) == circle$current) {
      cat(sprintf("(%d) ", right$marble))
    } else {
      cat(right$marble, " ")
    }
    right <- right$right
  }
  cat("\n")
}

init_game <- function(players = 9, rounds = 25) {
  circle         <- new.env(hash   = TRUE, 
                            parent = globalenv(), 
                            size   = rounds + 2)
  circle$scores  <- rep(0.0, players)
  circle$current <- NA
  circle
}

play_game <- function(players = 9, rounds = 25, verbose = FALSE) {
  g <- init_game(players, rounds)
  for (i in seq(0L, rounds)) {
    if (i > 0 && i %% 23 == 0) {
      current <- g[[g$current]]
      for (j in seq(6L)) {
        current <- current$left
      }
      g$current <- sprintf("m%d", current$marble)
      the_player <- i %% players
      g$scores[the_player] <- i + current$left$marble + g$scores[the_player]
      delete(current$left$marble, g)
    } else {
      insert(i, g)
    }
    if (i %% 1e5 == 0) {
      cat(as.character(Sys.time()), i, "\n")
    }
    if (verbose) {
      show_circle(g)
    }
  }
  g
}

play_and_score <- function(players = 9, rounds = 25, verbose = FALSE) {
  tim <- system.time(g <- play_game(players, rounds, verbose))
  highscore <- max(g$scores)
  player    <- which.max(g$scores)
  sprintf("
          %d players; last marble is worth %d points:
          
          High score: %f points
          Player    : %d
          Time      : %.2f seconds
          
",
          players,
          rounds,
          as.numeric(highscore),
          player,
          tim[["elapsed"]])
}
if (ex) {
  cat("Running example ...\n")
  cat(play_and_score(verbose = TRUE))
} else {
  cat(play_and_score(players = params[1], rounds = params[2]))
}
