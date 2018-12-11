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

get_row <- function(x, marble) {
  x[, 1] == marble
}
get_position <- function(x, marble) {
  x[get_row(x, marble), 2]
}
max_position <- function(x) {
  max(x[, 2], na.rm = TRUE)
}
next_position <- function(x, current) {
  last_position <- get_position(x, current - 1L)
  last_position <- if (is.na(last_position)) 0L else last_position
  if (max_position(x) == last_position) {
    return(2L)
  } else if (current %% 23 == 0) {
    return(last_position - 7L)
  } else if (last_position == 0L) {
    return(get_position(x, current - 2L) - 4L)
  } else {
    return(last_position + 2L)
  }
}
fill_position <- function(x, current) {
  pos     <- next_position(x, current)
  if (pos < 1) {
    pos <- max_position(x) + pos
  }
  the_row <- get_row(x, current)
  nona    <- !is.na(x[, 2])
  if (current %% 23 > 0) {
    to_replace <- x[, 2] >= pos & nona
    x[the_row, 2]    <- pos
    x[to_replace, 2] <- x[to_replace, 2] + 1L
  } else {
    to_replace <- x[, 2] > pos & nona
    the_score  <- x[, 2] == pos & nona
    x[the_row, "score"] <- current + x[the_score, 1]
    x[to_replace, 2]    <- x[to_replace, 2] - 1L 
    x[the_score, 2] <- NA
    x[the_row, 2]   <- NA
  }
  x
}

get_order <- function(x) {
  order(x[x[, 2] > 0, 2], na.last = NA)
}

for_mat <- function(x) {
  paste(format(x[get_order(x), 1]), collapse = " ")
}

play_game <- function(last_marble = 25) {
  mat <- matrix(0L, nrow = last_marble + 1L, ncol = 3,
                dimnames = list(NULL, c("marble", "position", "score"))
               )
  mat[, 1] <- seq(from = 0, to = last_marble)
  mat[1:4, 2] <- c(1, 3, 2, 4) 
  for (i in seq(from = 4, to = last_marble)) {

    x <- try(mat <- fill_position(mat, i))
    if (inherits(x, "try-error")) {
      stop(sprintf("fill_position(play_game(%d), %d)", i - 1, i))
    }
  }
  mat
}

find_winners <- function(game, n) {
  winners <- which(game[, "score"] > 0)
  wlist   <- split(game[winners, "score"], winners %% n)
  max(vapply(wlist, sum, numeric(1)))
}
# -   10 players; last marble is worth 1618 points: high score is 8317
mat <- play_game(1618)
find_winners(mat, 10)
# -   13 players; last marble is worth 7999 points: high score is 146373
mat <- play_game(7999)
find_winners(mat, 13)
# -   17 players; last marble is worth 1104 points: high score is 2764
mat <- play_game(1104)
find_winners(mat, 17)
# -   21 players; last marble is worth 6111 points: high score is 54718
mat <- play_game(6111)
find_winners(mat, 21)
# -   30 players; last marble is worth 5807 points: high score is 37305
mat <- play_game(5807)
find_winners(mat, 30)

# - Puzzle input: 476 players; last marble is worth 71431 points: high score is 384205
mat <- play_game(71431)
find_winners(mat, 476)
