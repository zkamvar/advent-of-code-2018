#!/usr/bin/env Rscript
#
# --- Day 6: Chronal Coordinates ---
#
# The device on your wrist beeps several times, and once again you feel like
# you're falling.  "Situation critical," the device announces. "Destination
# indeterminate. Chronal interference detected. Please specify new target
# coordinates." The device then produces a list of coordinates (your puzzle
# input). Are they places it thinks are safe or dangerous? It recommends you
# check manual page 729. The Elves did not give you a manual.
#
# If they're dangerous, maybe you can minimize the danger by finding the
# coordinate that gives the largest distance from the other points.  Using only
# the Manhattan distance, determine the area around each coordinate by counting
# the number of integer X,Y locations that are closest to that coordinate (and
# aren't tied in distance to any other coordinate).  Your goal is to find the
# size of the largest area that isn't infinite. For example, consider the
# following list of coordinates:
#
# 1, 1
# 1, 6
# 8, 3
# 3, 4
# 5, 5
# 8, 9
#
# If we name these coordinates A through F, we can draw them on a grid, putting
# 0,0 at the top left:
#
# ..........
# .A........
# ..........
# ........C.
# ...D......
# .....E....
# .B........
# ..........
# ..........
# ........F.
#
# This view is partial - the actual grid extends infinitely in all directions.
# Using the Manhattan distance, each location's closest coordinate can be
# determined, shown here in lowercase:
#
# aaaaa.cccc
# aAaaa.cccc
# aaaddecccc
# aadddeccCc
# ..dDdeeccc
# bb.deEeecc
# bBb.eeee..
# bbb.eeefff
# bbb.eeffff
# bbb.ffffFf
#
# Locations shown as . are equally far from two or more coordinates, and so
# they don't count as being closest to any.  In this example, the areas of
# coordinates A, B, C, and F are infinite - while not shown here, their areas
# extend forever outside the visible grid. However, the areas of coordinates D
# and E are finite: D is closest to 9 locations, and E is closest to 17 (both
# including the coordinate's location itself). Therefore, in this example, the
# size of the largest area is 17.
#
# What is the size of the largest area that isn't infinite?
x <- "1, 1
1, 6
8, 3
3, 4
5, 5
8, 9"
dat <- read.table(text = x, sep = ",")
names(dat) <- c("x", "y")
dat <- data.frame(lapply(dat, function(i) i + 1L))
dat$right  <- dat$x -> dat$left
dat$top    <- dat$y -> dat$bottom
dat$edge   <- FALSE
dat$re <- dat$le <- dat$te <- dat$be <- NA_integer_
rownames(dat) <- LETTERS[seq(nrow(dat))]

show_matrix <- function(coords) {
  dat_dim  <- apply(coords[, c("x", "y")], 2, max)
  mat      <- vector(mode = "character", length = prod(dat_dim))
  dim(mat) <- rev(dat_dim)
  the_names <- rownames(coords)
  for (coord in seq(nrow(coords))) {
    columns <- coords$left[coord]:coords$right[coord]
    rows    <- coords$top[coord]:coords$bottom[coord]
    mat[rows, columns] <- tolower(the_names[coord])
    x <- coords[coord, "x"]
    y <- coords[coord, "y"]
    mat[y, x] <- the_names[coord]
  }
  mat
}
MAX_RIGHT  <- max(dat$x)
MAX_BOTTOM <- max(dat$y)
dat

grow_wide <- function(dat) {
  dat$right <- dat$right + 1L
  dat$left  <- dat$left  - 1L
  dat
}

grow_tall <- function(dat) {
  dat$bottom <- dat$bottom + 1L
  dat$top    <- dat$top    - 1L
  dat
}

grow <- function(dat) {
  grow_tall(grow_wide(dat))
}

check_growth <- function(dat, right = MAX_RIGHT, bottom = MAX_BOTTOM) {
  great_right  <- dat$right  > right  | dat$left < 1
  great_bottom <- dat$bottom > bottom | dat$top  < 1
  dat$edge[great_right | great_bottom] <- TRUE
  dat
}

fix_bounds <- function(dat, right = MAX_RIGHT, bottom = MAX_BOTTOM) {
  dat$right[dat$right >= right] <- right
  dat$bottom[dat$bottom >= bottom] <- bottom
  dat$top[dat$top <= 1] <- 1L
  dat$left[dat$left <= 1] <- 1L
  dat
}

anastomose <- function(dat) {
  right_edge  <- match(dat$left, dat$right)  
  left_edge   <- match(dat$right, dat$left)  
  top_edge    <- match(dat$bottom, dat$top)  
  bottom_edge <- match(dat$top, dat$bottom)  
  barriers <- data.frame(right_edge, left_edge, top_edge, bottom_edge)
  replacena <- function(current, replacement) {
    ifelse(is.na(current), replacement, current)
  }
  cols <- c("re", "le", "te", "be")

  names(barriers) <- cols
  for (i in cols) {
    dat[[i]] <- replacena(dat[[i]], barriers[[i]])
  }
  dat
}

area <- function(dat) {
  d <- dat[!dat$edge, ]
  res <- matrix(1L, nrow = nrow(d), ncol = 2)
  width  <- abs(d$left - d$right)
  height <- abs(d$top - d$bottom)
  width * height
}
dat
odat <- dat
dat  <- odat
for (i in 1:2) {
  dat <- check_growth(grow(dat))
}
dat <- fix_bounds(dat)
area(dat)
mat <- show_matrix(dat)
mat
