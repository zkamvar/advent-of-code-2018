#!/usr/bin/env Rscript
msg <- "
        usage:
          zkamvar.R <input.txt>

        This script will take in an optional text file specifying claims of a
        bolt of fabric of unknown dimension and find the only claim that does
        not overlap with any other. If no input is given, a test input will be
        used. 

        Examples:
          Rscript zkamvar.R 
          Rscript zkamvar.R zkamvar-input.txt
"

the_args <- commandArgs(trailingOnly = TRUE)

the_test <- c(
              "#1 @ 1,3: 4x4",
              "#2 @ 3,1: 4x4",
              "#3 @ 5,5: 2x2"
)

if (length(the_args) == 0) {
  input <- the_test
} else {
  input <- readLines(the_args[1])
}
df_from_claims <- function(claims) {
  the_variables <- c("ID", "left", "top", "width", "height")
  df <- matrix(NA_integer_, 
               nrow = 5L,
               ncol = length(claims),
               dimnames = list(the_variables, NULL))
  res   <- strsplit(gsub("#", "", claims), "[[:punct:]x]+")
  df[]  <- as.integer(unlist(res, use.names = FALSE))
  df    <- as.data.frame(t(df))

  df$ID   <- as.character(df$ID)
  df$top  <- df$top  + 1L
  df$left <- df$left + 1L
  df
}

get_coords <- function(top, left, height, width) {
  list(the_rows = top:(top + height - 1L),
       the_cols = left:(left + width -1L)
      )
}
create_matrix <- function(df) {
  bottom <- df$top  + df$height - 1L
  right  <- df$left + df$width  - 1L
  mat <- matrix("", 
                nrow = max(bottom) + 1L,
                ncol = max(right)  + 1L
               )
  for (i in seq(nrow(df))) {
    the_rows <- df$top[i]:bottom[i]
    the_cols <- df$left[i]:right[i]
    res      <- mat[the_rows, the_cols]
    mat[the_rows, the_cols] <- paste(res, df$ID[i], sep = ";")
  }
  mat
}
count_clashes <- function(x) {
  sum(grepl("[[:digit:]];[[:digit:]]", x))
}
is_unclashed <- function(i, claims, clashes) {
  id   <- claims$ID[i]
  coo  <- get_coords(top    = claims$top[i],
                     left   = claims$left[i],
                     height = claims$height[i],
                     width  = claims$width[i]
                     )
  mat <- clashes[coo$the_rows, coo$the_cols]
  all(grepl(paste0("^;", id, "$"), mat))
}


claimdf <- df_from_claims(the_test)
clashes <- create_matrix(claimdf)
nclash  <- count_clashes(clashes)
stopifnot(nclash == 4)

claims    <- df_from_claims(input)
clashes   <- create_matrix(claims)
nclashes  <- count_clashes(clashes)
cromulent <- vapply(seq(nrow(claims)), is_unclashed, logical(1), claims, clashes)

cat(
    sprintf("Claims:  %d\nClashes: %d\nGood ID: %s\n",
    nrow(claims),
    nclashes,
    claims$ID[cromulent])
)
