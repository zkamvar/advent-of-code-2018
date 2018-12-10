#!/usr/bin/env Rscript
msg <- "
        usage:
          zkamvar.R [-c|--calibrate] <input.txt | ...>

        This script will take in a text file or cli input of signed integers
        representing frequency changes; one change per line. The output will
        determine what the resulting frequency will be assuming a starting
        frequency of zero.
        
        If --calibrate, then the first repeated frequency will be detected by
        repeating the sequence over until a repeat is found.

        Examples:
          Rscript zkamvar.R +3, +3, +4, -2, -4             # result: 4
          Rscript zkamvar.R -c +3, +3, +4, -2, -4          # result: 10
          Rscript zkamvar.R -c -6, +3, +8, +5, -6          # result: 5
          Rscript zkamvar.R --calibrate +7, +7, -2, -7, -4 # result: 14
          Rscript zkamvar.R zkamvar-input.txt
"

args  <- commandArgs(trailingOnly = TRUE)
largs <- length(args)
args <- gsub(",", "", args)
not_a_number  <- is.na(as.integer(args))
not_calibrate <- !args[1] %in% c("-c", "--calibrate")
if (largs < 1) {
  stop(msg)
} else if (largs == 1) {
  if (not_calibrate && not_a_number) {
    transitions <- scan(args[largs], what = integer())
  } else {
    stop(msg)
  } 
} else if (sum(not_a_number) == 0) {
  transitions <- as.integer(args)
} else if (largs == 2) {
  if (not_calibrate){
    stop(msg)
  } 
  if (not_a_number[2]) {
    transitions <- scan(args[largs], what = integer())
  } else {
    transitions <- as.integer(args[largs])
  }
} else if (not_calibrate) {
  stop(msg)
} else {
  transitions <- as.integer(args[-1])
}
calibrate     <- !not_calibrate
n             <- length(transitions)
if (calibrate) {
  no_duplicates   <- TRUE
  start_frequency <- 0
  loop_count      <- 0
  feelin_seen     <- vector(mode = "integer", length = (n * 10) + 1)
  feelin_seen[]   <- NA_integer_
  start_here      <- 1
  while(no_duplicates) {
    loop_count  <- loop_count + 1
    end_here    <- start_here + n
    the_range   <- start_here:end_here
    feelin_seen[the_range] <- cumsum(c(start_frequency, transitions))
    dups <- duplicated(feelin_seen, incomparables = NA_integer_)
    if (any(dups)) {
      first_duplicate <- feelin_seen[dups][1] 
      no_duplicates <- FALSE
    }
    cat(sprintf("Repeat: %s\r", loop_count))
    start_frequency <- feelin_seen[end_here]
    start_here      <- end_here
    if (start_here > length(feelin_seen)) {
      feelin_seen <- c(feelin_seen, rep(NA_integer_, length(feelin_seen)*2))
    }
  }

  cat(sprintf("
              Number of repeats:         %d
              Starting frequency:        0
              First repeated frequency:  %d
  ", 
  loop_count,
  first_duplicate))
} else {

  # Determining final frequency of input
  end_frequency <- cumsum(transitions)[n]
  cat(sprintf("
              Number of frequency changes:    %d
              Starting frequency:             0
              Final frequency:                %d
  ", 
  n,
  end_frequency))
}
