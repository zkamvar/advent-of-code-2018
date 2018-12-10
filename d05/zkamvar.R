#!/usr/bin/env Rscript
--- Day 5: Alchemical Reduction ---

# You've managed to sneak in to the prototype suit manufacturing lab. The Elves
# are making decent progress, but are still struggling with the suit's size
# reduction capabilities.

# While the very latest in 1518 alchemical technology might have solved their
# problem eventually, you can do better. You scan the chemical composition of
# the suit's material and discover that it is formed by extremely long polymers
# (one of which is available as your puzzle input).

# The polymer is formed by smaller units which, when triggered, react with each
# other such that two adjacent units of the same type and opposite polarity are
# destroyed. Units' types are represented by letters; units' polarity is
# represented by capitalization. For instance, r and R are units with the same
# type but opposite polarity, whereas r and s are entirely different types and
# do not react.

# For example:

#  -  In aA, a and A react, leaving nothing behind.
#  -  In abBA, bB destroys itself, leaving aA. As above, this then destroys
#     itself, leaving nothing.
#  -  In abAB, no two adjacent units are of the same type, and so nothing
#     happens.
#  -  In aabAAB, even though aa and AA are of the same type, their polarities
#     match, and so nothing happens.

# Now, consider a larger example, dabAcCaCBAcCcaDA:

# dabAcCaCBAcCcaDA  The first 'cC' is removed.
# dabAaCBAcCcaDA    This creates 'Aa', which is removed.
# dabCBAcCcaDA      Either 'cC' or 'Cc' are removed (the result is the same).
# dabCBAcaDA        No further actions can be taken.

# After all possible reactions, the resulting polymer contains 10 units.

# How many units remain after fully reacting the polymer you scanned? (Note: in
# this puzzle and others, the input is large; if you copy/paste your input,
# make sure you get the whole thing.)


if (!require("docopt")) install.packages("docopt", repos = "https://cran.r-project.org")

"
Usage: zkamvar.R [-h] [-f FILE]

  -h --help       show this message and exit
  -f --file FILE  specify an input FILE (default: zkamvar-input.txt)
" -> doc

args <- docopt::docopt(doc, help = TRUE)
use_example <- is.null(args$file)
if (use_example) {
  message(doc)
  message("Using example input...")
  input <- "dabAcCaCBAcCcaDAAAAAA"
  message("dabAcCaCBAcCcaDAAAAAAA")
} else {
  input <- readLines(args$file)
}


react <- function(word, regex = "(\\w)(=?\\1)") {
  gsub(regex, "", word, ignore.case = TRUE, perl = TRUE)
}

any_reactions <- function(word, regex = "(\\w)(=?\\1)") {
  grepl(regex, word, perl = TRUE, ignore.case = TRUE)
}

needs_mask <- function(word, regex = "(\\w)(=?\\1)") {
  grepl(regex, word, perl = TRUE)
}

mask <- function(word, regex = "(\\w)(=?\\1)") {
  while(needs_mask(word)) {
    word <- gsub(regex, "-\\1", word, perl = TRUE)
  }
  word
}

needs_unmask <- function(word) grepl("[-]", word)

unmask <- function(word, regex = "(-)(\\w)") {
  while(needs_unmask(word)) {
    word <- gsub(regex, "\\2\\2", word, perl = TRUE)
  }
  word
}

keep_going <- TRUE
while (keep_going) {
  message(input)
  input      <- mask(input)
  message(input)
  input      <- react(input)
  message(input)
  input      <- unmask(input)
  keep_going <- any_reactions(input)
  message("-------------------------------------------------------------")
}

if (use_example) stopifnot(input == "dabCBAcaDAAAAAA")
message(sprintf("
The final input is:\n%s\nWhich is %d units.
", 
input, nchar(input)))
