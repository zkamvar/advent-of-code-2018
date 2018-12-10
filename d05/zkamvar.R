#!/usr/bin/env Rscript


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
