#!/usr/bin/env Rscript
msg <- "
  Rscript zkamvar.R zkamvar-input.txt
"

the_args  <- commandArgs(trailingOnly = TRUE)
lthe_args <- length(the_args)
the_args  <- gsub(",", "", the_args)

if (lthe_args != 1) {
  stop(msg)
}

ids         <- sort(scan(the_args[1], what = character()))
# Part A
char_counts <- lapply(strsplit(ids, ""), table)
check_chars <- function(i) c(two = any(i == 2), three = any(i == 3))
twos_and_threes <- vapply(char_counts, check_chars, logical(2))
the_sums <- rowSums(twos_and_threes)
res <- prod(the_sums)

# Part B
dists <- adist(ids, costs = c(i = 10, d = 10, s = 1))
posns <- apply(dists, 1L, function(i) any(i == 1L))
my_ids <- ids[posns]
trans <- attr(adist(my_ids, counts = TRUE), "trafos")[1, 2]
the_position <- grep("S", strsplit(trans, "")[[1]])
the_change   <- my_ids[1]
substr(the_change, the_position, the_position) <- "-"
n <- nchar(my_ids[1])
common_letters <- paste0(substr(my_ids[1], 1, the_position - 1),
                         substr(my_ids[1], the_position + 1, n))

cat(
  sprintf("
          IDs:                      %d
          Two duplicated letters:   %d
          Three duplicated letters: %d
          Checksum:       %d * %d = %d

          ID 1:           %s
                          %s
          ID 2:           %s
          Common letters: %s
\n",
  length(ids),
  the_sums["two"],
  the_sums["three"],
  the_sums["two"],
  the_sums["three"],
  res,
  my_ids[1],
  the_change,
  my_ids[2],
  common_letters
  )
)
