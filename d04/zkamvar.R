#!/usr/bin/env Rscript

# Strategy 1: Find the guard that has the most minutes asleep. What minute does
# that guard spend asleep the most?

# In the example above, Guard #10 spent the most minutes asleep, a total of 50
# minutes (20+25+5), while Guard #99 only slept for a total of 30 minutes
# (10+10+10). Guard #10 was asleep most during minute 24 (on two days, whereas
# any other minute the guard was asleep was only seen on one day).

# While this example listed the entries in chronological order, your entries
# are in the order you found them. You'll need to organize them before they can
# be analyzed.

# What is the ID of the guard you chose multiplied by the minute you chose? (In
# the above example, the answer would be 10 * 24 = 240.)


if (!require("docopt")) install.packages("docopt", repos = "https://cran.r-project.org")

"
Usage: zkamvar.R [-h] [-f FILE]

  -h --help       show this message and exit
  -f --file FILE  specify an input FILE (default: zkamvar-input.txt)
" -> doc

args <- docopt::docopt(doc, help = TRUE)

if (is.null(args$file)) {
  message(doc)
  message("Running example...")
  times <- c( 
             "[1518-11-01 00:00] Guard #10 begins shift",
             "[1518-11-01 00:05] falls asleep",
             "[1518-11-01 00:25] wakes up",
             "[1518-11-01 00:30] falls asleep",
             "[1518-11-01 00:55] wakes up",
             "[1518-11-01 23:58] Guard #99 begins shift",
             "[1518-11-02 00:40] falls asleep",
             "[1518-11-02 00:50] wakes up",
             "[1518-11-03 00:05] Guard #10 begins shift",
             "[1518-11-03 00:24] falls asleep",
             "[1518-11-03 00:29] wakes up",
             "[1518-11-04 00:02] Guard #99 begins shift",
             "[1518-11-04 00:36] falls asleep",
             "[1518-11-04 00:46] wakes up",
             "[1518-11-05 00:03] Guard #99 begins shift",
             "[1518-11-05 00:45] falls asleep",
             "[1518-11-05 00:55] wakes up"
            )
  otime <- times
  times <- sample(times)
} else {
  times <- readLines(args$file)
}

get_time <- function(x) {
  re <- "^\\[([-: 0-9]{16})\\].+$"
  fmt <- "%Y-%m-%d %H:%M"
  tim <- sub(re, "\\1", x)
  strptime(tim, fmt, tz = "GMT")
}

month <- function(x) {
  x$mon + 1L
}

day <- function(x) {
  x$mday
}

hour <- function(x){
  x$hour
}

minute <- function(x) {
  x$min
}

mark_guard <- function(shifts) {
  re <- "^.{18} Guard \\#(\\d+) begins shift"
  suppressWarnings(nums <- as.integer(sub(re, "\\1", shifts)))
  grds <- !is.na(nums)
  dgrd <- diff(which(grds))
  rep(nums[grds], c(dgrd, length(nums) - sum(dgrd)))
}
tim  <- get_time(times)
timo <- order(tim)
times <- times[timo]
tim   <- tim[timo]
sched <- data.frame(record = times,
                    time   = tim,
                    day    = paste0(month(tim), "-", day(tim)),
                    hour   = hour(tim),
                    minute = minute(tim),
                    ID     = mark_guard(times),
                    sleep  = grepl("sleep", times),
                    begin  = grepl("begin", times),
                    stringsAsFactors = FALSE
                   )

sw <- sched[!sched$begin, ]
mat <- matrix(0L, nrow = length(unique(sched$ID)), ncol = 60,
              dimnames = list(unique(sched$ID), 0:59)
              )
for (i in seq(nrow(sw))) {
 if (sw$sleep[i]) {
   guard   <- as.character(sw$ID[i])
   minutes <- sw$minute[i]:(sw$minute[i + 1] - 1) + 1
   mat[guard, minutes] <- mat[guard, minutes] + 1
  }
}

sleepy_guards <- rowSums(mat)
sleepy_guard  <- rownames(mat)[which.max(sleepy_guards)]
sleepy_time   <- names(which.max(mat[sleepy_guard, ]))
res1 <- as.integer(sleepy_guard) * as.integer(sleepy_time)

the_max <- mat[which.max(mat)]
maxes <- apply(mat, 1, function(i){ m <- i == the_max; if (any(m)) which(m) else 0})
res <- maxes[maxes > 0]
res2 <- as.integer(names(res)) * (as.integer(res) - 1L)

msg <- sprintf("
        Guard %s was asleep for %d minutes, of which, minute %s was the
        sleepiest time for them. Multiplying their ID by the minute gives us: %d

        The most consistent guard was %s found asleep %d times at minute %d.
        Multiplying their ID by the minute gives us: %d 
        \n", 
  sleepy_guard,
  sleepy_guards[sleepy_guard],
  sleepy_time,
  res1,
  names(res),
  max(mat),
  res - 1L,
  res2
)

cat(msg)

