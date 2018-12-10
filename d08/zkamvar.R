#!/urs/bin/env Rscript

input <- "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
bc    <- as.integer(strsplit(input, " ")[[1]])
bcf   <- scan("zkamvar-input.txt", what = integer())
bc

next_node <- function(index, dat) {
  has_child <- dat[index] > 0L
  if (has_child) {
    index + 2L
  } else {
    index + dat[index + 1L] + 2L
  }
}

find_nodes <- function(dat) {
  i <- 1L -> count
  res <- vector(mode = "list", length = length(dat))
  res[[i]]$nchildren <- dat[i]
  res[[i]]$children  <- NULL
  res[[i]]$parent    <- NULL
  res[[i]]$meta      <- NULL
  while(i < length(dat)) {
    count <- count + 1L
    i <- next_node(i, dat)
    res[[i]]$nchildren <- dat[i]
    res[[i]]$children  <- if (dat[i] == 0) NULL else dat[i]
    res[[i]]$parent    <- NULL
  }
  res[!is.na(res)]
}



the_nodes <- find_nodes(bc)
bc[the_nodes]
fnodes <- find_nodes(bcf)
length(fnodes)
bcf[fnodes][1:100]
tail(bcf[fnodes])
bcf[last_node(bcf)]
tail(fnodes)
