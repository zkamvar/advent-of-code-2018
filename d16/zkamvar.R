# --- Day 16: Chronal Classification ---

# As you see the Elves defend their hot chocolate successfully, you go back to
# falling through time. This is going to become a problem.

# If you're ever going to return to your own time, you need to understand how
# this device on your wrist works. You have a little while before you reach
# your next destination, and with a bit of trial and error, you manage to pull
# up a programming manual on the device's tiny screen.

# According to the manual, the device has four registers (numbered 0 through 3)
# that can be manipulated by instructions containing one of 16 opcodes. The
# registers start with the value 0.

# Every instruction consists of four values: an opcode, two inputs (named A and
# B), and an output (named C), in that order. The opcode specifies the behavior
# of the instruction and how the inputs are interpreted. The output, C, is
# always treated as a register.

# In the opcode descriptions below, if something says "value A", it means to
# take the number given as A literally. (This is also called an "immediate"
# value.) If something says "register A", it means to use the number given as A
# to read from (or write to) the register with that number. So, if the opcode
# addi adds register A and value B, storing the result in register C, and the
# instruction addi 0 7 3 is encountered, it would add 7 to the value contained
# by register 0 and store the sum in register 3, never modifying registers 0,
# 1, or 2 in the process.

# Many opcodes are similar except for how they interpret their arguments. The
# opcodes fall into seven general categories:

# Addition:

# - addr (add register) stores into register C the result of adding register A
#   and register B.
# - addi (add immediate) stores into register C the result of adding register A
#   and value B.

# Multiplication:

# - mulr (multiply register) stores into register C the result of multiplying
#   register A and register B.
# - muli (multiply immediate) stores into register C the result of multiplying
#   register A and value B.

# Bitwise AND:

# - banr (bitwise AND register) stores into register C the result of the
#   bitwise AND of register A and register B.
# - bani (bitwise AND immediate) stores into register C the result of the
#   bitwise AND of register A and value B.

# Bitwise OR:

# - borr (bitwise OR register) stores into register C the result of the bitwise
#   OR of register A and register B.
# - bori (bitwise OR immediate) stores into register C the result of the
#   bitwise OR of register A and value B.

# Assignment:

# - setr (set register) copies the contents of register A into register C.
#   (Input B is ignored.)
# - seti (set immediate) stores value A into register C. (Input B is ignored.)

# Greater-than testing:

# - gtir (greater-than immediate/register) sets register C to 1 if value A is
#   greater than register B. Otherwise, register C is set to 0.
# - gtri (greater-than register/immediate) sets register C to 1 if register A
#   is greater than value B. Otherwise, register C is set to 0.
# - gtrr (greater-than register/register) sets register C to 1 if register A is
#   greater than register B. Otherwise, register C is set to 0.

# Equality testing:

# - eqir (equal immediate/register) sets register C to 1 if value A is equal to
#   register B. Otherwise, register C is set to 0.
# - eqri (equal register/immediate) sets register C to 1 if register A is equal
#   to value B. Otherwise, register C is set to 0.
# - eqrr (equal register/register) sets register C to 1 if register A is equal
#   to register B. Otherwise, register C is set to 0.

# Unfortunately, while the manual gives the name of each opcode, it doesn't
# seem to indicate the number. However, you can monitor the CPU to see the
# contents of the registers before and after instructions are executed to try
# to work them out. Each opcode has a number from 0 through 15, but the manual
# doesn't say which is which. For example, suppose you capture the following
# sample:

# Before: [3, 2, 1, 1]
# 9 2 1 2
# After:  [3, 2, 2, 1]

# This sample shows the effect of the instruction 9 2 1 2 on the registers.
# Before the instruction is executed, register 0 has value 3, register 1 has
# value 2, and registers 2 and 3 have value 1. After the instruction is
# executed, register 2's value becomes 2.

# The instruction itself, 9 2 1 2, means that opcode 9 was executed with A=2,
# B=1, and C=2. Opcode 9 could be any of the 16 opcodes listed above, but only
# three of them behave in a way that would cause the result shown in the
# sample:

# - Opcode 9 could be mulr: register 2 (which has a value of 1) times register
#   1 (which has a value of 2) produces 2, which matches the value stored in the
#   output register, register 2.
# - Opcode 9 could be addi: register 2 (which has a value of 1) plus value 1
#   produces 2, which matches the value stored in the output register, register
#   2.
# - Opcode 9 could be seti: value 2 matches the value stored in the output
#   register, register 2; the number given for B is irrelevant.

# None of the other opcodes produce the result captured in the sample. Because
# of this, the sample above behaves like three opcodes.

# You collect many of these samples (the first section of your puzzle input).
# The manual also includes a small test program (the second section of your
# puzzle input) - you can ignore it for now.

# Ignoring the opcode numbers, how many samples in your puzzle input behave
# like three or more opcodes?

get_instructions <- function(instructions) {
  A <- instructions[2]
  B <- instructions[3]
  C <- instructions[4]
  list(A = A, B = B, C = C)
}
funs <- list(
  addr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- registers[i$A + 1] + registers[i$B + 1]
    registers  
  },
  addi = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- registers[i$A + 1] + i$B
    registers  
  },
  mulr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- registers[i$A + 1] * registers[i$B + 1]
    registers  

  },
  muli = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- registers[i$A + 1] * i$B
    registers  
  },
  banr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- bitwAnd(registers[i$A + 1], registers[i$B + 1])
    registers  
  },
  bani = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- bitwAnd(registers[i$A + 1], i$B)
    registers  
  },
  borr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- bitwOr(registers[i$A + 1], registers[i$B + 1])
    registers  
  },
  bori = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- bitwOr(registers[i$A + 1], i$B)
    registers  
  },
  setr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- registers[i$A + 1]
    registers  
  },
  seti = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- i$A 
    registers  
  },
  gtir = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- as.integer(i$A > registers[i$B + 1])
    registers  
  },
  gtri = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- as.integer(registers[i$A + 1] > i$B)
    registers  
  },
  gtrr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- as.integer(registers[i$A + 1] > registers[i$B + 1])
    registers  
  },
  eqir = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- as.integer(i$A == registers[i$B + 1])
    registers  
  },
  eqri = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- as.integer(registers[i$A + 1] == i$B)
    registers  
  },
  eqrr = function(instructions, registers) {
    i <- get_instructions(instructions)
    registers[i$C + 1] <- as.integer(registers[i$A + 1] == registers[i$B + 1])
    registers  
  }
)
convert_to_matrix <- function(code) {
  res <- strsplit(code, " ")
  t(vapply(res, as.integer, integer(4)))
}

read_input <- function(dat = "zkamvar-input.txt") {
  ip <- readLines(dat)
  splitline <- max(which(ip == ""))
  p1 <- ip[seq(splitline)]
  p2 <- ip[seq(from = splitline + 1, to = length(ip))]
  p1 <- p1[p1 != ""]
  pat <- "[A-z]+[: ]{2,3}\\[(\\d{1,2}), (\\d{1,2}), (\\d{1,2}), (\\d{1,2})\\]"
  rplc <- "\\1 \\2 \\3 \\4"
  before <- trimws(gsub(pat, rplc, p1[grepl("Before", p1)]))
  after  <- trimws(gsub(pat, rplc, p1[grepl("After", p1)]))
  instruction <- p1[grepl("^[0-9]", p1)]
  mat <- array(0L, c(length(before), 4L, 3L),
               dimnames = list(NULL, NULL, c("before", "instruction", "after")))

  mat[, , "before"]      <- convert_to_matrix(before)
  mat[, , "instruction"] <- convert_to_matrix(instruction)
  mat[, , "after"]       <- convert_to_matrix(after)
  list(p1 = mat,
       p2 = convert_to_matrix(p2)
  )
}
run_tests <- function(instruction, before = NULL) {
  if (is.array(instruction)) {
    if (length(dim(instruction)) == 3) {
      before <- instruction[, , "before"]
      instruction <- instruction[ , , "instruction"]
    } else {
      before <- instruction[, "before"]
      instruction <- instruction[ , "instruction"]
    }
  }
  vapply(funs, do.call, FUN.VALUE = double(4), 
         list(instruction, before))
}
cromulence <- function(a, b) {
  all(a == b)
}
multi_codes <- function(instruction, before = NULL, after = NULL) {
  if (is.array(instruction)) {
    if (length(dim(instruction)) == 3) {
      before <- instruction[, , "before"]
      after  <- instruction[, , "after"]
      instruction <- instruction[ , , "instruction"]
    } else {
      before <- instruction[, "before"]
      after  <- instruction[, "after"]
      instruction <- instruction[ , "instruction"]
    }
  }
  tests <- run_tests(instruction, before)
  res <- apply(tests, MARGIN = 2, FUN = cromulence, after) 
  res
}
inp <- read_input()
test <- list(before = c(3, 2, 1, 1),
             after  = c(3, 2, 2, 1),
             instruction = c(9, 2, 1, 2)
            )
multi_codes(test$inst, test$before, test$after)
run_tests(inp$p1[1, , ])
multi_codes(inp$p1[1, , ])
test_array <- function(i) apply(i, MARGIN = 1, FUN = multi_codes)
# First question: how many samples contain 3 or more opcodes?
ta <- test_array(inp$p1)
sum(colSums(ta) > 2)


# Second question: determine order and then answer what value is contained in
# register 0 after execution of the program.
test_placement <- integer(length(funs))
names(test_placement) <- names(funs)
ip  <- inp$p1
while(any(test_placement == 0)) {
  stopifnot(all(rownames(ta) == names(test_placement[test_placement == 0])))
  cta        <- colSums(ta)
  definites  <- which(cta == 1)
  the_tests  <- ta[, definites, drop = FALSE]
  test_idx   <- rownames(ta)[apply(the_tests, 2, which.max)]
  test_idx
  for (j in unique(test_idx)) {
    z <- ip[definites[test_idx == j], 1, "instruction"]
    stopifnot(length(unique(z)) == 1)
    test_placement[j] <- unique(z) + 1L
  }
  success <- test_placement[test_placement > 0] - 1L
  incomplete <- !rownames(ta) %in% names(success)
  to_remove <- ip[, 1, "instruction"] %in% success
  ip <- ip[!to_remove, , , drop = FALSE]
  ta <- ta[, !to_remove, drop = FALSE]
  ta <- ta[incomplete, , drop = FALSE]
  print(test_placement)
}
funs <- funs[order(test_placement)]
registers <- c(0, 0, 0, 0)
for (i in seq(nrow(inp$p2))) {
  instruct  <- inp$p2[i, ]
  fun <- instruct[1] + 1L
  registers <- funs[[fun]](instruct, registers)
}
registers
