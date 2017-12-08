
## --- Day 6: Memory Reallocation ---

library(testthat)
library(digest)

sampleInput <- "0 2 7 0"
memorySample <- as.numeric(unlist(strsplit(sampleInput, split = "+\\s")))


getNextBank <- function(actualBank, nBanks) {
  ifelse((actualBank + 1) > nBanks, 1, actualBank + 1)
}


update <- function(x) {
  actualBank <- which.max(x)
  actualBlocks <- x[actualBank]
  x[actualBank] <- x[actualBank] - actualBlocks
  nBanks <- length(x)

  actualBank <- getNextBank(actualBank, nBanks)
  while (actualBlocks > 0) {
    x[actualBank] <- x[actualBank] + 1
    actualBlocks <- actualBlocks - 1
    
    actualBank <- getNextBank(actualBank, nBanks)
  }
  
  return(x)
}


detectNcircles <- function(x) {
  v <- c(digest(x))
  while (TRUE) {
    print(x)
    x <- update(x)
    v <- c(v, digest(x))
    
    if (any(duplicated(v))) {
      break
    }
  }
  indIdentical <- which(v == v[duplicated(v)])
  
  return (list(res1=length(v) - 1, res2=indIdentical[2] - indIdentical[1]))
}

obj <- detectNcircles(memorySample)
expect_equal(obj$res1, 5)
expect_equal(obj$res2, 4)


input <- "5	1	10	0	1	7	13	14	3	12	8	10	7	12	0	6"
memory <- as.numeric(unlist(strsplit(input, split = "+\\s")))

detectNcircles(memory)



