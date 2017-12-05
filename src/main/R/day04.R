
## --- Day 4: High-Entropy Passphrases ---

library(testthat)
library(data.table)
library(dplyr)

dat <- fread("../resources/day4inputSample.txt", header = FALSE, sep = "\n")

isValid <- function(x) {
  splitted <- strsplit(x, split = "+\\s")
  result <- lapply(splitted, function(y) {
    all(summary(as.factor(y)) == 1)
  })
  return (result)
}


dat[, isValid := lapply(.SD, isValid)]
validPassPhrases <- dat[,.(sum(as.integer(isValid)))]$V1

## we expect two valid passphrases
expect_equal(validPassPhrases, 2)


dat <- fread("../resources/day4input.txt", header = FALSE, sep = "\n")
dat[, isValid := lapply(.SD, isValid)]
dat[,.(sum(as.integer(isValid)))]$V1
## 451

## --- Part Two ---
## TODO

