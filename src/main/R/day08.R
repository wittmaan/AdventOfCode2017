
## --- Day 8: I Heard You Like Registers ---

library(testthat)
library(dplyr)
library(data.table)

dat <- read.table("../resources/day8input.txt", header = FALSE, sep = "\n", stringsAsFactors = FALSE)$V1 %>%
  strsplit(" ") %>%
  list() %>%
  rbindlist() %>%
  transpose() 

setnames(dat, old = colnames(dat), new = c("variable", "operation", "value", "condition", "conditionVariable", "operator", "reference"))
vars <- data.table(variable=character(), value=numeric())


update <- function(x, variableList) {
  ## check if conditionVariable already exists
  if (nrow(variableList[variable == x[1, conditionVariable]]) == 0) {
    variableList <- rbind(variableList, data.table(variable=x[1,conditionVariable], value=0))
  }
  
  ## check if variable already exists
  if (nrow(variableList[variable == x[1, variable]]) == 0) {
    variableList <- rbind(variableList, data.table(variable=x[1,variable], value=0))
  }
  
  cond <- paste("value", x[1,operator],x[1,reference])
  if (nrow(variableList[variable == x[1, conditionVariable] & eval(parse(text = cond))]) != 0) {
    if (x[1, operation] == "inc") {
      variableList[variable == x[1, variable], value := value + as.numeric(x[1, value])]  
    } else { ## dec
      variableList[variable == x[1, variable], value := value - as.numeric(x[1, value])]  
    }
  }

  return(variableList)
}

maxValue <- NULL
for (i in 1:nrow(dat)) {
  vars <- update(dat[i], vars)
  
  if (is.null(maxValue) || vars[, max(value)] > maxValue[, value]) {
    maxValue <- vars[value == max(value)]
  }
}

expect_equal(vars[, max(value)], 6061)
expect_equal(maxValue[,value], 6696)
