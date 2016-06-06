#
# best is a function that given a 2 letter abreviation of the
# state, and an an outcome ("heart attack", "heart failure",
# and "pneumonia"), find the hospital in that state with the
# best (lowest) outcome. The data is in the file
# "outcome-of-care-measures.csv".

best <- function(state, outcome) {
  
  cond <- c("heart attack", "heart failure", "pneumonia")

  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that state and outcome are valid
  if (!(state %in% df$State)) {
    stop("invalid state")
  }

  if (!(outcome %in% cond)) {
    stop("invalid outcome")
  }
  
  # create data.frame to convert ugly col names to numbers
  oc_rate <- data.frame(cond, c(11, 17, 23))
  colnames(oc_rate) <- c("cond", "condcol")
  
  # find column of the outcome in question and convert col values to numbers
  cond_col_num <- oc_rate$condcol[oc_rate$cond == outcome]
  df[,cond_col_num] <- as.numeric(df[,cond_col_num])
  
  # find all rows in the state ...
  df <- df[(df$State == state),]
  
  # find all rows with non-NA data ...
  df <- df[!is.na(df[,cond_col_num]),]
  
  # find all rows with the minimum score of the outcome
  min_score <- min(df[,cond_col_num])
  df <- df[df[,cond_col_num] == min_score,]

  # find the alphabetically first hospital name with this min score
  min(df[,"Hospital.Name"])
}

# run tests

# best("TX", "heart attack")
# best("TX", "heart failure")
# best("MD", "heart attack")
# best("MD", "pneumonia")
# best("BB", "heart attack")
# best("NY", "hert attack")

# clean up
# rm(list=ls())