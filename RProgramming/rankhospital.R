#
# rank is a function that given a 2 letter abreviation of the
# state, an outcome ("heart attack", "heart failure", and "pneumonia"),
# and a rank number (or "best" or "worst), find the hospital in that
# state with the rank, best or worst outcome.
#
# The data is in the file "outcome-of-care-measures.csv".
#

rankhospital <- function(state, outcome, num = "best") {
  
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
  
  # df has the non-NA rows for this state. if worst, then return the last row
  if(num == "best") {
    num <- 1
  }
  else if(num == "worst") {
    num <- nrow(df)
  }
  
  # Order the column with the condition and the Hospital Name
  df <- df[order(df[,cond_col_num], df$Hospital.Name),]

  # find the num'th row and return the hospital name
  df$Hospital.Name[num]
}

# run tests

# rankhospital("TX", "heart failure", 3)
# rankhospital("TX", "heart failure", 4)
# rankhospital("IN", "pneumonia", "best")
# rankhospital("MD", "heart attack", "worst")
# rankhospital("MN", "heart attack", 5000)
# rankhospital("CA", "pneumonia", 1)
# rankhospital("CA", "pneumonia", "best")


# clean up
# rm(list=ls())