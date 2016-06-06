#
# rankall is a function that given an outcome ("heart attack", "heart failure",
# and "pneumonia"), and a rank number (or "best" or "worst), return a data frame
# with the first column is the hospital from each state with that rank for the
# given outcome.
#
# The data is in the file "outcome-of-care-measures.csv".
#

rankall <- function(outcome, num = "best") {
  
  cond <- c("heart attack", "heart failure", "pneumonia")

  ## Read outcome data
  df <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  
  ## Check that outcome is valid
  if (!(outcome %in% cond)) {
    stop("invalid outcome")
  }
  
  # create data.frame to convert ugly col names to numbers
  oc_rate <- data.frame(cond, c(11, 17, 23))
  colnames(oc_rate) <- c("cond", "condcol")
  
  # find column of the outcome in question and convert col values to numbers
  cond_col_num <- oc_rate$condcol[oc_rate$cond == outcome]
  df[,cond_col_num] <- as.numeric(df[,cond_col_num])
  
  state <- levels(factor(df$State))
  hospital = as.character()

  # if best, get the first row
  if (num == "best") {
    num <- 1
  }
  
  for (f in state) {
    # find all rows in the state ...
    sf <- df[(df$State == f),]
    
    # find all rows with non-NA data ...
    sf <- sf[!is.na(sf[,cond_col_num]),]
    
    # Order the column with the condition and the Hospital Name
    sf <- sf[order(sf[,cond_col_num], sf$Hospital.Name),]
    
    # find the num'th row and return the hospital name
    hospital[state == f] <- sf$Hospital.Name[if(num=="worst") { nrow(sf) } else {num}]
  }
  data.frame(hospital, state)
}

# run tests

# head(rankall("heart attack", 20), 10)
# tail(rankall("pneumonia", "worst"), 3)
# tail(rankall("heart failure"), 10)
# tail(rankall("heart failure", "best"), 10)
# tail(rankall("heart failure", 4000), 10)


# clean up
# rm(list=ls())