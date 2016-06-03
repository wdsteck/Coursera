complete <- function(directory, id = 1:332) {
        ## directory is a character vector of length 1 indicating the location
        ## of the CSV files
        
        ## id is an integer vector indicating the monitor ID numbers to be used.
        
        ## returns a data frame of the form:
        ##      id      nobs
        ##      1       117
        ##      2       1041
        ##      ...
        ##
        ## where 'id' is the monitor ID number and 'nobs' is the number
        ## of complete cases
        
        df <- data.frame(id = numeric(), nobs = numeric())
        
        for (i in id) {
                datafile <- sprintf("%s/%03d.csv", directory, i)
                
                polldata <- read.csv(datafile)
                
                df[nrow(df)+1,] <-
                        c(i, sum(complete.cases(polldata[,"nitrate"],
                                                polldata[,"sulfate"])))
        }
        df
}