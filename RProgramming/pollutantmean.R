pollutantmean <- function(directory, pollutant, id = 1:332) {
        ## directory is a character vector of length 1 indicating the location
        ## of the CSV files
        
        ## pollutant is a character vector of length 1 indicating the name
        ## of the pollutant for which we will calculate the mean; either
        ## "sulfate" or "nitrate".
        
        ## id is an integer vector indicating the monitor ID numbers to be used.
        
        ## returns the mean of the pollutant across all monitors listed in the
        ## id vector (ignoring NA values). The result is NOT rounded.
        
        s <- 0;
        num <- 0;

        for (i in id) {
                datafile <- sprintf("%s/%03d.csv", directory, i)
                
                polldata <- read.csv(datafile)
                
                s <- s + sum(polldata[,pollutant], na.rm = TRUE)
                num <- num + sum(!is.na(polldata[,pollutant]))
        }

        s / num
}