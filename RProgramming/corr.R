corr <- function(directory, threshold = 0) {
        ## directory is a character vector of length 1 indicating the location
        ## of the CSV files
        
        ## threshold is a numeric vector of length 1 indicating the number of
        ## completely observed observations (on all variables) required to
        ## compute teh correlation between nitrate and sulfate; the default is 0
        
        ## returns a numeric vector of unrounded correlations

        cor_vect <- as.numeric()
        
        for (f in dir(directory)) {
                datafile <- sprintf("%s/%s", directory, f)
         
                polldata <- read.csv(datafile)
                
                if (threshold < sum(complete.cases(polldata[,"nitrate"],
                                                polldata[,"sulfate"]))) {
                        cor_vect <- append(cor_vect, cor(polldata[,"nitrate"],
                                             polldata[,"sulfate"],
                                             use = "complete.obs"))
                }
        }
        cor_vect
}