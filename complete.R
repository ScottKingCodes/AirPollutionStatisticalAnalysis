complete <- function(directory, id = 1:332)
{
    
    filenames <- list.files(directory, full.names = TRUE)[id]
    columnnames <- c("id","nobs")
    firstdata <- read.csv(filenames[1], header = TRUE)
    completefirstdata <- sum(complete.cases(firstdata))
    completedata <- data.frame(id[1],completefirstdata)
    
    
    if (length(filenames) > 1)
    {
        for (i in 2:length(filenames))
        {
            dataset <- read.csv(filenames[i])
            completenextdata <- sum(complete.cases(dataset))
            completedata[nrow(completedata) + 1,] <- list(id[i],completenextdata)
        }
    }
    colnames(completedata) <- columnnames
    completedata
   
}