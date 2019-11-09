corr <- function(directory, threshold = 0)
{
    filenames <- list.files(directory, full.names = TRUE)
    allpollcor <- numeric()
    
    for (i in 1:length(filenames))
    {
        dataset <- read.csv(filenames[i])
        totalcompletedata <- sum(complete.cases(dataset))
        completedata <- complete.cases(dataset)
        
        if (totalcompletedata >= threshold)
        {
            pollcorr <- cor(dataset[,2][completedata],dataset[,3][completedata])
            allpollcor <- c(allpollcor,pollcorr)
        }
    }
    
    allpollcor
}