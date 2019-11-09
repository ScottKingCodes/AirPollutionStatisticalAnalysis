pollutantmean <- function(directory, pollutant, id = 1:332)
{
    poll <- 2
    if (pollutant == "sulphate")
    {
        poll <- 2
    }
    else if (pollutant == "nitrate")
    {
        poll <- 3
    }
    
    filenames <- list.files(directory, full.names = TRUE)[id]
    
    firstdata <- read.csv(filenames[1], header = TRUE)
    totalpollutant <- firstdata[,poll][!is.na(firstdata[,poll])]
    
    if (length(filenames) > 1)
    {
        for (i in 2:length(filenames))
        {
            dataset <- read.csv(filenames[i])
            cleanpollutant <- dataset[,poll][!is.na(dataset[,poll])]
            totalpollutant <- c(totalpollutant,cleanpollutant)
        }
    }
    
    meanpollutant <- mean(totalpollutant)
    meanpollutant
}