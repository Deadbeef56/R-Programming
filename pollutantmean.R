pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the
  ## mean; either "sulfate" or "nitrate".
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  
  filelist <- character(max(id)-min(id)+1)
  for(i in id)
  {
    file <- paste("00",i,sep="")
    file <- substr(file, nchar(file)-2, nchar(file))
    file <- paste(directory, "/", file, ".csv", sep="")
    filelist[i-min(id)+1] <- file
  }
  
  filelist
  values <- numeric(0)
  for(file in filelist) 
  {
    df <- read.csv(file)
    values <- c(values, df[[pollutant]])
  }
  
  mean(values, na.rm=TRUE)
}
