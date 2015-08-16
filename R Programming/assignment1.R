# Coursera R Programming Assignment1
setwd("~/datasciencecoursera/")

# PART I
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  ## data sets can be large so read first 100 rows to identify column data types and then read them all
  setwd(directory)
  
  j <- 0
  for(i in id) {
    if(j==0) {
      initial <- read.csv(i, nrows  = 100)
      classes <- sapply(initial,class)
      finaldata <- read.csv(i, header=T, sep=",", colClasses = classes)
  } else {
    initial <- read.csv(i, nrows  = 100)
    classes <- sapply(initial,class)
    df <- read.csv(i,header=T, sep=",", colClasses = classes)
    finaldata <- rbind(finaldata,df)
  }}
  
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of the pollutant for which we will calculate the mean; either "sulfate" or "nitrate"
  ## therefore, if neither pollutant or sulfate throw back an error to user
  
  ## 'id' is an integer vector indicating the monitor ID numbers to be used
  
  ## Return the mean of the pollutant across all monitors list in the 'id' vector (ignoreing NA values)
  ## NOTE: do not round the result!
  mean(data$pollutant, na.rm=TRUE)
}


pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  files_list <- list.files(directory, full.names=T)
  files_list
  tmp <- vector(mode="list",length=length(id)) #our empty list we will fill
  for (i in seq_along(files_list)) {
    #initial <- read.csv(i, nrows  = 100)       ## data sets can be large so read first 100 rows 
    #classes <- sapply(initial,class)           ## to identify column data types and then read them all
    #tmp[[i]] <- read.csv(files_full[[i]], header=T, sep=",", colClasses = classes) 
    tmp[[i]] <- read.csv(files_full[[i]], header=T, sep=",") 
  }
  
  output <- do.call(rbind, tmp)
  
  if(pollutant=="sulfate") {
    mean(output$sulfate, na.rm=TRUE)
  }
  else if(pollutant=="nitrate") {
    mean(output$nitrate, na.rm=TRUE)
  }
  else{
    return("You are a naughty person! Please enter either 'sulfate' or 'nitrate' in double quotations!")
  }
}



pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating the location of the CSV files
  
  files_list <- list.files(directory, full.names=T)
  
  temp_file <- vector(mode="list",length=length(id)) #our empty list we will fill
  
  j <- 1
  for (i in seq_along(id)) {
    initial <- read.table(files_list[[i]], nrows  = 100, skipNul=T)       ## data sets can be large so read first 100 rows 
    classes <- sapply(initial,class)           ## to identify column data types and then read them all
    temp_file[[j]] <- read.table(files_list[[i]], header=T, sep=",", colClasses = classes) 
    j <- j + 1
  }
  
  output <- do.call(rbind, temp_file)
  
  if(pollutant=="sulfate") {
    mean(output$sulfate, na.rm=TRUE)
  }
  else if(pollutant=="nitrate") {
    mean(output$nitrate, na.rm=TRUE)
  }
  else{
    return("You are a naughty person! Please enter either 'sulfate' or 'nitrate' in double quotations!")
  }
}



##pollutantmean("~/datasciencecoursera/specdata/", "sulfate", 1:10)
## [1] 4.064  
pollutantmean("specdata", "sulfate", 1:10)
## [1] 4.064
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
pollutantmean("specdata", "nitrate", 23)
## [1] 1.281

directory <- "specdata"
pollutant <- "nitrate"
id1 <- 70:72

files_list <- list.files(directory, full.names=T)

temp_file <- vector(mode="list",length=length(id1)) #our empty list we will fill
j <- 1
for (i in seq_along(id1)) {
  initial <- read.table(files_list[[i]], nrows  = 100, skipNul=T)       ## data sets can be large so read first 100 rows 
  classes <- sapply(initial, class)           ## to identify column data types and then read them all
  temp_file[[j]] <- read.table(files_list[[i]], header=T, sep=",", colClasses = classes) 
  j <- j + 1
}

# PART II