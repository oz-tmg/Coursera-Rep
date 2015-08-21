# Coursera R Programming Assignment1
setwd("/Users/alexanderoswald/datasciencecoursera/R Programming")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)
  dat <- data.frame()
  for (i in seq_along(id)) {
    initial <- read.table(files_list[[id[i]]], nrows=200, header=T, sep=",")  
    classes <- sapply(initial, class)    
    dat <- rbind(dat, read.table(files_list[id[i]], header=T, sep=","))
  }
  
  if(pollutant=="sulfate") {
    mean(dat$sulfate, na.rm=TRUE)
  }
  else if(pollutant=="nitrate") {
    mean(dat$nitrate, na.rm=TRUE)
  }
  else {
    return("You are a naughty person! Please enter either 'sulfate' or 'nitrate' in double quotations!")
  }
}

pollutantmean("/Users/alexanderoswald/datasciencecoursera/R Programming/specdata/", "sulfate", 1:10)
## [1] 4.064
pollutantmean("specdata", "nitrate", 70:72)
## [1] 1.706
pollutantmean("specdata", "nitrate", 23)
## [1] 1.281

# PART II
complete <- function(directory, id = 1:332) {
  files_list <- list.files(directory, full.names=TRUE)
  nobs <- vector("numeric", length = length(id))
  
  for(i in seq_along(id)){
    dat <- read.table(files_list[id[i]], header=T, sep=",")
    nobs<- rbind(nobs,nrow(complete.cases(dat)))
  }
  
  complete <- cbind(id,nobs)
  return(complete)
}

complete("specdata", 1)
##   id nobs
## 1  1  117
complete("specdata", c(2, 4, 8, 10, 12))
##   id nobs
## 1  2 1041
## 2  4  474
## 3  8  192
## 4 10  148
## 5 12   96
complete("specdata", 30:25)
##   id nobs
## 1 30  932
## 2 29  711
## 3 28  475
## 4 27  338
## 5 26  586
## 6 25  463
complete("specdata", 3)
##   id nobs
## 1  3  243


#PART III
corr <- function(directory, threshold = 0) {
  complete_dat <- complete(directory, 1:332)
  corr_data <- vector("numeric", length = length(id))
  #Write a bit of code that eliminates id from complete_dat based on number of full rows
  
  for(i in seq_along(complete_dat$id)){
    temp_data <- read.table(files_list[complete_dat$id[i]], header=T, sep=",")
    corr_data[i] <- cor(temp_data$sulfate, temp_data$ironite)
  }
  
  
}
