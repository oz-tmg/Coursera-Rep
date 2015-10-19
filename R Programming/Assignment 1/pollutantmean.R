# pollutantmean.R
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