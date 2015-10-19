# complete.R

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