# corr.R
corr <- function(directory, threshold = 0) {
  files_list <- list.files(directory, full.names=TRUE)
  complete_dat <- complete(directory, 1:332)
  
  id <- complete_dat$id[complete_dat$nobs >= threshold]
  
  corr_data <- vector("numeric", length = length(id))
  
  for(i in seq_along(id)){
    temp_data <- read.table(files_list[[id[i]]], header=T, sep=",")
    corr_data[i] <- cor(temp_data$sulfate, temp_data$nitrate, use = "pairwise.complete.obs")
    #corr_data[i] <- cor(temp_data[,])
  }
  
  return(corr_data)
}