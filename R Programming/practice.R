# PRACTICE ASSIGNMENT
setwd("~/datasciencecoursera/R Programming/")   #sets the directory to where files will be downloaded

dataset_url <- "http://s3.amazonaws.com/practice_assignment/diet_data.zip"  #url for practice dataset
download.file(dataset_url, "diet_data.zip")                                 #downloads pract. dataset
unzip("diet_data.zip", exdir = "diet_data")

list.files("diet_data")

andy <- read.csv("diet_data/Andy.csv")
head(andy)

length(andy$Day)  #figures out how many rows there are by looking at the length of the 'Day' column
dim(andy)         #look at the dimensions of the data.frame (rows then columns!!!)

str(andy)       #provides data class() and how the data looks
summary(andy)   #provides classic summary stats for all the variables
names(andy)     #same function as ls() i believe

andy[1,"Weight"] #see andy's starting weight given in the first row of the weight column
any[30,"Weight"] #see andy's weight on day 30 given in the 30th row of the weight column

#subset the weight column where 'Day' is equal to 30
andy[which(andy$day==30),"Weight"]
andy[which(andy[,"Day"]==30), "Weight"]
subset(andy$Weight, andy$Day==30)

#Let's assign Andy's starting and ending weight to vectors:
andy_start <- andy[1, "Weight"]
andy_end <- andy[30, "Weight"]

#Find andy's weight loss/gain
andy_loss <- andy_start - andy_end
andy_loss

files <- list.files("diet_data")
files

#Knowing that 'files' is now a list of the contents of 'diet_data' in alphabetical order, we can call a 
#specific file by subsetting it:
files[1]
files[2]
files[3:5]

head(read.csv(files[3]))
#John.csv is sitting inside the diet_data folder. We just tried to run the equivalent of read.csv("John.csv") 
#and R correctly told us that there isn't a file called John.csv in our working directory. To fix this, we 
#need to append the directory to the beginning of the file name
files_full <- list.files("diet_data",full.names=TRUE)
files_full 
head(read.csv(files_full[3]))

#create one big data frame with everyone's data!!!
andy_david <- rbind(andy, read.csv(files_full[2]))

head(andy_david)
tail(andy_david)

#Practice looping
for (i in 1:5) {print(i)}

dat <- data.frame()                           #declare dat to be an empty dataframe
for (i in 1:5) {                              #loop through the total number of files you want to append
  dat <- rbind(dat, read.csv(files_full[i]))  #bind the rows of the dataset together now
}
str(dat)

median(dat$Weight)            #returns NA because we included missing (NA) data
median(dat$Weight, na.rm=T)   #strips out NA values allowing for calculation: complete.cases() or is.na() can subset data

#build a function that will return the median weight of a given day
weightmedian <- function(directory, day)  {
  files_list <- list.files(directory, full.names=TRUE)   #creates a list of files
  dat <- data.frame()                             #creates an empty data frame
  for (i in 1:5) {                                
    #loops through the files, rbinding them together 
    dat <- rbind(dat, read.csv(files_list[i]))
  }
  dat_subset <- dat[which(dat[, "Day"] == day),]  #subsets the rows that match the 'day' argument
  median(dat_subset[, "Weight"], na.rm=TRUE)      #identifies the median weight 
  #while stripping out the NAs  
}

weightmedian(directory = "diet_data", day = 20)
weightmedian("diet_data", 4)
weightmedian("diet_data", 17)

#another way of doing it!!! create an output object of an appropriate size and then fill it up.
#create an empty list that's the length of our expected output
summary(files_full)
tmp <- vector(mode="list",length=length(files_full)) #our empty list we will fill
summary(tmp)
nrow(is.na(tmp)) #all nulls, and this function won't work...balls

#read in those csv files and drop them into tmp
for (i in seq_along(files_full)) {
  tmp[[i]] <- read.csv(files_full[[i]]) 
}
str(tmp)
# What we just did was read in each of the csv files and place them inside of our list. 
# Now we have a list of 5 elements called tmp, where each element of the list is a data 
# frame containing one of the csv files. It just so happens that what we just did is 
# functionally identical to using lapply
str(lapply(files_full, read.csv))

str(tmp[[1]])
head(tmp[[1]][,"Day"])

output <- do.call(rbind, tmp)
str(output)