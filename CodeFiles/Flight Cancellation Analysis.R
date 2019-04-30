# Installing and loading necessary packages
install.packages('biganalytics')
install.packages('foreach')
install.packages('ggplot2')

library(biganalytics)
library(foreach)
library(ggplot2)

# Big Matrix Setup 

# Function to apply to a CSV file to convert a list of columns to integer index values.
#
convertCSVColumns <- function(file, collist){
  fulldata<-read.csv(file)
  for (i in collist) {
    fulldata[,i]<-convertColumn(fulldata[,i])
  }
  write.csv(fulldata, file, row.names=FALSE)
}
# The following function is called by convertCSVColumns. It converts a single
#column to integer indices.
convertColumn <- function(values){
  allvals<-as.character(values)
  valslist<-sort(unique(allvals))
  xx<-factor(allvals, valslist, labels=1:length(valslist))
  rm(allvals)
  rm(valslist)
  gc()
  as.numeric(levels(xx))[xx]
}

# Now use the function on the data.
convertCSVColumns("groupprojectcl.csv", c(9,11,17,18))

x <- read.big.matrix("groupprojectcl.csv", header = TRUE,
                     backingfile = "gp.bin",
                     descriptorfile = "gp.desc",
                     type = "integer")

x <- attach.big.matrix('gp.desc')
year98 <- x[which(x[,"Year"] == 1998),]
year02 <- x[which(x[,"Year"] == 2002),]


# Finding Overall General Statistics for Cancellation
year = split(1:nrow(x), x[, 'Year'])
cancelA = foreach(i = year, .combine = cbind) %do% {
  a = sum(x[i, 'Cancelled'])
  b = length(x[i, 'Cancelled'])
  c = (a/b)*100
  list(CancelledCount = a, Total = b, CancelledPercent = c)
}
colnames(cancelA) = c('1998', '2002')
cancelA


# Finding general statistics for cancellations in 1998 by month
month1998 = split(1:nrow(year98), year98[, 'Month'])
month1998A = foreach(i = month1998, .combine=cbind) %do% {
  a = sum(year98[i,'Cancelled'])
  b = length(year98[i, 'Cancelled'])
  c = (a/b)*100
  list(CancelledCount1998 = a, Total1998 = b, CancelledPercent1998 = c)
}
colnames(month1998A) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
month1998A


# Finding general statistics for cancellations in 2002 by month
month2002 = split(1:nrow(year02), year02[, 'Month'])
month2002A = foreach(i = month2002, .combine=cbind) %do% {
  a = sum(year02[i,'Cancelled'])
  b = length(year02[i, 'Cancelled'])
  c = (a/b)*100
  list(CancelledCount2002 = a, Total2002 = b, CancelledPercent2002 = c)
}
colnames(month2002A) = c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec')
month2002A


# Creating the 'Number of flights cancelled per month plot'
CancelledCountA = unlist(c(month1998A[1,], month2002A[1,]))
yearMonth = rep(c(1998, 2002), each = 12)
month = rep(seq(1:12), times = 2)
dfMonthCount = as.data.frame(t(rbind(yearMonth, month, CancelledCountA)))
#dfMonthCount
ggplot(dfMonthCount, aes(x = factor(month), y = CancelledCountA, fill = factor(yearMonth))) + geom_bar(stat = 'identity') + labs(title = 'Number of Flights Cancelled per Month', caption = 'Note: Month = 1 is January, Month = 2 is February, ... , Month = 12 is December.', x = 'Month', y = 'Number of Flights Cancelle', fill = 'Year') + scale_fill_manual(values = c('1998' = 'maroon', '2002' = 'pink'))


# Creating the 'Percentage of flights cancelled per month plot'
CancelledPercentA = unlist(c(month1998A[3,], month2002A[3,]))
dfMonthPercent = as.data.frame(t(rbind(yearMonth, month, CancelledPercentA)))
ggplot(dfMonthPercent, aes(x = factor(month), y = CancelledPercentA, fill = factor(yearMonth))) + geom_bar(stat = 'identity', position = 'dodge') + labs(title = 'Percentage of Flights Cancelled per Month', caption = 'Note: Month = 1 is January, Month = 2 is February, ... , Month = 12 is December.', x = 'Month', y = 'Percentage of Flights Cancelled', fill = 'Year') + scale_fill_manual(values = c('1998' = 'maroon', '2002' = 'pink'))


# Finding general statistics for cancellations in 1998 by day of week
day1998 = split(1:nrow(year98), year98[, 'DayOfWeek'])
day1998A = foreach(i = day1998, .combine=cbind) %do% {
  a = sum(year98[i,'Cancelled'])
  b = length(year98[i, 'Cancelled'])
  c = (a/b)*100
  list(CancelledCount1998 = a, Total1998 = b, CancelledPercent1998 = c)
}
colnames(day1998A) = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
day1998A


# Finding general statistics for cancellations in 2002 by day of week
day2002 = split(1:nrow(year02), year02[, 'DayOfWeek'])
day2002A = foreach(i = day2002, .combine=cbind) %do% {
  a = sum(year02[i,'Cancelled'])
  b = length(year02[i, 'Cancelled'])
  c = (a/b)*100
  list(CancelledCount2002 = a, Total2002 = b, CancelledPercent2002 = c)
}
colnames(day2002A) = c('Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat')
day2002A


# Creating the 'Number of flights cancelled per day of week plot'
CancelledCountB = unlist(c(day1998A[1,], day2002A[1,]))
yearDay = rep(c(1998, 2002), each = 7)
day = rep(seq(1:7), times = 2)
dfDayCount = as.data.frame(t(rbind(yearDay, day, CancelledCountB)))
#dfDayCount
ggplot(dfDayCount, aes(x = factor(day), y = CancelledCountB, fill = factor(yearDay))) + geom_bar(stat = 'identity') + labs(title = 'Number of Flights Cancelled per Day of Week', caption = 'Note: Day = 1 is Sunday, Day = 2 is Monday, ... , Day = 7 is Saturday', x = 'Day of Week', y = 'Number of Flights Cancelled', fill = 'Year') + scale_fill_manual(values = c('1998' = 'maroon', '2002' = 'pink'))


# Creating the 'Percentage of flights cancelled per day of week plot'
CancelledPercentB = unlist(c(day1998A[3,], day2002A[3,]))
dfDayPercent = as.data.frame(t(rbind(yearDay, day, CancelledPercentB)))
ggplot(dfDayPercent, aes(x = factor(day), y = CancelledPercentB, fill = factor(yearDay))) + geom_bar(stat = 'identity', position = 'dodge') + labs(title = 'Percent of Flights Cancelled per Day of Week', caption = 'Note: Day = 1 is Sunday, Day = 2 is Monday, ... , Day = 7 is Saturday', x = 'Day of Week', y = 'Percent of Flights Cancelled', fill = 'Year') + scale_fill_manual(values = c('1998' = 'maroon', '2002' = 'pink'))



