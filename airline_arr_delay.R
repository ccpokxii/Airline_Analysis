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

#x <- read.big.matrix("AirlineDataGroupProject.csv", header = TRUE, 
                    # backingfile = "airproj.bin",
                     #descriptorfile = "airproj.desc",
                    # type = "integer", extraCols = "age")

setwd("~/Stat480/RDataScience/AirlineDelays")
library(biganalytics)
air <-attach.big.matrix("airproj.desc")
year98 <- air[which(air[,"Year"] == 1998),]
year02 <- air[which(air[,"Year"] == 2002),]

Sun = which(year98[,"DayOfWeek"]==1)
Mon = which(year98[,"DayOfWeek"]==2)
Tues = which(year98[,"DayOfWeek"]==3)
Wed = which(year98[,"DayOfWeek"]==4)
Thurs = which(year98[,"DayOfWeek"]==5)
Fri = which(year98[,"DayOfWeek"]==6)
Sat = which(year98[,"DayOfWeek"]==7)


Jan = which(year98[,"Month"]==1)
Feb = which(year98[,"Month"]==2)
Mar = which(year98[,"Month"]==3)
Apr = which(year98[,"Month"]==4)
May = which(year98[,"Month"]==5)
Jun = which(year98[,"Month"]==6)
Jul = which(year98[,"Month"]==7)
Aug = which(year98[,"Month"]==8)
Sep = which(year98[,"Month"]==9)
Oct = which(year98[,"Month"]==10)
Nov = which(year98[,"Month"]==11)
Dec = which(year98[,"Month"]==12)

aveArrDelay = mean(year98[,"ArrDelay"],na.rm=TRUE)
aveDepDelay = mean(year98[,"DepDelay"],na.rm=TRUE)
percentiledep = quantile(year98[,"DepDelay"],na.rm=TRUE)
percentilearr = quantile(year98[,"ArrDelay"],na.rm=TRUE)

#slices <- c(length(Jan),length(Feb),length(Mar),length(Apr),length(May),length(Jun),length(Jul),length(Aug),length(Sep),length(Oct),length(Nov),length(Dec))
#lbls <- c("Jan","Feb","March","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#pct <- round(slices/sum(slices)*100)
#lbls <- paste(lbls, pct) # add percents to labels 
#lbls <- paste(lbls,"%",sep="") # ad % to labels 
#pie3D(slices, labels = lbls, main="Pie Chart of Delays by Day")

#Yutong#
# The streamgraph of the flights number everyday
number <-as.data.frame(read.csv("count_day.csv",sep= "\t", header = FALSE))
colnames(number) <- c("Year","Month","DayofMonth","Number")
number$date <- as.Date(paste(number$Month, number$DayofMonth, sep = '-'),format = "%m-%d")
streamgraph(number, "Year", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

## The streamgraph of the flights number everyday by carrier
number.dflights <- as.data.frame(read.csv("count_carrier_day.csv",sep= "\t", header = FALSE))
colnames(number.dflights) <- c("Year","Month","DayofMonth","Number","Carrier")
number.dflights$date <- as.Date(paste(number.dflights$Year, number.dflights$Month, number.dflights$DayofMonth, sep = '-'))

number.dflights1998 <- number.dflights[which(number.dflights["Year"] == "1998"),]
number.dflights2002 <- number.dflights[which(number.dflights["Year"] == "2002"),]

streamgraph(number.dflights1998, "Carrier", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

streamgraph(number.dflights2002, "Carrier", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)




