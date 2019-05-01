#Any package that is required by the script below is given here(This section and the Flights Routes section):
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp","devtools","streamgraph","biganalytics","gplots","reshape","treemap","maps","viridis","grDevices","reshape2","geosphere")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)
## Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

#Data preparation
###If There are error when read big matrix, the following code will help.
# Sys.setlocale('LC_ALL','C')
library(biganalytics)
x <- read.big.matrix("groupprojectcl.csv", header = TRUE,
                     backingfile = "gp.bin",
                     descriptorfile = "gp.desc",
                     type = 'integer', extraCols = "Elapsed")
###If the gp.desc is created, we can attach the big matrix directly.
# x<-attach.big.matrix("gp.desc")
year98 <- x[which(x[,"Year"] == 1998),]
year02 <- x[which(x[,"Year"] == 2002),]


##1-The distribution of the daily number of flights in these two years============================
##Data:This data sets created by hive in Overall Analysis (Number of Flights).sh
day.n <- as.data.frame(read.csv("count_day.csv",sep = "\t", header = FALSE))
colnames(day.n) <- c("Year","Month","DayofMonth","Number")
day.n$Year <- factor(day.n$Year)
day.n$date <- as.Date(paste(day.n$Month, day.n$DayofMonth),format = "%m%d")#Create a new column of Date
###Construct plot
###If need, de.off firstly
# dev.off()
ggplot(day.n, aes(x = Number)) + 
  geom_density(aes(group = Year, colour = Year, fill = Year), position="stack", alpha = 0.6) +
  labs(title = "The Distribution of the Daily Number of Flights") +
  theme(plot.title = element_text(hjust = 0.5))


##2-The daily number of flights for different carrier in two years=================================
##Data: This data sets created in hive in Overall Analysis (Number of Flights).sh
day.carrier.n <- as.data.frame(read.csv("count_carrier_day.csv",sep = "\t", header = FALSE))
colnames(day.carrier.n) <- c("Year","Month","DayofMonth","Carrier","Number")
day.carrier.n$date <- as.Date(paste(day.carrier.n $Year,day.carrier.n $Month, day.carrier.n $DayofMonth, sep = '-'))
carrier98 <- day.carrier.n[which(day.carrier.n["Year"] == "1998"),]
carrier02 <- day.carrier.n[which(day.carrier.n["Year"] == "2002"),]

###Construct Graphs
library(streamgraph)
streamgraph(carrier98, "Carrier", "Number", "date", interactive = FALSE) %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date) 

streamgraph(carrier02, "Carrier", "Number", "date", interactive = FALSE) %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date) 



##3-The Number of Flights at Different Times=================================================
#Data: x is the big matrix import at the beginning
hour <- floor(x[,"CRSDepTime"]/100) # Get hour from time
hour[which(hour == 24)] <- 0 # Reorganize the origin
library(reshape)
hour <- melt(table(hour,x[,"Year"]))
colnames(hour) <- c("Hour","Year","Number")
hour$Year <- as.factor(hour$Year)
ggplot(data = hour, aes(x = as.factor(Hour), y = Number, group = Year, col = Year))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  labs(x="Departure Hour",title ="The Number of Flights at Different Times")+
  theme(plot.title = element_text(hjust = 0.5))


##4-The monthly number of flights in two years================================================
#Data: day.n is creatd in the first part
f1998 <- day.n[which(day.n[,"Year"] == "1998"),]
f2002 <- day.n[which(day.n[,"Year"] == "2002"),]
month <- matrix(0,12,2)
colnames(month)<- c("1998","2002")
month[,"1998"] <- tapply(f1998$Number,f1998$Month, sum)
month[,"2002"] <- tapply(f2002$Number,f2002$Month, sum)
month <- melt(month)
colnames(month) <- c("Month","Year","Number")
month$Year <- factor(month$Year)
ggplot(data=month,aes(x = as.factor(Month), y = Number,group = Year, col = Year))+
  geom_line(size = 1)+
  geom_point(size = 2)+
  labs(x ="Month",title ="The Number of Flights over Months")+
  theme(plot.title = element_text(hjust = 0.5))

##5-heatmap================================================================================
###Data: This data sets created in hive in Overall Analysis (Number of Flights).sh
odc <- read.csv("OriDesCar.csv", header=FALSE,sep = "\t")
colnames(odc) <- c("Year","Origin", "Dest","UniqueCarrier")

###divide the data by years
odc98 <- odc[which(odc[,"Year"] == 1998),]
odc02 <- odc[which(odc[,"Year"] == 2002),]
###heatmap for 1998
carrier1998 = split(1:nrow(year98), odc98[, 'UniqueCarrier'])
library(biganalytics)
carrier98 = foreach(i = carrier1998, .combine=cbind) %do% {
  a = sum(year98[i,'Cancelled']) 
  b = length(which(year98[i,'ArrDelay'] > 0))
  c = length(which(year98[i,'DepDelay'] > 0))
  d = length(year98[i, 'Cancelled'])
  list(CancelledCount = a, ArrDelay = b, DepDelay = c,Total = d)
}
carrier98 = matrix(unlist(carrier98), nrow=4)
colnames(carrier98) <- names(carrier1998)
rownames(carrier98) <- c('Cancel', 'ArrDelay','DepDelay', 'Total')
library(gplots)
heatmap.2(t(carrier98[,-6]), scale='column',
          main = 'Statistics of Carriers in 1998',
          keysize=2,
          cexRow ='1',
          cexCol ='1')

###heat map for 2002
carrier2002 = split(1:nrow(year02), odc02[, 'UniqueCarrier'])
library(biganalytics)
carrier02 = foreach(i = carrier2002, .combine=cbind) %do% {
  a = sum(year02[i,'Cancelled']) 
  b = length(which(year02[i,'ArrDelay'] > 0))
  c = length(which(year02[i,'DepDelay'] > 0))
  d = length(year02[i, 'Cancelled'])
  list(CancelledCount = a, ArrDelay = b, DepDelay = c,Total = d)
}
carrier02 = matrix(unlist(carrier02), nrow=4)
colnames(carrier02) <- names(carrier2002)
rownames(carrier02) <- c('Cancel', 'ArrDelay','DepDelay', 'Total')
library(gplots)
heatmap.2(t(carrier02[,-8]), scale='column',
          main = 'Statistics of Carriers in 2002',
          keysize=2,
          cexRow ='1',
          cexCol ='1')

