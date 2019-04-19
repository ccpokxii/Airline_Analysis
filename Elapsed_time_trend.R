#Other Trends
installIfNeeded = function(cliblist){
  libsNeeded = cliblist
  libsNeeded = libsNeeded[!(libsNeeded %in% installed.packages()[,"Package"])]
  if(length(libsNeeded)>0) install.packages(libsNeeded)
}

installIfNeeded(c("bit64", "RSQLite", "foreach", "parallel", "doSNOW", "ggplot2", "biganalytics"))

setwd("~/Stat480/RDataScience/Group")

library(biganalytics)
x <- read.big.matrix("groupprojectcl.csv", header = TRUE, 
                     backingfile = "gp.bin",
                     descriptorfile = "gp.desc",
                     type = "integer", extraCols = "Elapsed")
x1=x[x[,"Year"]==1998,]
x2=x[x[,"Year"]==2002,]

##Elapsed trend for 1998
for (i in 1:nrow(x1)) {
  x1[i,"Elapsed"]=x1[i,"ActualElapsedTime"]-x1[i,"CRSElapsedTime"]}

x1[,"Elapsed"]  

over=x1[x1[,"Elapsed"]>0,]
nrow(over)
overpercent1998=nrow(over)/nrow(x1)

?plot
intime=x1[x1[,"Elapsed"]<0,]
nrow(intime)
intimepercent1998=nrow(intime)/nrow(x1)

ontime=x1[x1[,"Elapsed"]==0,]
nrow(ontime)
ontimepercent1998=nrow(ontime)/nrow(x1)

aveElap1998=mean(x1[,"Elapsed"],na.rm=TRUE)

myProbs <- seq(0.5, 0.99, by=0.01)
elapsedQuantiles=quantile(x1[, "Elapsed"], myProbs, 
                          na.rm = TRUE)
library(ggplot2)
library(reshape2) 
eq <- melt(elapsedQuantiles)
eq=cbind(percentile=rownames(eq), eq)
names(eq) <- c("percentile", "elapsed")
eq$percentile=as.double(eq$percentile)+50
qplot(percentile, elapsed, data = eq, color = percentile, geom = "line")



##Elapsed trend for 2002
for (i in 1:nrow(x2)) {
  x2[i,"Elapsed"]=x2[i,"ActualElapsedTime"]-x2[i,"CRSElapsedTime"]}

x2[,"Elapsed"]  

over2=x2[x2[,"Elapsed"]>0,]
nrow(over2)
overpercent2002=nrow(over2)/nrow(x2)

intime=x2[x2[,"Elapsed"]<0,]
nrow(intime)
intimepercent2002=nrow(intime)/nrow(x2)

ontime=x2[x2[,"Elapsed"]==0,]
nrow(ontime)
ontimepercent2002=nrow(ontime)/nrow(x2)

aveElap2002=mean(x2[,"Elapsed"],na.rm=TRUE)

myProbs <- seq(0.5, 0.99, by=0.01)
elapsedQuantiles=quantile(x2[, "Elapsed"], myProbs, 
                          na.rm = TRUE)
library(ggplot2)
library(reshape2) 
eq <- melt(elapsedQuantiles)
eq=cbind(percentile=rownames(eq), eq)
names(eq) <- c("percentile", "elapsed")
eq$percentile=as.double(eq$percentile)+50
qplot(percentile, elapsed, data = eq, color = percentile, geom = "line")


dayCount = integer(12)
for (i in 1:12) { 
  dayCount[i] <-  sum(over[,"Month"] == i, na.rm=TRUE)}
month=seq(1:12)
plot(month, dayCount, type = "o", col="blue", ylim=c(110000, 180000))

dayCount2 = integer(12)
for (i in 1:12) { 
  dayCount2[i] <-  sum(over2[,"Month"] == i, na.rm=TRUE)}
lines(dayCount2, type = "o")
title(main = "Days of over scheduled Elapsed time")
#number of flights by month, year; taxiin taxiout, elaspedtime;


class(x[,17])

x3=x[x[,"ActualElapsedTime"-"CRSElapsedTime"]>0,]

#Elapsed and carrier
groupprojectcsv=read.csv("groupprojectcl.csv")
groupprojectcl=groupprojectcsv[c("UniqueCarrier","ActualElapsedTime", "CRSElapsedTime")]
groupprojectcl$Elapsed=groupprojectcl$ActualElapsedTime-groupprojectcl$CRSElapsedTime
OvertimeEl=groupprojectcl[which(groupprojectcl$Elapsed>0),]

otcarrier=table(OvertimeEl$UniqueCarrier)
otcarrier=sort(otcarrier)
otcarrier=as.data.frame(otcarrier)
colnames(otcarrier) <- c("code", "freq")

carriercsv=read.csv("carriers.csv")
OvertimeCarrier=merge(otcarrier, carriercsv, by.x="code", by.y="Code")
OvertimeCarrier=OvertimeCarrier[order(-OvertimeCarrier$freq),] 

#Elapsed and origin airport
airportcsv=read.csv("airports.csv")
groupprojectcl2=groupprojectcsv[c("Origin", "Dest", "ActualElapsedTime", "CRSElapsedTime")]
groupprojectcl2$Elapsed=groupprojectcl2$ActualElapsedTime-groupprojectcl2$CRSElapsedTime
OvertimeEl2=groupprojectcl2[which(groupprojectcl2$Elapsed>0),]

otorigin=table(OvertimeEl2$Origin)
otorigin=sort(otorigin)
otorigin=as.data.frame(otorigin)
colnames(otorigin) <- c("airport", "freq")

OvertimeOrigin=merge(otorigin, airportcsv, by.x="airport", by.y="iata")
OvertimeOrigin=OvertimeOrigin[order(-OvertimeOrigin$freq),] 

#Elapsed and dest airport
otdest=table(OvertimeEl2$Dest)
otdest=sort(otdest)
otdest=as.data.frame(otdest)
colnames(otdest) <- c("airport", "freq")

OvertimeDest=merge(otdest, airportcsv, by.x="airport", by.y="iata")
OvertimeDest=OvertimeDest[order(-OvertimeDest$freq),] 

#Visualization: Count the airport/carrier's flight total number and average elapsed time 


#treemap
installIfNeeded(c("treemap"))
library(treemap)
carriermap=data.frame(table(groupprojectcl$UniqueCarrier))
colnames(carriermap) <- c("carrier", "freq")

AAcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='AA'),]
mean(AAcsv$Elapsed, na.rm=TRUE)
AScsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='AS'),]
mean(AScsv$Elapsed, na.rm=TRUE)
COcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='CO'),]
mean(COcsv$Elapsed, na.rm=TRUE)
DLcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='DL'),]
mean(DLcsv$Elapsed, na.rm=TRUE)
HPcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='HP'),]
mean(HPcsv$Elapsed, na.rm=TRUE)
MQcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='MQ'),]
mean(MQcsv$Elapsed, na.rm=TRUE)
NWcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='NW'),]
mean(NWcsv$Elapsed, na.rm=TRUE)
TWcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='TW'),]
mean(TWcsv$Elapsed, na.rm=TRUE)
UAcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='UA'),]
mean(UAcsv$Elapsed, na.rm=TRUE)
UScsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='US'),]
mean(UScsv$Elapsed, na.rm=TRUE)
WNcsv=groupprojectcl[which(groupprojectcl$UniqueCarrier=='WN'),]
mean(WNcsv$Elapsed, na.rm=TRUE)

carriermap$meanelapsed=c(-3.143742, 0.184173, -0.3499615, -0.2344786, -1.110613, -1.315059, -1.841802, -1.498755, -1.364657, -1.584349, -3.710075)

treemap(carriermap,
        index=c("carrier"),
        vSize="freq",
        vColor="meanelapsed",
        type="value")
