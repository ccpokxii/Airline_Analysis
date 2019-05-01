##5-heatmap================================================================================
###Data: This data sets created in hive in Overall Analysis (Number of Flights).sh
odc <- read.csv("OriDesCar.csv", header=FALSE,sep = "\t")
colnames(odc) <- c("Year","Origin", "Dest","UniqueCarrier")

x<-attach.big.matrix("gp.desc")
year98 <- x[which(x[,"Year"] == 1998),]
year02 <- x[which(x[,"Year"] == 2002),]
###divide the data by years
odc98 <- odc[which(odc[,"Year"] == 1998),]
odc02 <- odc[which(odc[,"Year"] == 2002),]


###Treemap for 1998
carrier1998 = split(1:nrow(year98), odc98[, 'UniqueCarrier'])
#The data part is created in Overall Analysis
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
treemap98 <- t(carrier98[,-6])
library(dplyr)
treemap98 <- as.data.frame(add_rownames(as.data.frame(treemap98), "Carrier"))
cancelper <- as.vector(treemap98[,"Cancel"]/treemap98[,"Total"])
treemap98 <- cbind(treemap98[,c("Carrier","Total")],cancelper)

treemap(treemap98,
        index = c("Carrier"),
        vSize = "Total",
        vColor = "cancelper",
        type = "value",
        palette = "Spectral",
        title = "Treemap for Cancellations by Carrier - 1998")


###Treemap for 2002
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
treemap02 <- t(carrier02[,-8])
library(dplyr)
treemap02 <- as.data.frame(add_rownames(as.data.frame(treemap02), "Carrier"))
cancelper <- as.vector(treemap02[,"Cancel"]/treemap02[,"Total"])
treemap02 <- cbind(treemap02[,c("Carrier","Total")],cancelper)

treemap(treemap02,
        index = c("Carrier"),
        vSize = "Total",
        vColor = "cancelper",
        type = "value",
        palette = "Spectral",
        title = "Treemap for Cancellations by Carrier - 2002")
