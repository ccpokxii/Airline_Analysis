#Any package that is required by the script below is given here:
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp","devtools","streamgraph","biganalytics","gplots","reshape","treemap","maps","viridis","grDevices","reshape2","geosphere")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)
## Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)

#1-The number of departure flights and landing flights for airports in the two years(Treemap for airports)
##Data: odc is imported im the Overall Analysis part. 
# odc <- read.csv("OriDesCar.csv", header=FALSE,sep = "\t")
# colnames(odc) <- c("Year","Origin", "Dest","UniqueCarrier")
# ###divide the data by years
# odc98 <- odc[which(odc[,"Year"] == 1998),]
# odc02 <- odc[which(odc[,"Year"] == 2002),]

org98 <- table(odc98[,"Origin"]) #Count the number of departure flights
des98 <- table(odc98[,"Dest"])#Count the number of landing flights
d98 <- merge(org98,des98, by.x="Var1",by.y="Var1") #Var1 is the name of the airport
colnames(d98) <- c("Name","Origin","Dest")
org02 <- table(odc02[,"Origin"])
des02 <- table(odc02[,"Dest"])
d02 <- merge(org02,des02, by.x="Var1",by.y="Var1")
colnames(d02) <- c("Name","Origin","Dest")

##Construct the treemap
library(treemap)
treemap(d98,
        index = c("Name"),
        vSize = "Origin",
        vColor = "Dest",
        type = "value",
        palette = "Spectral",
        title = "The Treemap of Departure flights and Landing flights in 1998")
treemap(d02,
        index = c("Name"),
        vSize = "Origin",
        vColor = "Dest",
        type = "value",
        palette = "Spectral",
        title = "The Treemap of Departure flights and Landing flights in 2002")

#2-The number of Des in the two years===================================================================
##reference https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2/
##Data: This data sets created in hive in Flight Routes Analysis.sh
##Prepare the data
dest <- read.csv("dest.csv", header=FALSE, sep="\t" ) 
colnames(dest) <- c("Year","Dest","Number")
airport <- read.table("airports.csv",header=T, sep=',')# airports.csv is the given data sets
destl <- merge(dest,airport, by.x="Dest", by.y="iata")
destl98 <- destl[which(destl[,"Year"] == 1998),]
destl98<- destl98[order(destl98[,"Number"]),]
destl02 <- destl[which(destl[,"Year"] == 2002),]
destl02 <- destl02[order(destl02[,"Number"]),]

##Construct the map plots
usam <- map_data("usa")
library(maps)
library(viridis)

ggplot() +
  geom_polygon(data = usam, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.6) +
  geom_point(data = destl98, aes(x = long, y = lat, size = Number, color = Number)) +
  scale_size_continuous(range = c(1,6))+
  scale_color_viridis(trans = "log")+
  labs(title = "The Number of Landing Flights for Airports in 1998")+
  theme(plot.title = element_text(hjust = 0.5))

ggplot() +
  geom_polygon(data = usam, aes(x = long, y = lat, group = group), fill = "grey", alpha = 0.6) +
  geom_point(data = destl02, aes(x = long, y = lat, size = Number, color = Number)) +
  scale_size_continuous(range = c(1,6))+
  scale_color_viridis(trans = "log")+
  labs(title = "The Number of Landing Flights for Airports in 2002")+
  theme(plot.title = element_text(hjust = 0.5))


#3-Netplot===================================================================================
##Data: the odc imported in the First part
##refrence https://kateto.net/wp-content/uploads/2018/06/Polnet%202018%20R%20Network%20Visualization%20Workshop.pdf
##Data Preparation
odc$Origin <- as.factor(odc$Origin)
odc$Dest <- as.factor(odc$Dest)
airport <- airport[which(airport$iata %in% union(odc$Origin,odc$Dest)),]
library(reshape2)
route <- melt(table(odc$Year,odc$Origin,odc$Dest))
route <- route[which(route$value!=0),]
colnames(route) <- c("Year","From","To","Number")

route1998 <- route[which(route$Year== "1998"),]
route2002 <- route[which(route$Year== "2002"),]



##Select the principal airports(Plot the quantile of the two years)
myProbs <- seq(0, 1, by=0.05)
q1998 <- quantile(route1998[,"Number"],myProbs,na.rm=TRUE)
q2002 <- quantile(route2002[,"Number"],myProbs,na.rm=TRUE)
routeq <- as.matrix(cbind(q1998,q2002), rownames=myProbs)
colnames(routeq) <- c("1998","2002")
mroute <- melt(routeq)
library(ggplot2) 
ggplot(mroute, aes(x=Var1, y=value, color=as.factor(mroute$Var2), group=as.factor(mroute$Var2))) +
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), axis.text.x = element_text(size = 6, angle = 45, hjust = 0.5, vjust = 0.5))+ 
  xlab("percentage") + ylab("Number") + labs(title = "The Quantile of the Airline Number")

###By the quntile, we can find the busier and less busy airlines
route1998 <- route1998[order(route1998[,"Number"]),]
route2002 <- route2002[order(route2002[,"Number"]),]
dvt98mb <- route1998[-c(1:max(which(route1998[,"Number"]<5203))),] 
dvt02mb <- route2002[-c(1:max(which(route1998[,"Number"]<4710))),] 

##Plot the Net on map
###generate a color gradient 
library(grDevices)
col.1 <- adjustcolor("turquoise2", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

##The greneral situation in 1998-----------------------------------
id <- table(route1998$From)
bid <- names(id)[id >10]
airports1998 <- airport[airport$iata %in% bid,]
flight1998 <- route1998[route1998$From %in% bid &
                           route1998$To %in% bid, ]
airports1998[,"iata"] <- factor(airports1998[,"iata"])

###create basemap
library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports1998$long,airports1998$lat, pch=3, cex=0.5, col="orange")
## install.packages("geosphere")
library(geosphere)
### add lines on the map
for(i in 1:nrow(flight1998)) {
  node1 <- airports1998[as.character(airports1998$iata) == as.character(flight1998[i,]$From),]
  node2 <- airports1998[as.character(airports1998$iata) == as.character(flight1998[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*flight1998[i,]$Number / max(flight1998$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}



##The general situation in 2002------------------------
id <- table(route2002$From)
bid <- names(id)[id >10]
airports2002 <- airport[airport$iata %in% bid,]
flight2002 <- route2002[route2002$From %in% bid &
                           route2002$To %in% bid, ]
airports2002[,"iata"] <- factor(airports2002[,"iata"])
###create basemap
library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports2002$long,airports2002$lat, pch=3, cex=0.5, col="orange")
library(geosphere)
### add lines on the map
for(i in 1:nrow(flight2002)) {
  node1 <- airports2002[as.character(airports2002$iata) == as.character(flight2002[i,]$From),]
  node2 <- airports2002[as.character(airports2002$iata) == as.character(flight2002[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*flight2002[i,]$Number / max(flight2002$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

##The more busy flights in 1998---------------------------------------------
airports98mb <- airport[airport$iata %in% union(dvt98mb[,"From"], dvt98mb[,"To"]),]
airports98mb[,"iata"] <- factor(airports98mb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports98mb $long,airports98mb $lat, pch=3, cex=0.5, col="orange")
library(geosphere)
### add lines on the map
for(i in 1:nrow(dvt98mb)) {
  node1 <- airports98mb[as.character(airports98mb$iata) == as.character(dvt98mb[i,]$From),]
  node2 <- airports98mb[as.character(airports98mb$iata) == as.character(dvt98mb[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*dvt98mb[i,]$Number / max(dvt98mb$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

##The more busy case in 2002--------------------------------------------------------
airports02mb <- airport[airport$iata %in% union(dvt02mb[,"From"], dvt02mb[,"To"]),]
airports02mb[,"iata"] <- factor(airports02mb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports02mb$long,airports02mb$lat, pch=3, cex=0.5, col="orange")
library(geosphere)

### add lines on the map
for(i in 1:nrow(dvt02mb)) {
  node1 <- airports02mb[as.character(airports02mb$iata) == as.character(dvt02mb[i,]$From),]
  node2 <- airports02mb[as.character(airports02mb$iata) == as.character(dvt02mb[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*dvt02mb[i,]$Number / max(dvt02mb$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

