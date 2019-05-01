## The number of departure flights and landing flights for airports in the two years(Treemap for airports)
route <- read.csv("route.csv", header=FALSE,sep = "\t")# These datasets created by hive
colnames(route) <- c("Year","Origin", "Dest")

#prepare the data
org98 <- as.data.frame(table(as.character(route[which(route["Year"] == "1998"),"Origin"])))
des98 <- as.data.frame(table(as.character(route[which(route["Year"] == "1998"),"Dest"])))
d98 <- merge(org98,des98, by.x="Var1",by.y="Var1")
colnames(d98) <- c("Name","Origin","Dest")
org02 <- table(as.character(route[which(route["Year"] == "2002"),"Origin"]))
des02 <- table(as.character(route[which(route["Year"] == "2002"),"Dest"]))
d02 <- merge(org02,des02, by.x="Var1",by.y="Var1")
colnames(d02) <- c("Name","Origin","Dest")

#Construct the treemap
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

##The number of Des in the two years
###reference https://www.r-graph-gallery.com/330-bubble-map-with-ggplot2/
dest <- read.csv("dest.csv", header=FALSE, sep="\t" ) #dest.csv is created in hive
colnames(dest) <- c("Year","Dest","Number")
airport <- read.table("airports.csv",header=T, sep=',')# airports.csv is the given data sets
destl <- merge(dest,airport, by.x="Dest", by.y="iata")
destl98 <- destl[which(destl[,"Year"] == 1998),]
destl98<- destl98[order(destl98[,"Number"]),]
destl02 <- destl[which(destl[,"Year"] == 2002),]
destl02 <- destl02[order(destl02[,"Number"]),]

#Construct the map plots
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


##Netplot
###Data Preparation
##refrence https://kateto.net/wp-content/uploads/2018/06/Polnet%202018%20R%20Network%20Visualization%20Workshop.pdf
airport <- airport[which(airport$iata %in% union(data$Origin,data$Dest)),]
library(reshape2)
divert <- melt(table(data$Year,data$Origin,data$Dest))
divert <- divert[which(divert$value!=0),]
colnames(divert) <- c("Year","From","To","Number")

divert1998 <- divert[which(divert$Year== "1998"),]
divert2002 <- divert[which(divert$Year== "2002"),]

##generate a color gradient 
library(grDevices)
col.1 <- adjustcolor("turquoise2", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

###Select the principal airports
## Plot the quantile of the two years
myProbs <- seq(0, 1, by=0.05)
q1998 <- quantile(divert1998[,"Number"],myProbs,na.rm=TRUE)
q2002 <- quantile(divert2002[,"Number"],myProbs,na.rm=TRUE)
divertq <- as.matrix(cbind(q1998,q2002), rownames=myProbs)
colnames(divertq) <- c("1998","2002")
mdivert <- melt(divertq)
library(ggplot2) 
ggplot(mdivert, aes(x=Var1, y=value, color=as.factor(mdivert$Var2), group=as.factor(mdivert$Var2))) +
  geom_point() + 
  theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank(), axis.text.x = element_text(size = 6, angle = 45, hjust = 0.5, vjust = 0.5))+ 
  xlab("percentage") + ylab("Number") + labs(title = "The Quantile of the Airline Number")

##By the quntile, we can find the busier and less busy airlines
divert1998 <- divert1998[order(divert1998[,"Number"]),]
divert2002 <- divert2002[order(divert2002[,"Number"]),]
dvt98mb <- divert1998[-c(1:max(which(divert1998[,"Number"]<5203))),] 
dvt02mb <- divert2002[-c(1:max(which(divert1998[,"Number"]<4710))),] 

###Plot the Net on map
##The greneral situation in 1998
id <- table(divert1998$From)
bid <- names(id)[id >10]
airports1998 <- airport[airport$iata %in% bid,]
flight1998 <- divert1998[divert1998$From %in% bid &
                           divert1998$To %in% bid, ]
airports1998[,"iata"] <- factor(airports1998[,"iata"])

##create basemap
library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports1998$long,airports1998$lat, pch=3, cex=0.5, col="orange")
## install.packages("geosphere")
library(geosphere)
## add lines on the map
for(i in 1:nrow(flight1998)) {
  node1 <- airports1998[as.character(airports1998$iata) == as.character(flight1998[i,]$From),]
  node2 <- airports1998[as.character(airports1998$iata) == as.character(flight1998[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*flight1998[i,]$Number / max(flight1998$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}



##The general situation in 2002
id <- table(divert2002$From)
bid <- names(id)[id >10]
airports2002 <- airport[airport$iata %in% bid,]
flight2002 <- divert2002[divert2002$From %in% bid &
                           divert2002$To %in% bid, ]
airports2002[,"iata"] <- factor(airports2002[,"iata"])
##create basemap
library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports2002$long,airports2002$lat, pch=3, cex=0.5, col="orange")
library(geosphere)
## add lines on the map
for(i in 1:nrow(flight2002)) {
  node1 <- airports2002[as.character(airports2002$iata) == as.character(flight2002[i,]$From),]
  node2 <- airports2002[as.character(airports2002$iata) == as.character(flight2002[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*flight2002[i,]$Number / max(flight2002$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

###The more busy flights in 1998
airports98mb <- airport[airport$iata %in% union(dvt98mb[,"From"], dvt98mb[,"To"]),]
airports98mb[,"iata"] <- factor(airports98mb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports98mb $long,airports98mb $lat, pch=3, cex=0.5, col="orange")
library(geosphere)
## add lines on the map
for(i in 1:nrow(dvt98mb)) {
  node1 <- airports98mb[as.character(airports98mb$iata) == as.character(dvt98mb[i,]$From),]
  node2 <- airports98mb[as.character(airports98mb$iata) == as.character(dvt98mb[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*dvt98mb[i,]$Number / max(dvt98mb$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

##The more busy case in 2002
airports02mb <- airport[airport$iata %in% union(dvt02mb[,"From"], dvt02mb[,"To"]),]
airports02mb[,"iata"] <- factor(airports02mb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports02mb$long,airports02mb$lat, pch=3, cex=0.5, col="orange")
library(geosphere)

## add lines on the map
for(i in 1:nrow(dvt02mb)) {
  node1 <- airports02mb[as.character(airports02mb$iata) == as.character(dvt02mb[i,]$From),]
  node2 <- airports02mb[as.character(airports02mb$iata) == as.character(dvt02mb[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*dvt02mb[i,]$Number / max(dvt02mb$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}
