
#Data preparation=================================================================================================
# refrence https://kateto.net/wp-content/uploads/2018/06/Polnet%202018%20R%20Network%20Visualization%20Workshop.pdf
data <- read.csv("groupprojectcl.csv")
airport <- read.table("airports.csv",header=T, sep=',')
airport <- airport[which(airport$iata %in% union(data$Origin,data$Dest)),]

library(reshape2)
divert <- melt(table(data$Year,data$Origin,data$Dest))
divert <- divert[which(divert$value!=0),]
colnames(divert) <-c("Year","From","To","Number")

divert1998 <- divert[which(divert$Year== "1998"),]
divert2002 <- divert[which(divert$Year== "2002"),]

#Select the important airports=======================================
## Plot the quantile of the two years
myProbs <- seq(0, 1, by=0.05)
q1998 <- quantile(divert1998[,"Number"],myProbs,na.rm=TRUE)
q2002 <- quantile(divert2002[,"Number"],myProbs,na.rm=TRUE)
divertq <-as.matrix(cbind(q1998,q2002), rownames=myProbs)
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
dvt98lb <-divert1998[c(1:max(which(divert1998[,"Number"]<3601))),] 
dvt02lb <-divert2002[c(1:max(which(divert1998[,"Number"]<3601))),] 
dvt98mb <-divert1998[-c(1:max(which(divert1998[,"Number"]<5203))),] 
dvt02mb <-divert2002[-c(1:max(which(divert1998[,"Number"]<4710))),] 

#Plot the NetWork========================================
## horizontal line ==========================================
library(igraph)
routes_igraph <- graph_from_data_frame(d = divert1998, vertices = nodes, directed = TRUE)
install.packages("ggraph")
#library(ggraph)
ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = Number), alpha = 0.8) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = name)) +
  labs(edge_width = "Letters") +
  theme_graph()

net <- graph_from_data_frame(d=divert[,c("From","To","Number")], vertices=nodes[,1],directed=TRUE) 
summary(net)

#In-class
inst_pkgs = load_pkgs =  c("network","sna", "ergm", "ggplot2")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]

library(network)
net <- network(divert[,c("From","To")], loops = FALSE,matrix.type="edgelist")
plot(net, displaylabels = FALSE, boxed.labels = FALSE)
airport <- union(divert[,"From"], divert[,"To"])
odes <- read.csv("airports.csv")
net <- graph_from_data_frame(d=divert[,c("Var2","Var3")],directed=F) 
plot(net, vertex.size	=5,label)


# library(network)
# routes_network <- network(divert1998, vertex.attrnames = nodes, matrix.type = "edgelist", ignore.eval = FALSE)


#Plot the net on map for the less busy airlines in the two years=========================================================
#reference https://www.gis-blog.com/flight-connection-map-with-r/
##generate a color gradient 
library(grDevices)
col.1 <- adjustcolor("turquoise2", alpha=0.4)
col.2 <- adjustcolor("orange", alpha=0.4)
edge.pal <- colorRampPalette(c(col.1, col.2), alpha = TRUE)
edge.col <- edge.pal(100)

###gernerally-----Two years
##create basemap
library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airport$long,airport$lat, pch=3, cex=0.5, col="orange")
## install.packages("geosphere")
library(geosphere)
## add lines on the map
for(i in 1:nrow(divert)) {
  node1 <- airport[as.character(airport$iata) == as.character(divert[i,]$From),]
  node2 <- airport[as.character(airport$iata) == as.character(divert[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*divert[i,]$Number / max(divert$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

##1998-------general
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

###1998-------less busy
##create basemap
airports98lb <- airport[airport$iata %in% union(dvt98lb[,"From"], dvt98lb[,"To"]),]
airports98lb[,"iata"] <- factor(airports98lb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1,  ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(dvt98lb$long,dvt98lb$lat, pch=3, cex=0.5, col="orange")

## install.packages("geosphere")
library(geosphere)
## add lines on the map
for(i in 1:nrow(dvt98lb)) {
  node1 <- airports98lb[as.character(airports98lb$iata) == as.character(dvt98lb[i,"From"]),]
  node2 <- airports98lb[as.character(airports98lb$iata) == as.character(dvt98lb[i,"To"]),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*dvt98lb[i,]$Number / max(dvt98lb$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

###1998-------more busy
airports98mb <- airport[airport$iata %in% union(dvt98mb[,"From"], dvt98mb[,"To"]),]
airports98mb[,"iata"] <- factor(airports98mb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports98mb $long,airports98mb $lat, pch=3, cex=0.5, col="orange")
## install.packages("geosphere")
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


###2002------ general
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
## install.packages("geosphere")
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


###2002-------less busy
airports02lb <- airport[airport$iata %in% union(dvt02lb[,"From"], dvt02lb[,"To"]),]
airports02lb[,"iata"] <- factor(airports02lb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(dvt02lb$long,dvt02lb$lat, pch=3, cex=0.5, col="orange")
## install.packages("geosphere")
library(geosphere)
## add lines on the map
for(i in 1:nrow(dvt02lb)) {
  node1 <- airports02lb[as.character(airports02lb$iata) == as.character(dvt02lb[i,]$From),]
  node2 <- airports02lb[as.character(airports02lb$iata) == as.character(dvt02lb[i,]$To),]
  arc <- gcIntermediate( c(node1[1,]$long, node1[1,]$lat),
                         c(node2[1,]$long, node2[1,]$lat),
                         n=1000, addStartEnd=TRUE )
  edge.ind <- round(100*dvt02lb[i,]$Number / max(dvt02lb$Number))
  lines(arc, col=edge.col[edge.ind], lwd=edge.ind/30)
}

###2002-------more busy
airports02mb <- airport[airport$iata %in% union(dvt02mb[,"From"], dvt02mb[,"To"]),]
airports02mb[,"iata"] <- factor(airports02mb[,"iata"])

library(maps)
map("world", regions=c("usa"), col="grey20", fill=TRUE, bg="black", lwd=0.1, ylim=c(15.0,75.0), xlim=c(-169.0,-63.0))
points(airports02mb$long,airports02mb$lat, pch=3, cex=0.5, col="orange")
## install.packages("geosphere")
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

