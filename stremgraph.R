setwd("~/project")

# Any package that is required by the script below is given here:
inst_pkgs = load_pkgs =  c("ggplot2","ggplot2movies", "dplyr","babynames","data.table","Rcpp","devtools")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)

git_pkgs = git_pkgs_load = c("streamgraph","DT")

git_pkgs = git_pkgs[!(git_pkgs %in% installed.packages()[,"Package"])]

if(length(git_pkgs)){
  library(devtools)
  install_github('rstudio/DT')
  install_github('hrbrmstr/streamgraph')
}

load_pkgs = c(load_pkgs, git_pkgs_load)

# Dynamically load packages
pkgs_loaded = lapply(load_pkgs, require, character.only=T)


#1-The streamgraph of the daily number of flights in these two years
day.n <-as.data.frame(read.csv("count_day.csv",sep= "\t", header = FALSE))
colnames(day.n) <- c("Year","Month","DayofMonth","Number")
day.n$date <- as.Date(paste(day.n$Month, day.n$DayofMonth),format = "%m%d")
streamgraph(day.n, "Year", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

#2-The distribution of the daily number of flights in these two years
ggplot(day.n, aes(x=Number)) + 
  geom_density(aes(group=as.factor(Year), colour=as.factor(Year), fill=as.factor(Year)), position="stack", alpha=0.6)


#3-he daily number of flights for different carrier in two years.
day.carrier.n <- as.data.frame(read.csv("count_carrier_day.csv",sep= "\t", header = FALSE))
colnames(day.carrier.n) <- c("Year","Month","DayofMonth","Number","Carrier")
day.carrier.n$date <- as.Date(paste(day.n$Year,day.n$Month, day.n$DayofMonth, sep = '-'))
flights1998 <- day.carrier.n[which(day.carrier.n["Year"] == "1998"),]
flights2002 <- day.carrier.n[which(day.carrier.n["Year"] == "2002"),]

streamgraph(flights1998, "Carrier", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

streamgraph(flights2002, "Carrier", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

#4-The daily number of flights for different airport in two years.
day.airport.n <- as.data.frame(read.csv("count_origin_day.csv",sep= "\t", header = FALSE))
colnames(day.airport.n) <- c("Year","Month","DayofMonth","Origin","Number")
day.airport.n$date <- as.Date(paste(day.airport.n$Year,day.airport.n$Month, day.airport.n$DayofMonth, sep = '-'))
airport1998 <- day.airport.n[which(day.airport.n["Year"] == "1998"),]
airport2002 <- day.airport.n[which(day.airport.n["Year"] == "2002"),]

streamgraph(airport1998, "Origin", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

streamgraph(airport2002, "Origin", "Number", "date") %>%
  sg_fill_brewer("Spectral") %>%
  sg_axis_x(tick_units = date)

#5-The monthly number of flights in two years
f1998<-day.n[which(day.n["Year"] == "1998"),]
f2002<-day.n[which(day.n["Year"] == "2002"),]
month<-matrix(0,12,2)
colnames(month)<- c("1998","2002")
month[,"1998"] <-tapply(f1998$Number,f1998$Month, sum)
month[,"2002"] <-tapply(f2002$Number,f2002$Month, sum)
month <- melt(month)
ggplot(data=month,aes(x=as.factor(Var1),y=value,group=factor(Var2), col=factor(Var2)))+
  geom_line(size=2)

#6-The basic netplot for different airport
#In-class
inst_pkgs = load_pkgs =  c("network","sna", "ergm", "ggplot2")
inst_pkgs = inst_pkgs[!(inst_pkgs %in% installed.packages()[,"Package"])]
if(length(inst_pkgs)) install.packages(inst_pkgs)
library(reshape2)
divert <- melt(table(data$Year,data$Origin,data$Dest))
divert <- divert[which(divert$value!=0),]
net <- network(divert[,c("Var2","Var3")], loops = FALSE,matrix.type="edgelist")
plot(net, displaylabels = FALSE, boxed.labels = FALSE)

#gif
library(networkD3)
simpleNetwork(Data=divert[,c("Var2","Var3")])

library(igraph)
nodes <- read.csv("airports.csv")
net <- graph_from_data_frame(d=divert[,c("Var2","Var3","value")], vertices=nodes[,c(1,2)],directed=F) 
summary(netZ)

airport <- unique(c(divert[,"Var2"], divert[,"Var3"]))
odes <- read.csv("airports.csv")
net <- graph_from_data_frame(d=divert[,c("Var2","Var3")],directed=F) 
plot(net, vertex.size	=5)

plot(net, vertex.shape="none", vertex.label=V(net), 
     
     vertex.label.font=2, vertex.label.color="gray40",
     
     vertex.label.cex=.7, edge.color="gray85" )





#basic2-The number of flights hourly in the two years
data <- read.csv("groupprojectcl.csv")
data$hour <- floor(data[,"CRSDepTime"]/100) 
data[which(data$hour==24),"hour"] <- 0
hour<-melt(table(data$hour,data$Year))
ggplot(data=hour,aes(x=as.factor(Var1),y=value,group=factor(Var2), col=factor(Var2)))+
  geom_line(size=1)+geom_point(size=2)+
  labs(x="Hour",y="Number of Flights")


