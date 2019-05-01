#This program creates a streamgraph by carrier for each month in the two years we observed
#To see the output of this program must be put in an Rmd file and an html file must be generated
library(dplyr)
library(streamgraph)
setwd("~/Stat480/RDataScience/GroupProject")



#Streamgraph by carrier for the year 1998 
ninetyeight = read.csv("1998.csv")
alldata = as.data.frame(ninetyeight)
alldata$Year <- as.factor(alldata$Year)
alldata$Month <- as.factor(alldata$Month)
data <- aggregate(ArrDelay ~ Month + UniqueCarrier, alldata[alldata$ArrDelay > 0,], sum)
data$Month <- paste("1998",data$Month,"01",sep = '-')
streamgraph(data, "UniqueCarrier", "ArrDelay", "Month", interactive=TRUE) %>%
  sg_fill_brewer("Spectral")%>%
  sg_title("Streamgraph of 1998")


#Streamgraph by carrier for the year 2002
twothousand = read.csv("2002.csv")
alldata = as.data.frame(twothousand)
alldata$Year <- as.factor(alldata$Year)
alldata$Month <- as.factor(alldata$Month)
data <- aggregate(ArrDelay ~ Month + UniqueCarrier, alldata[alldata$ArrDelay > 0,], sum)
data$Month <- paste("2002",data$Month,"01",sep = '-')
streamgraph(data, "UniqueCarrier", "ArrDelay", "Month", interactive=TRUE) %>%
  sg_fill_brewer("Spectral")%>%
  sg_title("Streamgraph of 2002")


