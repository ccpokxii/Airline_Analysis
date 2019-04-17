setwd("~/Stat480/RDataScience/AirlineDelays")
ninetyeight = read.csv("1998.csv")
twothousand = read.csv("2002.csv")


airportsort <- function(year)
  
  {
  
orig = unique(sort(year$Origin))
dest = unique(sort(year$Dest))

library(hash)
origlist = hash()
destlist = hash()
for(i in 1:length(orig))
{
  destlist[dest[i]] = length(which(year$Dest == dest[i] & year$ArrDelay > 1))
  origlist[orig[i]] = length(which(year$Origin == orig[i] & year$DepDelay > 1))
}
destvals = sort(values(destlist),decreasing = TRUE)
origvals = sort(values(origlist),decreasing = TRUE)
barplot(origvals[1:20],main = "Plot of Airports with Most Departure Delays",col=heat.colors(20))
barplot(destvals[1:20], main = "Plot of Airports with Most Arrival Delays",col = heat.colors(20))
}

library(treemap)
treemap(ninetyeight,
        index="UniqueCarrier",
        vSize="ArrDelay",
        vColor="Distance",
        title = "Carrier Arrival Delays for the Year 1998",
        type="value")

treemap(ninetyeight,
        index="UniqueCarrier",
        vSize="DepDelay",
        vColor="Distance",
        title = "Carrier Departure Delays for the Year 1998",
        type="value")

treemap(twothousand,
        index="UniqueCarrier",
        vSize="ArrDelay",
        vColor="Distance",
        title = "Carrier Arrival Delays for the Year 2002",
        type="value")

treemap(twothousand,
        index="UniqueCarrier",
        vSize="DepDelay",
        vColor="Distance",
        title = "Carrier Departure Delays for the Year 2002",
        type="value")

