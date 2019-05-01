setwd("~/Stat480/RDataScience/GroupProject")
ninetyeight = read.csv("1998.csv") #data from the year 1998
twothousand = read.csv("2002.csv") #data from the year 2002


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


#Creating treemaps for each carrier and its arrival or departure delays
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

airportsort(ninetyeight)
airportsort(twothousand)


#Determining the percentage of delayed flights per year for each unique carrier
carriers1998 = as.character(unique(ninetyeight$UniqueCarrier))
carriers2002 = as.character(unique(twothousand$UniqueCarrier))
arrdelaypercent = function(x,y)
{
  for(carrier in x)
  {
  totalflights = length(which(y$UniqueCarrier == carrier))
  delayedflights = length(which(y$UniqueCarrier == carrier & y$ArrDelay > 1))
  cat(carrier,delayedflights/totalflights,"\n")
  }
}

depdelaypercent = function(x,y)
{
  percents = c()
  for(carrier in x)
  {
    totalflights = length(which(y$UniqueCarrier == carrier))
    delayedflights = length(which(y$UniqueCarrier == carrier & y$DepDelay > 1))
    cat(carrier,delayedflights/totalflights,"\n")
    
  }
}
arrdelaypercent(carriers1998,ninetyeight)
arrdelaypercent(carriers2002,twothousand)
depdelaypercent(carriers1998,ninetyeight)
depdelaypercent(carriers2002,twothousand)
airportsort(ninetyeight)
airportsort(twothousand)

