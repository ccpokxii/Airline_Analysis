setwd("~/Stat480/RDataScience/AirlineDelays")
library(biganalytics)
air <-attach.big.matrix("airproj.desc")
year98 <- air[which(air[,"Year"] == 1998),]
year02 <- air[which(air[,"Year"] == 2002),]

Sun = which(year98[,"DayOfWeek"]==1)
Mon = which(year98[,"DayOfWeek"]==2)
Tues = which(year98[,"DayOfWeek"]==3)
Wed = which(year98[,"DayOfWeek"]==4)
Thurs = which(year98[,"DayOfWeek"]==5)
Fri = which(year98[,"DayOfWeek"]==6)
Sat = which(year98[,"DayOfWeek"]==7)


Jan = which(year98[,"Month"]==1)
Feb = which(year98[,"Month"]==2)
Mar = which(year98[,"Month"]==3)
Apr = which(year98[,"Month"]==4)
May = which(year98[,"Month"]==5)
Jun = which(year98[,"Month"]==6)
Jul = which(year98[,"Month"]==7)
Aug = which(year98[,"Month"]==8)
Sep = which(year98[,"Month"]==9)
Oct = which(year98[,"Month"]==10)
Nov = which(year98[,"Month"]==11)
Dec = which(year98[,"Month"]==12)

aveArrDelay = mean(year98[,"ArrDelay"],na.rm=TRUE)
aveDepDelay = mean(year98[,"DepDelay"],na.rm=TRUE)
percentiledep = quantile(year98[,"DepDelay"],na.rm=TRUE)
percentilearr = quantile(year98[,"ArrDelay"],na.rm=TRUE)

slices <- c(length(Jan),length(Feb),length(Mar),length(Apr),length(May),length(Jun),length(Jul),length(Aug),length(Sep),length(Oct),length(Nov),length(Dec))
lbls <- c("Jan","Feb","March","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
pie3D(slices, labels = lbls, main="Pie Chart of Delays by Day")


delayJan = which(air[,"Month"]==1 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayFeb = which(air[,"Month"]==2 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayMar = which(air[,"Month"]==3 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayApr = which(air[,"Month"]==4 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayMay = which(air[,"Month"]==5 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayJun = which(air[,"Month"]==6 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayJul = which(air[,"Month"]==7 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayAug = which(air[,"Month"]==8 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delaySep = which(air[,"Month"]==9 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayOct = which(air[,"Month"]==10 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayNov = which(air[,"Month"]==11 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)
delayDec = which(air[,"Month"]==12 & air[,"ArrDelay"] > 1 & air[,"DepDelay"] > 1)

delayed98 = length(which(year98[,"ArrDelay"] > 1 | year98[,"DepDelay"] > 1))
delayed02 = length(which(twothousand[,"ArrDelay"] > 1 | twothousand[,"DepDelay"] > 1))


