setwd("~/Stat480/RDataScience/GroupProject")
library(plotrix)
library(ggmap)
library(ggplot2)
data(state)
data(USArrests)
USArrests$State = state.abb
USArrests$Region = state.region
airports = read.csv("airports.csv")
states = state.abb
regions = state.region

southern = USArrests$State[which(USArrests$Region=="South")]
northeast = USArrests$State[which(USArrests$Region=="Northeast")]
west = USArrests$State[which(USArrests$Region=="West")]
northcentral = USArrests$State[which(USArrests$Region=="North Central")]
area = c()
for(i in 1:length(airports$state))
{
  if(airports[i,"state"] %in% southern)
  {
    area[i] = "South"
  }
  if(airports[i,"state"] %in% northeast)
  {
    area[i] = "Northeast"
  }
  if(airports[i,"state"] %in% west)
  {
    area[i] = "West"
  }
  if(airports[i,"state"] %in% northcentral)
  {
    area[i] = "North Central"
  }
}

airports$region = area
write.csv(airports,"airportregions.csv")

freqtable = table(airports$region)
statetable = sort(table(airports$state),decreasing = TRUE)
df = as.data.frame.table(freqtable)
df1 = as.data.frame.table(statetable)
colnames(df) = c("Region","Frequency")
colnames(df1) = c("State","Frequency")


#Bar and Pie Charts

bar = barplot(statetable[1:20],xlab = "State/Territories",ylab = "Number of Airports
        ", main = "Number of Airports Per State",col = heat.colors(20))
text(x = bar, y = df1$Frequency[1:20], labels = df1$Frequency[1:20],col = "black",pos = 1)


# Create a basic bar
pie = ggplot(df, aes(x="", y=Frequency, fill=Region)) + geom_bar(stat="identity", width=1)

# Convert to pie (polar coordinates) and add labels
pie = pie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round((Frequency/sum(df$Frequency))*100), "%")), position = position_stack(vjust = 0.5))

# Add color scale (hex colors)
pie = pie + scale_fill_manual(values=c("#800080", "#003366", "#ff0000", "#FFFF00")) 

# Remove labels and add title
pie = pie + labs(x = NULL, y = NULL, fill = NULL, title = "Percentage of Total Airports by Region")

# Tidy up the theme
pie = pie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
