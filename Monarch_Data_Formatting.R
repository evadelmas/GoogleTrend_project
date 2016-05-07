#Reads in monarch data, and standardizes observations by the maximum of that year. 
library(lubridate)

mon.data <- read.csv("monarch2006-2015.csv")
mon.data$date <- ymd(mon.data$date)
  
year_maxs <- aggregate(data = mon.data, count ~ year, FUN = max)

#taking the frequency of butterfly observations with each standardized by year.
mon.freq <- vector()
for(i in 1:10){
  mon.freq1 <- (mon.data[mon.data$year == year_maxs[i,1],]$count)/year_maxs[i,2]
  mon.freq <- c(mon.freq, mon.freq1)
}
mon.data$year_freq <- mon.freq
