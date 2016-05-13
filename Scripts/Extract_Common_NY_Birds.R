rm(list = ls())

library(rebird)

# Extract everything from june and july in the last 15 years
h <- ebirdfreq(loctype = "states",
                       startyear = 2000,
                       endyear = 2015,
                       startmonth = 6,
                       endmonth = 7,
                       loc = "US-NY")

# Compute average frequency per species
sorted_h <- aggregate(
  list(frequency = h$frequency),
  by = list(comName = h$comName),
  FUN = mean
)

sorted_h <- sorted_h[order(sorted_h$frequency,decreasing = TRUE),]

head(
  sorted_h$comName,
  n = 100
)
