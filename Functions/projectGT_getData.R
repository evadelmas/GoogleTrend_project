<<<<<<< HEAD
get_trends <- function(species, start_date, states = "US"){
  #Get the google trends for a particular species as keyword for the states and years selected
=======
# push try
library(gtrendsR)
library(getPass)
library(lubridate)
library(rebird)
library(dplyr)

#Connect to your Google account to be able to access the google trends
mail_adress <- "your.email@adress.ca"
ed <- gconnect(mail_adress, getPass::getPass())

#Select for wich species you want to do the analysis abd from when
species <- c("American Robin")
start_date <- "2006-01-01 EDT"

get_trends <- function(species, start_date){
>>>>>>> b7ff4d60f09f67b7ed624deaaeafeefb22263761
  for (i in 1:length(species)) {
    keyword <- species[i]
    res <- gtrends(keyword, geo = states)
    res$trend <- res$trend[
      (res$trend$start > "2006-01-01 EDT") &
        (res$trend$start < "2016-01-01 EDT"),
      ]
  }
  return(res)
}

get_ebird <- function(species, start_date){
  year_start <- as.numeric(strsplit(start_date, split = "-")[[1]][1])
  bird_df <- data.frame(comName = character(),
                        monthQt = character(),
                        frequency = numeric(),
                        sampleSize = numeric(),
                        year = numeric())
  for(i in year_start:2015){
    bird_test <- ebirdfreq(loctype = "states",
                           startyear = i,
                           endyear = i,
                           startmonth = 1,
                           endmonth = 12,
                           loc = "US-NY") %>% as.data.frame()
    bird_amer <- bird_test %>% filter(comName == species)
    year_vec <- rep(i, dim(bird_amer)[1])
    bird_amer2 <- cbind(bird_amer, year_vec)
    bird_df <- rbind(bird_df,
                     bird_amer2)
  }

  bird_df$month <- sapply(strsplit(as.character(bird_df$monthQt), "-"), "[[", 1)

  bird_bac <- bird_df

  #formatting the dates for the bird stuff
  # day of the year vector --------------------------------------------------
  source("week_starts.R")

  week_starts2 <- rep(week_starts, length(2006:2015))
  bird_df2 <- cbind(bird_df, week_starts2)

  bird_df$date <- paste(bird_df2$week_starts2, bird_df2$year_vec, sep = "/")
  bird_df$date <- mdy(bird_df$date)
  return(bird_df)
}

plot_trend <- function(google_trends, ebird_data, IsLowess = T, species){
      res <- google_trends
      bird_df <- ebird_data

      x11()
      par(mfrow=c(2, 1))

      plot(
        res$trend[[1]],
        res$trend[[3]],
        type = "l",
        col = "gray",
        main = species
      )
      if(IsLowess == T) {
        smoothed <- lowess(res$trend[[1]],
                           res$trend[[3]],
                           f = .06)
        lines(smoothed,col = "black")
      }

      plot(bird_df$date, bird_df$frequency)

      if(IsLowess == T) {
        bird_trend <- lowess(bird_df$date, bird_df$frequency, f = 0.06)
        lines(bird_trend)
      }
}
