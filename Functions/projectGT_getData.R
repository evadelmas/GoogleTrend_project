source("Functions/week_starts.R")

get_trends <- function(species_name, start_date, states = "US"){
  #for (i in 1:length(species_name)) {
    #keyword <- species[i]
    keyword <- species_name
    res <- gtrends(keyword, geo = states)
    res$trend <- res$trend[
      (res$trend$start > "2006-01-01 EDT") &
        (res$trend$start < "2016-01-01 EDT"),
      ]
  #}
  fname <- paste("outputs/GT_", keyword, "_from", strsplit(start_date, split = " ")[[1]][1], sep="")
  save(res, file = fname)
  return(res)
}

get_ebird <- function(species_name, start_date){
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
    bird_amer <- bird_test %>% filter(comName == species_name)
    year_vec <- rep(i, dim(bird_amer)[1])
    bird_amer2 <- cbind(bird_amer, year_vec)
    bird_df <- rbind(bird_df,
                     bird_amer2)
  }

  bird_df$month <- sapply(strsplit(as.character(bird_df$monthQt), "-"), "[[", 1)

  bird_bac <- bird_df

  #formatting the dates for the bird stuff
  # day of the year vector --------------------------------------------------

  week_starts2 <- rep(week_starts, length(2006:2015))
  bird_df2 <- cbind(bird_df, week_starts2)

  bird_df$date <- paste(bird_df2$week_starts2, bird_df2$year_vec, sep = "/")
  bird_df$date <- mdy(bird_df$date)
  fname <- paste("outputs/obsdata_", species_name, "_from", strsplit(start_date, split = " ")[[1]][1], sep= "")
  obs_data <- bird_df
  save(obs_data, file = fname)
  return(bird_df)
}

plot_trend <- function(google_trends, obs_data, IsLowess = TRUE, species_name){
      res <- google_trends

      x11()
      par(mfrow=c(2, 1))

      plot(
        res$trend[[1]],
        res$trend[[3]],
        type = "l",
        col = "gray",
        main = species_name,
        xlab = "time",
        ylab = "frequency"
      )
      if(IsLowess == T) {
        smoothed <- lowess(res$trend[[1]],
                           res$trend[[3]],
                           f = .06)
        lines(smoothed,col = "black")
      }

      plot(obs_data$date,
        obs_data$frequency,
        type = "l",
        col = "gray",
        xlab = "time",
        ylab = "frequency"
      )

      if(IsLowess == T) {
        obs_trend <- lowess(obs_data$date,
                      obs_data$frequency,
                      f = 0.06)
        lines(obs_trend, col = "black")
      }
}

get_gbif <- function(start_date, species_name, states = "US", common_name){
  start_d <- as.numeric(strsplit(start_date, split = "-")[[1]][1])
  years <- c(start_d:2016)
  months <- c(1:12)
  all_data <- data.frame()
  for (y in years){
    raw_data <- occ_search(species = species_name, country = states, year = y)
    sub_data <- data.frame(cbind(raw_data$data$decimalLatitude, raw_data$data$decimalLongitude, raw_data$data$year, raw_data$data$month, raw_data$data$day))
    names(sub_data) <- c("Lat", "Long", "year", "month", "day")
    all_data <- rbind(all_data, sub_data)
  }
  all_data$date <- apply(all_data[, c(3, 4, 5)], 1, paste, collapse = "/")
  all_data <- subset(all_data, all_data$day !=0)
  all_data$date <- ymd(all_data$date)
  all_data$count <- 1
  obs_counts <- tapply(all_data$count, all_data$date, sum)
  obs_data <- data.frame(dates = names(obs_counts), count = obs_counts)
  rownames(obs_data) <- NULL
  obs_data$yr <- year(obs_data[,1])
  year_maxs <- aggregate(data = obs_data, count ~ yr, FUN = max)
  count_to_freq <- vector()
  for(i in 1:nrow(year_maxs)){
    count_to_freq1 <- (obs_data[obs_data$yr == year_maxs[i,1],]$count)/year_maxs[i,2]
    count_to_freq <- c(count_to_freq, count_to_freq1)
  }
  obs_data$frequency <- count_to_freq
  fname <- paste("outputs/obsdata_", common_name, "_from", strsplit(start_date, split = " ")[[1]][1], sep= "")
  save(obs_data, file = fname)
  return(obs_data)
}

build_time_series <- function(google_trends, ebird_data){
  smooth_gtrends <- smooth.spline(
    x = google_trends$trend$start,
    y = google_trends$trend[[3]],
    cv = TRUE
  )

  smooth_ebird <- smooth.spline(
    x = ebird_data$date,
    y = ebird_data$frequency,
    cv = TRUE
  )

  time_series <- data.frame(x = smooth_gtrends$x)

  time_series[,2] <- scale(smooth_gtrends$y)
  time_series[,3] <- scale(predict(smooth_ebird,data.frame(x = smooth_gtrends$x))$y)

  return(time_series)
}
