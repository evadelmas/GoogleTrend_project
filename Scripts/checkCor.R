library(gtrendsR)
library(getPass)
library(lubridate)
library(rebird)
library(dplyr)

#Connect to your Google account to be able to access the google trends
mail_adress <- "your.email@adress.ca"
user_id <- gconnect(mail_adress, getPass::getPass())

#Select for wich species you want to do the analysis abd from when
species <- c("American Robin")
start_date <- "2006-01-01 EDT"

trends <- get_trends(species, start_date)
birds <- get_ebird(species, start_date)
plot_trend(trends, birds, species)
