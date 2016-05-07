library(gtrendsR)
library(getPass)
library(lubridate)
library(rebird)
library(dplyr)
library(rgbif)

source("Functions/projectGT_getData.R")

#Connect to your Google account to be able to access the google trends
mail_adress <- "your.email@adress.ca"
user_id <- gconnect(mail_adress, getPass::getPass())

#Select for wich species you want to do the analysis abd from when
species <- c("American Robin")
startdate <- "2006-01-01 EDT"

trends <- get_trends(species, stardate)
birds <- get_ebird(species, startdate)
plot_trend(trends, birds, species, IsLowess = TRUE)

monarch_species <- c("Danaus plexippus")
trends_monarch <- get_trends(monarch_species, startdate)
data_monarch <- get_gbif(startdate, monarch_species[1])
