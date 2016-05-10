rm(list=ls())

library(gtrendsR)
library(getPass)
library(lubridate)
library(rebird)
library(dplyr)
library(rgbif)

source("Functions/projectGT_getData.R")

#Connect to your Google account to be able to access the google trends
mail_adress <- "eva.delmas@gmail.com"
user_id <- gconnect(mail_adress, getPass::getPass())

startdate <- "2006-01-01 EDT"

## Extract & save birds data from e-bird
species <- c("American Robin",
             "Blue Jay"
            )

for (sp in species){
  trends <- get_trends(sp, startdate)
  birds <- get_ebird(sp, startdate)
  plot_trend(trends, birds, IsLowess = TRUE, sp)
}

## Extract & save the obs. data for other species from gbif
sciname_species <- c("Danaus plexippus",
                     "Antilocapra americana")
comname_species <- c("monarch butterfly",
                     "pronghorn antelope")

for (sp in 1:length(sciname_species)){
  sci <- sciname_species[sp]
  common <- comname_species[sp]
  trends <- get_trends(common, startdate)
  obs_data <- get_gbif(startdate, sci, common_name = common)
  plot_trend(trends, obs_data, IsLowess = TRUE, common)
}

species_list <- c(species, comname_species)
save(species_list, file = "outputs/list_species")
