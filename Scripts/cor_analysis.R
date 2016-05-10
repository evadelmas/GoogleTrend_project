rm(list=ls())
source("Functions/projectGT_getData.R")

load("outputs/list_species")
Google_trends <- list.files(path = "outputs/", pattern = "GT")
Obs <- list.files(path = "outputs/", pattern = "obsdata")

## Check the corelation between the Gtrend and observation time_series

for (sp in species_list){
  i <- grep(sp, Google_trends)
  j <- grep(sp, Obs)
  load(paste("outputs/", Google_trends[i], sep ="")) #res
  load(paste("outputs/", Obs[i], sep ="")) #obs_data
  ts <- build_time_series(res, obs_data)
}
