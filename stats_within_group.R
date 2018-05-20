setwd("C:/Users/Ruichen/Desktop/data/csvdata/")

##Import the function for obtaining statistics of each experiment
source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
source("normalization_against_E1.R")
source("fly_group_stats.R")
library(Hmisc)

stats_within_group <- function (flies,framerate){
  normflies = fly_group_stats(flies,framerate)
  
  sum = 0
  n = 0
  a=list()
  for (i in 1:6){
    for (j in 1:length(flies)){
      for (e in 1:length(normflies[[j]])){
        for (t in 1:length(normflies[[j]][[e]][[i]])){
          a[[i]][[e]][[j]][[t]]<-normflies[[j]][[e]][[i]][[t]]
        }
      }
    }
  }
  
  return(a)
}
  