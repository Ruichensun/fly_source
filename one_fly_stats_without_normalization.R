setwd("C:/Users/Ruichen/Desktop/data/csvdata")


##Import the function for obtaining statistics of each experiment
source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
library(Hmisc)

one_fly_stats_without_normalization <- function(fly_number, framerate){
  
  fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
  fly_files = fly_files[grep("T1_|R1_",fly_files,invert = T)]
  
  ##Figure E1X1E1 pattern        
  TRNpattern = basename(fly_files)
  TRNpattern_split = strsplit(TRNpattern,"_F_|_M_")
  TRNpattern = rep("",length(fly_files))
  for(i in 1:length(fly_files)){    
    TRNpattern[i] = gsub(paste0('ProcessedData_Fly',fly_number,"_"),"",TRNpattern_split[[i]][1])  
  }
  
  fly_metrics = list()
  for(i in 1:length(fly_files)){
    dat = one_fly_segmented_statistics(fly_files[i],framerate)
    fly_metrics = append(fly_metrics, list(dat))
  }
  
  
  names(fly_metrics) = TRNpattern
  #paste0("FLY",fly_number,"_",TRNpattern)
  return(fly_metrics)
}
