setwd("C:/Users/Ruichen/Desktop/data/csvdata")


##Import the function for obtaining statistics of each experiment
source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
library(Hmisc)

one_fly_stats <- function(fly_number, framerate){
  
  fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
  
  #plot original traces
  for (i in 1:length(fly_files)){
    #one_fly_segmented_statistics(fly_files[i],framerate)
    #get_fly_position(fly_files[i])
    #get_fly_pause_position(fly_files[i])
    output_prefix = basename(fly_files[i])
    output_prefix = gsub(".csv","",output_prefix)
    output_file = paste0("../pdfdata/",output_prefix,".pdf")
    smoothening(fly_files[i],framerate,output_file)
  }
  
  ##Figure E1X1E1 pattern        
  TRNpattern = basename(fly_files)
  TRNpattern_split = strsplit(TRNpattern,"_F_")
  TRNpattern = rep("",length(fly_files))
  for(i in 1:length(fly_files)){    
    TRNpattern[i] = gsub(paste0('ProcessedData_Fly',fly_number,"_"),"",TRNpattern_split[[i]][1])  
  }
  
  fly_metrics = list()
  for(i in 1:length(fly_files)){
    dat = one_fly_segmented_statistics(fly_files[i],framerate)
    fly_metrics = append(fly_metrics, list(dat))
  }
  
  normalized_fly_metrics = fly_metrics
  for (i in 1:length(fly_files)){
    for (j in 1:6){
      normalized_fly_metrics[[i]][[j]] = fly_metrics[[i]][[j]]/mean(fly_metrics[[1]][[j]], na.rm = T)
    }
  }
  names(normalized_fly_metrics) = TRNpattern
  #paste0("FLY",fly_number,"_",TRNpattern)
  return(normalized_fly_metrics)
}
