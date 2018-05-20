setwd("C:/Users/Ruichen/Desktop/data/csvdata")


##Import the function for obtaining statistics of each experiment
source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
library(Hmisc)

separating_training_test <- function(fly_number, framerate){
  
  test_file_number = c();
  test_files = c();
  training_file_number = c();
  training_files = c();
  
  files = list()
  
  fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
  
  test_file_number = grep(pattern=("E1_|E2_|E3_"),fly_files)
  for (i in 1:length(test_file_number)){
    test_files[i] <- fly_files[as.numeric(test_file_number[i])]
  }
  
  training_file_number = grep(pattern=("T1_|T2_|T3_"),fly_files)
  for (i in 1:length(training_file_number)){
    training_files[i] <- fly_files[as.numeric(training_file_number[i])]
  }
  
  files <- list(test_files, training_files);
  return(files)
  
}