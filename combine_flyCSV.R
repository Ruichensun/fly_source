setwd("E:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter")
source("E:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
library(zoo)

#Mutants
type = c("Mutants","Mutants", "mutants")
experimenter <- c("JD", "SW", "ES", "XC", "RS", "LW")
combine_flyCSV(experimenter, type)

#CS
type = rep("CS", 3)
experimenter <- c("ES", "RS", "JD", "SW")
combine_flyCSV(experimenter, type)

