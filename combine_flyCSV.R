setwd("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

#Mutants
type = c("Mutants","Mutants", "mutants")
experimenter <- c("JD", "SW", "JG", "ES", "XC")
combine_flyCSV(experimenter, type)
print("FlyMutantsREADY")
#CS
type = rep("CS", 3)
experimenter <- c("ES", "RS", "JD","SW")
combine_flyCSV(experimenter, type)

#Response
type = c("Response","Response", "response")
experimenter <- c("JD", "ES")
combine_flyCSV(experimenter, type)

#Constant_time
type = rep("CS_constant",3)
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

#CS - Controls1
type = c("CS_controls", "CS_controls1", "CS_controls1")
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

#CS - Controls2
type = c("CS_controls", "CS_controls2", "CS_controls2")
experimenter <- c("JD")
combine_flyCSV(experimenter, type)
