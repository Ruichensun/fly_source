setwd("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter")

combine_flyCSV <- function(experimenter, type){
  all_info = NULL;
  input_files = c()
  for (i in 1:length(experimenter)){
    filename = paste0("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/", 
                      experimenter[i], "/", type[1], "/CSV/Behavioral Experiments - ", type[2], "_", experimenter[i], ".csv")
    input_files = c(input_files, filename)
  }
  
  output_file = paste0("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/fly_info_", type[3], ".csv")
  for(i in 1:length(input_files)){
    info = read.csv(input_files[i],header=T,stringsAsFactors=F)
    info$experimenter = experimenter[i]
    info$Fly_Exp = paste(info$Fly,experimenter[i],sep='_')
    all_info = rbind(all_info,info)
  }
  write.table(all_info,
              output_file,
              quote=F,row.names=F,col.names=T,sep=",")  
}

#Mutants
type = c("Mutants","Mutants", "mutants")
experimenter <- c("JD", "SW", "JG", "ES", "XC")
combine_flyCSV(experimenter, type)

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
