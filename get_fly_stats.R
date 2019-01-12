# source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/prepoccess.R")
setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

metrices = c(
  "Number of Pause Starts", #1
  "Fraction Time in Pause", #2
  "Average Pause Duration", #3
  "Max Pause Duration", #4 
  "Average Moving Speed ", #5
  "Average Moving Speed (excluding pause)", #6
  "Average Speed When Enter Pause", #7
  "Average Speed When Exit Pause",#8
  "Moving Distance Per Minute",#9
  "Number of Turns",#10
  "Number of Middle Turns",#11
  "Fration of Middle Turns Out of Total Turns",#12
  "Burstiness (Pause)",#13
  "Burstiness (Inter Event Time)",#14
  "Burstiness (Scrambled)",#15
  "Burstiness (Walking bouts-thresholding)",#16
  "Burstiness (Walking events-thresholding)",#17
  "Beginning Pause Duration",#18 
  "First Pause Duration",#19 
  "Transition Probability (Pause not at the end): Pause to Pause",#20
  "Transition Probability (Pause not at the end): Pause to Walking",#21
  "Transition Probability (Pasue not at the end): Walking to Walking",#22
  "Transition Probability (Pause not at the end): Walking to Pause",#23
  "Memory",#24
  "Memory (inverted)",#25
  "Burstiness of Start of Walking (Pause not at the end)",#26 *
  "Burstiness of Start of Pause (Pause not at the end)",#27 *
  "Average Pause Duration (Pause not at the End)",#28
  "Fraction Time in Pause (Pause not at the End)",#29 
  "Max Pause Duration (Pause not at the End)", #30
  "First Pause Duration (Pause not at the End)" #31
)

sessions <- c(
  
  "E1",
  "E1T1E1",
  "E1T1E1T1E1",
  "E1T1E1T1E1T1E1",
  "E1T1E1T1E1T1E1T1E1",
  
  "E1T1E1T1E1T2E1",
  "E1T1E1T1E1T2E1T2E1",
  
  "E1T2E1",
  "E1T2E1T2E1",
  "E1T2E1T2E1T1E1",
  "E1T2E1T2E1T1E1T1E1",
  
  "E1R1E1",
  "E1R1E1R1E1",
  "E1R1E1R1E1R1E1",
  "E1R1E1R1E1R1E1R1E1",
  
  "E1N1E1",
  "E1N1E1N1E1",
  "E1N1E1N1E1N1E1",
  "E1N1E1N1E1N1E1N1E1",
  
  "E1T1E1R1E1",
  "E1T1E1R1E1R1E1",
  "E1T1E1T1E1R1E1R1E1",
  
  "E1T1E1N1E1",
  "E1T1E1N1E1N1E1",
  "E1T1E1T1E1N1E1N1E1"
)
sessions = unique(sessions)
query.sessions = sessions

count = 0
spec = NULL
fly.names = NULL

## WT
all_ofs_WT = list()
for(session in sessions){
  all_ofs_WT = append(all_ofs_WT,list(c()))
}
fly.info = read.csv("data/fly_info_CS_preprocessed.csv",header=T,stringsAsFactors=F)

for(ind in 1:nrow(fly.info)){
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/",
                                           fly.info$experimenter[ind],
                                           "/CS/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_WT",
                                              ".csv"),
                             full.names=T
    )
    if(length(input.file) == 0){
      all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(NA))
      next
    }   
    framerate = fly.info$Framerate[ind]        
    ofs = one_fly_statistics(input.file,framerate=framerate)
    
    all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(ofs))
  }
}

## Mutants
all_ofs_mutants = list()
for(session in sessions){
  all_ofs_mutants = append(all_ofs_mutants,list(c()))
}
fly.info.mutant = read.csv("data/fly_info_mutants_preprocessed.csv",header=T,stringsAsFactors=F)
for(ind in 1:nrow(fly.info.mutant)){
  query.sessions = gsub("X",fly.info.mutant$Category[ind],sessions)
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info.mutant$experimenter[ind], "/mutants/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info.mutant$Fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_",fly.info.mutant$Genotype[ind],
                                              ".csv"),
                             full.names=T
    )
    if(length(input.file) == 0){
      all_ofs_mutants[[sessions[ind.session]]] = append(all_ofs_mutants[[sessions[ind.session]]],list(NA))
      next
    }   
    framerate = fly.info.mutant$Framerate[ind]        
    ofs = one_fly_statistics(input.file,framerate=framerate)
    
    all_ofs_mutants[[sessions[ind.session]]] = append(all_ofs_mutants[[sessions[ind.session]]],list(ofs))
  }
}
save.image("all_ofs.Rdata")

#CS_constant
all_ofs_constant = list()
for(session in sessions){
  all_ofs_constant = append(all_ofs_constant,list(c()))
}
fly.info.constant = read.csv("data/fly_info_CS_constant_preprocessed.csv",header=T,stringsAsFactors=F)
for(ind in 1:nrow(fly.info.constant)){
  query.sessions = gsub("X",fly.info.constant$Category[ind],sessions)
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info.constant$experimenter[ind], "/CS_constant/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info.constant$Fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_WT",
                                              ".csv"),
                             full.names=T
    )
    if(length(input.file) == 0){
      all_ofs_constant[[sessions[ind.session]]] = append(all_ofs_constant[[sessions[ind.session]]],list(NA))
      next
    }   
    framerate = fly.info.constant$Framerate[ind]        
    ofs = one_fly_statistics(input.file,framerate=framerate)
    all_ofs_constant[[sessions[ind.session]]] = append(all_ofs_constant[[sessions[ind.session]]],list(ofs))
  }
}
save.image("all_ofs_constant.Rdata")
