# source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/prepoccess.R")
setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

metrices = c(
  "Number of Pause", #1
  "Number of Middle Pause", #2
  "Percentage Time Active", #3
  "Percentage Time Active - Pause not at the End", #4
  "Median Pause Duration",#5
  "Median Middle Pause Duration", #6    
  "Max Pause Duration", #7
  "Max Middle Pause Duration", #8
  "First Pause Duration", #9
  "First Middle Pause Duration", #10
  "Average Moving Speed", #11
  "Average Moving Speed (excluding pause)", #12
  "Average Speed When Enter Pause", #13
  "Average Speed When Exit Pause",#14
  "Moving Distance",#15
  "Number of Turns",#16
  "Number of Middle Turns",#17
  "Fration of Middle Turns Out of Total Turns",#18
  "Burstiness (Pause)",#19
  "Burstiness (Inter Event Time)",#20
  "Burstiness (Scrambled)",#21
  "Burstiness (Walking bouts-thresholding)",#22
  "Burstiness (Walking events-no thres)",#23
  "Memory of Pause", #24
  "Memory of Walking", #25
  "Transition Probability (Pause not at the end): Pause to Pause", #26
  "Transition Probability (Pause not at the end): Pause to Pause - middle", #27
  "Transition Probability (Pause not at the end): Pause to Pause - middle - no bump", #28
  "Transition Probability (Pause not at the end): Pause to Walking", #29
  "Transition Probability (Pause not at the end): Pause to Walking - middle", #30
  "Transition Probability (Pause not at the end): Pause to Walking - middle - no bump", #31
  "Transition Probability (Pause not at the end): Walking to Walking", #32
  "Transition Probability (Pause not at the end): Walking to Walking - middle", #33
  "Transition Probability (Pause not at the end): Walking to Walking - middle - no bump", #34
  "Transition Probability (Pause not at the end): Walking to Pause", #35
  "Transition Probability (Pause not at the end): Walking to Pause - middle", #36
  "Transition Probability (Pause not at the end): Walking to Pause - middle - no bump" #37
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
# for(ind in c(380:381)){
  print(paste0("data/", fly.info$Experimenter[ind], "/CS/", "ProcessedData_Fly",fly.info$Fly[ind], "_"))
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                             full.names=T)
    if(length(input.file) == 0){
      all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(NA))
      next
    }
    framerate = fly.info$Framerate[ind]        
    ofs = one_fly_statistics(input.file,framerate=framerate)
    basic = list(fly.info$Experimenter[ind], fly.info$Fly[ind], "WT")
    names(basic) = c("Experimenter", "FlyNumber", "Genotype")
    ofs = append(basic, ofs)
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
    input.file <- list.files(path = paste0("data/", fly.info.mutant$Experimenter[ind], "/mutants/"),                             
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
    basic = list(fly.info$Experimenter[ind], fly.info$Fly[ind], fly.info$Genotype[ind])
    names(basic) = c("Experimenter", "FlyNumber", "Genotype")
    ofs = append(basic, ofs)
    all_ofs_mutants[[sessions[ind.session]]] = append(all_ofs_mutants[[sessions[ind.session]]],list(ofs))
  }
}


#CS_constant
# all_ofs_constant = list()
# for(session in sessions){
#   all_ofs_constant = append(all_ofs_constant,list(c()))
# }
# fly.info.constant = read.csv("data/fly_info_CS_constant_preprocessed.csv",header=T,stringsAsFactors=F)
# for(ind in 1:nrow(fly.info.constant)){
#   query.sessions = gsub("X",fly.info.constant$Category[ind],sessions)
#   for(ind.session in 1:length(query.sessions)){
#     input.file <- list.files(path = paste0("data/", fly.info.constant$Experimenter[ind], "/CS_constant/"),                             
#                              pattern = paste0("ProcessedData_Fly",fly.info.constant$Fly[ind],
#                                               "_",query.sessions[ind.session],
#                                               "_WT",
#                                               ".csv"),
#                              full.names=T
#     )
#     if(length(input.file) == 0){
#       all_ofs_constant[[sessions[ind.session]]] = append(all_ofs_constant[[sessions[ind.session]]],list(NA))
#       next
#     }   
#     framerate = fly.info.constant$Framerate[ind]        
#     ofs = one_fly_statistics(input.file,framerate=framerate)
#     all_ofs_constant[[sessions[ind.session]]] = append(all_ofs_constant[[sessions[ind.session]]],list(ofs))
#   }
# }

save.image("all_ofs.Rdata")
