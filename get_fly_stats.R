# source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/prepoccess.R")
setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

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
  
  "E2",
  "E2T2E2",
  "E2T2E2T2E2",
  "E2T2E1T2E2T2E2",
 
  "E2R2E2",
  "E2R2E2R2E2",
  "E2R2E1R2E2R2E2",
  
  "E2N2E2",
  "E2N2E2N2E2",
  "E2N2E1N2E2N2E2",
  
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

## WT
all_ofs_WT = data.frame()
fly.info = fly.info.end[fly.info.end$Genotype %in% c("WT", "CS"),]

for(ind in 1:nrow(fly.info)){
  # print(paste0("data/", fly.info$Experimenter[ind], "/CS/", "ProcessedData_Fly",fly.info$Fly[ind]))
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                             full.names=T)
    if(length(input.file) == 0){next
      }else{
          framerate = fly.info$Framerate[ind]        
          ofs = one_fly_statistics(input.file, framerate=framerate)
          Type = fly.info$Category[ind]
          ofs = cbind(Type, ofs)
          all_ofs_WT = rbind(all_ofs_WT, ofs)
    }
    
  }
}

## Mutants
all_ofs_mutants = data.frame()
fly.info.mutant = fly.info.end[!(fly.info.end$Genotype %in% c("WT", "CS")),]

for(ind in 1:nrow(fly.info.mutant)){
  # print(paste0("data/", fly.info.mutant$Experimenter[ind], "/mutants/", "ProcessedData_Fly",fly.info.mutant$Fly[ind]))
  query.sessions = gsub("X",fly.info.mutant$Category[ind],sessions)
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info.mutant$Experimenter[ind], "/mutants/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info.mutant$Fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_",fly.info.mutant$Genotype[ind],
                                              ".csv"),
                             full.names=T
    )
    if(length(input.file) == 0){next
      }else{   
        framerate = fly.info.mutant$Framerate[ind]        
        ofs = one_fly_statistics(input.file, framerate=framerate)
        Type = fly.info.mutant$Category[ind]
        ofs = cbind(Type, ofs)
        all_ofs_mutants = rbind(all_ofs_mutants, ofs)
      }
  }
}

write.table(all_ofs_WT, file = "all_ofs_WT.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
write.table(all_ofs_mutants, file = "all_ofs_mutants.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofs = rbind(all_ofs_WT, all_ofs_mutants)

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

