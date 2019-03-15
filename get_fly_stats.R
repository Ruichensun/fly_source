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
  for(ind.session in 1:length(query.sessions)){
    if (fly.info$Genotype[ind]=="WT"){
      input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                               full.names=T)
    }else if(fly.info$Genotype[ind]=="CS"){
      input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/mutants/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",query.sessions[ind.session], "_CS",".csv"),
                               full.names=T)
    }
    if(length(input.file) == 0){next
      }else{
          framerate = fly.info$Framerate[ind]        
          ofs = one_fly_statistics(input.file, framerate=framerate)
          Type = fly.info$Category[ind]
          Gender = fly.info$Gender[ind]
          ofs = cbind(Type, Gender, ofs)
          all_ofs_WT = rbind(all_ofs_WT, ofs)
    }
    
  }
}

write.table(all_ofs_WT, file = "all_ofs_WT.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)
print("Done-WT")

## Mutants
all_ofs_mutants = data.frame()
fly.info.mutant = fly.info.end[!(fly.info.end$Genotype %in% c("WT", "CS")),]

for(ind in 1:nrow(fly.info.mutant)){
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
        Gender = fly.info.mutant$Gender[ind]
        ofs = cbind(Type, Gender, ofs)
        all_ofs_mutants = rbind(all_ofs_mutants, ofs)
      }
  }
}

write.table(all_ofs_mutants, file = "all_ofs_mutants.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofs_mutants = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)
print("Done-Mutants")

# laser segmentation
all_ofls_WT = data.frame()

sessions_laser = c(
  "E1T1",
  "E1T1E1T1",
  "E1R1",
  "E1R1E1R1"
)

for(ind in 1:nrow(fly.info)){
  for(ind.session in 1:length(sessions_laser)){
    if (fly.info$Genotype[ind]=="WT"){
      input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",sessions_laser[ind.session], "_WT",".csv"),
                               full.names=T)
    }else if(fly.info$Genotype[ind]=="CS"){
      input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/mutants/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",sessions_laser[ind.session], "_CS",".csv"),
                               full.names=T)
    }
    if(length(input.file) == 0){next
    }else{
      framerate = fly.info$Framerate[ind]        
      ofs = one_fly_laser_statistics(input.file, framerate=framerate)
      Type = fly.info$Category[ind]
      Gender = fly.info$Gender[ind]
      ofs = cbind(Type, Gender, ofs)
      all_ofls_WT = rbind(all_ofls_WT, ofs)
    }
  }
}

write.table(all_ofls_WT, file = "all_ofls_WT.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofls_WT = read.csv("all_ofls_WT.csv", header = T, stringsAsFactors = F)
print("Done - laser")

all_ofs = rbind(all_ofs_WT, all_ofs_mutants)

save.image("all_ofs.Rdata")

