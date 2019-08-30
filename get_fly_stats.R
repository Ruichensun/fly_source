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
fly.info = fly.info.end[fly.info.end$genotype %in% c("WT", "CS"),]

for(ind in 1:nrow(fly.info)){
  for(ind.session in 1:length(query.sessions)){
    if (fly.info$genotype[ind]=="WT"){
      input.file <- list.files(path = paste0("data/", fly.info$experimenter[ind], "/CS/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info$fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                               full.names=T)
    }else if(fly.info$genotype[ind]=="CS"){
      input.file <- list.files(path = paste0("data/", fly.info$experimenter[ind], "/mutants/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info$fly[ind], "_",query.sessions[ind.session], "_CS",".csv"),
                               full.names=T)
    }
    if(length(input.file) == 0){next
      }else{
          framerate = fly.info$Framerate[ind]        
          ofs = one_fly_statistics(input.file, framerate=framerate)
          type = fly.info$category[ind]
          gender = fly.info$gender[ind]
          ofs = cbind(type, gender, ofs)
          all_ofs_WT = rbind(all_ofs_WT, ofs)
    }
    
  }
}

write.table(all_ofs_WT, file = "all_ofs_WT.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)
print("Done-WT")

## Mutants
all_ofs_mutants = data.frame()
fly.info.mutant = fly.info.end[!(fly.info.end$genotype %in% c("WT", "CS")),]

for(ind in 1:nrow(fly.info.mutant)){
# for(ind in 2651:2653){
    for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info.mutant$experimenter[ind], "/mutants/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info.mutant$fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_",fly.info.mutant$genotype[ind],
                                              ".csv"),
                             full.names=T
    )
    if(length(input.file) == 0){next
      }else{   
        framerate = fly.info.mutant$Framerate[ind]        
        ofs = one_fly_statistics(input.file, framerate=framerate)
        type = fly.info.mutant$category[ind]
        gender = fly.info.mutant$gender[ind]
        ofs = cbind(type, gender, ofs)
        print(ofs[,3:4])
        all_ofs_mutants = rbind(all_ofs_mutants, ofs)
      }
  }
}

write.table(all_ofs_mutants, file = "all_ofs_mutants.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofs_mutants = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)
print("Done-Mutants")

# # laser segmentation
all_ofls_WT = data.frame()

sessions_laser = c(
  "E1T1",
  "E1T1E1T1",
  "E1R1",
  "E1R1E1R1"
)

for(ind in 1:nrow(fly.info)){
  for(ind.session in 1:length(sessions_laser)){
    if (fly.info$genotype[ind]=="WT"){
      input.file <- list.files(path = paste0("data/", 
                                             fly.info$experimenter[ind], 
                                             "/CS/"),
                               pattern = paste0("ProcessedData_Fly",
                                                fly.info$fly[ind], 
                                                "_",
                                                sessions_laser[ind.session],
                                                "_WT",".csv"),
                               full.names=T)
    }else if(fly.info$genotype[ind]=="CS"){
      input.file <- list.files(path = paste0("data/", 
                                             fly.info$experimenter[ind], 
                                             "/mutants/"),
                               pattern = paste0("ProcessedData_Fly",
                                                fly.info$fly[ind], 
                                                "_",
                                                sessions_laser[ind.session], 
                                                "_CS",
                                                ".csv"),
                               full.names=T)
    }
    if(length(input.file) == 0){next
    }else{
      print(input.file)
      framerate = fly.info$Framerate[ind]
      ofs = one_fly_laser_statistics(input.file, framerate=framerate)
      Type = as.character(fly.info$category[ind])
      Gender = as.character(fly.info$gender[ind])
      ofs = data.frame(Type, Gender, ofs)
      all_ofls_WT = rbind(all_ofls_WT, ofs)
    }
  }
}

write.table(all_ofls_WT, file = "all_ofls_WT.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)
all_ofls_WT = read.csv("all_ofls_WT.csv", header = T, stringsAsFactors = F)
print("Done - laser")

all_ofs = rbind(all_ofs_WT, all_ofs_mutants)

save.image("all_ofs.Rdata")


# Quality control - training protocol (certain flies receive non-standard training and are not consistent with the design of the experiment)
fly.info.end = read.csv("data/fly_info_end.csv", header = T, stringsAsFactors = F)
fly.info.movement.T = fly.info.end[(fly.info.end$category =="T"), ]
fly.info.movement.R = fly.info.end[(fly.info.end$category == "R") , ]
fly.info.movement.N = fly.info.end[(fly.info.end$category == "N") , ]

all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)
all_ofs_mutants = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)
all_ofs = rbind(all_ofs_WT, all_ofs_mutants)
T1 = Hit_by_laser("E1T1", fly.info.movement.T)
T1 = T1[!is.na(T1$Hit_W), ]
T1$Diff = T1$Hit_W - T1$Hit_P
temp = T1[(T1$Diff>=0.2)|(T1$Hit_All==0),]
T2 = Hit_by_laser("E1T1E1T1", temp[, 1:12])
colnames(T2)[13] <- "Hit_W_2"
colnames(T2)[14] <- "Hit_P_2"
colnames(T2)[15] <- "Hit_All_2"

T2$ID <- NA
temp$ID <- NA
for (i in 1:nrow(T2)){
  T2[i, "ID"] <- paste0(T2[i, "fly"], T2[i, "genotype"], T2[i, "experimenter"])
}
for (i in 1:nrow(temp)){
  temp[i, "ID"] <- paste0(temp[i, "fly"], temp[i, "genotype"], temp[i, "experimenter"])
}

temp$Hit_W_2 <- NaN
temp$Hit_P_2 <- NaN
temp$Hit_All_2 <- NaN
for (i in 1:nrow(temp)){
    ind = which(T2$ID == temp[i, "ID"])
    if (length(ind) != 0){
      temp[i, "Hit_W_2"] <- T2[ind, "Hit_W_2"]
      temp[i, "Hit_P_2"] <- T2[ind, "Hit_P_2"]
      temp[i, "Hit_All_2"] <- T2[ind, "Hit_All_2"]
    }
}
T.all = temp[!is.na(temp$Hit_All_2), ]

R1 = Hit_by_laser("E1R1", fly.info.movement.R)
R1 = R1[!is.na(R1$Hit_W), ]
R1$Diff = R1$Hit_W - R1$Hit_P
R2 = Hit_by_laser2("E1R1E1R1", R1)
R.all = cbind(R1, R2)

N1 = Hit_by_laser("E1N1", fly.info.movement.N)
N1 = N1[!is.na(N1$Hit_W), ]
N1$Diff = N1$Hit_W - N1$Hit_P
N2 = Hit_by_laser2("E1N1E1N1", N1)
N.all = cbind(N1, N2)

fly.info.end = rbind(T.all[, c(1:16, 18:20)], R.all, N.all)

write.csv(fly.info.end, file = "data/fly_info_end_final.csv", row.names = TRUE)

