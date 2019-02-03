setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

## Fly info
fly.info.CS = read.csv("data/fly_info_CS_preprocessed.csv", header = T, stringsAsFactors = F)
fly.info.CS$Genotype = "WT"

fly.info.mutants = read.csv("data/fly_info_mutants_preprocessed.csv",header = T, stringsAsFactors = F)

shared.info = c("Fly", "Category", "Gender","Genotype", "Exp.date", "Experimenter","Age",
                "Setup", "Fly.moving.speed", "Fly.pause","Framerate")

# All raw data to-date
fly.info = rbind(fly.info.CS[, shared.info], fly.info.mutants[, shared.info])


ind.T.excl = which((fly.info$Category=="T") & (fly.info$Fly.pause > 0.9))
ind.RfromT.excl = c()
for (i in 1:length(ind.T.excl)){
  ind.RfromT.excl = c(ind.RfromT.excl, Use_T_find_R(fly.info, ind.T.excl[i]))
}
ind.T.excl = c(ind.T.excl, ind.RfromT.excl)

ind.other.excl = which((fly.info$Category!="T") & (fly.info$Fly.pause > 0.9))

ind.all.excl = unique(c(ind.T.excl, ind.other.excl))

#For Mutant data collected after Mar 20, 2017
excl.fly.Mutant = na.omit(read.csv("excl_fly_mutant.csv",header = T, stringsAsFactors = F)[, 1:3])

#WT data
excl.fly.WT = data.frame(cbind(
  # Batch 1 ES-WT: 1-276; Batch 2 ES-WT: 276-400; Batch 3 ES-WT: 400-present (as of June 28, 2017)
  # Only Batch 3 data is for the current project. All prior data was for prototyping
  c(58, 48, 1:400, 1:100, 1:40,118,122),
  c("RS", "JD", rep("ES", 400), rep("RS", 100), rep("JD", 40),rep("SW",2)),
  c(rep("WT", 544))
))
colnames(excl.fly.WT) = colnames(excl.fly.Mutant)
excl.fly = rbind(excl.fly.Mutant, excl.fly.WT)

# All data to be excluded
ind.excl = NULL
for (ind in 1:nrow(excl.fly)) {
  ind.excl = c(ind.excl, which(fly.info$Genotype == excl.fly[ind, 3] & 
                                 fly.info$Experimenter == excl.fly[ind, 2] & 
                                 fly.info$Fly == excl.fly[ind, 1]))
}

# All data to be included
ind.include = NULL
for (genotype in unique(fly.info$Genotype)) {
  if (genotype == "CS") {next}
  else if (genotype == "WT") {ind = fly.info$Genotype %in% c("WT", "CS") &!(1:nrow(fly.info) %in% ind.excl) & !(1:nrow(fly.info) %in% ind.all.excl)}
  # else if (genotype == "WT") {ind = fly.info$Genotype %in% c("WT", "CS") &!(1:nrow(fly.info) %in% ind.excl)}
  
  else{ind = fly.info$Genotype == genotype & !(1:nrow(fly.info) %in% ind.excl) & !(1:nrow(fly.info) %in% ind.all.excl)}
  ind.include = c(ind.include,which(ind)) 
}

fly.info.include = fly.info[ind.include,]

# ind.filtered = data_filter(1, fly.info.include) 
# fly.info.end = fly.info.include[ind.filtered, ]
fly.info.end = fly.info.include

write.csv(fly.info.end, "data/fly_info_end.csv", quote = F, row.names = F)

checking_fly_numbers(fly.info.include, 1, filename="Mutants_headcount-updatedfilter.csv")