setwd("E:/Behavioral_project/behavior_experiment_data/Analysis")
source("E:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

## Fly info
fly.info.CS = read.csv("data/fly_info_CS_preprocessed.csv", header = T, stringsAsFactors = F)
fly.info.CS$genotype = "WT"
fly.info.mutants = read.csv("data/fly_info_mutants_preprocessed.csv",header = T, stringsAsFactors = F)

shared.info = c("fly", 
                "genotype",
                "gender", 
                "category", 
                "setup", 
                "exp_date", 
                "experimenter", 
                "age", 
                "life_span",
                "Fly.moving.speed", 
                "Fly.pause",
                "Framerate")

fly.info = rbind(fly.info.CS[, shared.info], 
                 fly.info.mutants[, shared.info])

# Filter by baseline behaviors
ind.T.excl = which((fly.info$category=="T") & (fly.info$Fly.pause > 0.9))
ind.RfromT.excl = c()
for (i in 1:length(ind.T.excl)){
  ind.RfromT.excl = c(ind.RfromT.excl, Use_T_find_R(fly.info, ind.T.excl[i]))
}
ind.T.excl = c(ind.T.excl, ind.RfromT.excl)
ind.other.excl = which((fly.info$category!="T") & (fly.info$Fly.pause > 0.9))
ind.all.excl = unique(c(ind.T.excl, ind.other.excl))

#For Mutant data collected after Mar 20, 2017
excl.fly.Mutant = na.omit(read.csv("excl_fly_mutant.csv",header = T, stringsAsFactors = F)[, 1:3])

#WT data
excl.fly.WT = data.frame(
  cbind(
  # Batch 1 ES-WT: 1-276; Batch 2 ES-WT: 276-400; Batch 3 ES-WT: 400-present (as of June 28, 2017)
  # Only Batch 3 data is for the current project. All prior data was for prototyping
  c(1:400, 1:100, 1:34, 57),
  c(rep("ES", 400), rep("RS", 100), rep("SW",35)),
  c(rep("WT", 535))
)
)
colnames(excl.fly.WT) = colnames(excl.fly.Mutant)
excl.fly = rbind(excl.fly.Mutant, excl.fly.WT)

# All data to be excluded
ind.excl = NULL
for (ind in 1:nrow(excl.fly)) {
  ind.excl = c(ind.excl, which(fly.info$genotype == excl.fly[ind, 3] & 
                                 fly.info$experimenter == excl.fly[ind, 2] & 
                                 fly.info$fly == excl.fly[ind, 1]))
}

# All data to be included
ind.include = NULL
for (genotype in unique(fly.info$genotype)) {
  if (genotype == "CS") {next}
  else if (genotype == "WT") {
    ind = fly.info$genotype %in% c("WT", "CS") &!(1:nrow(fly.info) %in% ind.excl) & !(1:nrow(fly.info) %in% ind.all.excl)
  }else{
      ind = fly.info$genotype == genotype & !(1:nrow(fly.info) %in% ind.excl) & !(1:nrow(fly.info) %in% ind.all.excl)
  }
  ind.include = c(ind.include,which(ind)) 
}

fly.info.end = fly.info[ind.include,]
write.csv(fly.info.end, "data/fly_info_end.csv", quote = F, row.names = F)
fly.info.end = read.csv("data/fly_info_end.csv", header = T, stringsAsFactors = F)

checking_fly_numbers(fly.info.end, 1, filename="Mutants_headcount.csv")
