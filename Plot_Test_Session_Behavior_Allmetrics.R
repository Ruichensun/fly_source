#This script is for plotting all metrics relevant after fit_lm_data_cleaning.R
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
load('all_ofs.Rdata')

# WT flies
query.genotype <- c("WT")
query.fly = fly.info$Fly
query.experimenter = fly.info$Experimenter
plot_all_raw_metrics(query.genotype, query.fly, query.experimenter, fly.info)


# Mutant Flies

for (i in 1:length(unique(fly.info.mutants$Genotype))){
  query.genotype = unique(fly.info.mutants$Genotype)[i]
  query.fly = fly.info.mutants[(fly.info.mutants$Genotype == query.genotype), ]$Fly
  query.experimenter = fly.info.mutants[(fly.info.mutants$Genotype == query.genotype), ]$Experimenter
  plot_all_raw_metrics(query.genotype, query.fly, query.experimenter, fly.info.mutants)
}


