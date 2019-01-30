#This script is for plotting all metrics relevant after fit_lm_data_cleaning.R
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
load('all_ofs.Rdata')

# Plotting
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

# Hypothesis Testing

# 9: Percentage Time Active - Middle Pause
E1 = hypothesis_testing_E1(8, fly.info.mutants)
E5 = hypothesis_testing_3rdE1(8, fly.info.mutants)

# Some backup codes

# input.file = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1_WT.csv"
# input.file2 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1_WT.csv"
# input.file3 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1N1E1_WT.csv"
# input.file4 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1_WT.csv"
# input.file5 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1E1_WT.csv"
# input.file6 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1E1T1E1_WT.csv"
