#This script is for plotting all metrics relevant after fit_lm_data_cleaning.R
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
load('all_ofs.Rdata')

# Plotting All Metrics for each genotype
# WT flies
query.genotype <- c("WT")
query.fly = fly.info$Fly
query.experimenter = fly.info$Experimenter
plot_all_raw_metrics(query.genotype, query.fly, query.experimenter, fly.info)
# Mutant Flies
for (i in 1:length(unique(fly.info.mutant$Genotype))){
  query.genotype = unique(fly.info.mutant$Genotype)[i]
  query.fly = fly.info.mutant[(fly.info.mutant$Genotype == query.genotype), ]$Fly
  query.experimenter = fly.info.mutant[(fly.info.mutant$Genotype == query.genotype), ]$Experimenter
  plot_all_raw_metrics(query.genotype, query.fly, query.experimenter, fly.info.mutant)
}

# Hypothesis Testing
# 9: Percentage Time Active - Middle Pause
num = 9
E1 = hypothesis_testing_E1(num, fly.info.mutant)
E3 = hypothesis_testing_2ndE1(num, fly.info.mutant)
E5 = hypothesis_testing_3rdE1(num, fly.info.mutant)
E1[3,]
E3[3,]
E5[3,]

plot_comparison("SUN1", 9, all_ofs)
plot_comparison("SUN2", 9, all_ofs)
plot_comparison("SUN3", 9, all_ofs)
plot_comparison("MB009B x JU30", 9, all_ofs)
plot_comparison("R60D05 x JU30", 9, all_ofs)
plot_comparison("CS x PKCi", 9, all_ofs)
plot_comparison("UAS-DopR1-IR x 51635", 9, all_ofs)
plot_comparison("CS x JU30", 9, all_ofs)

category = "T"
par(mfrow = c(3, 1))
g_list = c("WT", "SUN1", "SUN1 x CS", "SUN2", "SUN2 x CS", "SUN3", "SUN3 x CS",  "THGAL4 x JU29", "THGAL4 x JU30")
learning_index = get_learning_index(fly.info.end, all_ofs, 8, category, g_list)
boxplot(Learning~Genotype, learning_index, ylim = c(-2, 2), notch = F, outline = T)

g_list2 = c("WT","MB009B x JU30", "MB131B x JU30", "MB419B x JU30", "MB607B x JU30", "OK107 x JU30","Empty-Gal4 x CS", "CS x JU30")
learning_index2 = get_learning_index(fly.info.end, all_ofs, 8, category, g_list2)
boxplot(Learning~Genotype, learning_index2, ylim = c(-1, 1), notch = F, outline = F)

g_list3 = c("WT","R60D05 x JU30", "JG17 x JU30", "Empty-Gal4 x CS", "CS x JU30")
learning_index3 = get_learning_index(fly.info.end, all_ofs, 8, category, g_list3)
boxplot(Learning~Genotype, learning_index3, ylim = c(-1, 1), notch = F, outline = F)

g_list4 = c("WT","UAS-DopR1-IR x 51635", "MB009B x DopR1-IR", "MB131B x DopR1-IR", "MB419B x DopR1-IR", "MB607B x DopR1-IR", "R60D05 x DopR1-IR", "JG17 x DopR1-IR")
learning_index4 = get_learning_index(fly.info.end, all_ofs, 8, category, g_list4)
boxplot(Learning~Genotype, learning_index4, ylim = c(-1, 1), notch = F)

g_list5 = c("WT", "MB009B x PKCi", "MB131B x PKCi", "MB419B x PKCi", "MB607B x PKCi", "CS x PKCi", "R60D05 x PKCi","JG17 x PKCi")
learning_index5 = get_learning_index(fly.info.end, all_ofs, 8, category, g_list5)
boxplot(Learning~Genotype, learning_index5, ylim = c(-1, 1), notch = F)

g_list6 = c("WT", "W1118", "R5", "R3", "Empty-Gal4 x JU30", "DopR2 x JU30", "DopR1 x JU30")
learning_index6 = get_learning_index(fly.info.end, all_ofs, 8, category, g_list6)
boxplot(Learning~Genotype, learning_index6, ylim = c(-1, 1), notch = F)