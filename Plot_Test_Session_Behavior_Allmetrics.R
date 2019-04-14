#This script is for plotting all metrics relevant after fit_lm_data_cleaning.R
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
load('all_ofs.Rdata')


all_ofs = all_ofs[all_ofs$Genotype == "WT" | all_ofs$Genotype == "CS", ]
fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]
fly.info.movement.N = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "N") , ]
R1 = Hit_by_laser("E1R1", fly.info.movement.R)
T1 = Hit_by_laser("E1T1", fly.info.movement.T)
N1 = Hit_by_laser("E1N1", fly.info.movement.N)
RT = rbind(R1, T1, N1)
RT.include = RT[!is.na(RT$Hit_W), ]
RT.exclude = RT[is.na(RT$Hit_W), ]
temp = data.frame()
for (j in 1:nrow(RT.include)){
  temp = rbind(temp, 
               all_ofs[all_ofs$Fly.Number == RT.include[j, ]$Fly & 
                         all_ofs$Experimenter == RT.include[j, ]$Experimenter &
                         all_ofs$Genotype == RT.include[j, ]$Genotype,])
}
df = temp

WT_9 = list(
T_E1 = df[df$Type=="T" & df$Session =="E1", ]$Percentage.Time.Active,
R_E1 = df[df$Type=="R" & df$Session =="E1", ]$Percentage.Time.Active,
N_E1 = df[df$Type=="N" & df$Session =="E1", ]$Percentage.Time.Active,
T_E3 = df[df$Type=="T" & df$Session =="E1T1E1", ]$Percentage.Time.Active,
R_E3 = df[df$Type=="R" & df$Session =="E1R1E1", ]$Percentage.Time.Active,
N_E3 = df[df$Type=="N" & df$Session =="E1N1E1", ]$Percentage.Time.Active,
T_E5 = df[df$Type=="T" & df$Session =="E1T1E1T1E1", ]$Percentage.Time.Active,
R_E5 = df[df$Type=="R" & df$Session =="E1R1E1R1E1", ]$Percentage.Time.Active,
N_E5 = df[df$Type=="N" & df$Session =="E1N1E1N1E1", ]$Percentage.Time.Active
)

CI_df_WT9 = data.frame()
for (i in 1:9){
  CI_df_WT9 = rbind.data.frame(CI_df_WT9, get_Wald_CI(WT_9[[i]]))
}
colnames(CI_df_WT9) = c("Median", "CI_Lower", "CI_Upper")

WT_25 = list(
  T_E1 = df[df$Type=="T" & df$Session =="E1", ]$Average.Pause.Duration,
  R_E1 = df[df$Type=="R" & df$Session =="E1", ]$Average.Pause.Duration,
  N_E1 = df[df$Type=="N" & df$Session =="E1", ]$Average.Pause.Duration,
  T_E3 = df[df$Type=="T" & df$Session =="E1T1E1", ]$Average.Pause.Duration,
  R_E3 = df[df$Type=="R" & df$Session =="E1R1E1", ]$Average.Pause.Duration,
  N_E3 = df[df$Type=="N" & df$Session =="E1N1E1", ]$Average.Pause.Duration,
  T_E5 = df[df$Type=="T" & df$Session =="E1T1E1T1E1", ]$Average.Pause.Duration,
  R_E5 = df[df$Type=="R" & df$Session =="E1R1E1R1E1", ]$Average.Pause.Duration,
  N_E5 = df[df$Type=="N" & df$Session =="E1N1E1N1E1", ]$Average.Pause.Duration
)

CI_df_WT25 = data.frame()
for (i in 1:9){
  CI_df_WT25 = rbind.data.frame(CI_df_WT25, get_Wald_CI(WT_25[[i]]))
}
colnames(CI_df_WT25) = c("Median", "CI_Lower", "CI_Upper")

WT_7 = list(
  T_E1 = df[df$Type=="T" & df$Session =="E1", ]$Number.of.Pause,
  R_E1 = df[df$Type=="R" & df$Session =="E1", ]$Number.of.Pause,
  N_E1 = df[df$Type=="N" & df$Session =="E1", ]$Number.of.Pause,
  T_E3 = df[df$Type=="T" & df$Session =="E1T1E1", ]$Number.of.Pause,
  R_E3 = df[df$Type=="R" & df$Session =="E1R1E1", ]$Number.of.Pause,
  N_E3 = df[df$Type=="N" & df$Session =="E1N1E1", ]$Number.of.Pause,
  T_E5 = df[df$Type=="T" & df$Session =="E1T1E1T1E1", ]$Number.of.Pause,
  R_E5 = df[df$Type=="R" & df$Session =="E1R1E1R1E1", ]$Number.of.Pause,
  N_E5 = df[df$Type=="N" & df$Session =="E1N1E1N1E1", ]$Number.of.Pause
)

CI_df_WT7 = data.frame()
for (i in 1:9){
  CI_df_WT7 = rbind.data.frame(CI_df_WT7, get_Wald_CI(WT_7[[i]]))
}
colnames(CI_df_WT7) = c("Median", "CI_Lower", "CI_Upper")

WT_22 = list(
  T_E1 = df[df$Type=="T" & df$Session =="E1", ]$Number.of.Turns,
  R_E1 = df[df$Type=="R" & df$Session =="E1", ]$Number.of.Turns,
  N_E1 = df[df$Type=="N" & df$Session =="E1", ]$Number.of.Turns,
  T_E3 = df[df$Type=="T" & df$Session =="E1T1E1", ]$Number.of.Turns,
  R_E3 = df[df$Type=="R" & df$Session =="E1R1E1", ]$Number.of.Turns,
  N_E3 = df[df$Type=="N" & df$Session =="E1N1E1", ]$Number.of.Turns,
  T_E5 = df[df$Type=="T" & df$Session =="E1T1E1T1E1", ]$Number.of.Turns,
  R_E5 = df[df$Type=="R" & df$Session =="E1R1E1R1E1", ]$Number.of.Turns,
  N_E5 = df[df$Type=="N" & df$Session =="E1N1E1N1E1", ]$Number.of.Turns
)

CI_df_WT22 = data.frame()
for (i in 1:9){
  CI_df_WT22 = rbind.data.frame(CI_df_WT22, get_Wald_CI(WT_22[[i]]))
}
colnames(CI_df_WT22) = c("Median", "CI_Lower", "CI_Upper")

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

fly.info.end_3 = fly.info.end[fly.info.end$age==3,] 
fly.info.end_5 = fly.info.end[fly.info.end$age==5,] 


sample(nrow(fly.WT.T), size = 29, replace = TRUE)
fly.WT.T[sample(nrow(fly.WT.T), size = 29, replace = TRUE), ]

