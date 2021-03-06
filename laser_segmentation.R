setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

# Segmenting WT flies into long-exposure group and short-exposure group 

# Load training data and test data
all_ofls_WT = read.csv("all_ofls_WT.csv", header = T, stringsAsFactors = F)
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)

# Some of the samples do not have laser exposure info (denoted as -1)
# Subset the samples with laser exposure recorded. 

ofls = subset(all_ofls_WT, laser_exposure > 0)

for (i in 2:nrow(ofls)){
  if ((ofls[i, ]$flynum == ofls[i-1, ]$flynum)&&
      (ofls[i, ]$experimenter == ofls[i-1, ]$experimenter)){
    ofls[i, ]$laser_exposure = ofls[i, ]$laser_exposure + ofls[i-1, ]$laser_exposure
    ofls[i, ]$laser_count = ofls[i, ]$laser_count + ofls[i-1, ]$laser_count
  }
}

laser_T = ofls[ofls$session=="E1T1E1T1" & ofls$laser_count < 45, ]
laser_R = ofls[ofls$session=="E1R1E1R1" & ofls$laser_count < 45, ]
laser_R31 = ofls[ofls$session=="E1R1", ]

# # Remember to remove flies that receive more than 45 clicks
# med = median(laser_T$Laser_Exposure)
# T_med_abv = laser_T[laser_T$Laser_Exposure >= med, ]
# T_med_bel = laser_T[laser_T$Laser_Exposure < med, ]
# R_med_abv = laser_R[laser_R$Laser_Exposure >= med, ]
# R_med_bel = laser_R[laser_R$Laser_Exposure < med, ] 
# 
# # Get fly activity for the two groups
# T_abv_test = subset(all_ofs_WT, 
#                     ((all_ofs_WT$Experimenter=="ES") & 
#                       (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="ES",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="RS") & 
#                       (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="RS",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="JD") & 
#                       (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="JD",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="SW") & 
#                       (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="SW",]$Fly.Number))
# )
# 
# T_bel_test = subset(all_ofs_WT, 
#                     ((all_ofs_WT$Experimenter=="ES") & 
#                        (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="ES",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="RS") & 
#                        (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="RS",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="JD") & 
#                        (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="JD",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="SW") & 
#                        (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="SW",]$Fly.Number))
# )
# 
# R_abv_test = subset(all_ofs_WT, 
#                     ((all_ofs_WT$Experimenter=="ES") & 
#                        (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="ES",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="RS") & 
#                        (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="RS",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="JD") & 
#                        (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="JD",]$Fly.Number))|
#                     ((all_ofs_WT$Experimenter=="SW") & 
#                        (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="SW",]$Fly.Number))
# )
# 
# R_bel_test = subset(all_ofs_WT, 
#                     ((all_ofs_WT$Experimenter=="ES") & 
#                        (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="ES",]$Fly.Number))|
#                       ((all_ofs_WT$Experimenter=="RS") & 
#                          (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="RS",]$Fly.Number))|
#                       ((all_ofs_WT$Experimenter=="JD") & 
#                          (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="JD",]$Fly.Number))|
#                       ((all_ofs_WT$Experimenter=="SW") & 
#                          (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="SW",]$Fly.Number))
# )
# 
# 
# 
# # Raw data plot 
# num = c(length(R_abv_test[R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active),
#         length(R_bel_test[R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active))
# 
# m = data.frame(
#   factor = c(
#     rep("R-Top50", length(R_abv_test[R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active)),
#     rep("R-Bottom50", length(R_bel_test[R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active))),
#   value = as.numeric(c(
#     R_abv_test[R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active,
#     R_bel_test[R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active
#   ))
# )
# 
# 
# colnames(m) = c("Segment", "Value")
# m$Segment = factor(m$Segment, levels = c("R-Top50", "R-Bottom50"))
# 
# a = dunn.test(x = m$Value, g = m$Segment, method = c("bonferroni"))
# 
# col.pool = c(
#   "light blue", "dark blue")
# 
# boxplot(
#   Value ~ Segment,
#   data = m
# )
# 
# stripchart(
#   Value ~ Segment,
#   vertical = TRUE,
#   data = m,
#   method = "jitter",
#   add = TRUE,
#   pch = 15,
#   cex = 0.5,
#   col =  col.pool
# )
# 
# # Get diff boxplot
# 
# R_bel_diff = c()
# 
# for (i in 1:nrow(R_med_bel)){
#   temp = R_bel_test[R_bel_test$Fly.Number==R_med_bel[i,]$Fly.Number &
#                     R_bel_test$Experimenter == R_med_bel[i, ]$Experimenter &
#                     R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active -
#          R_bel_test[R_bel_test$Fly.Number==R_med_bel[i,]$Fly.Number & 
#                     R_bel_test$Experimenter == R_med_bel[i, ]$Experimenter & 
#                     R_bel_test$Session=="E1", ]$Percentage.Time.Active
#   R_bel_diff = c(R_bel_diff, temp)
#   }
# 
# R_abv_diff = c()
# 
# for (i in 1:nrow(R_med_abv)){
#   temp = R_abv_test[R_abv_test$Fly.Number==R_med_abv[i,]$Fly.Number &
#                     R_abv_test$Experimenter == R_med_abv[i, ]$Experimenter &
#                     R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active -
#          R_abv_test[R_abv_test$Fly.Number==R_med_abv[i,]$Fly.Number &
#                     R_abv_test$Experimenter == R_med_abv[i, ]$Experimenter &
#                     R_abv_test$Session=="E1", ]$Percentage.Time.Active
#   R_abv_diff = c(R_abv_diff, temp)
# }
# 
# num = c(
#   length(R_abv_diff),
#   length(R_bel_diff))
# 
# m = data.frame(
#   factor = c(
#     rep("R-Top50", length(R_abv_diff)),
#     rep("R-Bottom50", length(R_bel_diff))),
#   value = as.numeric(c(
#     R_abv_diff,
#     R_bel_diff
#   ))
# )
# 
# colnames(m) = c("Segment", "Value")
# m$Segment = factor(m$Segment, levels = c("R-Top50", "R-Bottom50"))
# 
# col.pool = c("light blue", "dark blue")
# 
# boxplot(
#   Value ~ Segment,
#   data = m
# )
# 
# stripchart(
#   Value ~ Segment,
#   vertical = TRUE,
#   data = m,
#   method = "jitter",
#   add = TRUE,
#   pch = 15,
#   cex = 0.5,
#   col =  col.pool
# )

# Regression of laser exposure to activity level

laser_vs_pta = data.frame()
pta_R = c()
for (i in 1:nrow(laser_R)){
  temp = all_ofs_WT[all_ofs_WT$experimenter==laser_R[i, ]$experimenter & all_ofs_WT$flynum == laser_R[i, ]$flynum & 
                    all_ofs_WT$genotype == laser_R[i, ]$genotype & all_ofs_WT$session == "E1R1E1R1E1", ]$percentage.time.active -
         all_ofs_WT[all_ofs_WT$experimenter==laser_R[i, ]$experimenter & all_ofs_WT$flynum == laser_R[i, ]$flynum & 
                    all_ofs_WT$genotype == laser_R[i, ]$genotype & all_ofs_WT$session == "E1", ]$percentage.time.active 
  pta_R = c(pta_R, temp)
}
laser_vs_pta = cbind(laser_R, pta_R)

# laser_vs_pta31 = data.frame()
# pta_R31 = c()
# for (i in 1:nrow(laser_R31)){
#   temp31 = all_ofs_WT[all_ofs_WT$Experimenter==laser_R31[i, ]$Experimenter & all_ofs_WT$Fly.Number == laser_R31[i, ]$Fly.Number & 
#                         all_ofs_WT$Genotype == laser_R31[i, ]$Genotype & all_ofs_WT$Session == "E1R1E1", ]$Percentage.Time.Active -
#     all_ofs_WT[all_ofs_WT$Experimenter==laser_R31[i, ]$Experimenter & all_ofs_WT$Fly.Number == laser_R31[i, ]$Fly.Number & 
#                  all_ofs_WT$Genotype == laser_R31[i, ]$Genotype & all_ofs_WT$Session == "E1", ]$Percentage.Time.Active 
#   pta_R31 = c(pta_R31, temp31)
# }
# 
# laser_vs_pta31 = cbind(laser_R31, pta_R31)

cor(laser_vs_pta$laser_exposure, laser_vs_pta$pta_R, method = c("pearson"))

pdf("Exposure_Duration_vs_Activity_Change_052719.pdf", width = 10, height = 8)
plot(laser_vs_pta$laser_exposure, 
     laser_vs_pta$pta_R, 
     ylim = c(-1, 1),
     col = "light blue",
     pch = 15,
     yaxt = "n",
     xaxt = "n",
     cex.lab = 1.5,
     xlab = "Duration (sec)",
     ylab = "Activity Difference")

axis(side=2, at=c(-1, -0.5, 0, 0.5, 1.0), cex.axis = 1.5)
axis(side=1, at=c(0, 500, 1000, 1500, 2000), cex.axis = 1.5)
model = lm(formula = laser_vs_pta$pta_R ~ laser_vs_pta$laser_exposure)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = 500, y = 0.8, paste0("Slope = ",sprintf("%.5f", model$coefficients[[2]])), cex = 1.5)
text(x = 600, y = 0.7, paste0("s.e. = ", sprintf("%.5f", coef(summary(model))[2,2])), cex = 1.5)
text(x = 800, y = 0.6, paste0("Correlation coefficient = ", sprintf("%.5f", cor(laser_vs_pta$laser_exposure, laser_vs_pta$pta_R, method = c("pearson"))
)), cex = 1.5)
dev.off()


plot(laser_vs_pta31$Laser_Exposure, 
     laser_vs_pta31$pta_R31, 
     ylim = c(-1, 1),
     col = "light blue",
     pch = 15,
     yaxt = "n",
     xaxt = "n",
     cex.lab = 1.5,
     xlab = "Time (sec)",
     ylab = "Activity Difference")
model = lm(formula = laser_vs_pta31$pta_R31 ~ laser_vs_pta31$Laser_Exposure)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = 500, y = 0.7, paste0("Slope = ",sprintf("%.4f", model$coefficients[[2]])), cex = 1.5)
text(x = 500, y = 0.6, paste0("s.e. = ", sprintf("%.4f", coef(summary(model))[2,2])), cex = 1.5)
text(x = 500, y = 0.5, paste0("R^2 = ", sprintf("%.4f", summary(model)$r.squared)), cex = 1.5)


# Segment R-Control group by initial activity level and regress the activity change to laser exposure

activity_med = median(all_ofs_WT[all_ofs_WT$Type=="R" & all_ofs_WT$Session=="E1", ]$Percentage.Time.Active)
initial_act_laser = data.frame()

for (i in 1:nrow(laser_R)){
  # get diff
  initial = all_ofs_WT[all_ofs_WT$Type=="R" & all_ofs_WT$Session=="E1" & 
                         all_ofs_WT$Fly.Number == laser_R[i, ]$Fly.Number & 
                         all_ofs_WT$Experimenter == laser_R[i, ]$Experimenter &
                         all_ofs_WT$Genotype == laser_R[i, ]$Genotype, ]$Percentage.Time.Active
  end_test = all_ofs_WT[all_ofs_WT$Type=="R" & all_ofs_WT$Session=="E1R1E1R1E1" & 
                          all_ofs_WT$Fly.Number == laser_R[i, ]$Fly.Number & 
                          all_ofs_WT$Experimenter == laser_R[i, ]$Experimenter &
                          all_ofs_WT$Genotype == laser_R[i, ]$Genotype, ]$Percentage.Time.Active
  difference = end_test - initial
  temp = cbind(laser_R[i, ], initial, end_test, difference)
  initial_act_laser = rbind(initial_act_laser, temp)
}
initial_med = median(initial_act_laser$initial)


# Plot Flies with More Initial Activity
plot(initial_act_laser[initial_act_laser$initial>=initial_med,]$Laser_Exposure,
     initial_act_laser[initial_act_laser$initial>=initial_med,]$difference, main="Flies Initially More Active")

cor(initial_act_laser[initial_act_laser$initial>=initial_med,]$Laser_Exposure, 
    initial_act_laser[initial_act_laser$initial>=initial_med,]$difference, 
    method = c("pearson"))
model = lm(formula = initial_act_laser[initial_act_laser$initial>=initial_med,]$difference ~ 
             initial_act_laser[initial_act_laser$initial>=initial_med,]$Laser_Exposure)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = 1000, y = -0.4, paste0("Slope = ",model$coefficients[[2]], ", s.e. = ", coef(summary(model))[2,2]))


# Plot Flies with Less Initial Activity
plot(initial_act_laser[initial_act_laser$initial<initial_med,]$Laser_Exposure,
     initial_act_laser[initial_act_laser$initial<initial_med,]$difference, main="Flies Initially Less Active")
cor(initial_act_laser[initial_act_laser$initial<initial_med,]$Laser_Exposure, 
    initial_act_laser[initial_act_laser$initial<initial_med,]$difference, 
    method = c("pearson"))

model = lm(formula = initial_act_laser[initial_act_laser$initial<initial_med,]$difference ~ 
             initial_act_laser[initial_act_laser$initial<initial_med,]$Laser_Exposure)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = 1000, y = -0.4, paste0("Slope = ",model$coefficients[[2]], ", s.e. = ", coef(summary(model))[2,2]))

