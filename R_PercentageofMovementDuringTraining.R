setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

#Quantify the mean of delay onset/off of laser of one file
fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)

# Segmenting both the T and R flies' the Exposure Differential to [-0.2, 0.2]
# After 1st training session
R1 = Hit_by_laser("E1R1", fly.info.movement.R)
R1 = R1[!is.na(R1$Hit_W), ]
R1$Diff = R1$Hit_W - R1$Hit_P
R1$ActDiff = NA
for (i in 1:nrow(R1)){
  R1[i, ]$ActDiff = all_ofs_WT[all_ofs_WT$Experimenter == R1[i, ]$Experimenter &
                               all_ofs_WT$Fly.Number == R1[i, ]$Fly & 
                               all_ofs_WT$Genotype == R1[i, ]$Genotype & 
                               all_ofs_WT$Session == "E1R1E1", ]$Percentage.Time.Active -
                    all_ofs_WT[all_ofs_WT$Experimenter == R1[i, ]$Experimenter &
                               all_ofs_WT$Fly.Number == R1[i, ]$Fly & 
                               all_ofs_WT$Genotype == R1[i, ]$Genotype & 
                               all_ofs_WT$Session == "E1" & 
                               all_ofs_WT$Type == "R", ]$Percentage.Time.Active 
}

T1 = Hit_by_laser("E1T1", fly.info.movement.T)
T1 = T1[!is.na(T1$Hit_W), ]
T1$Diff = T1$Hit_W - T1$Hit_P
T1$ActDiff = NA
for (i in 1:nrow(T1)){
  T1[i, ]$ActDiff = all_ofs_WT[all_ofs_WT$Experimenter == T1[i, ]$Experimenter &
                               all_ofs_WT$Fly.Number == T1[i, ]$Fly &
                               all_ofs_WT$Genotype == T1[i, ]$Genotype &
                               all_ofs_WT$Session == "E1T1E1", ]$Percentage.Time.Active -
                    all_ofs_WT[all_ofs_WT$Experimenter == T1[i, ]$Experimenter &
                               all_ofs_WT$Fly.Number == T1[i, ]$Fly &
                               all_ofs_WT$Genotype == T1[i, ]$Genotype &
                               all_ofs_WT$Session == "E1" &
                               all_ofs_WT$Type == "T", ]$Percentage.Time.Active
}

RT_val = subset_laser_expo(fly.info.end, Random = R1, Training = T1, threshold = 0.2)

plot(R1$Diff, R1$ActDiff, 
     main = "Exposure Probability Difference vs Changes in Activity After 1st Training",
     xlab = "Exposure Differential =  Probability of Exposure during Walking - Probability of Exposure during Pause",
     ylab = "Activity Difference",
     xlim = c(-1, 1), ylim = c(-1, 1),
     col = "light blue",
     pch = 15)
# points(T1$Diff, T1$ActDiff, col = "indianred3",  pch = 15)
model = lm(formula = R1$ActDiff ~ R1$Diff)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = -0.6, y = -0.4, paste0("Slope = ", sprintf("%.3f",model$coefficients[[2]])))
text(x = -0.6, y = -0.5, paste0("s.e. = ", sprintf("%.3f",coef(summary(model))[2,2])))
text(x = -0.6, y = -0.6, paste0("correlation = ", sprintf("%.3f", 
                                                          cor(R1$Diff, R1$ActDiff, method = c("pearson")))))

# After second training session

R2 = Hit_by_laser("E1R1E1R1", fly.info.movement.R)
R2 = R2[!is.na(R2$Hit_W), ]
R2$Diff = R2$Hit_W - R2$Hit_P
a = c()
R2$ActDiff = NA
for (i in 1:nrow(R2)){
  temp = all_ofs_WT[all_ofs_WT$Experimenter == R2[i, ]$Experimenter & 
                      all_ofs_WT$Fly.Number == R2[i, ]$Fly &
                      all_ofs_WT$Genotype == R2[i, ]$Genotype & 
                      all_ofs_WT$Session == "E1R1E1R1E1", ]$Percentage.Time.Active -
               all_ofs_WT[all_ofs_WT$Experimenter == R2[i, ]$Experimenter & 
                            all_ofs_WT$Fly.Number == R2[i, ]$Fly & 
                            all_ofs_WT$Genotype == R2[i, ]$Genotype & 
                            all_ofs_WT$Session == "E1R1E1" & all_ofs_WT$Type == "R", ]$Percentage.Time.Active 

  R2[i, ]$ActDiff = temp
}

T2 = Hit_by_laser( "E1T1E1T1",fly.info.movement.T)
T2 = T2[!is.na(T2$Hit_W), ]
T2$Diff = T2$Hit_W - T2$Hit_P
T2$ActDiff = NA
for (i in 1:nrow(T2)){
  T2[i, ]$ActDiff = all_ofs_WT[all_ofs_WT$Experimenter == T2[i, ]$Experimenter &
                               all_ofs_WT$Fly.Number == T2[i, ]$Fly &
                               all_ofs_WT$Genotype == T2[i, ]$Genotype &
                               all_ofs_WT$Session == "E1T1E1T1E1", ]$Percentage.Time.Active -
    all_ofs_WT[all_ofs_WT$Experimenter == T2[i, ]$Experimenter &
               all_ofs_WT$Fly.Number == T2[i, ]$Fly &
               all_ofs_WT$Genotype == T2[i, ]$Genotype &
               all_ofs_WT$Session == "E1T1E1" &
               all_ofs_WT$Type == "T", ]$Percentage.Time.Active
}

RT2_val = subset_laser_expo(fly.info.end, Random = R2, Training = T2, threshold = 0.2)

boxplot(T2$ActDiff, R2$ActDiff, 
        RT2_val[RT2_val$Category=="T", ]$ActDiff, 
        RT2_val[RT2_val$Category=="R", ]$ActDiff)

plot(R2$Diff, R2$ActDiff, 
     main = "Exposure Probability Difference vs Changes in Activity After 1st Training",
     xlab = "Exposure Differential =  Probability of Exposure during Walking - Probability of Exposure during Pause",
     ylab = "Activity Difference",
     xlim = c(-1, 1), ylim = c(-1, 1), 
     col = "light blue",
     pch = 15)
# points(T2$Diff, T2$ActDiff, col = "red")

model = lm(formula = R2$ActDiff ~ R2$Diff)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))

text(x = -0.6, y = -0.4, paste0("Slope = ", sprintf("%.3f",model$coefficients[[2]])))
text(x = -0.6, y = -0.5, paste0("s.e. = ", sprintf("%.3f",coef(summary(model))[2,2])))
text(x = -0.6, y = -0.6, paste0("correlation = ", sprintf("%.3f", 
                                                          cor(R2$Diff, R2$ActDiff, method = c("pearson")))))

pdf("ExpoDiff_and_ActDiff.pdf", onefile = T, width = 8, height = 8)
R1_with_laser = R1
plot(R1_with_laser$Diff, R1_with_laser$ActDiff, 
     xlab = "Exposure Differential",
     ylab = "Activity Difference",
     xlim = c(-1, 1), ylim = c(-1, 1),
     col = "light blue",
     pch = 20,
     cex.lab = 1.5,
     xaxt = "n",
     yaxt = "n")
axis(1, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
axis(2, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
model_R1_with_laser = lm(formula = R1_with_laser$ActDiff ~ R1_with_laser$Diff)
abline(model_R1_with_laser$coefficients[[1]], model_R1_with_laser$coefficients[[2]])
coef(summary(model_R1_with_laser))
text(x = -0.6, 
     y = 0.6, 
     paste0("Slope = ",sprintf("%.3f",model_R1_with_laser$coefficients[[2]])),
     cex = 1.5
)
text(x = -0.6, 
     y = 0.5, 
     paste0("Standard Error = ", sprintf("%.3f",coef(summary(model_R1_with_laser))[2,2])),
     cex = 1.5)
text(x = -0.6, 
     y = 0.4, 
     paste0("R^2 = ",
            sprintf("%.3f", summary(model_R1_with_laser)$r.squared)),
     cex = 1.5)

R2_with_laser = R2
plot(R2_with_laser$Diff, R2_with_laser$ActDiff, 
     xlab = "Exposure Differential",
     ylab = "Activity Difference",
     xlim = c(-1, 1), ylim = c(-1, 1),
     col = "light blue",
     pch = 20,
     cex.lab = 1.5,
     xaxt = "n",
     yaxt = "n")
axis(1, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
axis(2, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
model_with_laser = lm(formula = R2_with_laser$ActDiff ~ R2_with_laser$Diff)
abline(model_with_laser$coefficients[[1]], model_with_laser$coefficients[[2]])
coef(summary(model_with_laser))
text(x = -0.6, 
     y = 0.6, 
     paste0("Slope = ",sprintf("%.3f",model_with_laser$coefficients[[2]])),
     cex = 1.5
)
text(x = -0.6, 
     y = 0.5, 
     paste0("Standard Error = ", sprintf("%.3f",coef(summary(model_with_laser))[2,2])),
     cex = 1.5)
text(x = -0.6, 
     y = 0.4, 
     paste0("R^2 = ", 
            sprintf("%.3f", summary(model_with_laser)$r.squared)),
     cex = 1.5)
dev.off()

pdf("ExpoDiff_and_ActDiff_T.pdf", onefile = T, width = 8, height = 8)
T1_with_laser = T1
plot(T1_with_laser$Diff, T1_with_laser$ActDiff, 
     xlab = "Exposure Differential",
     ylab = "Activity Difference",
     xlim = c(-1, 1), ylim = c(-1, 1),
     col = "indianred3",
     pch = 20,
     cex.lab = 1.5,
     xaxt = "n",
     yaxt = "n")
axis(1, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
axis(2, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
model_T1_with_Laser = lm(formula = T1_with_laser$ActDiff ~ T1_with_laser$Diff)
abline(model_T1_with_Laser$coefficients[[1]], model_T1_with_Laser$coefficients[[2]])
coef(summary(model_T1_with_Laser))
text(x = -0.6, 
     y = 0.6, 
     paste0("Slope = ",sprintf("%.3f",model_T1_with_Laser$coefficients[[2]])),
     cex = 1.5
)
text(x = -0.6, 
     y = 0.5, 
     paste0("Standard Error = ", sprintf("%.3f",coef(summary(model_T1_with_Laser))[2,2])),
     cex = 1.5)
text(x = -0.6, 
     y = 0.4, 
     paste0("Correlation = ", 
            sprintf("%.3f", cor(T1_with_laser$Diff, T1_with_laser$ActDiff, method = c("pearson")))),
     cex = 1.5)

T2_with_laser = T2[T2$Hit_All>0, ]
T2_with_laser = T2
plot(T2_with_laser$Diff, T2_with_laser$ActDiff, 
     xlab = "Exposure Differential",
     ylab = "Activity Difference",
     xlim = c(-1, 1), ylim = c(-1, 1),
     col = "indianred3",
     pch = 20,
     cex.lab = 1.5,
     xaxt = "n",
     yaxt = "n")
axis(1, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
axis(2, at=c(-1, -0.5, 0, 0.5, 1), cex.axis = 1.5)
model_with_laser = lm(formula = T2_with_laser$ActDiff ~ T2_with_laser$Diff)
abline(model_with_laser$coefficients[[1]], model_with_laser$coefficients[[2]])
coef(summary(model_with_laser))
text(x = -0.6, 
     y = 0.6, 
     paste0("Slope = ",sprintf("%.3f",model_with_laser$coefficients[[2]])),
     cex = 1.5
)
text(x = -0.6, 
     y = 0.5, 
     paste0("Standard Error = ", sprintf("%.3f",coef(summary(model_with_laser))[2,2])),
     cex = 1.5)
text(x = -0.6, 
     y = 0.4, 
     paste0("r = ", 
            sprintf("%.3f", cor(T2_with_laser$Diff, T2_with_laser$ActDiff, method = c("pearson")))),
     cex = 1.5)
dev.off()

pdf(paste0("ChanceofBeingHitCS_", Sys.Date(),".pdf"),
    onefile = T, width = 8, height = 8)

Chance_of_being_hit = list(
  T1$Hit_W,
  T1$Hit_P,
  R1$Hit_W,
  R1$Hit_P,
  T2$Hit_W,
  T2$Hit_P,
  R2$Hit_W,
  R2$Hit_P
)

p = c(wilcox.test(Chance_of_being_hit[[1]], Chance_of_being_hit[[2]])$p.value,
      wilcox.test(Chance_of_being_hit[[3]], Chance_of_being_hit[[4]])$p.value,
      wilcox.test(Chance_of_being_hit[[5]], Chance_of_being_hit[[6]])$p.value,
      wilcox.test(Chance_of_being_hit[[7]], Chance_of_being_hit[[8]])$p.value)

sig = c()
for (i in 1:length(p)){
  if (p[i] >= 0.05){
    significance = "n.s."
  }else if (p[i] < 0.05 & p[i] >= 0.01){
    significance = "*"
  }else if (p[i] < 0.01 & p[i] >= 0.001){
    significance = "**"
  }else if (p[i] < 0.001 & p[i] >= 0.0001){
    significance = "***"
  }else if (p[i] < 0.0001){
    significance = "****"
  }
  sig = c(sig, significance)
}

col.pool <- c( "indianred3",
               "indianred3",
               "light blue",
               "light blue"
               )
boxplot(
  Chance_of_being_hit[1:4],
  ylim = c(0, 1),
  outline = F,
  notch = F,
  medlwd = 1,
  ylab = "Likelihood",
  xaxt = "n",
  ann = FALSE,
  axes=F,
  cex.lab = 1.5
)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), cex.axis = 1.5)
stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[1:4],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  col.pool
)

text(
  x = c(1.3, 3.3),
  y = 1.08,
  labels = c(
    paste0("n = ", length(Chance_of_being_hit[[1]])),
    paste0("n = ", length(Chance_of_being_hit[[3]]))
  ),
  xpd = T,
  srt = 0,
  adj = 0,
  cex = 1.5
)

text(
  x = (1:length(Chance_of_being_hit[1:4])) - 0.25,
  y = -0.05,
  labels = c(
    "Walking",
    "Pause",
    "Walking",
    "Pause"
  ),
  xpd = T,
  srt = 0,
  adj = 0,
  cex = 1.5
)

text(
  x = c(1.3, 3),
  y = -0.1,
  labels = c(
    "Train",
    "Yoked Control"
  ),
  xpd = T,
  srt = 0,
  adj = 0,
  cex = 1.5
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)

text(
     x = c(1.5, 3.5),
     y = 1.03,
     c(sig[1], sig[2]),
     xpd = NA, cex = 1.5)

lines(c(1, 2), 
      y = c(1.01, 1.01), 
      xpd = NA)

lines(c(3, 4), 
      y = c(1.01, 1.01), 
      xpd = NA)
lines(c(1,1), c(1.00, 1.01))
lines(c(2,2), c(1.00, 1.01))
lines(c(3,3), c(1.00, 1.01))
lines(c(4,4), c(1.00, 1.01))

boxplot(
  Chance_of_being_hit[5:8],
  ylim = c(0, 1),
  outline = F,
  notch = F,
  medlwd = 1,
  ylab = "Likelihood",
  xaxt = "n",
  ann = FALSE,
  axes=F,
  cex.lab = 1.5
)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0), cex.axis = 1.5)
stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[5:8],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  col.pool
)

text(
  x = c(1.3, 3.3),
  y = 1.08,
  labels = c(
    paste0("n = ", length(Chance_of_being_hit[[5]])),
    paste0("n = ", length(Chance_of_being_hit[[7]]))
  ),
  xpd = T,
  srt = 0,
  adj = 0,
  cex = 1.5
)

text(
  x = (1:length(Chance_of_being_hit[5:8])) - 0.25,
  y = -0.05,
  labels = c(
    "Walking",
    "Pause",
    "Walking",
    "Pause"
  ),
  xpd = T,
  srt = 0,
  adj = 0,
  cex = 1.5
)

text(
  x = c(1.3, 3),
  y = -0.1,
  labels = c(
    "Train",
    "Yoked Control"
  ),
  xpd = T,
  srt = 0,
  adj = 0,
  cex = 1.5
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)

text(
  x = c(1.5, 3.5),
  y = 1.03,
  c(sig[3], sig[4]),
  xpd = NA, cex = 1.5)

lines(c(1, 2), 
      y = c(1.01, 1.01), 
      xpd = NA)

lines(c(3, 4), 
      y = c(1.01, 1.01), 
      xpd = NA)
lines(c(1,1), c(1.00, 1.01))
lines(c(2,2), c(1.00, 1.01))
lines(c(3,3), c(1.00, 1.01))
lines(c(4,4), c(1.00, 1.01))

dev.off()

chance_df = data.frame()

chance_1 = Chance_of_being_hit[[1]][!is.na(Chance_of_being_hit[[1]])]
chance_1_lab = rep("T1-W", length(chance_1))
a = cbind.data.frame(chance_1, chance_1_lab)

chance_2 = Chance_of_being_hit[[2]][!is.na(Chance_of_being_hit[[2]])]
chance_2_lab = rep("T1-P", length(chance_2))

chance_3 = Chance_of_being_hit[[3]][!is.na(Chance_of_being_hit[[3]])]
chance_3_lab = rep("R1-W", length(chance_3))

chance_4 = Chance_of_being_hit[[4]][!is.na(Chance_of_being_hit[[4]])]
chance_4_lab = rep("R1-P", length(chance_4))

chance_5 = Chance_of_being_hit[[5]][!is.na(Chance_of_being_hit[[5]])]
chance_5_lab = rep("T2-W", length(chance_5))

chance_6 = Chance_of_being_hit[[6]][!is.na(Chance_of_being_hit[[6]])]
chance_6_lab = rep("T2-P", length(chance_6))

chance_7 = Chance_of_being_hit[[7]][!is.na(Chance_of_being_hit[[7]])]
chance_7_lab = rep("R2-W", length(chance_7))

chance_8 = Chance_of_being_hit[[8]][!is.na(Chance_of_being_hit[[8]])]
chance_8_lab = rep("R2-P", length(chance_8))

# Confidence Interval
CI_df = data.frame()
for (i in 1:8){
  CI_df = rbind.data.frame(CI_df, get_Wald_CI(Chance_of_being_hit[[i]][!is.na(Chance_of_being_hit[[i]])]))
}
colnames(CI_df) = c("Median", "CI_Lower", "CI_Upper")

