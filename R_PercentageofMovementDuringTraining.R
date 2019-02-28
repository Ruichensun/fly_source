source("plot_trend.R")
setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")

#Quantify the mean of delay onset/off of laser of one file
fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)

R1 = Hit_by_laser("E1R1", fly.info.movement.R)
R1 = R1[!is.na(R1$Hit_W), ]
R1$Diff = R1$Hit_W - R1$Hit_P
R1$ActDiff = NA
for (i in 1:nrow(R1)){
  R1[i, ]$ActDiff = all_ofs_WT[all_ofs_WT$Experimenter == R1[i, ]$Experimenter & all_ofs_WT$Fly.Number == R1[i, ]$Fly & 
                            all_ofs_WT$Genotype == R1[i, ]$Genotype & all_ofs_WT$Session == "E1R1E1", ]$Percentage.Time.Active -
               all_ofs_WT[all_ofs_WT$Experimenter == R1[i, ]$Experimenter & all_ofs_WT$Fly.Number == R1[i, ]$Fly & 
                            all_ofs_WT$Genotype == R1[i, ]$Genotype & all_ofs_WT$Session == "E1" & all_ofs_WT$Type == "R", ]$Percentage.Time.Active 
}
plot(R1$Diff, R1$ActDiff, main = "Exposure Probability Difference vs Changes in Activity After 1st Training",
     xlab = "Exposure Differential =  Probability of Exposure during Walking - Probability of Exposure during Pause",
     ylab = "Activity Difference")

model = lm(formula = R1$ActDiff ~ R1$Diff)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = -0.3, y = -0.4, paste0("Slope = ",model$coefficients[[2]]))
text(x = -0.3, y = -0.5, paste0("s.e. = ", coef(summary(model))[2,2]))
text(x = -0.3, y = -0.6, paste0("correlation = ", cor(R1$Diff, R1$ActDiff, method = c("pearson"))))



R2 = Hit_by_laser("E1R1E1R1", fly.info.movement.R)
R2 = R2[!is.na(R2$Hit_W), ]
R2$Diff = R2$Hit_W - R2$Hit_P
a = c()
R2$ActDiff = NA
for (i in 1:nrow(R2)){
  temp = all_ofs_WT[all_ofs_WT$Experimenter == R2[i, ]$Experimenter & all_ofs_WT$Fly.Number == R2[i, ]$Fly &
                            all_ofs_WT$Genotype == R2[i, ]$Genotype & all_ofs_WT$Session == "E1R1E1R1E1", ]$Percentage.Time.Active -
               all_ofs_WT[all_ofs_WT$Experimenter == R2[i, ]$Experimenter & all_ofs_WT$Fly.Number == R2[i, ]$Fly & 
                            all_ofs_WT$Genotype == R2[i, ]$Genotype & all_ofs_WT$Session == "E1" & all_ofs_WT$Type == "R", ]$Percentage.Time.Active 

  R2[i, ]$ActDiff = temp
}
plot(R2$Diff, R2$ActDiff, main = "Exposure Probability Difference vs Changes in Activity After 2nd Training",
     xlab = "Exposure Differential =  Probability of Exposure during Walking - Probability of Exposure during Pause",
     ylab = "Activity Difference")

A = c(R2$Diff[1:30], R2$Diff[32:144])
B = c(R2$ActDiff[1:30], R2$ActDiff[32:144])
model = lm(formula = B ~ A)
abline(model$coefficients[[1]], model$coefficients[[2]])
coef(summary(model))
text(x = -0.3, y = -0.4, paste0("Slope = ",model$coefficients[[2]]))
text(x = -0.3, y = -0.5, paste0("s.e. = ", coef(summary(model))[2,2]))


text(x = -0.3, y = -0.6, paste0("correlation = ", cor(A, B, method = c("pearson"))))




T1 = Hit_by_laser("E1T1", fly.info.movement.T)
# T1 = T1[complete.cases(T1), ]
T2 = Hit_by_laser( "E1T1E1T1",fly.info.movement.T)
# T2 = T2[complete.cases(T2), ]
pdf(paste0("ChanceofBeingHitCS_", Sys.Date(),".pdf"),
    onefile = T, width = 5, height = 5)

Chance_of_being_hit = list(
  T1$`Chances of being hit during walking`,
  T1$`Chances of being hit during pause `,
  R1$`Chances of being hit during walking`,
  R1$`Chances of being hit during pause `,
  T2$`Chances of being hit during walking`,
  T2$`Chances of being hit during pause `,
  R2$`Chances of being hit during walking`,
  R2$`Chances of being hit during pause `
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
  ylab = "Percentage",
  xaxt = "n",
  ann = FALSE,
  axes=F
)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
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
  x = c(1.35, 3.35),
  y = 1.2,
  labels = c(
    paste0("n = ", length(Chance_of_being_hit[[1]])),
    paste0("n = ", length(Chance_of_being_hit[[3]]))
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = (1:length(Chance_of_being_hit[1:4])) - 0.1,
  y = -0.1,
  labels = c(
    "Walking",
    "Pause",
    "Walking",
    "Pause"
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = c(1.35, 3.35),
  y = -0.2,
  labels = c(
    "Training",
    "Yoked Control"
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)

text(
     x = c(1.5, 3.5),
     y = 1.1,
     c(sig[1], sig[2]),
     xpd = NA)

lines(c(1, 2), 
      y = c(1.07, 1.07), 
      xpd = NA)

lines(c(3, 4), 
      y = c(1.07, 1.07), 
      xpd = NA)

boxplot(
  Chance_of_being_hit[5:8],
  ylim = c(0, 1),
  outline = F,
  notch = F,
  medlwd = 1,
  ylab = "Percentage",
  xaxt = "n",
  ann = FALSE,
  axes=F
)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))

stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[5:8],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col = col.pool
)

text(
  x = c(1.35, 3.35),
  y = 1.2,
  labels = c(
    paste0("n = ", length(Chance_of_being_hit[[5]])),
    paste0("n = ", length(Chance_of_being_hit[[7]]))
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = (1:length(Chance_of_being_hit[5:8])) - 0.1,
  y = -0.1,
  labels = c(
    "Walking",
    "Pause",
    "Walking",
    "Pause"
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)


text(
  x = c(1.35, 3.35),
  y = -0.2,
  labels = c(
    "Training",
    "Yoked Control"
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = c(1.5, 3.5),
  y = 1.1,
  c(sig[3], sig[4]),
  xpd = NA)

lines(c(1, 2), 
      y = c(1.07, 1.07), 
      xpd = NA)

lines(c(3, 4), 
      y = c(1.07, 1.07), 
      xpd = NA)
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


chance_df_1st = rbind(
                  cbind(chance_1, chance_1_lab),
                  cbind(chance_2, chance_2_lab),
                  cbind(chance_3, chance_3_lab),
                  cbind(chance_4, chance_4_lab)
                 )

colnames(chance_df_1st) = c("Probability", "Sessions")
dunn.test(x=as.numeric(chance_df_1st[,1]), g=chance_df_1st[,2], method=c("bonferroni"))

chance_df_2nd = rbind(
                  cbind(chance_5, chance_5_lab),
                  cbind(chance_6, chance_6_lab),
                  cbind(chance_7, chance_7_lab),
                  cbind(chance_8, chance_8_lab)
                )

colnames(chance_df_2nd) = c("Probability", "Sessions")
dunn.test(x=as.numeric(chance_df_2nd[,1]), g=chance_df_2nd[,2], method=c("bonferroni"))

