source("plot_trend.R")
setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")

#Quantify the mean of delay onset/off of laser of one file

fly.info.movement.T = fly.info.include[((fly.info.include$Genotype == "WT") |
                                          (fly.info.include$Genotype == "CS")) &
                                         (fly.info.include$Category =="T")&
                                         (fly.info.include$experimenter!="SW"), ]

fly.info.movement.R = fly.info.include[((fly.info.include$Genotype == "WT") |
                                          (fly.info.include$Genotype == "CS")) &
                                         (fly.info.include$Category == "R")&
                                         (fly.info.include$experimenter!="SW"), ]

first_yoked_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1R1",
                                                         fly.info.movement.R)
first_yoked_session = first_yoked_session[complete.cases(first_yoked_session), ]

second_yoked_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1R1E1R1", 
                                                          fly.info.movement.R)
second_yoked_session = second_yoked_session[complete.cases(second_yoked_session), ]

first_training_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1T1", 
                                                            fly.info.movement.T)
first_training_session = first_training_session[complete.cases(first_training_session), ]

second_training_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1T1E1T1",
                                                             fly.info.movement.T)
second_training_session = second_training_session[complete.cases(second_training_session), ]

pdf(paste0("ChanceofBeingHitCS_", Sys.Date(),".pdf"),
    onefile = T, width = 5, height = 5)

Chance_of_being_hit = list(
  
  first_training_session$`Chances of being hit during walking`,
  first_training_session$`Chances of being hit during pause `,
  
  first_yoked_session$`Chances of being hit during walking`,
  first_yoked_session$`Chances of being hit during pause `,
  
  second_training_session$`Chances of being hit during walking`,
  second_training_session$`Chances of being hit during pause `,
  
  second_yoked_session$`Chances of being hit during walking`,
  second_yoked_session$`Chances of being hit during pause `
)

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
  lwd = 2,
  ylab = "Percentage",
  xaxt = "n",
  col = col.pool,
  # main = "Chance of being punished in first session",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[1:4],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  "grey40"
)

text(
  x = (1:length(Chance_of_being_hit[1:4])) - 0.1,
  y = 1.02,
  labels = c(
    length(Chance_of_being_hit[[1]]),
    length(Chance_of_being_hit[[2]]),
    length(Chance_of_being_hit[[3]]),
    length(Chance_of_being_hit[[4]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = (1:length(Chance_of_being_hit[1:4])) - 0.3,
  y = -0.1,
  labels = c(
    "T - Walking",
    "T - Pause",
    "R - Walking",
    "R - Pause"
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)
# lines(c(4.5, 4.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)
# lines(c(6.5, 6.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)

boxplot(
  Chance_of_being_hit[5:8],
  ylim = c(0, 1),
  outline = F,
  notch = F,
  lwd = 2,
  ylab = "Percentage",
  xaxt = "n",
  col = col.pool,
  # main = "Chance of being punished in second session",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[5:8],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  "grey40"
)

text(
  x = (1:length(Chance_of_being_hit[5:8])) - 0.1,
  y = 1.02,
  labels = c(
    length(Chance_of_being_hit[[5]]),
    length(Chance_of_being_hit[[6]]),
    length(Chance_of_being_hit[[7]]),
    length(Chance_of_being_hit[[8]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = (1:length(Chance_of_being_hit[5:8])) - 0.3,
  y = -0.1,
  labels = c(
    "T - Walking",
    "T - Pause",
    "R - Walking",
    "R - Pause"
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)
# lines(c(4.5, 4.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)
# lines(c(6.5, 6.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)


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

