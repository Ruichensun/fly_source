setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
library(zoo)
library(boot)
library(dunn.test)

# Finding out when the fly is moving vs not moving
# Input: fly_pos; Output: a vector of 0 and 1 of (length of input) - 1 
fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "T") , ]

T1 = Hit_by_laser("E1T1", fly.info.movement.T)
T1 = T1[!is.na(T1$Hit_W), ]
T1 = T1[, 1:6]

fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]
R1 = Hit_by_laser("E1R1", fly.info.movement.R)
R1 = R1[!is.na(R1$Hit_W), ]
R1 = R1[, 1:6]
fly.info.movement.N = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                        (fly.info.end$Genotype == "CS")) & 
                                        (fly.info.end$Category == "N") , ]

N1 = Hit_by_laser("E1N1", fly.info.movement.N)
N1 = N1[!is.na(N1$Hit_W), ]
N1 = N1[, 1:6]


###Including All Relevant Sessions
sessions <- c(
  "E1T1",
  "E1R1",
  "E1N1",
  "E1T1E1T1",
  "E1R1E1R1",
  "E1N1E1N1"
  )
cumsums_total = list(get_cumsums_total(sessions[1], T1),
                     get_cumsums_total(sessions[2], R1),
                     get_cumsums_total(sessions[3], N1),
                     get_cumsums_total(sessions[4], T1),
                     get_cumsums_total(sessions[5], R1),
                     get_cumsums_total(sessions[6], N1))

min_sequence_length = min(dim(cumsums_total[[1]])[1],
                          dim(cumsums_total[[2]])[1],
                          dim(cumsums_total[[3]])[1],
                          dim(cumsums_total[[4]])[1],
                          dim(cumsums_total[[5]])[1],
                          dim(cumsums_total[[6]])[1])

cumsums_mean = list(rowMeans(cumsums_total[[1]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[2]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[3]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[4]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[5]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[6]][1:min_sequence_length, ]))

cumsums_median = list(apply(cumsums_total[[1]][1:min_sequence_length, ], 1, median),
                      apply(cumsums_total[[2]][1:min_sequence_length, ], 1, median),
                      apply(cumsums_total[[3]][1:min_sequence_length, ], 1, median),
                      apply(cumsums_total[[4]][1:min_sequence_length, ], 1, median),
                      apply(cumsums_total[[5]][1:min_sequence_length, ], 1, median),
                      apply(cumsums_total[[6]][1:min_sequence_length, ], 1, median))

cumsums_CI = list(apply((cumsums_total[[1]][1:min_sequence_length,])/framerate, 1, get_Wald_CI),
                  apply((cumsums_total[[2]][1:min_sequence_length,])/framerate, 1, get_Wald_CI),
                  apply((cumsums_total[[3]][1:min_sequence_length,])/framerate, 1, get_Wald_CI),
                  apply((cumsums_total[[4]][1:min_sequence_length,])/framerate, 1, get_Wald_CI),
                  apply((cumsums_total[[5]][1:min_sequence_length,])/framerate, 1, get_Wald_CI),
                  apply((cumsums_total[[6]][1:min_sequence_length,])/framerate, 1, get_Wald_CI)
                  )
# cumsums_CI structure: for each of the 6 lists: 3 rows by 8147 columns. 
# First row is median
# Second row is CI lower bound
# Third row is CI upper bound

trained_1 = cumsums_total[[1]][min_sequence_length,] 
yoked_1   = cumsums_total[[2]][min_sequence_length,] 
blank_1   = cumsums_total[[3]][min_sequence_length,] 
trained_2 = cumsums_total[[4]][min_sequence_length,] 
yoked_2   = cumsums_total[[5]][min_sequence_length,] 
blank_2   = cumsums_total[[6]][min_sequence_length,] 
T1_label = rep("T1", length(trained_1))
R1_label = rep("R1", length(yoked_1))
N1_label = rep("N1", length(blank_1))
T2_label = rep("T2", length(trained_2))
R2_label = rep("R2", length(yoked_2))
N2_label = rep("N2", length(blank_2))
T1_df = cbind.data.frame(trained_1, T1_label)
R1_df = cbind.data.frame(yoked_1, R1_label)
N1_df = cbind.data.frame(blank_1, N1_label)
T2_df = cbind.data.frame(trained_2, T2_label)
R2_df = cbind.data.frame(yoked_2, R2_label)
N2_df = cbind.data.frame(blank_2, N2_label)
colnames(T1_df) = c("Value", "Session")
colnames(R1_df) = c("Value", "Session")
colnames(N1_df) = c("Value", "Session")
colnames(T2_df) = c("Value", "Session")
colnames(R2_df) = c("Value", "Session")
colnames(N2_df) = c("Value", "Session")

Session1_df = rbind.data.frame(T1_df, R1_df, N1_df)
Session2_df = rbind.data.frame(T2_df, R2_df, N2_df)

CAD = kruskal.test(Value~Session, data = Session1_df)
pairwise_CAD = pairwise.wilcox.test(Session1_df$Value, Session1_df$Session, p.adjust.method = "BH")
CAD2 = kruskal.test(Value~Session, data = Session2_df)
pairwise_CAD2 = pairwise.wilcox.test(Session2_df$Value, Session2_df$Session, p.adjust.method = "BH")
p = c(pairwise_CAD$p.value[1], pairwise_CAD2$p.value[1])

# Confidence Interval

CI_df_cumsum = data.frame()
for (i in 1:6){
  CI_df_cumsum = rbind.data.frame(CI_df_cumsum, get_Wald_CI(cumsums_total[[i]][min_sequence_length, ] / framerate ))
}

colnames(CI_df_cumsum) = c("Median", "CI_Lower", "CI_Upper")

##X axis coordinate
forward_index = c((1:min_sequence_length) / framerate)
reverse_index = rev(forward_index)
index = c(forward_index, reverse_index)

##Y axis coordinate
coordinates = list(append(cumsums_CI[[1]][2,], rev(cumsums_CI[[1]][3,])),
                   append(cumsums_CI[[2]][2,], rev(cumsums_CI[[2]][3,])),
                   append(cumsums_CI[[3]][2,], rev(cumsums_CI[[3]][3,])),
                   append(cumsums_CI[[4]][2,], rev(cumsums_CI[[4]][3,])),
                   append(cumsums_CI[[5]][2,], rev(cumsums_CI[[5]][3,])),
                   append(cumsums_CI[[6]][2,], rev(cumsums_CI[[6]][3,]))
                   )


pdf(paste0("Training_Session_CS_", Sys.Date(),".pdf"),
    onefile = T, width = 8, height = 8)
    # First training session
    plot(
      1,
      type = "n",
      xlab = "Time (sec)",
      ylab = "Cumulative Activity Duration (sec)",
      xlim = c(0, 200),
      ylim = c(0, 100),
      cex.lab = 1.5,
      xaxt = "n",
      yaxt = "n"
    )
    axis(side=1, at=c(0, 50, 100, 150, 200), cex.axis = 1.5)
    axis(side=2, at=c(0, 20, 40, 60, 80, 100), cex.axis = 1.5)
    
    polygon(
      index,
      coordinates[[1]],
      lty = 2,
      lwd = 2,
      border = NA,
      col = rgb(0.8, 0, 0, 0.1)
    )
    polygon(
      index,
      coordinates[[2]],
      lty = 2,
      lwd = 2,
      border = NA,
      col = rgb(0, 0, 0.8, 0.1)
    )
    polygon(
      index,
      coordinates[[3]],
      lty = 2,
      lwd = 2,
      border = NA,
      col = rgb(0.5, 0.5, 0.5, 0.1)
    )
    
    lines(
      forward_index,
      cumsums_CI[[1]][1,],
      lty = 1,
      lwd = 3,
      col = rgb(0.8, 0, 0, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[2]][1,],
      lty = 1,
      lwd = 3,
      col = rgb(0.0, 0, 0.8, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[3]][1,],
      lty = 1,
      lwd = 3,
      col = rgb(0.5, 0.5, 0.5, 0.5)
    )
    
    # T - Red 
    lines(
      x = c(168, 171),
      y = c(cumsums_CI[[1]][2,][min_sequence_length],cumsums_CI[[1]][2,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    
    lines(
      x = c(168, 171),
      y = c(cumsums_CI[[1]][3,][min_sequence_length],cumsums_CI[[1]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    
    lines(
      x = c(169.5, 169.5),
      y = c(cumsums_CI[[1]][2,][min_sequence_length],cumsums_CI[[1]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    
    lines(
      x = c(169, 170),
      y = c(cumsums_CI[[1]][1,][min_sequence_length],cumsums_CI[[1]][1,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    # R - Blue
    lines(
      x = c(172, 175),
      y = c(cumsums_CI[[2]][2,][min_sequence_length],cumsums_CI[[2]][2,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "blue"
    )
    
    lines(
      x = c(172, 175),
      y = c(cumsums_CI[[2]][3,][min_sequence_length],cumsums_CI[[2]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "blue"
    )
    
    lines(
      x = c(173.5, 173.5),
      y = c(cumsums_CI[[2]][2,][min_sequence_length],cumsums_CI[[2]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "blue"
    )
    
    lines(
      x = c(173, 174),
      y = c(cumsums_CI[[2]][1,][min_sequence_length],cumsums_CI[[2]][1,][min_sequence_length]),
      lty = 1,
      lwd = 3,
      col = "blue"
    )
    
    # N - Grey
    lines(
      x = c(164, 167),
      y = c(cumsums_CI[[3]][2,][min_sequence_length],cumsums_CI[[3]][2,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "grey"
    )
    
    lines(
      x = c(164, 167),
      y = c(cumsums_CI[[3]][3,][min_sequence_length],cumsums_CI[[3]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "grey"
    )
    
    lines(
      x = c(165.5, 165.5),
      y = c(cumsums_CI[[3]][2,][min_sequence_length],cumsums_CI[[3]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "grey"
    )
    
    lines(
      x = c(165, 166),
      y = c(cumsums_CI[[3]][1,][min_sequence_length],cumsums_CI[[3]][1,][min_sequence_length]),
      lty = 1,
      lwd = 3,
      col = "grey"
    )
    
    if (p[1] >= 0.05){
      significance = "n.s."
    }else if (p[1] < 0.05 & p[1] >= 0.01){
      significance = "*"
    }else if (p[1] < 0.01 & p[1] >= 0.001){
      significance = "**"
    }else if (p[1] < 0.001 & p[1] >= 0.0001){
      significance = "***"
    }else if (p[1] < 0.0001){
      significance = "****"
    }
    
    text(182, 
         57, 
         significance, 
         xpd = NA,
         srt = 270,
         # font = 24
         cex = 1.5
    )
    lines(c(176, 178), 
          c(c(cumsums_CI[[1]][1,][min_sequence_length],cumsums_CI[[1]][1,][min_sequence_length])), 
          xpd = NA)
    lines(c(176, 178), 
          c(c(cumsums_CI[[2]][1,][min_sequence_length],cumsums_CI[[2]][1,][min_sequence_length])), 
          xpd = NA)
    lines(
      c(178,178),
      c(cumsums_CI[[1]][1,][min_sequence_length], cumsums_CI[[2]][1,][min_sequence_length]),
      xpd = NA
    )

    # Second training session
    
    plot(
      1,
      type = "n",
      xlab = "Time (sec)",
      ylab = "Cumulative Activity Duration (sec)",
      xlim = c(0, 200),
      ylim = c(0, 100),
      cex.lab = 1.5,
      xaxt = "n",
      yaxt = "n"
    )
    axis(side=1, at=c(0, 50, 100, 150, 200), cex.axis = 1.5)
    axis(side=2, at=c(0, 20, 40, 60, 80, 100), cex.axis = 1.5)
    
    polygon(
      index,
      coordinates[[4]],
      lty = 2,
      lwd = 2,
      border = NA,
      col = rgb(0.8, 0, 0, 0.1)
    )
    polygon(
      index,
      coordinates[[5]],
      lty = 2,
      lwd = 2,
      border = NA,
      col = rgb(0, 0, 0.8, 0.1)
    )
    polygon(
      index,
      coordinates[[6]],
      lty = 2,
      lwd = 2,
      border = NA,
      col = rgb(0.5, 0.5, 0.5, 0.1)
    )
    
    lines(
      forward_index,
      cumsums_CI[[4]][1,],
      lty = 1,
      lwd = 3,
      col = rgb(0.8, 0, 0, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[5]][1,],
      lty = 1,
      lwd = 3,
      col = rgb(0.0, 0, 0.8, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[6]][1,],
      lty = 1,
      lwd = 3,
      col = rgb(0.5, 0.5, 0.5, 0.5)
    )
    # T - Red 
    lines(
      x = c(172, 175),
      y = c(cumsums_CI[[4]][2,][min_sequence_length],cumsums_CI[[4]][2,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    
    lines(
      x = c(172, 175),
      y = c(cumsums_CI[[4]][3,][min_sequence_length],cumsums_CI[[4]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    
    lines(
      x = c(173.5, 173.5),
      y = c(cumsums_CI[[4]][2,][min_sequence_length],cumsums_CI[[4]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    
    lines(
      x = c(173, 174),
      y = c(cumsums_CI[[4]][1,][min_sequence_length],cumsums_CI[[4]][1,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "indianred3"
    )
    # R - Blue
    lines(
      x = c(172, 175),
      y = c(cumsums_CI[[5]][2,][min_sequence_length],cumsums_CI[[5]][2,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "blue"
    )
    
    lines(
      x = c(172, 175),
      y = c(cumsums_CI[[5]][3,][min_sequence_length],cumsums_CI[[5]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "blue"
    )
    
    lines(
      x = c(173.5, 173.5),
      y = c(cumsums_CI[[5]][2,][min_sequence_length],cumsums_CI[[5]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "blue"
    )
    
    lines(
      x = c(173, 174),
      y = c(cumsums_CI[[5]][1,][min_sequence_length],cumsums_CI[[5]][1,][min_sequence_length]),
      lty = 1,
      lwd = 3,
      col = "blue"
    )
    
    # N - Grey
    lines(
      x = c(164, 167),
      y = c(cumsums_CI[[6]][2,][min_sequence_length],cumsums_CI[[6]][2,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "grey"
    )
    
    lines(
      x = c(164, 167),
      y = c(cumsums_CI[[6]][3,][min_sequence_length],cumsums_CI[[6]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "grey"
    )
    
    lines(
      x = c(165.5, 165.5),
      y = c(cumsums_CI[[6]][2,][min_sequence_length],cumsums_CI[[6]][3,][min_sequence_length]),
      lty = 1,
      lwd = 2,
      col = "grey"
    )
    
    lines(
      x = c(165, 166),
      y = c(cumsums_CI[[6]][1,][min_sequence_length],cumsums_CI[[6]][1,][min_sequence_length]),
      lty = 1,
      lwd = 3,
      col = "grey"
    )
    
    if (p[2] >= 0.05){
      significance = "n.s."
    }else if (p[2] < 0.05 & p[2] >= 0.01){
      significance = "*"
    }else if (p[2] < 0.01 & p[2] >= 0.001){
      significance = "**"
    }else if (p[2] < 0.001 & p[2] >= 0.0001){
      significance = "***"
    }else if (p[2] < 0.0001){
      significance = "****"
    }
      
    text(180, 
         47, 
         significance, 
         xpd = NA,
         srt = 90,
         # font = 24
         cex = 1.5
         )
    lines(c(176, 178), 
          c(c(cumsums_CI[[4]][1,][min_sequence_length],cumsums_CI[[4]][1,][min_sequence_length])), 
          xpd = NA)
    lines(c(176, 178), 
          c(c(cumsums_CI[[5]][1,][min_sequence_length],cumsums_CI[[5]][1,][min_sequence_length])), 
          xpd = NA)
    lines(
      c(178,178),
      c(cumsums_CI[[5]][1,][min_sequence_length], cumsums_CI[[4]][1,][min_sequence_length]),
      xpd = NA
    )

dev.off()

save.image("cumsums.Rdata")