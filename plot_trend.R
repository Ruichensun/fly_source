setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
library(zoo)
library(boot)
library(dunn.test)

# Finding out when the fly is moving vs not moving
# Input: fly_pos; Output: a vector of 0 and 1 of (length of input) - 1 
fly.info.movement.T = fly.info.include[((fly.info.include$Genotype == "WT") | 
                                        (fly.info.include$Genotype == "CS")) & 
                                        (fly.info.include$Category =="T") & 
                                        (fly.info.include$experimenter!="SW"), ]
fly.info.movement.R = fly.info.include[((fly.info.include$Genotype == "WT") | 
                                        (fly.info.include$Genotype == "CS")) & 
                                        (fly.info.include$Category == "R") & 
                                        (fly.info.include$experimenter!="SW"), ]
fly.info.movement.N = fly.info.include[((fly.info.include$Genotype == "WT") | 
                                        (fly.info.include$Genotype == "CS")) & 
                                        (fly.info.include$Category == "N") & 
                                        (fly.info.include$experimenter!="SW"), ]

###Including All Relevant Sessions
sessions <- c(
  "E1T1",
  "E1R1",
  "E1N1",
  "E1T1E1T1",
  "E1R1E1R1",
  "E1N1E1N1"
  )
cumsums_total = list(get_cumsums_total(sessions[1], fly.info.movement.T),
                     get_cumsums_total(sessions[2], fly.info.movement.R),
                     get_cumsums_total(sessions[3], fly.info.movement.N),
                     get_cumsums_total(sessions[4], fly.info.movement.T),
                     get_cumsums_total(sessions[5], fly.info.movement.R),
                     get_cumsums_total(sessions[6], fly.info.movement.N))

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
# cumsums_CI structure: for each of the 6 lists: 3 rows by 8568 columns. First row is median
# Second row is CI lower bound, Third row is CI upper bound

trained_1 = cumsums_CI[[1]][1,] 
yoked_1   = cumsums_CI[[2]][1,] 
blank_1   = cumsums_CI[[3]][1,]
trained_2 = cumsums_CI[[4]][1,] 
yoked_2   = cumsums_CI[[5]][1,] 
blank_2   = cumsums_CI[[6]][1,] 
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
dunn.test(x = Session1_df$Value, g = Session1_df$Session, method = c("bonferroni"))
dunn.test(x = Session2_df$Value, g = Session2_df$Session, method = c("bonferroni"))


# Confidence Interval

CI_df = data.frame()
for (i in 1:6){
  CI_df = rbind.data.frame(CI_df, get_Wald_CI(cumsums_total[[i]][min_sequence_length, ] / framerate ))
}

colnames(CI_df) = c("Median", "CI_Lower", "CI_Upper")

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
    onefile = T, width = 5, height = 5)
    # First training session
    plot(
      1,
      type = "n",
      xlab = "",
      ylab = "",
      xlim = c(0, 200),
      ylim = c(0, 100)
    )
    
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
      lwd = 2,
      col = rgb(0.8, 0, 0, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[2]][1,],
      lty = 1,
      lwd = 2,
      col = rgb(0.0, 0, 0.8, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[3]][1,],
      lty = 1,
      lwd = 2,
      col = rgb(0.5, 0.5, 0.5, 0.5)
    )
    # Second training session
    plot(
      1,
      type = "n",
      xlab = "",
      ylab = "",
      xlim = c(0, 200),
      ylim = c(0, 100)
    )
    
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
      lwd = 2,
      col = rgb(0.8, 0, 0, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[5]][1,],
      lty = 1,
      lwd = 2,
      col = rgb(0.0, 0, 0.8, 0.5)
    )
    lines(
      forward_index,
      cumsums_CI[[6]][1,],
      lty = 1,
      lwd = 2,
      col = rgb(0.5, 0.5, 0.5, 0.5)
    )
  
dev.off()
