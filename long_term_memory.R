source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
setwd("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/JGLNRS/CS/CSV")
JG_fly.info = read.csv("Behavioral Experiments - CS_JG.csv", header = T, sep = ",", stringsAsFactors = F)
# Tgroup = JG_fly.info[JG_fly.info$Category=="T",]
# Rgroup = JG_fly.info[JG_fly.info$Category=="R",]
# Ngroup = JG_fly.info[JG_fly.info$Category=="N",]
# 
# sessions <- c(
#   
#   "E1",
#   "E1T1",
#   "E1T1E1",
#   "E1T1E1T1",
#   "E1T1E1T1E1",
#   
#   "E2",
#   "E2T2",
#   "E2T2E2",
#   "E2T2E2T2",
#   "E2T2E2T2E2",
#   
#   "E3T3",
#   "E3T3E3",
#   "E3T3E3T3",
#   "E3T3E3T3E3",
#   
#   
#   "E1R1",
#   "E1R1E1",
#   "E1R1E1R1",
#   "E1R1E1R1E1",
# 
#   
#   "E2R2",
#   "E2R2E2",
#   "E2R2E2R2",
#   "E2R2E2R2E2",
#   
#   "E3R3",
#   "E3R3E3",
#   "E3R3E3R3",
#   "E3R3E3R3E3",
#   
#   "E1N1",
#   "E1N1E1",
#   "E1N1E1N1",
#   "E1N1E1N1E1",
#   
#   "E2N2",
#   "E2N2E2",
#   "E2N2E2N2",
#   "E2N2E2N2E2"
#   
# )
# sessions = unique(sessions)
# query.sessions = sessions

all_ofs_JG = data.frame()

for(ind in 1:nrow(JG_fly.info)){
  print(paste0("ProcessedData_Fly",JG_fly.info$Fly[ind]))
  input.files <- list.files(pattern = paste0("ProcessedData_Fly",JG_fly.info$Fly[ind]),
                             full.names=F)
  if(length(input.files) == 0){
    next()
  }else{
  for (ind.file in 1:length(input.files)){
    if((length(input.files[ind.file]) == 0)|(is.na(input.files[ind.file]))){next
    }else{
    ofs = one_fly_statistics(input.files[ind.file], framerate = 16, special_case = T)
    Type = JG_fly.info$Category[ind]
    Gap = JG_fly.info$Gap[ind]
    ofs = cbind(Type, Gap, ofs)
    all_ofs_JG = rbind(all_ofs_JG, ofs)
    }
  }
  }
}

write.table(all_ofs_JG, file = "all_ofs_JG.csv", append = FALSE, 
            col.names = TRUE, sep = ",", row.names = FALSE)


# JG_test = function(i, session, time){
#   metric.df = read.table("all_ofs_JG.csv", stringsAsFactors = F, sep = ',', header = T)
#   metric.df = metric.df[metric.df$Gap==time, ]
#   metric = data.frame(
#     factor = c(rep("T", length(metric.df[metric.df$Type=="T" & metric.df$Session==session, ][, i])),
#                rep("R", length(metric.df[metric.df$Type=="R" & metric.df$Session==session, ][, i])),
#                rep("N", length(metric.df[metric.df$Type=="N" & metric.df$Session==session, ][, i]))),
#     value = as.numeric(c(metric.df[metric.df$Type=="T" & metric.df$Session==session, ][, i], 
#                          metric.df[metric.df$Type=="R" & metric.df$Session==session, ][, i],
#                          metric.df[metric.df$Type=="N" & metric.df$Session==session, ][, i]))
#   )
#   colnames(metric) = c("Session", "Value")
#   metric$Session = factor(metric$Session, levels=c("T", "R", "N"))
#   a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
#   return (a)
# }

plot_JG_metrics = function(gap, fly.info.end){
  metrices =  c(
    "Type", #1
    "Gap", #2
    "Experimenter", #3
    "Genotype", #4
    "Fly Number", #5
    "Session", #6
    "Number of Pause", #7
    "Number of Middle Pause", #8
    "Percentage Time Active", #9
    "Percentage Time Active - Pause not at the End", #10
    "Median Pause Duration",#11
    "Median Middle Pause Duration", #12    
    "Max Pause Duration", #13
    "Max Middle Pause Duration", #14
    "First Pause Duration", #15
    "First Middle Pause Duration", #16
    "Average Moving Speed", #17
    "Average Moving Speed (excluding pause)", #18
    "Average Speed When Enter Pause", #19
    "Average Speed When Exit Pause",#20
    "Moving Distance",#21
    "Number of Turns",#22
    "Number of Middle Turns",#23
    "Fration of Middle Turns Out of Total Turns",#24
    "Burstiness (Pause)",#25
    "Burstiness (Inter Event Time)",#26
    "Burstiness (Scrambled)",#27
    "Burstiness (Walking bouts-thresholding)",#28
    "Burstiness (Walking events-no thres)",#29
    "Memory of Pause", #30
    "Memory of Walking", #31
    "Transition Probability (Pause not at the end): Pause to Pause", #32
    "Transition Probability (Pause not at the end): Pause to Pause - middle", #33
    "Transition Probability (Pause not at the end): Pause to Walking", #34
    "Transition Probability (Pause not at the end): Pause to Walking - middle", #35
    "Transition Probability (Pause not at the end): Walking to Walking", #36
    "Transition Probability (Pause not at the end): Walking to Walking - middle", #37
    "Transition Probability (Pause not at the end): Walking to Pause", #38
    "Transition Probability (Pause not at the end): Walking to Pause - middle" #39
  )
  
    metric.df = read.table("all_ofs_JG.csv", stringsAsFactors = F, sep = ',', header = T)
    metric.df = metric.df[metric.df$Gap==gap, ]
  
  pdf(paste0("JG_", gap, "hr_", Sys.Date(), ".pdf"),
      onefile = T, width = 8
  )
  
  for (i in 7:length(metrices)) {
    metric = data.frame(
      factor = c(rep("E1-T", length(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i])),
                 rep("E1-R", length(metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i])),
                 rep("E1-N", length(metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i])),
                 rep("E1T1E1", length(metric.df[metric.df$Session=="E1T1E1", ][, i])),
                 rep("E1R1E1", length(metric.df[metric.df$Session=="E1R1E1", ][, i])),
                 rep("E1N1E1", length(metric.df[metric.df$Session=="E1N1E1", ][, i])),
                 rep("E1T1E1T1E1", length(metric.df[metric.df$Session=="E1T1E1T1E1", ][, i])),
                 rep("E1R1E1R1E1", length(metric.df[metric.df$Session=="E1R1E1R1E1", ][, i])),
                 rep("E1N1E1N1E1", length(metric.df[metric.df$Session=="E1N1E1N1E1", ][, i])),
                 rep("E2-T", length(metric.df[metric.df$Type=="T" & metric.df$Session=="E2", ][, i])),
                 rep("E2-R", length(metric.df[metric.df$Type=="R" & metric.df$Session=="E2", ][, i])),
                 rep("E2-N", length(metric.df[metric.df$Type=="N" & metric.df$Session=="E2", ][, i])),
                 rep("E2T2E2", length(metric.df[metric.df$Session=="E2T2E2", ][, i])),
                 rep("E2R2E2", length(metric.df[metric.df$Session=="E2R2E2", ][, i])),
                 rep("E2N2E2", length(metric.df[metric.df$Session=="E2N2E2", ][, i])),
                 rep("E2T2E2T2E2", length(metric.df[metric.df$Session=="E2T2E2T2E2", ][, i])),
                 rep("E2R2E2R2E2", length(metric.df[metric.df$Session=="E2R2E2R2E2", ][, i])),
                 rep("E2N2E2N2E2", length(metric.df[metric.df$Session=="E2N2E2N2E2", ][, i]))
                 
                 ),
      value = as.numeric(c(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i], 
                           metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i],
                           metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i],
                           metric.df[metric.df$Session=="E1T1E1", ][, i],
                           metric.df[metric.df$Session=="E1R1E1", ][, i],
                           metric.df[metric.df$Session=="E1N1E1", ][, i],
                           metric.df[metric.df$Session=="E1T1E1T1E1", ][, i],
                           metric.df[metric.df$Session=="E1R1E1R1E1", ][, i],
                           metric.df[metric.df$Session=="E1N1E1N1E1", ][, i],
                           metric.df[metric.df$Type=="T" & metric.df$Session=="E2", ][, i], 
                           metric.df[metric.df$Type=="R" & metric.df$Session=="E2", ][, i],
                           metric.df[metric.df$Type=="N" & metric.df$Session=="E2", ][, i],
                           metric.df[metric.df$Session=="E2T2E2", ][, i],
                           metric.df[metric.df$Session=="E2R2E2", ][, i],
                           metric.df[metric.df$Session=="E2N2E2", ][, i],
                           metric.df[metric.df$Session=="E2T2E2T2E2", ][, i],
                           metric.df[metric.df$Session=="E2R2E2R2E2", ][, i],
                           metric.df[metric.df$Session=="E2N2E2N2E2", ][, i]))
    )
    colnames(metric) = c("Session", "Value")
    metric$Session = factor(metric$Session, levels=c("E1-T", "E1-R", "E1-N",
                                                     "E1T1E1", "E1R1E1", "E1N1E1",
                                                     "E1T1E1T1E1", "E1R1E1R1E1", "E1N1E1N1E1",
                                                     "E2-T", "E2-R", "E2-N",
                                                     "E2T2E2", "E2R2E2", "E2N2E2",
                                                     "E2T2E2T2E2","E2R2E2R2E2", "E2N2E2N2E2"
                                                     ))
    num = as.data.frame(table(metric[!is.na(metric$Value),]$Session))$Freq
    
    ## special cases
    y_text = c()
    if (i %in% c(7, 8)) {
      yrange = c(0, 200)
      y_text = 200
    }
    
    if (i %in% c(9, 10)) {
      yrange = c(0, 1)
      y_text = 1
    }
    
    if (i %in% c(11:12)) {
      yrange = c(0, 50)
      y_text = 52
    }
    
    if (i %in% c(13:16)) {
      yrange = c(0, 600)
      y_text = 610
    }
    
    if (i %in% c(17:20)) {
      yrange = c(0, 50)
      y_text = 50.5
    }
    
    if (i == 21) {
      yrange = c(0, 10000)
      y_text = 10000
    }
    
    if (i %in% c(22, 23)) {
      yrange = c(0, 100)
      y_text = 100
    }
    
    if (i == 24) {
      yrange = c(0, 1)
      y_text = 1
    }
    if (i %in% c(25:31)) {
      yrange = c(-1, 1)
      y_text = 1.1
    }
    if (i %in% c(32:33)) {
      yrange = c(0.8, 1)
      y_text = 1.1
    }
    if (i %in% c(34:35)) {
      yrange = c(0, 0.2)
      y_text = 0.22
    }
    if (i %in% c(36:39)) {
      yrange = c(0, 1)
      y_text = 1.1
    }
    
    col.pool = c(
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80"
    )
    boxplot(
      Value ~ Session,
      data = metric,
      ylim = yrange,
      outline = F,
      notch = F,
      lwd = 1,
      ylab = metrices[i],
      xlab = "",
      medlwd = 1,
      # boxwex = 1,
      xaxt = "n"
    )
    stripchart(
      Value ~ Session,
      vertical = TRUE,
      data = metric,
      method = "jitter",
      add = TRUE,
      pch = 15,
      cex = 0.5,
      col =  col.pool
    )
    if (any(is.na(yrange)) |
        any(is.infinite(yrange)) | any(is.nan(yrange))) {
      yrange = c(-1, 1)
      ylim = c(-1, 1)
    }
    text(x = (1:length(num)) - 0.1,
         y = y_text,
         # y_text,
         num,
         xpd = T,
         srt = 0,
         adj = 0
    )
    for (j in c(3, 6, 9, 12)) {
      lines(c(j, j) + 0.5,
            c(yrange[1] - 1e3, yrange[1] + 1e3),
            col = "light grey",
            lty = 1)
    }
  }
  dev.off()
  
}