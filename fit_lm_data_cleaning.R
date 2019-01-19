setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

metrices = c(
  "Number of Pause", #1
  "Number of Middle Pause", #2
  "Percentage Time Active", #3
  "Percentage Time Active - Pause not at the End", #4
  "Median Pause Duration",#5
  "Median Middle Pause Duration", #6    
  "Max Pause Duration", #7
  "Max Middle Pause Duration", #8
  "First Pause Duration", #9
  "First Middle Pause Duration", #10
  "Average Moving Speed", #11
  "Average Moving Speed (excluding pause)", #12
  "Average Speed When Enter Pause", #13
  "Average Speed When Exit Pause",#14
  "Moving Distance",#15
  "Number of Turns",#16
  "Number of Middle Turns",#17
  "Fration of Middle Turns Out of Total Turns",#18
  "Burstiness (Pause)",#19
  "Burstiness (Inter Event Time)",#20
  "Burstiness (Scrambled)",#21
  "Burstiness (Walking bouts-thresholding)",#22
  "Burstiness (Walking events-no thres)",#23
  "Memory of Pause", #24
  "Memory of Walking", #25
  "Transition Probability (Pause not at the end): Pause to Pause", #26
  "Transition Probability (Pause not at the end): Pause to Pause - middle", #27
  "Transition Probability (Pause not at the end): Pause to Pause - middle - no bump", #28
  "Transition Probability (Pause not at the end): Pause to Walking", #29
  "Transition Probability (Pause not at the end): Pause to Walking - middle", #30
  "Transition Probability (Pause not at the end): Pause to Walking - middle - no bump", #31
  "Transition Probability (Pause not at the end): Walking to Walking", #32
  "Transition Probability (Pause not at the end): Walking to Walking - middle", #33
  "Transition Probability (Pause not at the end): Walking to Walking - middle - no bump", #34
  "Transition Probability (Pause not at the end): Walking to Pause", #35
  "Transition Probability (Pause not at the end): Walking to Pause - middle", #36
  "Transition Probability (Pause not at the end): Walking to Pause - middle - no bump" #37
)

sessions <- c(
  
  "E1",
  "E1T1E1",
  "E1T1E1T1E1",
  "E1T1E1T1E1T1E1",
  "E1T1E1T1E1T1E1T1E1",
  
  "E1T1E1T1E1T2E1",
  "E1T1E1T1E1T2E1T2E1",
  
  "E1T2E1",
  "E1T2E1T2E1",
  "E1T2E1T2E1T1E1",
  "E1T2E1T2E1T1E1T1E1",
  
  "E1R1E1",
  "E1R1E1R1E1",
  "E1R1E1R1E1R1E1",
  "E1R1E1R1E1R1E1R1E1",
  
  "E1N1E1",
  "E1N1E1N1E1",
  "E1N1E1N1E1N1E1",
  "E1N1E1N1E1N1E1N1E1",
  
  "E1T1E1R1E1",
  "E1T1E1R1E1R1E1",
  "E1T1E1T1E1R1E1R1E1",
  
  "E1T1E1N1E1",
  "E1T1E1N1E1N1E1",
  "E1T1E1T1E1N1E1N1E1"
)
sessions <- unique(sessions)

all_ofs = list()
for (session in sessions) {
  all_ofs = append(all_ofs, list(c(all_ofs_WT[[session]], all_ofs_mutants[[session]])))
}
names(all_ofs) = sessions


## Fit linear model for each metric
for (ind in 1:length(metrices)) {
  metric.df = NULL
  for (session in sessions) {
    metric <- sapply(all_ofs[[session]], function(x) {
                    if (length(x) == 1) {return(NA)} else{return(x[[ind]])}
                     })
    array.session = rep(session, length(metric))
    for (i in 1:length(array.session)) {
      array.session[i] = gsub("X", fly.info$Category[i], array.session[i])
    }
    metric.df <- rbind(metric.df, cbind(metric,
                                        fly.info$Fly,
                                        fly.info$Category,
                                        fly.info$Gender,
                                        fly.info$Genotype,
                                        fly.info$Experimenter,
                                        fly.info$Age,
                                        fly.info$Setup,
                                        array.session
                                       )[ind.include, ]
                      )
  }
  colnames(metric.df) = c(
    "Value",
    "Fly",
    "Category",
    "Gender",
    "Genotype",
    "Experimenter",
    "Age",
    "Setup",
    "Session"
  )
  write.table(
    metric.df,
    paste0("metrics/metric_", ind, ".csv"),
    row.names = F,
    quote = F,
    sep = ","
  )
}

write.table(
  metrices,
  "metrics/list_metrices.csv",
  col.names = F,
  row.names = F,
  quote = F,
  sep = ","
)
