#We define learning index as (3rd E1's performance - 1st E1's performance)/(1st E1's performance), calculated at an individual level.
#This script follows the laser_power_based_segmentation.R, and cannot be run prior to running laser_power_based_segmentation.R

input.y.df = data.frame()
p_value_sum = matrix(nrow = 0, ncol = 9)

#Reference for metric index
 
#   "Number of Pause Starts", #1
#   "Fraction Time in Pause", #2
#   "Average Pause Duration", #3
#   "Max Pause Duration", #4 
#   "Average Moving Speed ", #5
#   "Average Moving Speed (excluding pause)", #6
#   "Average Speed When Enter Pause", #7
#   "Average Speed When Exit Pause",#8
#   "Moving Distance Per Minute",#9
#   "Number of Turns",#10
#   "Number of Middle Turns",#11
#   "Fration of Middle Turns Out of Total Turns",#12
#   "Burstiness (Pause)",#13
#   "Burstiness (Inter Event Time)",#14
#   "Burstiness (Scrambled)",#15
#   "Burstiness (Walking bouts-thresholding)",#16
#   "Burstiness (Walking events-thresholding)",#17
#   "Beginning Pause Duration",#18 
#   "First Pause Duration",#19 
#   "Transition Probability (Pause not at the end): Pause to Pause",#20
#   "Transition Probability (Pause not at the end): Pause to Walking",#21
#   "Transition Probability (Pasue not at the end): Walking to Walking",#22
#   "Transition Probability (Pause not at the end): Walking to Pause",#23
#   "Memory",#24
#   "Memory (inverted)",#25
#   "Burstiness of Start of Walking (Pause not at the end)",#26 *
#   "Burstiness of Start of Pause (Pause not at the end)",#27 *
#   "Average Pause Duration (Pause not at the End)",#28
#   "Fraction Time in Pause (Pause not at the End)",#29 
#   "Max Pause Duration (Pause not at the End)", #30
#   "First Pause Duration (Pause not at the End)" #31


sessions <- c(                     
  "E1T1",                   #4
  "E1T1E1",                 #5 
  "E1T1E1T1",               #6
  "E1T1E1T1E1",             #7
  "E1T1E1T1E1T1",           #8
  "E1T1E1T1E1T1E1",         #9
  "E1T1E1T1E1T1E1T1",       #10
  "E1T1E1T1E1T1E1T1E1",     #11
  
  "E1R1",                   #12
  "E1R1E1",                 #13
  "E1R1E1R1",               #14
  "E1R1E1R1E1",             #15
  "E1R1E1R1E1R1",           #16
  "E1R1E1R1E1R1E1",         #17
  "E1R1E1R1E1R1E1R1",       #18
  "E1R1E1R1E1R1E1R1E1",     #19
  
  "E1N1",                   #20
  "E1N1E1",                 #21
  "E1N1E1N1",               #22
  "E1N1E1N1E1",             #23
  "E1N1E1N1E1N1",           #24
  "E1N1E1N1E1N1E1",         #25
  "E1N1E1N1E1N1E1N1",       #26
  "E1N1E1N1E1N1E1N1E1"      #27
)



#WT flies
query.genotype <- c("WT","CS")
query.fly = fly.info.include[((fly.info.include$Genotype == "WT") |
                                (fly.info.include$Genotype == "CS")) &
                               (fly.info.include$experimenter!="SW"), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "WT") |
                                         (fly.info.include$Genotype == "CS")) &
                                        (fly.info.include$experimenter!="SW"), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "WT") |
                      (fly.info.include$Genotype == "CS")) &
                     (fly.info.include$experimenter!="SW"), ],
  "fly_info_include_WT.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)
fly_genotype = "CS"

metric.ind = 29

input.file = paste0("metrics/metric_", metric.ind, ".csv")
if (!file.exists(input.file)) {
  next
}
metric.df = read.csv(input.file)
## covariates of interest: genotype, session
y = list()
## E1 data
session = "E1"
for (category in c("T", "R")) {
  query.session = gsub("X", category, session)
  ind <- metric.df$Session == query.session &
    metric.df$Genotype %in% query.genotype &
    metric.df$Category == category &
    metric.df$Fly %in% query.fly&
    metric.df$Experimenter  %in%  query.experimenter
  ind.E1 <- metric.df$Session == "E1" &
    metric.df$Genotype %in% query.genotype &
    metric.df$Category == category &
    metric.df$Fly %in% query.fly&
    metric.df$Experimenter  %in%  query.experimenter
  z = metric.df[ind,"Value"]
  y = append(y, list(na.omit(z)))
}
for (category in c("N")) {
  query.session = gsub("X", category, session)
  ind <- metric.df$Session == query.session &
    metric.df$Genotype %in% query.genotype &
    metric.df$Category == category &
    metric.df$Fly %in% query.fly&
    metric.df$Experimenter  %in%  query.experimenter
  ind.E1 <- metric.df$Session == "E1" &
    metric.df$Genotype %in% query.genotype &
    metric.df$Category == category &
    metric.df$Fly %in% query.fly&
    metric.df$Experimenter  %in%  query.experimenter
  z = metric.df[ind,"Value"]
  y = append(y, list(na.omit(z)))
}
## input sessions data
for (session in sessions) {
  print(session)
  if (grepl("T", session) == T) {
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == "T" &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    ind <- metric.df$Session == session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == "T" &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    z = metric.df[ind,"Value"] 
    
  }
  if (grepl("R", session) == T) {
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == "R" &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    ind <- metric.df$Session == session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == "R" &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    z = metric.df[ind,"Value"]
  }
  if (grepl("N", session) == T) {
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == "N" &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    ind <- metric.df$Session == session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == "N" &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    z = metric.df[ind,"Value"]
  }
  y = append(y, list(na.omit(z)))
}
y.1 = y
# yrange = c(min(sapply(y,min)),max(sapply(y,max)))
## special cases
y_text = c()
input.y = list(y.1[[1]], y.1[[2]], y.1[[3]], y.1[[7]], y.1[[15]], y.1[[23]]) # input.y = y.1[7:9]

median(input.y[[1]])
median(input.y[[2]])
median(input.y[[3]])
median(input.y[[4]])
median(input.y[[5]])
median(input.y[[6]])

median(append(input.y[[1]], input.y[[2]], input.y[[3]]))