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


JG_test = function(i, session, time){
  metric.df = read.table("all_ofs_JG.csv", stringsAsFactors = F, sep = ',', header = T)
  metric.df = metric.df[metric.df$Gap==time, ]
  metric = data.frame(
    factor = c(rep("T", length(metric.df[metric.df$Type=="T" & metric.df$Session==session, ][, i])),
               rep("R", length(metric.df[metric.df$Type=="R" & metric.df$Session==session, ][, i])),
               rep("N", length(metric.df[metric.df$Type=="N" & metric.df$Session==session, ][, i]))),
    value = as.numeric(c(metric.df[metric.df$Type=="T" & metric.df$Session==session, ][, i], 
                         metric.df[metric.df$Type=="R" & metric.df$Session==session, ][, i],
                         metric.df[metric.df$Type=="N" & metric.df$Session==session, ][, i]))
  )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c("T", "R", "N"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return (a)
}