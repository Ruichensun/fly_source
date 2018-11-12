#This code is to get the number of laser clicks and total laser exposure duration a fly experience in a given session
one_fly_laser_statistics <- function(input_file, framerate){
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = as.numeric(fly.file[[1]])
  fly.laser.raw = as.numeric(fly.file[[2]])
  if (is.na(fly.laser.raw[1]) == T) {
    number_of_laser_clicks = NA
    total_laser_ON = NA
    ret = list(number_of_laser_clicks,
               total_laser_ON #in seconds
               )
               names(ret) = c("Number of Laser Punishments", "Total Laser Exposure in Seconds")
  }else{
    data_start = 21
    fly_pos = fly.position.raw[data_start:length(fly.position.raw)]
    fly_laser = fly.laser.raw[data_start:length(fly.laser.raw)]
    if (fly_laser[length(fly_laser)] > 0) {
      fly_laser[length(fly_laser)] = 0
    }
    if (fly_laser[1] > 0) {
      fly_laser[1] = 0
    }
    for (i in 1:length(fly_laser)) {
      if (fly_laser[i] > 0) {
        fly_laser[i] = 1
      }
    }
    laser_ON  = rle(fly_laser)$length[rle(fly_laser)$values == 1]
    laser_OFF = rle(fly_laser)$length[rle(fly_laser)$values == 0]
    label_for_laser = rep(0, length(fly_laser))
    for (i in 1:(length(label_for_laser) - 1)) {
      if ((fly_laser[i] == 0) & (fly_laser[i + 1] > 0)) {
        label_for_laser[i + 1] = 1
      }
      if ((fly_laser[i] > 0) & (fly_laser[i + 1] == 0)) {
        label_for_laser[i + 1] = 2
      }
    }
    laser_df = data.frame()
    if (laser_OFF[1] == length(fly_laser)) {
      laser_df = data.frame(0,
                            0,
                            0,
                            (laser_OFF[1]) / framerate,
                            (laser_OFF[1] - 0) / framerate,
                            TRUE)
    } else{laser_df = data.frame (which(label_for_laser == 1),
                                  which(label_for_laser == 2),
                                  laser_ON / framerate,
                                  (laser_OFF[2:length(laser_OFF)]) / framerate,
                                  (laser_OFF[2:length(laser_OFF)] - laser_ON) / framerate,
                                  laser_OFF[2:length(laser_OFF)] > 8 * 60 * framerate
                                  )
    }
    colnames(laser_df) = c(
      "Laser_On",
      "Laser_Off",
      "ON_duration",
      "OFF_duration",
      "Difference",
      "eight_min_OFF"
    )
    number_of_laser_clicks = length(laser_df$Laser_On)
    total_laser_ON = sum(laser_df$ON_duration)
    if (number_of_laser_clicks == 1) {
      if (laser_df$ON_duration == 0) {
        number_of_laser_clicks = 0
      }
    }
    ret = list(number_of_laser_clicks,
               total_laser_ON 
               )
    names(ret) = c("Number of Laser Punishments","Total Laser Exposure in Seconds")
    return(ret)
  }
}

setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/fit_lm_data_cleaning.R")
sessions <- c(
  "E1",
  "E1T1",
  "E1T1E1",
  "E1T1E1T1",
  "E1T1E1T1E1",
  "E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1",
  "E1T1E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1T1E1",
  
  "E1R1",
  "E1R1E1",
  "E1R1E1R1",
  "E1R1E1R1E1",
  "E1R1E1R1E1R1",
  "E1R1E1R1E1R1E1",
  "E1R1E1R1E1R1E1R1",
  "E1R1E1R1E1R1E1R1E1",
  
  "E1N1",
  "E1N1E1",
  "E1N1E1N1",
  "E1N1E1N1E1",
  "E1N1E1N1E1N1",
  "E1N1E1N1E1N1E1",
  "E1N1E1N1E1N1E1N1",
  "E1N1E1N1E1N1E1N1E1"
)
query.sessions = sessions
count = 0
spec = NULL
fly.names = NULL

## WT
all_ofls = list()
for (session in sessions) {
  all_ofls = append(all_ofls, list(c()))
}
for (ind in 1:nrow(fly.info.include)) {
  for (ind.session in 1:length(query.sessions)) {
    if ((fly.info.include[ind, ]$Genotype == "WT") |
        (fly.info.include[ind, ]$Genotype == "CS")) {
      input.file <- list.files(
        path = paste0("data/",
                      fly.info.include$experimenter[ind],
                      "/CS/"),
        pattern = paste0(
          "ProcessedData_Fly",
          fly.info.include$Fly[ind],
          "_",
          query.sessions[ind.session],
          "_WT",
          ".csv"
        ),
        full.names = T
      )
    } else{
      input.file <- list.files(
        path = paste0("data/",
                      fly.info.include$experimenter[ind],
                      "/mutants/"),
        pattern = paste0(
          "ProcessedData_Fly",
          fly.info.include$Fly[ind],
          "_",
          query.sessions[ind.session],
          "_",
          fly.info.include$Genotype[ind],
          ".csv"
        ),
        full.names = T
      )
    }
    if (length(input.file) == 0) {
      all_ofls[[sessions[ind.session]]] = append(all_ofls[[sessions[ind.session]]], list(NA))
      next
    }
    framerate = fly.info.include$Framerate[ind]
    ofls = one_fly_laser_statistics(input.file, framerate = framerate)
    all_ofls[[sessions[ind.session]]] = append(all_ofls[[sessions[ind.session]]], list(ofls))
  }
}
save.image("all_ofls.Rdata")
metrics = c("Number of Laser Punishments", "Total Laser Exposure in Seconds")
for (ind in 1:length(metrics)) {
  metric.df = NULL
  for (session in sessions) {
    print(session)
    metric <- sapply(all_ofls[[session]],
                     function(x) {
                       if (length(x) == 1) {
                         return(NA)
                       } else{
                         return(x[[ind]])
                       }
                     })
    array.session = rep(session, length(metric))
    for (i in 1:length(array.session)) {
      array.session[i] = gsub("X", fly.info.include$Category[i], array.session[i])
    }
    metric.df <- rbind(
      metric.df,
      cbind(
        metric,
        fly.info.include$Fly,
        fly.info.include$Category,
        fly.info.include$Gender,
        fly.info.include$Genotype,
        fly.info.include$experimenter,
        fly.info.include$Age,
        fly.info.include$Setup,
        array.session
      )
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
  metric.df = data.frame(metric.df)
  for (i in 1:ncol(metric.df)) {
    metric.df[, i] = unlist(metric.df[, i])
  }
  metric.df$Value = as.numeric(as.character(metric.df$Value))
  ## NO LM FIT
  write.table(
    metric.df,
    paste0("metric_laser/metric_", ind, ".csv"),
    row.names = F,
    quote = F,
    sep = ","
  )
}

write.table(
  metrics,
  "metric_laser/list_metrics.csv",
  col.names = F,
  row.names = F,
  quote = F,
  sep = ","
)
            
