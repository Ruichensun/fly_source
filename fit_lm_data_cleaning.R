setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/get_fly_stats.R")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/Checking_fly_numbers.R")
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
load("all_ofs.Rdata")

pass_fly_QC <- function(input_file,
                        framerate = 50,
                        speed_max_thres = 50,
                        speed_zero_thres = 1e-2,
                        pause_frame_thres = 25,
                        chamber_end_thres = 50) {
  ## read input file
  tryCatch({
    x = read.table(
      input_file,
      header = T,
      sep = ",",
      stringsAsFactors = F
    )
  }, error = function(e) {
    stop(paste0(
      "Input file is empty!:\n",
      "  Input files is: ",
      input_file,
      "\n\n"
    ))
  })
  if (nrow(x) < 10) {
    stop(paste0(
      "Input file is empty!:\n",
      "  Input files is: ",
      input_file,
      "\n\n"
    ))
  }
  x = as.numeric(x[, 1])
  fly_num = sapply(strsplit(input_file, "_"), function(x)
    return(gsub("Fly", "", x[2])))
  data_start = 21 
  fly_pos = x[data_start:min(600 * framerate, length(x))]
  experiment_time = length(fly_pos)
  ##Get the transient speed
  if (data_start > 1) {
    fly_speed = diff(c(x[data_start - 1], fly_pos))
  } else {
    fly_speed = diff(c(NA, fly_pos))
  }
  pause_start = NULL
  pause_end = NULL
  potential_pause_start = 1
  current_zero_length = 0
  is_pause = rep(0, experiment_time)
  if (fly_speed[1] == 0) {
    current_zero_length = 1
  }
  for (t in 2:experiment_time) {
    if (fly_speed[t] == 0) {
      if (current_zero_length == 0) {
        potential_pause_start = t
        current_zero_length = current_zero_length + 1
      } else{
        current_zero_length = current_zero_length + 1
      }
    } else{
      if (current_zero_length >= pause_frame_thres) {
        pause_start = c(pause_start, potential_pause_start)
        pause_end = c(pause_end, t - 1)
        is_pause[potential_pause_start:(t - 1)] = 1
      }
      current_zero_length = 0
    }
  }
  if (current_zero_length >= pause_frame_thres) {
    pause_start = c(pause_start, potential_pause_start)
    pause_end = c(pause_end, experiment_time)
    is_pause[potential_pause_start:experiment_time] = 1
  }
  num_pause = length(pause_start)
  if (max(pause_end - pause_start + 1) >= (20 * framerate)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}

data_filter <- function(filter, fly.info){
  if (filter == 1) {
    ind.include = NULL
    for (genotype in unique(fly.info$Genotype)) {
      if (genotype == "CS") {
        next
      }
      if (genotype == "WT") {
        ind = fly.info$Genotype %in% c("WT", "CS") &
          !(1:nrow(fly.info) %in% ind.excl)
      }else{
        ind = fly.info$Genotype == genotype &
          !(1:nrow(fly.info) %in% ind.excl)
      }
      fms <- fly.info$Fly.moving.speed[ind]
      rank_fms = rank(fms)
      ind.filter =  rank_fms <= length(fms) * 1.0 & rank_fms >= length(fms) * 0.0 # changed from 0.3 to 0.0
      ind.include = c(ind.include, which(ind)[ind.filter])
    }
  }
  if (filter == 2) {
    ind.include = NULL
    session = "E1"
    for (ind in 1:nrow(fly.info)) {
      if (fly.info$Genotype[ind] == "WT") {
        input.file <- paste0(
          "data/",
          fly.info$experimenter[ind],
          "/CS/",
          "ProcessedData_Fly",
          fly.info$Fly[ind],
          "_",
          session,
          "_WT",
          ".csv"
        )
      } else{
        input.file <- paste0(
          "data/",
          fly.info$experimenter[ind],
          "/Mutants/",
          "ProcessedData_Fly",
          fly.info$Fly[ind],
          "_",
          session,
          "_",
          fly.info$Genotype[ind],
          ".csv"
        )
      }
      framerate =	fly.info$Framerate[ind]
      if (pass_fly_QC(input.file, framerate)) {
        ind.include = c(ind.include, ind)
      }
    }
  }
  return(ind.include)
}

metrices = c(
  "Number of Pause Starts",#1
  "Percentage Time Active",#2
  "Average Pause Duration",#3
  "Max Pause Duration",#4
  "Average Moving Speed ",#5
  "Average Moving Speed (excluding pause)",#6
  "Average Speed When Enter Pause",#7
  "Average Speed When Exit Pause",#8
  "Moving Distance Per Minute",#9
  "Number of Turns",#10
  "Number of Middle Turns",#11
  "Fration of Middle Turns Out of Total Turns",#12
  "Burstiness (Pause)",#13
  "Burstiness (Inter Event Time)",#14
  "Burstiness (Scrambled)",#15
  "Burstiness (Walking bouts-thresholding)",#16
  "Burstiness (Walking events-thresholding)",#17
  "Beginning Pause Duration",#18
  "First Pause Duration",#19
  "Transition Probability (Pause not at the end): Pause to Pause",#20
  "Transition Probability (Pause not at the end): Pause to Walking",#21
  "Transition Probability (Pasue not at the end): Walking to Walking",#22
  "Transition Probability (Pause not at the end): Walking to Pause",#23
  "Memory",#24
  "Memory (inverted)",#25
  "Burstiness of Start of Walking (Pause not at the end)",#26
  "Burstiness of Start of Pause (Pause not at the end)",#27 *
  "Average Pause Duration (Pause not at the End)",#28
  "Percentage Time Active (Pause not at the End)",#29
  "Max Pause Duration (Pause not at the End)",#30
  "First Pause Duration (Pause not at the End)" #31
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

## Fly info
fly.info.CS = read.csv(
  "data/fly_info_CS_preprocessed.csv",
  header = T,
  stringsAsFactors = F
)
fly.info.CS$Genotype = "WT"

fly.info.mutants = read.csv(
  "data/fly_info_mutants_preprocessed.csv",
  header = T,
  stringsAsFactors = F
)

shared.info = c(
  "Fly",
  "Category",
  "Gender",
  "Genotype",
  "experimenter",
  "Age",
  "Setup",
  "Fly.moving.speed",
  "Framerate"
)

fly.info = rbind(fly.info.CS[, shared.info], fly.info.mutants[, shared.info])

#For Mutant data collected after Mar 20, 2017
excl.fly.Mutant = na.omit(read.csv(
  "excl_fly_mutant.csv",
  header = T,
  stringsAsFactors = F
)[, 1:3])

excl.fly.WT = data.frame(cbind(
  # Batch 1 ES-WT: 1-276; 
  # Batch 2 ES-WT: 276-400; 
  # Batch 3 ES-WT: 400-present (as of June 28, 2017)
  # Only Batch 3 data is for the current project
   c(58, 48, 1:400, 1:100, 1:40,118,122),
   c("RS", "JD", rep("ES", 400), rep("RS", 100), rep("JD", 40),rep("SW",2)),
   c(rep("WT", 544))
))
colnames(excl.fly.WT) = colnames(excl.fly.Mutant)
excl.fly = rbind(excl.fly.Mutant,
                 excl.fly.WT)

ind.excl = NULL
for (ind in 1:nrow(excl.fly)) {
  #Mutant data
  ind.excl = c(
    ind.excl,
    which(
      fly.info$Genotype == excl.fly[ind, 3] &
        fly.info$experimenter == excl.fly[ind, 2] &
        fly.info$Fly == excl.fly[ind, 1]
    )
  )
}

ind.include = data_filter(1, fly.info)

write.csv(
  fly.info[ind.include, ],
  "data/fly_info.csv",
  quote = F,
  row.names = F
)

fly.info.include = fly.info[ind.include,]
checking_fly_numbers(fly.info.include, filename="Mutants_headcount.csv")

## Fit linear model for each metric
for (ind in 1:length(metrices)) {
  metric.df = NULL
  for (session in sessions) {
    metric <- sapply(all_ofs[[session]],
                     function(x) {
                       if (length(x) == 1) {
                         return(NA)
                       } else{
                         return(x[[ind]])
                       }
                     })
    
    array.session = rep(session, length(metric))
    for (i in 1:length(array.session)) {
      array.session[i] = gsub("X", fly.info$Category[i], array.session[i])
    }
    
    metric.df <- rbind(
      metric.df,
      cbind(
        metric,
        fly.info$Fly,
        fly.info$Category,
        fly.info$Gender,
        fly.info$Genotype,
        fly.info$experimenter,
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
  
  ## Prepare input dataframe
  metric.df = data.frame(metric.df)
  for (i in 1:ncol(metric.df)) {
    metric.df[, i] = unlist(metric.df[, i])
  }
  metric.df$Setup <- factor(metric.df$Setup,
                            levels = levels(metric.df$Setup)[c(2, 1, 3, 4)])
  metric.df$Age <- factor(metric.df$Age, levels = levels(metric.df$Age)[c(3, 1, 2, 4)])
  metric.df$Value = as.numeric(as.character(metric.df$Value))
  
  # Need to update batch info
  # metric.df$batch = as.numeric(as.character(metric.df$fly))
  #
  # metric.df$batch[metric.df$batch <= 276] = 1
  # metric.df$batch[metric.df$batch >= 277 & metric.df$batch <=400 ] = 2
  # metric.df$batch[metric.df$batch > 400] = 3
  # metric.df$batch = factor(metric.df$batch)
  #
  # metric.df$value[is.infinite(metric.df$value)] = NA
  
  #metric.df = na.omit(metric.df)
  #metric.df = metric.df[complete.cases(metric.df),]
  # ## NO LM FIT
  # write.table(metric.df,
  #             paste0("metrics/metric_",ind,".csv"),
  #             row.names=F,quote=F,sep=",")
  # next
  #
  # metric.df = metric.df[metric.df$genotype %in% c("WT","SUN1","SUN2","SUN3","R3"),]
  # metric.df$genotype = factor(metric.df$genotype,
  #                             levels(metric.df$genotype)[levels(metric.df$genotype) %in% c("WT","SUN1","SUN2","SUN3","R3")][c(5,1:4)])
  #
  # ## Linear fit
  # lm.fit.orig <- lm(value ~ genotype + experimenter + gender + session + batch + setup + fly + age,
  #                   metric.df)
  # #lm.fit.orig = glm(value ~ age + gender + batch + session + fly, metric.df,family="gaussian")
  # lm.fit = summary(lm.fit.orig)
  # ## If the model does not fit the data well, just save the original data and continue
  # if(lm.fit$adj.r.squared < 0.6){
  # write.table(metric.df,
  #             paste0("metrics/metric_",ind,".csv"),
  #             row.names=F,quote=F,sep=",")
  # next
  # #}
  
  # coef = na.omit(lm.fit.orig$coefficients)
  #
  # model = model.matrix(value ~ genotype + experimenter + gender + session + batch + setup + fly + age, metric.df)
  # model = data.matrix(model[,names(coef)])
  # ## Confounding covariates
  # coef_ind <- c(#1,## intercept
  #     grep("experimenter",names(coef)),
  #     grep("batch",names(coef)),
  #     grep("setup",names(coef)),
  #     grep("age",names(coef)),
  #     grep("gender",names(coef))
  #                                     #grep("fly",names(coef))
  #          )
  # uwv <- model[,coef_ind] %*% coef[coef_ind]
  # wv <- model[,-coef_ind] %*% coef[-coef_ind]
  #
  # metric.df$value.uw = uwv
  # metric.df$value.w = wv
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
