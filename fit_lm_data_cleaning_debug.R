setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
load("all_ofs.Rdata")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

metrices = c(
 
  "Burstiness (Pause)",#13

  "Percentage Time Active (Pause not at the End)"#29
)

sessions <- c(
  "E1",
  "E1T1E1",
  "E1T1E1T1E1",
  "E1R1E1",
  "E1R1E1R1E1",
  "E1N1E1",
  "E1N1E1N1E1"
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
  "Fly.pause",
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
fly.info.include = fly.info[ind.include, ][1:10,]


## Fit linear model for each metric
for (ind in 1:length(metrices)) {
  print(ind)
  metric.df = NULL
  for (session in sessions) {
    metric <- sapply(all_ofs[[session]], function(x) {
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
  write.table(
    metric.df,
    paste0("metrics/metric_debug_", ind, ".csv"),
    row.names = F,
    quote = F,
    sep = ","
  )
}

