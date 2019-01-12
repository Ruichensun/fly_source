setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

query.sessions <- c(
  "E1",
  "E1T1",
  "E1T1E1",
  "E1T1E1T1",
  "E1T1E1T1E1",
  "E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1",
  "E1T1E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1T1E1",
  
  "E1T1E1T1E1T2",
  "E1T1E1T1E1T2E1",
  "E1T1E1T1E1T2E1T2",
  "E1T1E1T1E1T2E1T2E1",
  
  "E1T2",
  "E1T2E1",
  "E1T2E1T2",
  "E1T2E1T2E1",
  "E1T2E1T2E1T1",
  "E1T2E1T2E1T1E1",
  "E1T2E1T2E1T1E1T1",
  "E1T2E1T2E1T1E1T1E1",
  
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
  "E1N1E1N1E1N1E1N1E1",
  
  "E1T1E1R1",
  "E1T1E1R1E1",
  "E1T1E1R1E1R1",
  "E1T1E1R1E1R1E1",
  "E1T1E1R1E1R1E1R1",
  "E1T1E1T1E1R1E1R1E1",
  
  "E1T1E1N1",
  "E1T1E1N1E1",
  "E1T1E1N1E1N1",
  "E1T1E1N1E1N1E1",
  "E1T1E1N1E1N1E1N1",
  "E1T1E1T1E1N1E1N1E1"
)
query.sessions <- unique(query.sessions)

## CS
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp
fly.info = read.csv("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/fly_info_CS.csv",
                    header = T,
                    stringsAsFactors = F
                    )
fly.info$Age = as.Date(fly.info$Exp.date, format = '%m/%d/%Y')  - as.Date(fly.info$Birth.date, format = '%m/%d/%Y')
fly.info$Fly = as.numeric(fly.info$Fly)

fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.out = NULL
laser.status = NULL
count = 0
fly.pause = NULL

for (ind in 1:nrow(fly.info)) {
  for (session in query.sessions) {
    input.file <-
      list.files(
        path = paste0(
          "D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/",
          fly.info$experimenter[ind],
          "/CS/CSV/"
        ),
        pattern = paste0(
          "ProcessedData_Fly",
          fly.info$Fly[ind],
          "_",
          session,
          "_",
          fly.info$Gender[ind],
          "_.*"
        ),
        
        full.names = T
      )
    
    if (fly.info$experimenter[ind] == "ES" &
        session == "E1" & length(input.file) == 0) {
      cat("M",
          fly.info$Fly[ind],
          fly.info$Category[ind],
          input.file,
          "\n",
          sep = "\t")
      break
    }
    
    if (length(input.file) == 0) {
      next
    }
    framerate = 50
    
    ## Read data
    fly.pos <- read.csv(input.file, stringsAsFactors = F)[, 1]
    
    if (dim(read.csv(input.file, stringsAsFactors = F))[2] == 1) {
      laser.status <- rep(NA, length(fly.pos))#
    }
    
    if (dim(read.csv(input.file, stringsAsFactors = F))[2] == 2) {
      #
      laser.status <-
        read.csv(input.file, stringsAsFactors = F)[, 2]#
    }#
    
    exp.time = length(fly.pos)
    ## Processing for some special cases
    if (((fly.info$experimenter[ind] == "JG") &
         (fly.info$Fly[ind] <= 72))
        |
        ((fly.info$experimenter[ind] == "RS") &
         (fly.info$Fly[ind] <= 64))
        |
        ((fly.info$experimenter[ind] == "ES") &
         (fly.info$Fly[ind] <= 32))
        |
        ((fly.info$experimenter[ind] == "JE") &
         (fly.info$Fly[ind] <= 71))
        | (fly.info$experimenter[ind] == "LM")) {
      # print(input.file)
      fly.pos <- fly.pos[seq(1, exp.time, by = 2)]
      
      laser.status <- rep(NA, length(fly.pos))
      
      if (dim(read.csv(input.file, stringsAsFactors = F))[2] == 2) {
        #
        laser.status <-
          read.csv(input.file, stringsAsFactors = F)[, 2]#
      }#
      
      framerate = 10
    }
    
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")#
    
    dir.create(
      paste0("data/", fly.info$experimenter[ind], "/CS/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    output.file <- paste0(
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
    write.table(
      fly.pos.dat,
      output.file,
      row.names = F,
      quote = F,
      sep = ','
    )
    if (session == "E1") {
      fly.info.framerate = c(fly.info.framerate, framerate)
      fly.info.out = rbind(fly.info.out, fly.info[ind, ])
    }
  }
}

colnames(fly.info.out) = colnames(fly.info)
fly.info.out$Framerate = fly.info.framerate
fly.info.out$Fly.moving.speed = fly.moving.speed
fly.info.out$Fly.pause = fly.pause
write.table(
  fly.info.out,
  "data/fly_info_CS_preprocessed.csv",
  row.names = F,
  quote = F,
  sep = ","
)

## Mutants
fly.info = read.csv("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/fly_info_mutants.csv",
                    header = T,
                    stringsAsFactors = F
                   )
fly.info$Age = as.Date(fly.info$Exp.date, format = '%m/%d/%Y')  - as.Date(fly.info$Birth.date, format = '%m/%d/%Y')
fly.info$Fly = as.numeric(fly.info$Fly)

fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.out = NULL
count = 0
fly.pause = NULL

for (ind in 1:nrow(fly.info)) {
  for (session in query.sessions) {
    input.file <-
      list.files(
        path = paste0(
          "D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/",
          fly.info$experimenter[ind],
          "/Mutants/CSV/"
        ),
        pattern = paste0(
          "ProcessedData_Fly",
          fly.info$Fly[ind],
          "_",
          session,
          "_",
          fly.info$Gender[ind],
          "_.*"
        ),
        
        full.names = T
      )
    
    if (fly.info$experimenter[ind] == "ES" &
        session == "E1" & length(input.file) == 0) {
      cat("M",
          fly.info$Fly[ind],
          fly.info$Category[ind],
          input.file,
          "\n",
          sep = "\t")
      break
    }
    
    if (length(input.file) == 0) {
      next
    }
    framerate = 50
    
    ## Read data
    fly.pos <- read.csv(input.file, stringsAsFactors = F)[, 1]

    laser.status <- rep(NA, length(fly.pos))#
    if (dim(read.csv(input.file, stringsAsFactors = F))[2] == 2) {
      laser.status <-
        read.csv(input.file, stringsAsFactors = F)[, 2]#
    }
    
    exp.time = length(fly.pos)
    
    if ((fly.info$Fly[ind] <= 113) &
        (fly.info$experimenter[ind] == "JG")){
      fly.pos <- fly.pos[seq(1, exp.time, by = 2)]
      laser.status <- rep(NA, length(fly.pos))#
      if (dim(read.csv(input.file, stringsAsFactors = F))[2] == 2) {
        laser.status <-
          read.csv(input.file, stringsAsFactors = F)[, 2]#
      }
      framerate = 10
      print(framerate)
    }
    
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")#
    dir.create(
      paste0("data/", fly.info$experimenter[ind], "/mutants/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    output.file <- paste0(
      "data/",
      fly.info$experimenter[ind],
      "/mutants/",
      "ProcessedData_Fly",
      fly.info$Fly[ind],
      "_",
      session,
      "_",
      fly.info$Genotype[ind],
      ".csv"
    )
    write.table(
      fly.pos.dat,
      output.file,
      row.names = F,
      quote = F,
      sep = ','
    )
    if (session == "E1") {
      fly.info.framerate = c(fly.info.framerate, framerate)
      fly.info.out = rbind(fly.info.out, fly.info[ind, ])
    }
  }
}

colnames(fly.info.out) = colnames(fly.info)
fly.info.out$Framerate = fly.info.framerate
fly.info.out$Fly.moving.speed = fly.moving.speed
fly.info.out$Fly.pause = fly.pause
write.table(
  fly.info.out,
  "data/fly_info_mutants_preprocessed.csv",
  row.names = F,
  quote = F,
  sep = ","
)
