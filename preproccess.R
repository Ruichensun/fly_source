setwd("E:/Behavioral_project/behavior_experiment_data/Analysis")
source("E:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
path = "E:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/"
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
  "E2",
  "E2T2",
  "E2T2E2",
  "E2T2E2T2",
  "E2T2E2T2E2",
  "E2T2E2T2E2T2",
  "E2T2E1T2E2T2E2",
  "E2R2",
  "E2R2E2",
  "E2R2E2R2",
  "E2R2E2R2E2",
  "E2R2E2R2E2R2",
  "E2R2E1R2E2R2E2",
  "E2N2",
  "E2N2E2",
  "E2N2E2N2",
  "E2N2E2N2E2",
  "E2N2E2N2E2N2",
  "E2N2E1N2E2N2E2",
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
query.sessions <- unique(query.sessions)

## CS
fly.info = read.csv(paste0(path,"fly_info_CS.csv"),header = T,stringsAsFactors = F)
fly.info$fly = as.numeric(fly.info$fly)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.out = NULL
laser.status = NULL
fly.pause = NULL

for (ind in 1:nrow(fly.info)) {
  for (session in query.sessions) {
    input.file <-list.files(path = paste0(path, fly.info$experimenter[ind],"/CS/CSV/"),
                            pattern = paste0("ProcessedData_Fly", fly.info$fly[ind], "_", session, 
                                             "_", fly.info$gender[ind],"_.*"), full.names = T)
    print(input.file)
    if (fly.info$experimenter[ind] == "ES" & session == "E1" & length(input.file) == 0) {
      cat("M", fly.info$fly[ind], fly.info$category[ind],input.file, "\n",sep = "\t")
      break
    }
    if (length(input.file) == 0) {
      next
    }
    ## Processing for some special cases
    if (((fly.info$experimenter[ind] == "JG") & (fly.info$fly[ind] <= 72))
        | ((fly.info$experimenter[ind] == "RS") & (fly.info$fly[ind] <= 64))
        | ((fly.info$experimenter[ind] == "ES") &(fly.info$fly[ind] <= 32))) {
      framerate = 10
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        exp.time = length(fly.pos)
        fly.pos <- fly.pos[seq(1, exp.time, by = 2)]
        laser.status <- rep(NA, length(fly.pos))
      } else if(dim(read.csv(input.file, header = T, nrow = 1))[2] == 2){
        file = read.csv(input.file, header = T, stringsAsFactors = F)
        fly.pos = file[, 1]
        laser.status = file[, 2]
      }
    }else{
      framerate = 50
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        laser.status <- rep(NA, length(fly.pos))
      } else {file = read.csv(input.file, header = T, stringsAsFactors = F)
            fly.pos <- file[, 1]
            laser.status <- file[, 2]
      }
    }
    if (session == "E1") {
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
      fly.info.framerate = c(fly.info.framerate, framerate)
      fly.info.out = rbind(fly.info.out, fly.info[ind, ])
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")
    dir.create(
      paste0("data/", fly.info$experimenter[ind], "/CS/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    output.file <- paste0("data/", fly.info$experimenter[ind], "/CS/", "ProcessedData_Fly",
                          fly.info$fly[ind],"_",session,"_WT", ".csv")
    print(output.file)
    write.table(fly.pos.dat, output.file, row.names = F, quote = F, sep = ',')
  }
}
fly.info.out$Framerate = fly.info.framerate
fly.info.out$Fly.moving.speed = fly.moving.speed
fly.info.out$Fly.pause = fly.pause
write.table(fly.info.out, "data/fly_info_CS_preprocessed.csv", row.names = F,quote = F,sep = ",")

## Mutants
fly.info = read.csv(paste0(path, "fly_info_mutants.csv"),header = T,stringsAsFactors = F)
fly.info$fly = as.numeric(fly.info$fly)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.out = NULL
laser.status = NULL
count = 0
fly.pause = NULL

for (ind in 1:nrow(fly.info)) {
  for (session in query.sessions) {
    input.file <- list.files(path = paste0(path, fly.info$experimenter[ind],"/Mutants/CSV/"),
                             pattern = paste0("ProcessedData_Fly", fly.info$fly[ind],"_",
                                              session,"_",fly.info$gender[ind], "_.*"),
                             full.names = T)
    print(input.file)
    if (fly.info$experimenter[ind] == "ES" & session == "E1" & length(input.file) == 0) {
      cat("M", fly.info$fly[ind],fly.info$category[ind],input.file,"\n",sep = "\t")
      break
    }
    if (length(input.file) == 0) {
      next
    }
    if ((fly.info$fly[ind] <= 113) & (fly.info$experimenter[ind] == "JG")){
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1){
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        exp.time = length(fly.pos)
        fly.pos <- fly.pos[seq(1, exp.time, by = 2)]
        laser.status <- rep(NA, length(fly.pos))
      } else if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 2) {
        file = read.csv(input.file, header = T, stringsAsFactors = F)
        fly.pos = file[,1]
        laser.status <- file[,2]
      }
      framerate = 10
    } else {
        framerate = 50
        if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
         fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
         laser.status <- rep(NA, length(fly.pos))
       }else{
          file = read.csv(input.file, header = T, stringsAsFactors = F)
          fly.pos = file[,1]
          laser.status <- file[,2]
       }
    }
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
      fly.info.framerate = c(fly.info.framerate, framerate)
      fly.info.out = rbind(fly.info.out, fly.info[ind, ])
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")
    dir.create(
      paste0("data/", fly.info$experimenter[ind], "/mutants/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    
    output.file <- paste0("data/", fly.info$experimenter[ind], "/mutants/","ProcessedData_Fly", 
                          fly.info$fly[ind], "_", session, "_", fly.info$genotype[ind], ".csv")
    print(output.file)
    write.table(fly.pos.dat,output.file, row.names = F, quote = F, sep = ',')
  }
}
fly.info.out$Framerate = fly.info.framerate
fly.info.out$Fly.moving.speed = fly.moving.speed
fly.info.out$Fly.pause = fly.pause
write.table(fly.info.out, "data/fly_info_mutants_preprocessed.csv", row.names = F, quote = F, sep = ",")
