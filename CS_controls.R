setwd("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

#Response
type = c("Response","Response", "response")
experimenter <- c("JD", "ES")
combine_flyCSV(experimenter, type)

fly.info.response = read.csv(paste0(path,"fly_info_response.csv"),header = T,stringsAsFactors = F)
fly.info.response$Age = as.Date(fly.info.response$Exp.date, format = '%m/%d/%Y') - as.Date(fly.info.response$Birth.date, format = '%m/%d/%Y')
fly.info.response$Fly = as.numeric(fly.info.response$Fly)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.response.out = NULL
laser.status = NULL
count = 0
fly.pause = NULL

query.sessions = c(
  "E1",
  "E1L1",
  "E1L1E1",
  "E1L10",
  "E1L10E1"
)


for (ind in 1:nrow(fly.info.response)) {
  for (session in query.sessions) {
    input.file <-list.files(path = paste0(path, fly.info.response$Experimenter[ind],"/CS/CSV/"),
                            pattern = paste0("ProcessedData_Fly", fly.info.response$Fly[ind], "_", session, 
                                             "_", fly.info.response$Gender[ind],"_.*"), full.names = T)
    if (length(input.file) == 0) {
      next
    }else{
      framerate = 50
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        laser.status <- rep(NA, length(fly.pos))#
      } else {file = read.csv(input.file, header = T, stringsAsFactors = F)
      fly.pos <- file[, 1]
      laser.status <- file[, 2]
      }
    }
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
      fly.info.framerate = c(fly.info.framerate, framerate)
      # print(fly.info[ind,])
      fly.info.response.out = rbind(fly.info.response.out, fly.info.response[ind, ])
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")
    dir.create(
      paste0("data/", fly.info.response$Experimenter[ind], "/CS/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    output.file <- paste0("data/", fly.info.response$Experimenter[ind], "/CS/", "ProcessedData_Fly",
                          fly.info.response$Fly[ind],"_",session,"_WT", ".csv")
    # print(output.file)
    write.table(fly.pos.dat, output.file, row.names = F, quote = F, sep = ',')
  }
}
fly.info.response.out$Framerate = fly.info.framerate
fly.info.response.out$Fly.moving.speed = fly.moving.speed
fly.info.response.out$Fly.pause = fly.pause
write.table(fly.info.response.out, "data/fly_info_response.csv", row.names = F,quote = F,sep = ",")


#Constant_time
type = rep("CS_constant",3)
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

fly.info.response = read.csv(paste0(path,"fly_info_response.csv"),header = T,stringsAsFactors = F)
fly.info.response$Age = as.Date(fly.info.response$Exp.date, format = '%m/%d/%Y') - as.Date(fly.info.response$Birth.date, format = '%m/%d/%Y')
fly.info.response$Fly = as.numeric(fly.info.response$Fly)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.response.out = NULL
laser.status = NULL
count = 0
fly.pause = NULL

query.sessions = c(
  "E1",
  "E1L1",
  "E1L1E1",
  "E1L10",
  "E1L10E1"
)


for (ind in 1:nrow(fly.info.response)) {
  for (session in query.sessions) {
    input.file <-list.files(path = paste0(path, fly.info.response$Experimenter[ind],"/CS/CSV/"),
                            pattern = paste0("ProcessedData_Fly", fly.info.response$Fly[ind], "_", session, 
                                             "_", fly.info.response$Gender[ind],"_.*"), full.names = T)
    if (length(input.file) == 0) {
      next
    }else{
      framerate = 50
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        laser.status <- rep(NA, length(fly.pos))#
      } else {file = read.csv(input.file, header = T, stringsAsFactors = F)
      fly.pos <- file[, 1]
      laser.status <- file[, 2]
      }
    }
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
      fly.info.framerate = c(fly.info.framerate, framerate)
      # print(fly.info[ind,])
      fly.info.response.out = rbind(fly.info.response.out, fly.info.response[ind, ])
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")
    dir.create(
      paste0("data/", fly.info.response$Experimenter[ind], "/CS/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    output.file <- paste0("data/", fly.info.response$Experimenter[ind], "/CS/", "ProcessedData_Fly",
                          fly.info.response$Fly[ind],"_",session,"_WT", ".csv")
    # print(output.file)
    write.table(fly.pos.dat, output.file, row.names = F, quote = F, sep = ',')
  }
}
fly.info.response.out$Framerate = fly.info.framerate
fly.info.response.out$Fly.moving.speed = fly.moving.speed
fly.info.response.out$Fly.pause = fly.pause
write.table(fly.info.response.out, "data/fly_info_response.csv", row.names = F,quote = F,sep = ",")



#CS - Controls1
type = c("CS_controls", "CS_controls1", "CS_controls1")
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

#CS - Controls2
type = c("CS_controls", "CS_controls2", "CS_controls2")
experimenter <- c("JD")
combine_flyCSV(experimenter, type)
