# source("plot_trend.R")
# Requires fly_pos_to_moving_status function from "plot_trend.R"
setwd("D:/Behavioral_project/Behavior Experiment Data/Analysis/")

Delay_of_Laser_On_Off = function(input_file){
  
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = fly.file$fly_pos.framerate.50
  fly.laser.raw = fly.file$laser_status
  # If laser status not recorded, return NA
  if (is.na(fly.laser.raw[1]) == T) {
    return(c(NA,NA,NA))
  }else{
    fly.moving.status.raw = fly_pos_to_moving_status(fly.position.raw)
    starting_point = 21
    fly.position = fly.position.raw[starting_point:length(fly.position.raw)]
    fly.laser.status = fly.laser.raw[starting_point:length(fly.laser.raw)]
    fly.moving.status = fly.moving.status.raw[(starting_point-1):length(fly.moving.status.raw)] 
  }
  for (i in 1:length(fly.moving.status)){
    if ((fly.position[i]<50)){
      fly.moving.status[i] = 1
    }
    if ((fly.position[i]>717)){
      fly.moving.status[i] = 1
    }
  }
  moving_status_summary = rle(fly.moving.status)
  
  total_frame_moving = sum(moving_status_summary$lengths[moving_status_summary$values!=0])
  total_frame_pause = sum(moving_status_summary$lengths[moving_status_summary$values==0])
  
  moving_laser_status.df = data.frame(fly.moving.status,fly.laser.status)
  
  fly.laser.status.ONSET = replace(fly.laser.status, fly.laser.status > 0, 1)
  laser.moving.status = fly.laser.status.ONSET - fly.moving.status
  
  laser.moving.status_summary = rle(laser.moving.status)
  Laser_On_Delay_by_event = laser.moving.status_summary$lengths[laser.moving.status_summary$values==-1]
  Laser_Off_Delay_by_event = laser.moving.status_summary$lengths[laser.moving.status_summary$values==1]
  
  Laser_On_Delay = mean(Laser_On_Delay_by_event)
  Laser_Off_Delay = mean(Laser_Off_Delay_by_event)
  
  return(c(Laser_On_Delay,
           Laser_Off_Delay))
}

chance_of_being_hit_by_laser = function(input_file){
  
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = fly.file$fly_pos.framerate.50
  fly.laser.raw = fly.file$laser_status
  # If laser status not recorded, return NA
  if (is.na(fly.laser.raw[1]) == T) {
    return(c(NA,NA,NA))
  }else{
    fly.moving.status.raw = fly_pos_to_moving_status(fly.position.raw)
    starting_point = 21
    fly.position = fly.position.raw[starting_point:length(fly.position.raw)]
    fly.laser.status = fly.laser.raw[starting_point:length(fly.laser.raw)]
    fly.moving.status = fly.moving.status.raw[(starting_point-1):length(fly.moving.status.raw)]

    for (i in 1:length(fly.moving.status)){
      if ((fly.position[i]<50)){
        fly.moving.status[i] = 1
      }
      if ((fly.position[i]>717)){
        fly.moving.status[i] = 1
      }
    }
    
    moving_status_summary = rle(fly.moving.status)
    
    total_frame_moving = sum(moving_status_summary$lengths[moving_status_summary$values!=0])
    total_frame_pause = sum(moving_status_summary$lengths[moving_status_summary$values==0])
    
    moving_laser_status.df = data.frame(fly.moving.status,fly.laser.status)
    moving_status_during_laser_ON = rle(moving_laser_status.df$fly.moving.status[moving_laser_status.df$fly.laser.status!=0])
    
    moving_laser_ON = sum(moving_status_during_laser_ON$lengths[moving_status_during_laser_ON$values==1])
    pause_laser_ON = sum(moving_status_during_laser_ON$lengths[moving_status_during_laser_ON$values==0])
    
    chance_of_being_hit_by_laser_during_moving = moving_laser_ON/total_frame_moving
    chance_of_being_hit_by_laser_during_pause = pause_laser_ON/total_frame_pause
    laser_on_percentage = length(moving_laser_status.df$fly.moving.status[moving_laser_status.df$fly.laser.status!=0])/length(fly.moving.status)
  
    return(c(chance_of_being_hit_by_laser_during_moving,
             chance_of_being_hit_by_laser_during_pause,
             laser_on_percentage))
  }
}

###Calculating all the flies' chance of being hit by types (T/R)

total_chance_of_being_hit_by_laser <- function(file_name_filter, fly.info.movement) {
  laser_chance = data.frame()
  for (ind in 1:nrow(fly.info.movement)) {
    input.file <- list.files(
      path = paste0("data/",
                    fly.info.movement$experimenter[ind],
                    "/CS/"),
      pattern = paste0(
        "ProcessedData_Fly",
        fly.info.movement$Fly[ind],
        "_",
        file_name_filter,
        "_WT",
        ".csv"
      ),
      full.names = T
    )
        if(length(input.file)==0){
     next() 
    }
    laser_chance = rbind(laser_chance, chance_of_being_hit_by_laser(input.file))
  }
  
  names(laser_chance) = c("Chances of being hit during walking", "Chances of being hit during pause ", "Laser ON duration percentage")
  return(laser_chance)
}

# file_name_filter =
  # "E1T1"
  # "E1R1"
  # "E1T1E1T1"
  # "E1R1E1R1"

fly.info.movement.T = fly.info.include[((fly.info.include$Genotype == "WT") |
                                          (fly.info.include$Genotype == "CS")) &
                                         (fly.info.include$Category =="T")&
                                         (fly.info.include$experimenter!="SW"), ]

fly.info.movement.R = fly.info.include[((fly.info.include$Genotype == "WT") |
                                          (fly.info.include$Genotype == "CS")) &
                                         (fly.info.include$Category == "R")&
                                         (fly.info.include$experimenter!="SW"), ]
# fly.info.movement.T = fly.info.include[((fly.info.include$Genotype == "SUN2")) &
#                                          (fly.info.include$Category =="T")&
#                                          (fly.info.include$experimenter!="SW"), ]
# 
# fly.info.movement.R = fly.info.include[((fly.info.include$Genotype == "SUN2")) &
#                                          (fly.info.include$Category == "R")&
#                                          (fly.info.include$experimenter!="SW"), ]


first_yoked_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1R1",fly.info.movement.R)
second_yoked_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1R1E1R1", fly.info.movement.R)
first_training_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1T1",fly.info.movement.T)
second_training_session = total_chance_of_being_hit_by_laser(file_name_filter = "E1T1E1T1",fly.info.movement.T)







pdf("ChanceofBeingHitCS_063018.pdf",
    onefile = T,
    width = 10)

Chance_of_being_hit = list(
  
  first_training_session$`Chances of being hit during walking`,
  first_training_session$`Chances of being hit during pause `,
  
  first_yoked_session$`Chances of being hit during walking`,
  first_yoked_session$`Chances of being hit during pause `,
  
  second_training_session$`Chances of being hit during walking`,
  second_training_session$`Chances of being hit during pause `,
  
  second_yoked_session$`Chances of being hit during walking`,
  second_yoked_session$`Chances of being hit during pause `
)




col.pool <- c( "indianred3",
               "indianred3",
               "light blue",
               "light blue"
               )

boxplot(
  Chance_of_being_hit[1:4],
  ylim = c(0, 1),
  outline = F,
  notch = T,
  lwd = 2,
  ylab = "Percentage",
  xaxt = "n",
  col = col.pool,
  main = "Chance of being punished in first session",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[1:4],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  "grey40"
)

text(
  x = (1:length(Chance_of_being_hit[1:4])) - 0.1,
  y = 1.02,
  labels = c(
    length(Chance_of_being_hit[[1]]),
    length(Chance_of_being_hit[[2]]),
    length(Chance_of_being_hit[[3]]),
    length(Chance_of_being_hit[[4]])
    # length(Chance_of_being_hit[[5]]),
    # length(Chance_of_being_hit[[6]]),
    # length(Chance_of_being_hit[[7]]),
    # length(Chance_of_being_hit[[8]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = (1:length(Chance_of_being_hit[1:4])) - 0.3,
  y = -0.1,
  labels = c(
    "T - Walking",
    "T - Pause",
    "R - Walking",
    "R - Pause"
    # length(Chance_of_being_hit[[5]]),
    # length(Chance_of_being_hit[[6]]),
    # length(Chance_of_being_hit[[7]]),
    # length(Chance_of_being_hit[[8]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)
# lines(c(4.5, 4.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)
# lines(c(6.5, 6.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)

boxplot(
  Chance_of_being_hit[5:8],
  ylim = c(0, 1),
  outline = F,
  notch = T,
  lwd = 2,
  ylab = "Percentage",
  xaxt = "n",
  col = col.pool,
  main = "Chance of being punished in second session",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = Chance_of_being_hit[5:8],
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  "grey40"
)

text(
  x = (1:length(Chance_of_being_hit[5:8])) - 0.1,
  y = 1.02,
  labels = c(
    # length(Chance_of_being_hit[[1]]),
    # length(Chance_of_being_hit[[2]]),
    # length(Chance_of_being_hit[[3]]),
    # length(Chance_of_being_hit[[4]])
    length(Chance_of_being_hit[[5]]),
    length(Chance_of_being_hit[[6]]),
    length(Chance_of_being_hit[[7]]),
    length(Chance_of_being_hit[[8]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

text(
  x = (1:length(Chance_of_being_hit[5:8])) - 0.3,
  y = -0.1,
  labels = c(
    "T - Walking",
    "T - Pause",
    "R - Walking",
    "R - Pause"
    # length(Chance_of_being_hit[[5]]),
    # length(Chance_of_being_hit[[6]]),
    # length(Chance_of_being_hit[[7]]),
    # length(Chance_of_being_hit[[8]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)
# lines(c(4.5, 4.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)
# lines(c(6.5, 6.5), c(-1, 1.2),
#       col = "light grey",
#       lty = 1)


dev.off()

p_being_hit = c(wilcox.test(Chance_of_being_hit[[1]],Chance_of_being_hit[[2]])$p.value,
                wilcox.test(Chance_of_being_hit[[3]],Chance_of_being_hit[[4]])$p.value,
                wilcox.test(Chance_of_being_hit[[5]],Chance_of_being_hit[[6]])$p.value,
                wilcox.test(Chance_of_being_hit[[7]],Chance_of_being_hit[[8]])$p.value
)



pdf("YokedFlyChanceOfBeingHitSUN2.pdf",
    onefile = T,
    width = 10)
yokedflybeinghit= list(
  first_yoked_session$`Laser ON duration percentage`,
  second_yoked_session$`Laser ON duration percentage`,
  first_yoked_session$`Chances of being hit during walking`,
  second_yoked_session$`Chances of being hit during walking`,
  first_yoked_session$`Chances of being hit during pause `,
  second_yoked_session$`Chances of being hit during pause `
)

col.pool <- c("light blue",
              "light blue",
              "light blue",
              "light blue",
              "light blue",
              "light blue"
              )

boxplot(
  yokedflybeinghit,
  ylim = c(0, 1),
  outline = F,
  notch = T,
  lwd = 2,
  ylab = "Chances of Being Punished",
  xaxt = "n",
  col = col.pool,
  main = "Yoked's total chance of being punished, during walking and pause",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = yokedflybeinghit,
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  "grey40"
)

text(
  x = (1:length(yokedflybeinghit)) - 0.1,
  y = 1,
  labels = c(
    length(yokedflybeinghit[[1]]),
    length(yokedflybeinghit[[2]]),
    length(yokedflybeinghit[[3]]),
    length(yokedflybeinghit[[4]]),
    length(yokedflybeinghit[[5]]),
    length(yokedflybeinghit[[6]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)
lines(c(4.5, 4.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)

dev.off()


pdf("TrainedFlyChanceOfBeingHitSUN2.pdf",
    onefile = T,
    width = 10)
trainedflybeinghit= list(
  first_training_session$`Laser ON duration percentage`,
  second_training_session$`Laser ON duration percentage`,
  first_training_session$`Chances of being hit during walking`,
  second_training_session$`Chances of being hit during walking`,
  first_training_session$`Chances of being hit during pause `,
  second_training_session$`Chances of being hit during pause `
)

col.pool <- c("indianred3",
              "indianred3",
              "indianred3",
              "indianred3",
              "indianred3",
              "indianred3"
)

boxplot(
  trainedflybeinghit,
  ylim = c(0, 1),
  outline = F,
  notch = T,
  lwd = 2,
  ylab = "Chances of Being Punished",
  xaxt = "n",
  col = col.pool,
  main = "Trained's total chance of being punished, during walking and pause",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = trainedflybeinghit,
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  "grey40"
)

text(
  x = (1:length(trainedflybeinghit)) - 0.1,
  y = 1,
  labels = c(
    length(trainedflybeinghit[[1]]),
    length(trainedflybeinghit[[2]]),
    length(trainedflybeinghit[[3]]),
    length(trainedflybeinghit[[4]]),
    length(trainedflybeinghit[[5]]),
    length(trainedflybeinghit[[6]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(2.5, 2.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)
lines(c(4.5, 4.5), c(-1, 1.2),
      col = "light grey",
      lty = 1)

dev.off()

