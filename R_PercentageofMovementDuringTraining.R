# source("plot_trend.R")
# Requires fly_pos_to_moving_status function from "plot_trend.R"

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
    return(c(chance_of_being_hit_by_laser_during_moving,chance_of_being_hit_by_laser_during_pause,laser_on_percentage))
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

file_name_filter =
  # "E1T1"
  # "E1R1"
  # "E1T1E1T1"
  "E1R1E1R1"

fly.info.movement.T = fly.info.include[((fly.info.include$Genotype == "WT") |
                                          (fly.info.include$Genotype == "CS")) &
                                         (fly.info.include$Category ==
                                            "T"), ]

fly.info.movement.R = fly.info.include[((fly.info.include$Genotype == "WT") |
                                          (fly.info.include$Genotype == "CS")) &
                                         (fly.info.include$Category == "R"), ]

total_chance_of_being_hit_by_laser(file_name_filter = "E1R1",fly.info.movement.R)

