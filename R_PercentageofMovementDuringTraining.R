# source("plot_trend.R")


fly.info.movement.R = fly.info.include[((fly.info.include$Genotype=="WT")|(fly.info.include$Genotype=="CS"))&
                                         (fly.info.include$Category=="R"),]


##Demo
setwd( "D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/data/JD/CS")
input_file = "ProcessedData_Fly237_E1R1_WT.csv"

moving_status = function(input_file){
  
  fly.file = read.csv(input_file,header=T,stringsAsFactors=F)
  
  fly.position.raw = as.numeric(fly.file[[1]])
  fly.laser.raw = as.numeric(fly.file[[2]])
  
  if (is.na(fly.laser.raw[1])==T){
    
    number_of_laser_clicks = NA
    total_laser_ON = NA
    
    ret = list(
      number_of_laser_clicks,
      total_laser_ON #in seconds
    )
    
    names(ret) = c(
      "Number of Laser Punishments", #1
      "Total Laser Exposure in Seconds" #2
    )
    
  }else{
    data_start = 20
    
    fly_pos = fly.position.raw[data_start:length(fly.position.raw)]
    fly_laser = fly.laser.raw[data_start:length(fly.laser.raw)]
    
    
    
    for (i in 1:length(fly_pos)){
      if(fly_laser[i]!=0){
        
      }
    }
    
    
    if (fly_laser[length(fly_laser)]>0){
      # fly_laser[length(fly_laser)-1]=0
      fly_laser[length(fly_laser)]=0
    }
    
    if (fly_laser[1]>0){
      # fly_laser[length(fly_laser)-1]=0
      fly_laser[1]=0
    }
    for (i in 1:length(fly_laser)){
      if (fly_laser[i]>0){
        fly_laser[i]=1
      }
    }
    laser_ON  = rle(fly_laser)$length[rle(fly_laser)$values==1]
    laser_OFF = rle(fly_laser)$length[rle(fly_laser)$values==0]
    
    label_for_laser= rep(0, length(fly_laser))
    for (i in 1:(length(label_for_laser)-1)){
      if ((fly_laser[i]==0)&(fly_laser[i+1]>0)){
        label_for_laser[i+1] = 1
      }
      if ((fly_laser[i]>0)&(fly_laser[i+1]==0)){
        label_for_laser[i+1] = 2
      }
    }
    
    laser_df = data.frame()
    
    if (laser_OFF[1] == length(fly_laser)){
      laser_df = data.frame(0,0,0,(laser_OFF[1])/framerate,(laser_OFF[1]-0)/framerate,TRUE) 
      
    }else{
      laser_df = data.frame (which(label_for_laser==1),which(label_for_laser==2),laser_ON/framerate,(laser_OFF[2:length(laser_OFF)])/framerate,(laser_OFF[2:length(laser_OFF)]-laser_ON)/framerate,laser_OFF[2:length(laser_OFF)]>8*60*framerate)
    }
    
    colnames(laser_df) = c("Laser_On","Laser_Off","ON_duration","OFF_duration","Difference","eight_min_OFF")
    
    number_of_laser_clicks = length(laser_df$Laser_On)
    total_laser_ON = sum(laser_df$ON_duration)
    
    if (number_of_laser_clicks == 1){
      if (laser_df$ON_duration==0){
        number_of_laser_clicks = 0
      }
    }
    
    
    ## Return output
    ret = list(
      number_of_laser_clicks,
      total_laser_ON #in seconds
    )
    
    
    
    names(ret) = c(
      "Number of Laser Punishments", #1
      "Total Laser Exposure in Seconds" #2
    )
    return(ret)
    
  }
  
  speed_threshold = 28 # This is determined by quantile(abs(fly_moving_status),c(0.97, 0.975, 0.98)), and the 97.5% corresponds to 28.6
  framerate = 50
  ##Finding out the fly's moving status by two criteria: velocity = 0 or velocity much larger than a speed threshold
  fly_pos = a$fly_pos.framerate.50
  starting_point = 21
  fly_pos = fly_pos[starting_point:length(fly_pos)]
  fly_moving_status = c(diff(c(a$fly_pos.framerate.50[starting_point-1],fly_pos)))
  fly_moving_status_discretized = replace(fly_moving_status, fly_moving_status>28,0)
  fly_moving_status_discretized = replace(fly_moving_status_discretized, fly_moving_status_discretized!=0,1)
  
  # for (i in 2:(length(fly_moving_status_discretized)-1)){
  #   if ((fly_moving_status_discretized[i-1]==0)&(fly_moving_status_discretized[i+1]==0)){
  #     fly_moving_status_discretized[i]=0
  #     }
  #   if ((fly_moving_status_discretized[i-1]==1)&(fly_moving_status_discretized[i+1]==1)){
  #   fly_moving_status_discretized[i]=1
  #     }
  #   }
  # for troubleshooting
  # a = replace(fly_moving_status_discretized,fly_moving_status_discretized==1,fly_moving_status)
  # a = replace(a,a==0,100)
  
  ###Labeling the No-Move and Move moments
  
  # label_for_pause = rep(0, length(fly_moving_status_discretized))
  # 
  # for (i in 2:length(label_for_pause)){
  #   if ((fly_moving_status_discretized[i]==0)&(fly_moving_status_discretized[i-1]>0)){
  #     label_for_pause[i] = 1
  #   }
  #   else if ((fly_moving_status_discretized[i]>0)&(fly_moving_status_discretized[i-1]==0)){
  #     label_for_pause[i] = 2
  #   }
  #   # else if ((fly_moving_status_discretized[i]<0)&(fly_moving_status_discretized[i-1]==0)){
  #   #   label_for_pause[i] = 3
  #   # }
  #   # else if ((fly_moving_status_discretized[i]==0)&(fly_moving_status_discretized[i-1]<0)){
  #   #   label_for_pause[i] = 4
  #   # }
  # }
  
  # run_length_movement = rle(fly_moving_status_discretized)
  # Moving = run_length_movement$lengths[run_length_movement$values==1]
  # Pause = run_length_movement$lengths[run_length_movement$values==0]
  # Movement_Difference = c()
  # 
  ## length(rle(fly_moving_status_discretized)$length[rle(fly_moving_status_discretized)$values==0])
  # run_length_movement$values[length(run_length_movement$values)]
  # 
  # if ((run_length_movement$values[1]==0)&(run_length_movement$values[length(run_length_movement$values)]==1)){
  #   Moving = c(0,Moving)
  #   Pause = c(Pause,0)
  #   Movement_Difference = Pause - Moving
  # }
  # 
  # if ((run_length_movement$values[1]==0)&(run_length_movement$values[length(run_length_movement$values)]==0)){
  #   Moving = c(0,Moving)
  #   Movement_Difference = Pause - Moving
  # }
  # 
  # if ((run_length_movement$values[1]==1)&(run_length_movement$values[length(run_length_movement$values)]==0)){
  #   Movement_Difference = Pause - Moving
  # }
  # 
  # if ((run_length_movement$values[1]==1)&(run_length_movement$values[length(run_length_movement$values)]==1)){
  #   Pause = c(Pause,0)
  #   Movement_Difference = Pause - Moving
  # }
  # 
  # Movement_Difference = Movement_Difference/framerate
  # normalized_x = (1:length(Movement_Difference))/length(Movement_Difference)
  # 
  # Movement_Difference = list(normalized_x,Movement_Difference)
  # names(Movement_Difference) = c("Pairs", "Duration")
  # return(Movement_Difference)
  return(cumsum(fly_moving_status_discretized))
}