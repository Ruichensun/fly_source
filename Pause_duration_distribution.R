speed_max_thres = 20 #Maximum speed allowed #changed to 20 from 50 on Oct.5
speed_zero_thres = 1e-2 #It determines how small the speed is to be regarded as immobile (i.e. zero)
#pause_frame_thres = 2 #Least of number of frames in a pause *changed from 40 to 20

one_fly_pause_duration <- function(input_file){
  x = read.table(input_file,header=T,sep=",",stringsAsFactors=F)
  x = as.numeric(x$fly.position)
  data_start_fly_position = 20#changed it to 20 on Oct 5, 2016 to avoid the initial gap
  fly_pos = x[data_start_fly_position:length(x)]
  
  # pos_fly_position = as.numeric(x[data_start_fly_position:nrow(x),1])
  ##Get the transient speed
  # vel = as.numeric(diff(c(x[data_start_fly_position-1,1],pos_fly_position)))    
  
  # Smoothening traces
  # for (i in 3:(length(fly_pos)-2)){
  #   fly_pos[i]<-(fly_pos[i+2]+fly_pos[i+1]+fly_pos[i]+fly_pos[i-1]+fly_pos[i-2])/5
  # }
  
  experiment_time = length(fly_pos)        
  
  set_time=30000
  
  if(data_start_fly_position > 1){
    fly_speed = diff(c(x[data_start_fly_position - 1],fly_pos))
  } else {
    fly_speed = diff(c(NA,fly_pos))
  }
  
  ##Smoothing speed
  if(fly_speed[1] > speed_max_thres){
    fly_speed[1] = 0
  }
  if(fly_speed[length(fly_pos)] > speed_max_thres){
    fly_speed[length(fly_pos)] = 0
  }
  for(t in 2:(length(fly_pos)-1)){
    pre_diff = fly_speed[t] - fly_speed[t-1]
    pro_diff = fly_speed[t+1] - fly_speed[t]
    if(abs(pre_diff) > speed_max_thres &
       abs(pro_diff) > speed_max_thres &
       pre_diff * pro_diff < 0){
      fly_speed[t] = (fly_speed[t-1]+fly_speed[t+1])/2#Average
    }
    if(abs(fly_speed[t]) <= speed_zero_thres){
      fly_speed[t] = 0
    }
  }
  
  ##Get the time spans when fly paused
  ##A pause is defined as time spans greater than pause_frame_thres frames
  ##when the transient speed remain 0 (less than a speed threshold)
  pause_start = c()
  pause_end = c()
  potential_pause_start = 1
  current_zero_length = 0
  is_pause = rep(0,experiment_time)
  if(fly_speed[1] == 0){
    current_zero_length = 1
  }
  for(t in 2:length(fly_pos)){
    if(fly_speed[t] == 0){
      if(current_zero_length == 0){
        potential_pause_start = t
      }
      current_zero_length = current_zero_length + 1
      next
    }
    else{
      if(current_zero_length >= pause_frame_thres){
        ##record the time span as a pause
        pause_start = c(pause_start,potential_pause_start)
        pause_end = c(pause_end,t-1)
        is_pause[potential_pause_start:(t-1)] = 1
        ##reset
        current_zero_length = 0
      }
      else{
        ##reset
        current_zero_length = 0
      }
    }
  }
  if(current_zero_length >= pause_frame_thres){
    ##record the time span as a pause
    pause_start = c(pause_start,potential_pause_start)
    pause_end = c(pause_end,t)
    is_pause[potential_pause_start:t] = 1
  }
  
  is_pause_middle=is_pause
  for (i in 1:experiment_time){
    if(is_pause[i]==1){
      if(fly_pos[i]<50){
        is_pause_middle[i]=0
      }
      else if (fly_pos[i]>717){
        is_pause_middle[i]=0
      }
      else{
        is_pause_middle[i]=1 
      }
    }
  }
  
  pause_middle_dur<-rle(is_pause_middle)$length[rle(is_pause_middle)$values==1]
  
  # ##Get pause duration
  # pause_duration = NULL;
  # for(i in 1:length(pause_start)){
  #   pause_duration = c(pause_duration, pause_end[i] - pause_start[i])
  # }
  # 
  # ##Get walking duration
  # walking_duration = NULL;
  # if(length(pause_start)>=2){
  #   for(i in 2:length(pause_start)){
  #     walking_duration = c(walking_duration, pause_start[i] - pause_end[i-1])
  #   }
  # }
  # return(list(walking_duration,pause_duration))
  return(pause_middle_dur)
}

################################################################################
frame_rate = 50
setwd("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov")
# fly_info = read.csv("Behavioral Experiments - CS_ES.csv",header=T,stringsAsFactors = F)
fly_info = read.csv("data/fly_info_CS_preprocessed.csv",header=T,stringsAsFactors=F)

# flies_to_exclude = c(108,116,124,130,142,180,181,201,212,213,224,225,248,251,262,263,266,267,271,272,294:312,348,350,377,378,385,386)


# path = "C:/Users/Ruichen/Desktop/data/csvdata" ##Please change here
# path = "F:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1" ##Please change here
flies_to_exclude<-c()
# flies_to_exclude <- c(paste(c(1:33, #Practice runs
#                               34,35,36,102,101,103,104,132,#file error or missing files
#                               37,40,45,46,47,48,56,65,74,90,111,112,117,161,163,178,187,#E1 performance did not pass quality control
#                               95,121:124,154,156,179,180,#Flies died during experiment or affected by that, or data mishandling such as filename unchanged
#                               72,40,50,54,68,76,80,149,152,183,186,187,191,192, #E1 performance suboptimal -- did not walk the whole length.etc
#                               49,50,51,52,61:64,105:108 #Test periods' file length incorrect
# )))
ind_query = fly_info$Fly >= 401 & (fly_info$experimenter=="ES")
  # & fly_info$Fly. >= 168
  !(fly_info$Fly %in% flies_to_exclude)

query_flies = sort(unique(fly_info$Fly[ind_query]))



path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/ES/CS/CSV" ##Please change here

###E1
query_sessions = c("E1")

pause_duration = NULL
# walking_duration = NULL
##T flies' E1
# for(fly_number in query_flies){
for(i in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",i,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    pause = one_fly_pause_duration(input_files[1])
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    # walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, pause)
  }
}

# x_T = sort(walking_duration)/50 #walking duration in E1 ranked
pause_E1= sort(pause_duration)/50 #pause duration in E1 ranked

####E1T1E1
query_sessions = c("E1T1E1")

pause_duration = NULL
# walking_duration = NULL
##T flies' E1
# for(fly_number in query_flies){
for(i in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",i,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    pause = one_fly_pause_duration(input_files[1])
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    # walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, pause)
  }
}

# x_T = sort(walking_duration)/50 #walking duration in E1 ranked
pause_E1T1E1= sort(pause_duration)/50 #pause duration in E1 ranked

####E1R1E1
query_sessions = c("E1R1E1")

pause_duration = NULL
# walking_duration = NULL
##T flies' E1
# for(fly_number in query_flies){
for(i in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",i,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    pause = one_fly_pause_duration(input_files[1])
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    # walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, pause)
  }
}

# x_T = sort(walking_duration)/50 #walking duration in E1 ranked
pause_E1R1E1= sort(pause_duration)/50 #pause duration in E1 ranked

####E1T1E1T1E1
query_sessions = c("E1T1E1T1E1")

pause_duration = NULL
# walking_duration = NULL
##T flies' E1
# for(fly_number in query_flies){
for(i in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",i,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    pause = one_fly_pause_duration(input_files[1])
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    # walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, pause)
  }
}

# x_T = sort(walking_duration)/50 #walking duration in E1 ranked
pause_E1T1E1T1E1= sort(pause_duration)/50 #pause duration in E1 ranked

#########E1R1E1R1E1
query_sessions = c("E1R1E1R1E1")

pause_duration = NULL
# walking_duration = NULL
##T flies' E1
# for(fly_number in query_flies){
for(i in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",i,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    pause = one_fly_pause_duration(input_files[1])
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    # walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, pause)
  }
}

# x_T = sort(walking_duration)/50 #walking duration in E1 ranked
pause_E1R1E1R1E1= sort(pause_duration)/50 #pause duration in E1 ranked

plot(density(pause_E1),xlim=c(0,20),ylim=c(0,0.6))
lines(density(pause_E1T1E1),col="red")
lines(density(pause_E1R1E1),col="blue")

lines(density(pause_E1T1E1T1E1),col="pink")
lines(density(pause_E1R1E1R1E1),col="brown")


pause_duration = NULL
walking_duration = NULL
##R flies' E1
# for(fly_number in query_flies){
for(fly_number in flies_R){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, wp_dur[[2]])
  }
}

x_R = sort(walking_duration)/50 #walking duration in E1 ranked
y_R = sort(pause_duration)/50 #pause duration in E1 ranked

pause_duration = NULL
walking_duration = NULL
##N flies' E1
# for(fly_number in query_flies){
for(fly_number in flies_N){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration = c(walking_duration, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration = c(pause_duration, wp_dur[[2]])
  }
}

x_N = sort(walking_duration)/50 #walking duration in E1 ranked
y_N = sort(pause_duration)/50 #pause duration in E1 ranked


###################T flies##################
path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1T1E1" ##Please change here
query_sessions = c("E1T1E1")

pause_duration_E1T1E1 = NULL
walking_duration_E1T1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1T1E1 = c(walking_duration_E1T1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1T1E1 = c(pause_duration_E1T1E1, wp_dur[[2]])
  }
}

x1 = sort(walking_duration_E1T1E1)/50 #walking duration in E1T1E1 ranked
y1 = sort(pause_duration_E1T1E1)/50 #pause duration in E1T1E1 ranked

path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1T1E1T1E1" ##Please change here
query_sessions = c("E1T1E1T1E1")

pause_duration_E1T1E1T1E1 = NULL
walking_duration_E1T1E1T1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1T1E1T1E1 = c(walking_duration_E1T1E1T1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1T1E1T1E1 = c(pause_duration_E1T1E1T1E1, wp_dur[[2]])
  }
}

x2 = sort(walking_duration_E1T1E1T1E1)/50 #walking duration in E1T1E1T1E1 ranked
y2 = sort(pause_duration_E1T1E1T1E1)/50 #pause duration in E1T1E1T1E1 ranked



path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1T1E1T1E1T2E1" ##Please change here
query_sessions = c("E1T1E1T1E1T2E1")

pause_duration_E1T1E1T1E1T2E1 = NULL
walking_duration_E1T1E1T1E1T2E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1T1E1T1E1T2E1 = c(walking_duration_E1T1E1T1E1T2E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1T1E1T1E1T2E1 = c(pause_duration_E1T1E1T1E1T2E1, wp_dur[[2]])
  }
}

x3 = sort(walking_duration_E1T1E1T1E1T2E1)/50 #walking duration in E1T1E1T1E1T2E1 ranked
y3 = sort(pause_duration_E1T1E1T1E1T2E1)/50 #pause duration in E1T1E1T1E1T2E1 ranked


################R flies#################
path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1R1E1" ##Please change here
query_sessions = c("E1R1E1")

pause_duration_E1R1E1 = NULL
walking_duration_E1R1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1R1E1 = c(walking_duration_E1R1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1R1E1 = c(pause_duration_E1R1E1, wp_dur[[2]])
  }
}

x11 = sort(walking_duration_E1R1E1)/50 #walking duration in E1R1E1 ranked
y11 = sort(pause_duration_E1R1E1)/50 #pause duration in E1R1E1 ranked


path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1R1E1R1E1" ##Please change here
query_sessions = c("E1R1E1R1E1")

pause_duration_E1R1E1R1E1 = NULL
walking_duration_E1R1E1R1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1R1E1R1E1 = c(walking_duration_E1R1E1R1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1R1E1R1E1 = c(pause_duration_E1R1E1R1E1, wp_dur[[2]])
  }
}

x12 = sort(walking_duration_E1R1E1R1E1)/50 #walking duration in E1R1E1R1E1 ranked
y12 = sort(pause_duration_E1R1E1R1E1)/50 #pause duration in E1R1E1R1E1 ranked


path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1R1E1R1E1R1E1" ##Please change here
query_sessions = c("E1R1E1R1E1R1E1")

pause_duration_E1R1E1R1E1R1E1 = NULL
walking_duration_E1R1E1R1E1R1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1R1E1R1E1R1E1 = c(walking_duration_E1R1E1R1E1R1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1R1E1R1E1R1E1 = c(pause_duration_E1R1E1R1E1R1E1, wp_dur[[2]])
  }
}

x13 = sort(walking_duration_E1R1E1R1E1R1E1)/50 #walking duration in E1R1E1R1E1R1E1 ranked
y13 = sort(pause_duration_E1R1E1R1E1R1E1)/50 #pause duration in E1R1E1R1E1R1E1 ranked

########## N flies #############
path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1N1E1" ##Please change here
query_sessions = c("E1N1E1")

pause_duration_E1N1E1 = NULL
walking_duration_E1N1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1N1E1 = c(walking_duration_E1N1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1N1E1 = c(pause_duration_E1N1E1, wp_dur[[2]])
  }
}

x21 = sort(walking_duration_E1N1E1)/50 #walking duration in E1N1E1 ranked
y21 = sort(pause_duration_E1N1E1)/50 #pause duration in E1N1E1 ranked



path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1N1E1N1E1" ##Please change here
query_sessions = c("E1N1E1N1E1")

pause_duration_E1N1E1N1E1 = NULL
walking_duration_E1N1E1N1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1N1E1N1E1 = c(walking_duration_E1N1E1N1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1N1E1N1E1 = c(pause_duration_E1N1E1N1E1, wp_dur[[2]])
  }
}

x22 = sort(walking_duration_E1N1E1N1E1)/50 #walking duration in E1N1E1N1E1 ranked
y22 = sort(pause_duration_E1N1E1N1E1)/50 #pause duration in E1N1E1N1E1 ranked



path = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/Emily/Markov/E1N1E1N1E1N1E1" ##Please change here
query_sessions = c("E1N1E1N1E1N1E1")

pause_duration_E1N1E1N1E1N1E1 = NULL
walking_duration_E1N1E1N1E1N1E1 = NULL
for(fly_number in query_flies){
  fly_files = dir(path=path,pattern = paste0("Fly",fly_number,"_.*\\.csv$"),
                  full.names = T, ignore.case = T)
  for(query_session in query_sessions){
    input_files = fly_files[grep(paste0("_",query_session,"_"),fly_files)]
    if(length(input_files) > 1){warning(input_files)}
    if(length(input_files) == 0){
      next;
    }
    wp_dur = one_fly_walk_pause_duration(input_files[1],frame_rate)
    # walking_duration = c(walking_duration, wp_dur[[1]]/mean(wp_dur[[1]]))
    walking_duration_E1N1E1N1E1N1E1 = c(walking_duration_E1N1E1N1E1N1E1, wp_dur[[1]])
    # pause_duration = c(pause_duration, wp_dur[[2]]/mean(wp_dur[[2]]))
    pause_duration_E1N1E1N1E1N1E1 = c(pause_duration_E1N1E1N1E1N1E1, wp_dur[[2]])
  }
}

x23 = sort(walking_duration_E1N1E1N1E1N1E1)/50 #walking duration in E1N1E1N1E1N1E1 ranked
y23 = sort(pause_duration_E1N1E1N1E1N1E1)/50 #pause duration in E1N1E1N1E1N1E1 ranked

# jpeg('Semi-log plot of all flies walking distribution in all sessions-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
# 
# jpeg('Semi-log plot of all flies pause distribution in all sessions-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)

############Trained flies##############
# jpeg('Semi-log plot of Trained flies walking distribution in all sessions-1second_10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
# 
# # plot(x[which(x>=1)[[1]]:length(x)],log10(1-(1:length(x[which(x>=1)[[1]]:length(x)]))/length(x[which(x>=1)[[1]]:length(x)])),type='l',xlim=c(0,20),lwd=1,col='red',
# #      main="Semi-log plot of Trained flies walking distribution in all sessions-1second and above",
# #      xlab = "Duration (sec)",
# #      ylab = "Log10(1-cumulative probability)"
# # )
# 
# plot(x_T[which(x_T>=1)[[1]]:length(x_T)],log10(1-(1:length(x_T[which(x_T>=1)[[1]]:length(x_T)]))/length(x_T[which(x_T>=1)[[1]]:length(x_T)])),type='l',xlim=c(0,10),lwd=1,col='red',
#      main="Semi-log plot of Trained flies walking distribution in all sessions-1second and above",
#      xlab = "Duration (sec)",
#      ylab = "Log10(1-cumulative probability)"
# )
# 
# lines(x1[which(x1>=1)[[1]]:length(x1)],log10(1-(1:length(x1[which(x1>=1)[[1]]:length(x1)]))/length(x1[which(x1>=1)[[1]]:length(x1)])),lwd=2,col='red')
# lines(x2[which(x2>=1)[[1]]:length(x2)],log10(1-(1:length(x2[which(x2>=1)[[1]]:length(x2)]))/length(x2[which(x2>=1)[[1]]:length(x2)])),lwd=3,col='red')
# lines(x3[which(x3>=1)[[1]]:length(x3)],log10(1-(1:length(x3[which(x3>=1)[[1]]:length(x3)]))/length(x3[which(x3>=1)[[1]]:length(x3)])),lwd=4,col='red')
# 
# jpeg('Semi-log plot of Trained flies pause distribution in all sessions-1second and above_up_to_10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
# plot(y_T[which(y_T>=1)[[1]]:length(y_T)],log10(1-(1:length(y_T[which(y_T>=1)[[1]]:length(y_T)]))/length(y_T[which(y_T>=1)[[1]]:length(y_T)])),xlim=c(0,10),type='l',lwd=1,col='red',
#      main="Semi-log plot of Trained flies pause distribution in all sessions-1second and above",
#      xlab = "Duration (sec)",
#      ylab = "Log10(1-cumulative probability)"
#      )
# lines(y1[which(y1>=1)[[1]]:length(y1)],log10(1-(1:length(y1[which(y1>=1)[[1]]:length(y1)]))/length(y1[which(y1>=1)[[1]]:length(y1)])),lwd=2,col='red')
# lines(y2[which(y2>=1)[[1]]:length(y2)],log10(1-(1:length(y2[which(y2>=1)[[1]]:length(y2)]))/length(y2[which(y2>=1)[[1]]:length(y2)])),lwd=3,col='red')
# lines(y3[which(y3>=1)[[1]]:length(y3)],log10(1-(1:length(y3[which(y3>=1)[[1]]:length(y3)]))/length(y3[which(y3>=1)[[1]]:length(y3)])),lwd=4,col='red')

###Trained flies no truncation###
# plot.ecdf(x_T,col=colors()[30],
#      xlim=c(0,20),
#      main="Semi-log plot of Trained flies walking distribution in all sessions-1second and above",
#      xlab = "Duration (sec)",
#      ylab = "Cumulative Probability"
# )
# 
# lines(ecdf(x1),col=colors()[36])
# lines(ecdf(x2),col=colors()[50])
# lines(ecdf(x3),col=colors()[150])
# 
# jpeg('Semi-log plot of Trained flies pause distribution in all sessions-1second and above_up_to_10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
# plot.ecdf(y_T, col=colors()[30],
#      main="Semi-log plot of Trained flies pause distribution in all sessions-1second and above",
#      xlab = "Duration (sec)",
#      ylab = "Log10(1-cumulative probability)"
# )
# lines(ecdf(y1),col=colors()[36])
# lines(ecdf(y2),col=colors()[50])
# lines(ecdf(y3),col=colors()[150])
# 
# yy_T = (1:length(y_T))/(length(y_T)+0.01)
# yyy_T= log10(1-(1:length(y_T))/length(y_T))
# 
# formula <- yy_T ~ (1-b*exp(-c*(y_T-a))-(1-b)*exp(-d*(y_T-a)))
# nlsfit = nls(formula,data.frame(yy_T,y_T),start = list(a=0, b=0.95, c=0.99,d=0.01),
#                      #lower = list(a=0,b=0,c=1e-3,d=1e-3),upper = list(a=1,b=1,c=10,d=10),
#                      control = nls.control(maxiter = 1000, tol = 1e-06, minFactor = 1/4096, warnOnly=F))
# lines(y_T,predict(nlsfit,y_T),col="red",lwd=2)

#############Trained fly density plot#############
jpeg(paste0('Density plot of Trained flies walking duration.jpeg', width=1280, height=800,units="px",pointsize=24, quality=100)
plot(density(x_T),col='red',lwd=1,
     xlim=c(0,20),
     ylim=c(0,0.6),
     main = "Density plot of Trained flies walking duration-Threshold: 20 frame",
     xlab="Duration (sec)",
     ylab="Density"
     )

lines(density(x1),col='red',lwd=2)
lines(density(x2),col='red',lwd=3)
lines(density(x3),col='red',lwd=4)

dev.off()
graphics.off()

density(x_T)$x[which.max(density(x_T)$y)]
density(x1)$x[which.max(density(x1)$y)]

jpeg('Density plot of Trained flies pause duration.jpeg', width=1280, height=800,units="px",pointsize=24, quality=100)
plot(density(y_T),col='red',lwd=1,
     xlim=c(0,30),
     ylim=c(0,0.9),
     main = "Density plot of Trained flies pause duration-Threshold: 20 frame",
     xlab="Duration (sec)",
     ylab="Density"
)

lines(density(y1),col='red',lwd=2)
lines(density(y2),col='red',lwd=3)
lines(density(y3),col='red',lwd=4)

dev.off()
graphics.off()


###Random flies###
jpeg('Semi-log plot of Random flies walking distribution in all sessions-1second and above-10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)

plot(x_R[which(x_R>=1)[[1]]:length(x_R)],log10(1-(1:length(x_R[which(x_R>=1)[[1]]:length(x_R)]))/length(x_R[which(x_R>=1)[[1]]:length(x_R)])),type='l',xlim=c(0,10),lwd=1,col='blue',
     main="Semi-log plot of Random flies walking distribution in all sessions-1second and above",
     xlab = "Duration (sec)",
     ylab = "Log10(1-cumulative probability)"
)

lines(x11[which(x11>=1)[[1]]:length(x11)],log10(1-(1:length(x11[which(x11>=1)[[1]]:length(x11)]))/length(x11[which(x11>=1)[[1]]:length(x11)])),lwd=2,col='blue')
lines(x12[which(x12>=1)[[1]]:length(x12)],log10(1-(1:length(x12[which(x12>=1)[[1]]:length(x12)]))/length(x12[which(x12>=1)[[1]]:length(x12)])),lwd=3,col='blue')
lines(x13[which(x13>=1)[[1]]:length(x13)],log10(1-(1:length(x13[which(x13>=1)[[1]]:length(x13)]))/length(x13[which(x13>=1)[[1]]:length(x13)])),lwd=4,col='blue')

jpeg('Semi-log plot of Random flies pause distribution in all sessions-1second and above-10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y_R[which(y_R>=1)[[1]]:length(y_R)],log10(1-(1:length(y_R[which(y_R>=1)[[1]]:length(y_R)]))/length(y_R[which(y_R>=1)[[1]]:length(y_R)])),xlim=c(0,10),type='l',lwd=1,col='blue',
     main="Semi-log plot of Random flies pause distribution in all sessions-1second and above",
     xlab = "Duration (sec)",
     ylab = "Log10(1-cumulative probability)"
)
lines(y11[which(y11>=1)[[1]]:length(y11)],log10(1-(1:length(y11[which(y11>=1)[[1]]:length(y11)]))/length(y11[which(y11>=1)[[1]]:length(y11)])),lwd=2,col='blue')
lines(y12[which(y12>=1)[[1]]:length(y12)],log10(1-(1:length(y12[which(y12>=1)[[1]]:length(y12)]))/length(y12[which(y12>=1)[[1]]:length(y12)])),lwd=3,col='blue')
lines(y13[which(y13>=1)[[1]]:length(y13)],log10(1-(1:length(y13[which(y13>=1)[[1]]:length(y13)]))/length(y13[which(y13>=1)[[1]]:length(y13)])),lwd=4,col='blue')


#############Random fly density plot#############
jpeg('Density plot of Random flies walking duration.jpeg', width=1280, height=800,units="px",pointsize=24, quality=100)
plot(density(x_R),col='blue',lwd=1,
     xlim=c(0,20),
     ylim=c(0,0.6),
     main = "Density plot of Random flies walking duration-Threshold: 20 frame",
     xlab="Duration (sec)",
     ylab="Density"
)

lines(density(x11),col='blue',lwd=2)
lines(density(x12),col='blue',lwd=3)
lines(density(x13),col='blue',lwd=4)

dev.off()
graphics.off()

jpeg('Density plot of Random flies pause duration.jpeg', width=1280, height=800,units="px",pointsize=24, quality=100)
plot(density(y_R),col='blue',lwd=1,
     xlim=c(0,30),
     ylim=c(0,0.9),
     main = "Density plot of Random flies pause duration-Threshold: 20 frame",
     xlab="Duration (sec)",
     ylab="Density"
)

lines(density(y11),col='blue',lwd=2)
lines(density(y12),col='blue',lwd=3)
lines(density(y13),col='blue',lwd=4)

dev.off()
graphics.off()




###Blank Flies###
jpeg('Semi-log plot of Blank flies walking distribution in all sessions-1second and above-10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)

plot(x_N[which(x_N>=1)[[1]]:length(x_N)],log10(1-(1:length(x_N[which(x_N>=1)[[1]]:length(x_N)]))/length(x_N[which(x_N>=1)[[1]]:length(x_N)])),type='l',xlim=c(0,10),lwd=1,col='black',
     main="Semi-log plot of Blank flies walking distribution in all sessions-1second and above",
     xlab = "Duration (sec)",
     ylab = "Log10(1-cumulative probability)"
)

lines(x21[which(x21>=1)[[1]]:length(x21)],log10(1-(1:length(x21[which(x21>=1)[[1]]:length(x21)]))/length(x21[which(x21>=1)[[1]]:length(x21)])),lwd=2,col='black')
lines(x22[which(x22>=1)[[1]]:length(x22)],log10(1-(1:length(x22[which(x22>=1)[[1]]:length(x22)]))/length(x22[which(x22>=1)[[1]]:length(x22)])),lwd=3,col='black')
lines(x23[which(x23>=1)[[1]]:length(x23)],log10(1-(1:length(x23[which(x23>=1)[[1]]:length(x23)]))/length(x23[which(x23>=1)[[1]]:length(x23)])),lwd=4,col='black')

jpeg('Semi-log plot of Blank flies pause distribution in all sessions-1second and above_10sec.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y_N[which(y_N>=1)[[1]]:length(y_N)],log10(1-(1:length(y_N[which(y_N>=1)[[1]]:length(y_N)]))/length(y_N[which(y_N>=1)[[1]]:length(y_N)])),xlim=c(0,10),type='l',lwd=1,col='black',
     main="Semi-log plot of Blank flies pause distribution in all sessions-1second and above",
     xlab = "Duration (sec)",
     ylab = "Log10(1-cumulative probability)"
)
lines(y21[which(y21>=1)[[1]]:length(y21)],log10(1-(1:length(y21[which(y21>=1)[[1]]:length(y21)]))/length(y21[which(y21>=1)[[1]]:length(y21)])),lwd=2,col='black')
lines(y22[which(y22>=1)[[1]]:length(y22)],log10(1-(1:length(y22[which(y22>=1)[[1]]:length(y22)]))/length(y22[which(y22>=1)[[1]]:length(y22)])),lwd=3,col='black')
lines(y23[which(y23>=1)[[1]]:length(y23)],log10(1-(1:length(y23[which(y23>=1)[[1]]:length(y23)]))/length(y23[which(y23>=1)[[1]]:length(y23)])),lwd=4,col='black')



#############Blank fly density plot#############
jpeg('Density plot of Blank flies walking duration.jpeg', width=1280, height=800,units="px",pointsize=24, quality=100)
plot(density(x_N),col='black',lwd=1,
     xlim=c(0,20),
     ylim=c(0,0.6),
     main = "Density plot of Blank flies walking duration-Threshold: 20 frame",
     xlab="Duration (sec)",
     ylab="Density"
)

lines(density(x21),col='black',lwd=2)
lines(density(x22),col='black',lwd=3)
lines(density(x23),col='black',lwd=4)

dev.off()
graphics.off()

jpeg('Density plot of Blank flies pause duration.jpeg', width=1280, height=800,units="px",pointsize=24, quality=100)
plot(density(y_N),col='black',lwd=1,
     xlim=c(0,30),
     ylim=c(0,0.9),
     main = "Density plot of Blank flies pause duration-Threshold: 20 frame",
     xlab="Duration (sec)",
     ylab="Density"
)

lines(density(y21),col='black',lwd=2)
lines(density(y22),col='black',lwd=3)
lines(density(y23),col='black',lwd=4)

dev.off()
graphics.off()







# 
# plot(y[1739:length(y)],log10(1-(1:length(y[1739:length(y)]))/length(y[1739:length(y)])),type='l',
#      main="semi-log plot of all flies pre test pause distribution",
#      xlab = "normalized pause duration",
#      ylab = "Log10(1-cumulative probability)"
# )

############Normalized Way####################
##Threshold: 1second
# E1:x[2315]*mean(wp_dur[[1]])/50 ~1s
jpeg('Semi-log plot of all flies pre test walking distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[2315:length(x)],log10(1-(1:length(x[2315:length(x)]))/length(x[2315:length(x)])),#type='l',
     main="Semi-log plot of all flies pre test walking distribution-1second and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()
##Threshold: 0.1 second
# E1:x[16]*mean(wp_dur[[1]])/50 ~100ms
jpeg('Semi-log plot of all flies pre test walking distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[16:length(x)],log10(1-(1:length(x[16:length(x)]))/length(x[16:length(x)])),#type='l',
     main="Semi-log plot of all flies pre test walking distribution-100ms and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()

##Threshold: 1second
# E1T1E1:x[549]*mean(wp_dur[[1]])/50 ~1s
jpeg('Semi-log plot of trained flies 1st test walking distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[549:length(x)],log10(1-(1:length(x[549:length(x)]))/length(x[549:length(x)])),#type='l',
     main="Semi-log plot of trained flies 1st test walking distribution-1second and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 0.1 second
# E1T1E1:x[74]*mean(wp_dur[[1]])/50 ~100ms
jpeg('Semi-log plot of trained flies 1st test walking distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[73:length(x)],log10(1-(1:length(x[73:length(x)]))/length(x[73:length(x)])),#type='l',
     main="Semi-log plot of trained flies 1st test walking distribution-100ms and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 1second
# E1T1E1T1E1:x[553]*mean(wp_dur[[1]])/50 ~1s
jpeg('Semi-log plot of trained flies 2nd test walking distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[553:length(x)],log10(1-(1:length(x[553:length(x)]))/length(x[553:length(x)])),#type='l',
     main="Semi-log plot of trained flies 1st test walking distribution-1second and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 0.1 second
# E1T1E1T1E1:x[185]*mean(wp_dur[[1]])/50 ~100ms
jpeg('Semi-log plot of trained flies 2nd test walking distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[185:length(x)],log10(1-(1:length(x[185:length(x)]))/length(x[185:length(x)])),#type='l',
     main="Semi-log plot of trained flies 2nd test walking distribution-100ms and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 1second
# E1T1E1T1E1T2E1:x[479]*mean(wp_dur[[1]])/50 ~1s
jpeg('Semi-log plot of trained flies reverse test walking distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[479:length(x)],log10(1-(1:length(x[479:length(x)]))/length(x[479:length(x)])),#type='l',
     main="Semi-log plot of trained flies reverse test walking distribution-1second and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 0.1 second
# E1T1E1T1E1T2E1:x[25]*mean(wp_dur[[1]])/50 ~100ms
jpeg('Semi-log plot of trained flies reverse test walking distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[25:length(x)],log10(1-(1:length(x[25:length(x)]))/length(x[25:length(x)])),#type='l',
     main="Semi-log plot of trained flies reverse test walking distribution-100ms and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 1second
# E1R1E1R1E1R1E1:x[423]*mean(wp_dur[[1]])/50 ~1s
jpeg('Semi-log plot of random flies reverse test walking distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[423:length(x)],log10(1-(1:length(x[423:length(x)]))/length(x[423:length(x)])),#type='l',
     main="Semi-log plot of random flies reverse test walking distribution-1second and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


##Threshold: 0.1 second
# E1R1E1R1E1R1E1:x[1]*mean(wp_dur[[1]])/50 ~100ms
jpeg('Semi-log plot of random flies reverse test walking distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[1:length(x)],log10(1-(1:length(x[1:length(x)]))/length(x[1:length(x)])),#type='l',
     main="Semi-log plot of random flies reverse test walking distribution-100ms and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()



##Threshold: 1second
# E1R1E1R1E1:x[670]*mean(wp_dur[[1]])/50 ~1s
jpeg('Semi-log plot of random flies 2nd test walking distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(x[670:length(x)],log10(1-(1:length(x[670:length(x)]))/length(x[670:length(x)])),#type='l',
     main="Semi-log plot of random flies 2nd test walking distribution-1second and above",
     xlab = "normalized walking duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


# ##Threshold: 0.1 second
# # E1R1E1R1E1R1E1:x[1]*mean(wp_dur[[1]])/50 ~100ms
# jpeg('Semi-log plot of random flies reverse test walking distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
# plot(x[1:length(x)],log10(1-(1:length(x[1:length(x)]))/length(x[1:length(x)])),#type='l',
#      main="Semi-log plot of random flies reverse test walking distribution-100ms and above",
#      xlab = "normalized walking duration",
#      ylab = "Log10(1-cumulative probability)"
# )
# dev.off()
# graphics.off()
################################################################Pauses#########################################################


y = sort(pause_duration)/50

#The calculating process of pause requires the pause to reach certain threshold, which makes y[1] starts at 0.38s.
#E1: y[2355]*mean(wp_dur[[2]])/50 ~1s
# pdf("semi-log plot of trained flies 2nd test pause distribution.pdf")
jpeg('Semi-log plot of all flies pre test pause distribution-1second and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y[2355:length(y)],log10(1-(1:length(y[2355:length(y)]))/length(y[2355:length(y)])),type='l',
     main="semi-log plot of all flies pre test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()

#E1T1E1: y[1]*mean(wp_dur[[2]])/50 ~4.3s
jpeg('semi-log plot of trained flies 1st test pause distribution.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y,log10(1-(1:length(y))/length(y)),type='l',
     main="semi-log plot of trained flies 1st test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


#E1T1E1T1E1: y[358]*mean(wp_dur[[2]])/50 ~1s
jpeg('semi-log plot of trained flies 2nd test pause distribution.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y[358:length(y)],log10(1-(1:length(y[358:length(y)]))/length(y[358:length(y)])),type='l',
     main="semi-log plot of trained flies 2nd test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


#E1T1E1T1E1: y[9]*mean(wp_dur[[2]])/50 ~0.1s
jpeg('semi-log plot of trained flies 2nd test pause distribution-100ms and above.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y[9:length(y)],log10(1-(1:length(y[9:length(y)]))/length(y[9:length(y)])),type='l',
     main="semi-log plot of trained flies 2nd test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


#E1T1E1T1E1T2E1: y[25]*mean(wp_dur[[2]])/50 ~1s
jpeg('semi-log plot of trained flies reverse test pause distribution.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y[25:length(y)],log10(1-(1:length(y[25:length(y)]))/length(y[25:length(y)])),type='l',
     main="semi-log plot of trained flies reverse test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()


#E1R1E1R1E1R1E1: y[207]*mean(wp_dur[[2]])/50 ~1s
jpeg('semi-log plot of random flies reverse test pause distribution.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y[207:length(y)],log10(1-(1:length(y[207:length(y)]))/length(y[207:length(y)])),type='l',
     main="semi-log plot of random flies reverse test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()

#E1R1E1R1E1: y[494]*mean(wp_dur[[2]])/50 ~1s
jpeg('semi-log plot of random flies 2nd test pause distribution.jpeg',width=1280, height=800, units="px", pointsize =24, quality = 100)
plot(y[494:length(y)],log10(1-(1:length(y[494:length(y)]))/length(y[494:length(y)])),type='l',
     main="semi-log plot of random flies 2nd test pause distribution",
     xlab = "normalized pause duration",
     ylab = "Log10(1-cumulative probability)"
)
dev.off()
graphics.off()



# 
# pdf("E1 cumulative walking and pause distribution.pdf")
# x = sort(walking_duration) / 19
# plot(x,(1:length(x))/length(x),type='l',
#      main="walking duration cumulative distribution",
#      xlab = "Walking duration (sec)",
#      ylab = "Cumulative probability",
#      xlim = c(0,400)
#      )
# # plot(x,log10((1:length(x))/length(x)),type='l',
# #      main="walking duration cumulative distribution",
# #      xlab = "Walking duration (sec)",
# #      ylab = "Log10 cumulative probability"
# #      )
# 
# y = sort(pause_duration) / 19
# plot(y,(1:length(y))/length(y),type='l',
#      main="Pause duration cumulative distribution",
#      xlab = "Pause duration (sec)",
#      ylab = "Cumulative probability",
#      xlim = c(0,600)
# )
# # plot(y,log10((1:length(y))/length(y)),type='l',
# #      main="Pause duration cumulative distribution",
# #      xlab = "Pause duration (sec)",
# #      ylab = "Log10 cumulative probability"
# # )
# dev.off()
# graphics.off()


# normalized_walking_duration=c()
# 
# for (i in 1:length(walking_duration)){
# 
# normalized_walking_duration=c(normalized_walking_duration, walking_duration[i]/mean(walking_duration))
# }
# 
# normalized_pause_duration=c()
# 
# for (i in 1:length(pause_duration)){
#   
#   normalized_pause_duration=c(normalized_pause_duration, pause_duration[i]/mean(pause_duration))
# }


# pdf("Semi-log plot of trained flies 2nd test walking distribution.pdf")