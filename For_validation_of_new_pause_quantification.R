### For validating the new quantification of pause behavior

##Initial values needed
framerate = 50
speed_max_thres = 20 
speed_zero_thres = 1e-2 
pause_frame_thres = 25 
chamber_end_thres = 50

input_file = "data/ES/CS/ProcessedData_Fly519_E1_WT.csv"
x = read.table(input_file,header=T,sep=",",stringsAsFactors=F)  

x = as.numeric(x[[1]])

data_start = 20 #changed it to 20 from 10 on Oct 5, 2016
fly_pos = x[data_start:length(x)]

if (length(fly_pos)>= 600*framerate){
  fly_pos = fly_pos[1:(600*framerate)]
}else{
  fly_pos = fly_pos
}

experiment_time = length(fly_pos)        

if(data_start > 1){
  fly_speed = diff(c(x[data_start - 1],fly_pos))
} else {
  fly_speed = diff(c(NA,fly_pos))
}


for (i in 1:experiment_time){
  if (abs(fly_speed[i])>=speed_max_thres){
    fly_speed[i]=0
  }
}

## Get the time spans when fly paused
## A pause is a time span with greater than 25 continous frames at speed 0
pause_start = NULL
pause_end = NULL
potential_pause_start = 1
current_zero_length = 0
is_pause = rep(0,experiment_time)
if(fly_speed[1] == 0){
  current_zero_length = 1
}
for(t in 2:experiment_time){
  if(fly_speed[t] == 0){
    if(current_zero_length == 0){
      ## Start new counting
      potential_pause_start = t
      current_zero_length = current_zero_length + 1
    }else{
      ## Continue counting
      current_zero_length = current_zero_length + 1
    }
  }else{
    if(current_zero_length >= pause_frame_thres){
      ## Counting is long enough to be a pause
      pause_start = c(pause_start, potential_pause_start)
      pause_end = c(pause_end,t-1)
      is_pause[potential_pause_start:(t-1)] = 1
    }
    ## Reset counting
    current_zero_length = 0
  }
}
## The speed at last time point is zero otherwise current_zero_length will 0
if(current_zero_length >= pause_frame_thres){
  ## Record the time span as a pause
  pause_start = c(pause_start,potential_pause_start)
  pause_end = c(pause_end,experiment_time)
  is_pause[potential_pause_start:experiment_time] = 1
}
num_pause = length(pause_start)

###Pause not at the end (May 29, 2017)###(Two criteria:[31,754] or [51,716])
##Currently using [51,716]
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

is_pause_plot<-is_pause
for(i in 1:experiment_time){
  if (is_pause[i]==1){
    is_pause_plot[i]=fly_pos[i]
  }
  else{is_pause_plot[i]=-100}
}

is_pause_middle_plot<-is_pause_middle
for(i in 1:experiment_time){
  if (is_pause_middle[i]==1){
    is_pause_middle_plot[i]=fly_pos[i]
  }
  else{is_pause_middle_plot[i]=-100}
}


pause_middle_dur<-rle(is_pause_middle)$length[rle(is_pause_middle)$values==1]
avg_pause_middle_dur<-(mean(pause_middle_dur))/framerate
frac_pause_middle<-(sum(pause_middle_dur))/experiment_time
max_pause_middle<-(max(pause_middle_dur))/framerate
first_pause_middle<-(pause_middle_dur[1])/framerate

pause_middle_dur <- subset(pause_df,(Pause_Duration>=25)&(Start_Position>=50)&(Start_Position<=717))$Pause_Duration
avg_pause_middle_dur<-(mean(pause_middle_dur))/framerate
frac_pause_middle<-(sum(pause_middle_dur))/experiment_time
max_pause_middle<-(max(pause_middle_dur))/framerate
first_pause_middle<-(pause_middle_dur[1])/framerate