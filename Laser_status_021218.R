##Demo for testing the 

setwd("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/Mutants/CSV")

fly.file = read.csv("ProcessedData_Fly790_E1T1_F_2018Feb7.csv",header=T,stringsAsFactors=F)

xx = as.numeric(fly.file[[1]])
yy = as.numeric(fly.file[[2]])

data_start = 20
fly_pos = xx[data_start:length(xx)]
fly_laser = yy[data_start:length(yy)]

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
laser_df = data.frame (which(label_for_laser==1),which(label_for_laser==2),laser_ON,laser_OFF[2:length(laser_OFF)],laser_OFF[2:length(laser_OFF)]-laser_ON)
colnames(laser_df) = c("Laser_On","Laser_Off","ON_duration","OFF_duration","Difference")



fly.file2 = read.csv("ProcessedData_Fly790_E1T1E1T1_F_2018Feb7.csv",header=T,stringsAsFactors=F)

xx2 = as.numeric(fly.file2[[1]])
yy2 = as.numeric(fly.file2[[2]])

data_start2 = 20
fly_pos2 = xx2[data_start2:length(xx)]
fly_laser2 = yy2[data_start2:length(yy)]

for (i in 1:length(fly_laser2)){
  if (fly_laser2[i]>0){
    fly_laser2[i]=1
  }
}

laser_ON2  = rle(fly_laser2)$length[rle(fly_laser2)$values==1]
laser_OFF2 = rle(fly_laser2)$length[rle(fly_laser2)$values==0]

label_for_laser2= rep(0, length(fly_laser2))
for (i in 1:(length(label_for_laser2)-1)){
  if ((fly_laser2[i]==0)&(fly_laser2[i+1]>0)){
    label_for_laser2[i+1] = 1
  }
  if ((fly_laser2[i]>0)&(fly_laser2[i+1]==0)){
    label_for_laser2[i+1] = 2
  }
}

laser_df2 = data.frame()
laser_df2 = data.frame (which(label_for_laser2==1),which(label_for_laser2==2),laser_ON2,laser_OFF2[2:length(laser_OFF2)],laser_OFF2[2:length(laser_OFF2)]-laser_ON2)
colnames(laser_df2) = c("Laser_On","Laser_Off","ON_duration","OFF_duration","Difference")


plot(laser_df2$OFF_duration, type = 's', col = 'black', ylim=c(0,3000))
lines(laser_df2$ON_duration,col = 'red', type='s')

framerate=50
difference = c(laser_df$Difference/framerate,laser_df2$Difference/framerate)



