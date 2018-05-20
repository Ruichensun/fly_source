setwd("C:/Users/Ruichen/Desktop/data/csvdata")

##Import the function for obtaining statistics of each experiment
source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
library(Hmisc)
library(ggplot2)

one_fly_vel_and_acce_histogram <- function(fly_number,framerate){

pdf(paste0("Fly_",fly_number, "_traces_speed_acceleration.pdf"))
  test_file_number = c();
  test_files = c();
  fly_files = dir(pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
  test_file_number = grep(pattern=("E1_|E2_|E3_"),fly_files)
  for (i in 1:length(test_file_number)){
    test_files[i] <- fly_files[as.numeric(test_file_number[i])]
  }
  
  for (i in 1:length(test_files)){
    #plot(1,1):raw traces
    pos_fly_position_info = get_fly_position(test_files[i])
    t = (1:length(pos_fly_position_info))/framerate
    for (k in 3:(length(t)-2)){
      if(!is.na(pos_fly_position_info[k])){
        if(sum(is.na(pos_fly_position_info[c(k-2,k-1,k+1,k+2)]))==4){
          pos_fly_position_info[k] = NaN
        }
       }
     }
    for (k in 2:length(t)){
      if (is.nan(pos_fly_position_info[k])){
        pos_fly_position_info[k] <- pos_fly_position_info[k-1];
        }
    }
    par (mfrow = c(2,2))
    output_title_0 = gsub("ProcessedData_","",basename(test_files[i])) 
    output_title_1 = gsub(".csv","",output_title_0)
    plot(t,pos_fly_position_info,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")        
    lines(t,pos_fly_position_info,col="blue")
    
    #plot(1,2):smoothened traces
    pos_fly_position_info_s = get_fly_position_smoothened(test_files[i])
    t = (1:length(pos_fly_position_info_s))/framerate
    
    plot(t,pos_fly_position_info_s,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")        
    lines(t,pos_fly_position_info_s,col="blue")
    
    #plot(2,1):transient speed
    #plot(2,2):transient acceleration
    
    x = read.table(test_files[i],header=T,sep=",",stringsAsFactors=F)
    
    ##Remove the initial XXX points and the last XXX points
    data_start_fly_position = which(x[,1]!=0)[1]
    data_start_center_of_mass = which(!is.nan(x[,2]))
    
    ##Get the transient speed
    vel = as.numeric(diff(c(x[data_start_fly_position-1,1],pos_fly_position_info_s)))
    
    acceleration = as.numeric(diff(c(x[data_start_fly_position-2,1],vel)))
    hist(vel, breaks = 200, main = 'histogram of speed,breaks=200',xlim=c(-50,50))
    hist(acceleration, breaks = 200, main = 'histogram of acceleration, breaks =200', xlim=c(-50,50))
  }  
    
dev.off()
graphics.off()
}
