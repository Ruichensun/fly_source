setwd("C:/Users/Ruichen/Desktop/data/csvdata")  ##Import the function for obtaining statistics of each experiment


source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
source("normalization_against_E1.R")
source("fly_group_stats.R")
library(Hmisc)
library(ggplot2)

framerate = 19

#plot center of mass traces
plot_fly_test_raw_traces <- function (fly_number1,fly_number2){

pdf(paste0("Fly_",fly_number1,"-",fly_number2, "_raw_test_traces.pdf"))

test_file_number = c()
test_files = c()
  
#plot original traces
  for (j in fly_number1:fly_number2){
    fly_files = dir(pattern = paste0("Fly",j,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE);
    if (length(fly_files)!=0){
      test_file_number = grep(pattern=("E1_|E2_|E3_"),fly_files)
      for (i in 1:length(test_file_number)){
        test_files[i] <- fly_files[as.numeric(test_file_number[i])]
      }
      for (i in 1:length(test_files)){
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
        output_title_0 = gsub("ProcessedData_","",basename(test_files[i])) 
        output_title_1 = gsub(".csv","",output_title_0)
        plot(t,pos_fly_position_info,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")        
        lines(t,pos_fly_position_info,col="blue")
      }
    }
  }
dev.off()
graphics.off()
}

#plot smoothened traces
plot_fly_smoothened_test_traces <- function(fly_number1,fly_number2){

  test_file_number = c()
  test_files = c()

#pdf(paste0("Fly_",fly_number1,"-",fly_number2, "_smoothened_test_traces.pdf"))
    for (j in fly_number1:fly_number2){
     fly_files = dir(pattern = paste0("Fly",j,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE);
     if (length(fly_files)!=0){
       test_file_number = grep(pattern=("E1_|E2_|E3_"),fly_files)
       for (i in 1:length(test_file_number)){
         test_files[[j]][i] <- fly_files[as.numeric(test_file_number[i])]
       }
       
       for (i in 1:length(test_files[[j]])){  
         pos_fly_position_info_s = get_fly_position_smoothened(test_files[[j]][i])
         t = (1:length(pos_fly_position_info_s))/framerate
         output_title_0 = gsub("ProcessedData_","",basename(test_files[[j]][i])) 
         output_title_1 = gsub(".csv","",output_title_0)
         print(output_title_1)
         plot(t,pos_fly_position_info_s,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")        
         lines(t,pos_fly_position_info_s,col="blue")
       }
     }
   }

#dev.off()
graphics.off()
}
