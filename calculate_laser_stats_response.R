#!/usr/bin/env Rscript


get_laser_stats <- function(x,
                            framerate = 50
){
  
  
  data_start = 20 #changed it to 20 from 10 on Oct 5, 2016
  fly_pos = x[data_start:min(600*framerate,length(x))]
  experiment_time = length(fly_pos)        
  
  
  ##Get the transient speed
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
  ## A pause is a time span with greater than 10 continous frames at speed 0
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
}


sessions <- c(  "E1",
                "E1X",
                "E1XE1"
)
sessions = unique(sessions)
query.sessions = sessions


## read fly info
## Fly info
fly.info = unique(read.csv("data/fly_info_Response.csv",header=T,stringsAsFactors=F))

## browsing files
fly.info.out = NULL
fly.info.framerate = NULL
for(ind in 1:nrow(fly.info)){
  gt = "Response"
  query.sessions = gsub("X",fly.info$Category[ind],sessions)
  for(session in query.sessions){
    input.file <- list.files(path = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",
                                           fly.info$experimenter[ind],
                                           "/",gt,"/CSV/"),
                             
                             pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                              "_",session,"_",
                                              fly.info$Gender[ind],"_.*"),
                             
                             full.names=T
    )
    
    if(session == "E1" & length(input.file) == 0){
      cat("M",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")
      break
    }
    
    if(length(input.file) == 0){next}
    
    framerate = 50
    fly.pos <- read.csv(input.file,stringsAsFactors = F,nrow=2)
    if(ncol(fly.pos) == 1){## No laser data
      #cat("NA",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")
      next
    }
    cat("P",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")

    fly.laser <- read.csv(input.file,stringsAsFactors = F)[,2]
    time.laser <- sum(fly.laser>0)
    perc.time.laser <- sum(fly.laser>0)/length(fly.laser)
    num.laser.shot <- sum(rle(fly.laser)$values > 0)
    
    fly.info.out = rbind(fly.info.out,
                         t(c(fly.info[ind,],
                             session,
                             time.laser/framerate,
                             perc.time.laser,
                             num.laser.shot))
    )
  }
}
colnames(fly.info.out)[c(1:4)+ncol(fly.info)] = c("session","time.laser",
                                 "perc.time.laser","num.laser.shot")
write.table(fly.info.out,
            "data/fly_laser_info_response.csv",
            row.names=F,quote=F,sep=",")

  