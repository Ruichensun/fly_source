

fly.info = read.csv("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_CS.csv",header=T,stringsAsFactors=F)

session = "E1R1E1R1E1" #Subject to change

ind = 300 #Subject to change

input.file <- list.files(path = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",
                                       fly.info$experimenter[ind],
                                       "/CS/CSV/"),
                         
                         pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                          "_",session,"_",
                                          fly.info$Gender[ind],"_.*"),
                         
                         full.names=T
)

input_file=input.file #change names

framerate = 50 #this can be 10 or 50. Need to watch out for it.


speed_max_thres = 20
speed_zero_thres = 1e-2 
pause_frame_thres = 25
chamber_end_thres = 50


## Read input file
  tryCatch({
    x = read.table(input_file,header=T,sep=",",stringsAsFactors=F)        
  },error = function(e){
    stop(paste0("Input file is empty!:\n",
                "  Input files is: ", input_file,"\n\n")
    )}
  )
  
  if(nrow(x) < 10){
    stop(paste0("Input file is empty!:\n",
                "  Input files is: ", input_file,"\n\n")
    )
  }
  
  ##Remove the initial XXX points
  x = as.numeric(x[[1]])
  #x = as.numeric(x)
  data_start = 20 #changed it to 20 from 10 on Oct 5, 2016
  fly_pos = x[data_start:length(x)]
  
  if (length(fly_pos)>= 600*framerate){
    fly_pos = fly_pos[1:(600*framerate)]
  }else{
    fly_pos = fly_pos
  }
  
  experiment_time = length(fly_pos)        
  
  set_time=30000
  
  ##Get the transient speed
  if(data_start > 1){
    fly_speed = diff(c(x[data_start - 1],fly_pos))
  } else {
    fly_speed = diff(c(NA,fly_pos))
  }
  
  ## Thresholding the max speed
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
  ## The speed at last time point is zero otherwise current_zero_length will 0
  if(current_zero_length >= pause_frame_thres){
    ## Record the time span as a pause
    pause_start = c(pause_start,potential_pause_start)
    pause_end = c(pause_end,experiment_time)
    is_pause[potential_pause_start:experiment_time] = 1
  }
  num_pause = length(pause_start)
  
  ###Pause not at the end (May 29, 2017)###(Two criteria:[31,754] or [51,716])
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
  
 pdf(paste0("Pause locations_","CS","_Fly",fly.info$Fly[ind], "_",fly.info$experimenter[ind],"_",session,".pdf"))
 {
   t<-c(0:length(fly_pos))/50
   plot(1, type="n", xlab="Time (sec)",ylab="Location", 
        main=paste0("CS","_Fly",fly.info$Fly[ind], "_",fly.info$experimenter[ind],"_",session),
        yaxt="n",
        xlim=c(0, 600), ylim=c(0, 767))
   lines(t[0:(length(t)-1)],fly_pos
      # yaxt="n",
   )
   points(t[0:(length(t)-1)],is_pause_plot,col='blue')
   points(t[0:(length(t)-1)],is_pause_middle_plot,col='red')
   axis(2,c(0,767),labels=c("0","767"))
   # legend(500,875,c("Fly trejactory","All pauses","Pauses not at the end"),
   #        pch =21, 
   #        pt.lwd=c(3,3,3),col=c("black","blue","red"),cex=0.8)
   
 }
 dev.off()
 graphics.off()
