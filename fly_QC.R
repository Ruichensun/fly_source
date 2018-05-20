pass_fly_QC <- function(input_file,
                        framerate = 50,
                        speed_max_thres = 20, 
                        speed_zero_thres = 1e-2, 
                        pause_frame_thres = 25,
                        chamber_end_thres = 50
                       ){
    ## read input file
    tryCatch(
        {
            x = read.table(input_file,header=T,sep=",",stringsAsFactors=F)        
        },error = function(e){
            stop(paste0("Input file is empty!:\n",
                        "  Input files is: ", input_file,"\n\n")
                 )
        }
        )
    
    
    if(nrow(x) < 10){
      stop(paste0("Input file is empty!:\n",
                  "  Input files is: ", input_file,"\n\n")
      )
    }
  

    ## To deal with the old data
    x = as.numeric(x$fly.position)
    fly_num = sapply(strsplit(input_file,"_"),function(x) return(gsub("Fly","",x[2])))
    if(fly_num <= 32){
        x = x[seq(1,length(x),by=2)]
    }
    
    #x = as.numeric(x)
    data_start = 20 #changed it to 20 from 10 on Oct 5, 2016
    fly_pos = x[data_start:min(600*framerate,length(x))]

    
    experiment_time = length(fly_pos)        

    #plot(fly_pos,type='l',main=input_file)
    
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
    ## The speed at last time point is zero otherwise current_zero_length will 0
    if(current_zero_length >= pause_frame_thres){
        ## Record the time span as a pause
        pause_start = c(pause_start,potential_pause_start)
        pause_end = c(pause_end,experiment_time)
        is_pause[potential_pause_start:experiment_time] = 1
    }
    num_pause = length(pause_start)

    ##if(num_pause == 0){
    ##    return(NA)
    ##}
    ##return(max(pause_end-pause_start+1))

    if(max(pause_end-pause_start+1) >= 20 * framerate){
        return(FALSE)
    }
    
    
    ## range 0 - 767
    ## One end
    chamber_end = rep(0,length(fly_pos))
    chamber_end[fly_pos >=0 & fly_pos < chamber_end_thres] = -1
    chamber_end[fly_pos <= 767 & fly_pos > 767-chamber_end_thres] = 1
    #chamber_end[fly_pos >=chamber_end_thres & fly_pos < chamber_end_thres*2] = -0.5
    #chamber_end[fly_pos <= 767-chamber_end_thres & fly_pos > 767-chamber_end_thres*2] = 0.5
    chamber_end[fly_pos >= 383.5-chamber_end_thres & fly_pos < 383.5+chamber_end_thres] = 0.5

    end2end = rle(chamber_end)$values
    end2end = end2end[end2end!=0]

    end2end = rle(end2end)$values
    end2end = end2end[end2end != 0.5]
    if(length(end2end) >= 10){
        ## start: end2end[-length(end2end)] 
        ## end: end2end[-1]
        incomplete_lap = sum(end2end[-length(end2end)] == end2end[-1])
        complete_lap = sum(end2end[-length(end2end)] != end2end[-1])
        tot_lap = complete_lap+incomplete_lap
        #print(end2end)
        #print(paste(complete_lap,incomplete_lap,tot_lap))
        if(complete_lap >= 10 & incomplete_lap/tot_lap <=0.2){
            return(TRUE)
        }            
    }
    return(FALSE)
}


#input_files = commandArgs(trailingOnly=TRUE)

#max_pause = NULL
#for(input_file in input_files){
#    mp = pass_fly_QC(input_file)#

#    if(!is.na(mp)){        
#        max_pause = c(max_pause,log10(mp))
#    }
    #if(!pass_fly_QC(input_file)){
    #    print(input_file)
    #}
#}
#hist(max_pause,20)
