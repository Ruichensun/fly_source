#!/usr/bin/env Rscript


pass_fly_QC <- function(x,
                        framerate = 50,
                        speed_max_thres = 20, 
                        speed_zero_thres = 1e-2, 
                        pause_frame_thres = 25,
                        chamber_end_thres = 50
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


## Frame rate
## frame rate = 10 for data with 0s inserted
## otherwise frame rate = 50

## How to find 0s inserted data
                                        #if (((fly_number<=113) && (experimenter == "JG") && (genotype=="Mutants"))
                                        #    ||((experimenter == "JG") && (genotype=="CS"))
                                        #    ||((experimenter=="RS")&& (genotype=="CS"))
                                        #    ||((experimenter=="ES")&& (genotype=="CS") &&(fly_number<=32))
                                        #    )


sessions <- c("E1",
              "E1X1E1",
              "E1X1E1X1E1",
              "E1X1E1X1E1X1E1",
              "E1X1E1X1E1X1E1X1E1",

              "E1X1E1X1E1X2E1",
              "E1X1E1X1E1X2E1X2E1")


## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp
fly.info = read.csv("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_CS.csv",header=T,stringsAsFactors=F)
fly.info$Age = as.Date(fly.info$Exp.date,format='%m/%d/%Y')  - as.Date(fly.info$Birth.date,format='%m/%d/%Y')
# fly.info$E1_loading_time <- sapply(strsplit(fly.info$E1_loading_time,"[:\ ]"),
#                                    function(x) {
#                                        if(x[3] == "PM" & x[1] >= 2){
#                                            return("late")
#                                        }else{
#                                            return("early")
#                                        }
#                                    }
#                                   )



ind.passQC = NULL
fly.info.framerate = NULL
count = 0;
for(ind in 1:nrow(fly.info)){        
    query.sessions = gsub("X",fly.info$Category[ind],sessions)
    for(session in query.sessions){
        input.file <- list.files(path = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",
                                     fly.info$experimenter[ind],
                                     "/CS/CSV/"),
                                 
                                 pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                     "_",session,"_",
                                     fly.info$Gender[ind],"_.*"),
                                 
                                 full.names=T
                                 )

        if(fly.info$experimenter[ind] == "ES" & session == "E1" & length(input.file) == 0){
            cat("M",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")
            break
        }
        
        if(length(input.file) == 0){next}
        
        framerate = 50
        
        ## Read data
        #fly.pos <- scan(input.file,
        #                skip=1,quiet=T)
        fly.pos <- read.csv(input.file,stringsAsFactors = F)[,1]
        
        exp.time = length(fly.pos)
        
        ## Processing for some special cases
        if(((fly.info$experimenter == "JG")&&(fly.info$Fly<=72))
           ||((fly.info$experimenter=="RS")&&(fly.info$Fly<=64))
           ||((fly.info$experimenter=="ES")&&(fly.info$Fly<=32))
           ||((fly.info$experimenter=="JE")&&(fly.info$Fly<=71))
           ||(fly.info$experimenter=="LM")
        ){
            fly.pos <- fly.pos[seq(1,exp.time,by=2)]
            framerate = 10
        }

        if(session == "E1"){
            count = count + 1
            
            if(!pass_fly_QC(fly.pos,framerate)){
                cat("F",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")
                break
            }else{           
                cat("P",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")
            }
        }
        
        fly.pos.dat = data.frame(fly.pos)
        colnames(fly.pos.dat) = paste0("fly_pos;framerate=",framerate)

        dir.create(paste0("data/",fly.info$experimenter[ind],"/CS/"), showWarnings = FALSE, recursive = TRUE)
        
        output.file <- paste0("data/",
                              fly.info$experimenter[ind],
                              "/CS/",
                              "ProcessedData_Fly",
                              fly.info$Fly[ind],
                              "_",session,
                              "_WT",
                              ".csv")
                              
                              
        write.table(fly.pos.dat,
                    output.file,
                    row.names=F,quote=F)


        if(session == "E1"){
            fly.info.framerate = c(fly.info.framerate,framerate)
            ind.passQC = c(ind.passQC,ind)
        }
    }
}

fly.info.out = fly.info[ind.passQC,]
fly.info.out$Framerate = fly.info.framerate

write.table(fly.info.out,
            "data/fly_info_CS_QCed.csv",
            row.names=F,quote=F,sep=",")


## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp
