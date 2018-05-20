#!/usr/bin/env Rscript
# source("get_fly_speed_and_position.R")

# load("all_ofs.Rdata")
load("all_ofs_WT_controls.Rdata")

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
      # x=input_file    
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
  # x = as.numeric(x$fly.position)
  # x = as.numeric(x$fly_pos.framerate.10)
  x = as.numeric(x[,1])
  fly_num = sapply(strsplit(input_file,"_"),function(x) return(gsub("Fly","",x[2])))
  # if(as.numeric(fly_num) <= 32){
  #   x = x[seq(1,length(x),by=2)]
  # }
  
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
  print(input_file)
  for (i in 1:experiment_time){
    if (abs(fly_speed[i])>=speed_max_thres){
      fly_speed[i]=0
    }
  }
  print(paste0("Error here ",input_file))
  
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
  
  if(max(pause_end-pause_start+1) >= (20 * framerate)){
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




metrices = c(
  "Number of Pause Starts", #1
  "Fraction Time in Pause", #2
  "Average Pause Duration", #3
  "Max Pause Duration", #4 
  "Average Moving Speed ", #5
  "Average Moving Speed (excluding pause)", #6
  "Average Speed When Enter Pause", #7
  "Average Speed When Exit Pause",#8
  "Moving Distance Per Minute",#9
  "Number of Turns",#10
  "Number of Middle Turns",#11
  "Fration of Middle Turns Out of Total Turns",#12
  "Burstiness (Pause)",#13
  "Burstiness (Inter Event Time)",#14
  "Burstiness (Scrambled)",#15
  "Burstiness (Walking bouts-thresholding)",#16
  "Burstiness (Walking events-thresholding)",#17
  "Beginning Pause Duration",#18 
  "First Pause Duration",#19 
  # "State_transitions (Pause not at the end): pp, pw, ww, wp",#20
  "Transition Probability (Pause not at the end): Pause to Pause",#20
  "Transition Probability (Pause not at the end): Pause to Walking",#21
  "Transition Probability (Pasue not at the end): Walking to Walking",#22
  "Transition Probability (Pause not at the end): Walking to Pause",#23
  "Memory",#24
  "Memory (inverted)",#25
  "Burstiness of Start of Walking (Pause not at the end)",#26
  "Burstiness of Start of Pause (Pause not at the end)",#27 *
  "Average Pause Duration (Pause not at the End)",#28 
  "Fraction Time in Pause (Pause not at the End)",#29 
  "Max Pause Duration (Pause not at the End)", #30
  "First Pause Duration (Pause not at the End)" #31
)

sessions <- c(      
  "E1",
  "E1T1E1",
  "E1R1E1",
  "E1N1E1"
)
# sessions <- unique(sessions)

all_ofs = list()
for(session in sessions){
  all_ofs = append(all_ofs,list(c(all_ofs_WT_controls[[session]])))
}
names(all_ofs) = sessions

## Fly info
fly.info.CS_controls = read.csv("data/fly_info_CS_controls_preprocessed.csv",header=T,stringsAsFactors=F)
fly.info.CS_controls$Genotype = "WT"

# fly.info.mutants = read.csv("data/fly_info_mutants_preprocessed.csv",header=T,stringsAsFactors=F)
# fly.info.mutants = fly.info.mutants[fly.info.mutants$Genotype %in% c("SUN1","SUN2","SUN3","R3","R5"),]

shared.info = c("Fly","Category","Gender","Genotype","experimenter","Age","Setup","Fly.moving.speed","Framerate")

# fly.info = rbind(fly.info.CS[,shared.info],fly.info.mutants[,shared.info])
fly.info = fly.info.CS_controls[,shared.info]
fly.info = fly.info[fly.info$Fly>2000,]


## Further filtering
# JG-CS-30
# RS-CS-58
# JD-CS-48
# CS,69,ES
# CS,86,ES
# CS,89,ES
# CS,113,ES
# CS,163,ES
# CS,164,ES
# CS,165,ES
# CS,202,ES
# CS,213,ES
# CS,234,ES
# CS,249,ES
# CS,253,ES

#For WT data collected after Mar 20, 2017
# excl.fly = cbind(c(rep("WT",856)),
#                  c("JG","RS","JD",rep("ES",12), rep("ES",400),rep("JG",120),rep("JE",221),rep("RS",100)),
#                  c(30,58,48,69,86,89,113,163:165,202,213,234,249,253,1:400,1:120,1:221,1:100)
# 
#                  )

#For Mutant data collected after Mar 20, 2017
# excl.fly = cbind(c(rep("WT",713),rep("R3",4),"R5",rep("R3",2)),
# 
#                  c(rep("ES",308),rep("JG",313),rep("JE",88),rep("RS",4),rep("SW",7)),
#                  c(1:308,
#                    1:313,
#                    1:88,
#                    61:64,
#                    25:28,49:51
#                    )
# )
#For Mutant data collected after Mar 20, 2017
excl.fly.Mutant = na.omit(read.csv("excl_fly_mutant.csv",header=T,stringsAsFactors = F)[,1:3]) #Added June 12, 2017

excl.fly.WT = data.frame(
  cbind(c(30,58,48,69,86,89,113,163:165,202,213,234,249,253,277:400,1:120,1:221,1:100), ###Batch 1 ES-WT: 1-276; Batch 2 ES-WT: 276-400; Batch 3 ES-WT: 400-present (as of June 28, 2017)
        c("JG","RS","JD",rep("ES",12), rep("ES",124),rep("JG",120),rep("JE",221),rep("RS",100)),
        
        c(rep("WT",580))
  )
)
colnames(excl.fly.WT) = colnames(excl.fly.Mutant)
excl.fly = rbind(excl.fly.Mutant,
                 excl.fly.WT
)

# #For WT data collected between Jan,2017 - Mar. 2017
# excl.fly = cbind(c(rep("WT",831)),
#                  c("JG","RS","JD",
#                    rep("ES",12),
#                    rep("ES",276),rep("ES",102),
#                    rep("JG",72),rep("JG",4),
#                    rep("JE",181),
#                    rep("RS",72),rep("RS",4),
#                    rep("JD",72),
#                    rep("SW",33)
#                    ),
#                  c(30,58,48,69,86,89,113,163:165,202,213,234,249,253,
#                    1:276,401:502,
#                    1:72,117:120,
#                    1:181,
#                    1:72,101:104,
#                    1:72,
#                    1:33
#                    )
# )

#For WT data collected between July, 2016 - Dec. 2016
# excl.fly = cbind(c(rep("WT",791)),
#                  c("JG","RS","JD",
#                    rep("ES",12),
#                    rep("ES",226),
#                    rep("JG",120),
#                    rep("JE",221),
#                    rep("RS",104),
#                    rep("JD",72),
#                    rep("SW",33)
#                  ),
#                  c(30,58,48,69,86,89,113,163:165,202,213,234,249,253,
#                    277:502,
#                    1:120,
#                    1:221,
#                    1:104,
#                    1:72,
#                    1:33
#                  )
# )

ind.excl = NULL
for(ind in 1:nrow(excl.fly)){
  # ##CS data
  #  ind.excl = c(ind.excl,
  #               which(fly.info$Genotype == excl.fly[ind,1] &
  #                       fly.info$experimenter == excl.fly[ind,2] &
  #                       fly.info$Fly == excl.fly[ind,3]
  #               )
  #  )
  
  
  #Mutant data
  ind.excl = c(ind.excl,
               which(fly.info$Genotype == excl.fly[ind,3] &
                       fly.info$experimenter == excl.fly[ind,2] &
                       fly.info$Fly == excl.fly[ind,1]
               )
  )
}

## Filter1
ind.include = NULL
for(genotype in unique(fly.info$Genotype)){
  if (genotype=="CS"){next}
  if (genotype=="WT"){ind = fly.info$Genotype %in% c("WT","CS") & !(1:nrow(fly.info) %in% ind.excl)}
  else{
    ind = fly.info$Genotype == genotype & !(1:nrow(fly.info) %in% ind.excl)}
  fms <- fly.info$Fly.moving.speed[ind]
  rank_fms = rank(fms)
  # ind.filter =  rank_fms <= length(fms) * 0.95 &
  #   rank_fms >= length(fms) * 0.2
  ind.filter =  rank_fms <= length(fms) * 1 &
    rank_fms >= length(fms) * 0.3
  ind.include = c(ind.include, which(ind)[ind.filter])
}


##Filter2
ind.include=NULL

session = "E1"
for(ind in 1:nrow(fly.info)){
  if(fly.info$Genotype[ind]=="WT"){
    input.file <- paste0("data/",
                         fly.info$experimenter[ind],
                         "/CS/",
                         "ProcessedData_Fly",
                         fly.info$Fly[ind],
                         "_",session,
                         "_WT",
                         ".csv")
  }else{
    input.file <- paste0("data/",
                         fly.info$experimenter[ind],
                         "/Mutants/",
                         "ProcessedData_Fly",
                         fly.info$Fly[ind],
                         "_",session,
                         "_",fly.info$Genotype[ind],
                         ".csv")
  }
  framerate =	fly.info$Framerate[ind]
  # fly.pos <- read.csv(input.file,stringsAsFactors = F)[,1]
  if(pass_fly_QC(input.file,framerate)){
    ind.include = c(ind.include,ind)
  }
}

## Exclude all WT
# ind.excl = unique(c(ind.excl,which(fly.info$Genotype=="WT")))

#Need to run this line for filter2
# ind.include = ind.include[!(ind.include %in% ind.excl)]





write.csv(fly.info[ind.include,],
          # "data/fly_info_pass_QC.csv",
          # "data/fly_info_CS_July2016-Dec2016.csv",
          # "data/fly_info_CS_Jan2017-Mar2017.csv",
          # "data/fly_info_CS_Mar2017_present20-95.csv",
          # "data/fly_info_CS_Mar2017_present.csv",
          # "data/fly_info_Jan_2018_present_B1_B3.csv",
          # "data/fly_info_Feb9_2018_present_B1_B3.csv",
          "data/fly_info_Apr5_2018_CScontrosl_2000series_.csv",
          quote=F,row.names = F)


## Fit linear model for each metric
for(ind in 1:length(metrices)){
  # if(ind == 20 | metrices[ind] == "State_transitions (Pause not at the end): pp, pw, ww, wp"){
  #     next
  # }
  
  metric.df = NULL;
  for(session in sessions){
    print(session)
    metric <- sapply(all_ofs[[session]],
                     function(x){
                       ##if((length(x) == 1) & (is.na(x))){
                       if(length(x) == 1){
                         return(NA)
                       }else{
                         return(x[[ind]])
                       }
                     }
    )
    
    array.session = rep(session,length(metric))
    for(i in 1:length(array.session)){
      array.session[i] = gsub("X",fly.info$Category[i],array.session[i])
    }
    
    array.age = fly.info$Age
    array.age[array.age > 5] = 5
    #array.age[array.age < 3] = 3
    
    metric.df <- rbind(metric.df,
                       cbind(metric,
                             fly.info$Category,
                             fly.info$Fly,
                             fly.info$Genotype,
                             fly.info$experimenter,
                             fly.info$Gender,
                             array.age,
                             fly.info$Setup,
                             array.session
                       )[ind.include,]
    )
  }
  colnames(metric.df) = c("value","category","fly","genotype","experimenter","gender","age","setup","session")
  
  ## Prepare input dataframe
  metric.df = data.frame(metric.df)
  for(i in 1:ncol(metric.df)){
    metric.df[,i] = unlist(metric.df[,i])
  }
  
  metric.df$setup <- factor(metric.df$setup,
                            levels = levels(metric.df$setup)[c(2,1,3,4)])
  metric.df$age <- factor(metric.df$age,levels = levels(metric.df$age)[c(3,1,2,4)])
  
  metric.df$value = as.numeric(as.character(metric.df$value))
  
  ###Need to update batch info!!####
  metric.df$batch = as.numeric(as.character(metric.df$fly))
  
  metric.df$batch[metric.df$batch <= 276] = 1
  metric.df$batch[metric.df$batch >= 277 & metric.df$batch <=400 ] = 2
  metric.df$batch[metric.df$batch > 400] = 3
  metric.df$batch = factor(metric.df$batch)
  
  metric.df$value[is.infinite(metric.df$value)] = NA
  
  #metric.df = na.omit(metric.df)
  #metric.df = metric.df[complete.cases(metric.df),]
  
  
  ## NO LM FIT
  write.table(metric.df,
              paste0("metrics/metric_",ind,".csv"),
              row.names=F,quote=F,sep=",")
  next
  
  
  
  metric.df = metric.df[metric.df$genotype %in% c("WT","SUN1","SUN2","SUN3","R3"),]
  metric.df$genotype = factor(metric.df$genotype,
                              levels(metric.df$genotype)[levels(metric.df$genotype) %in% c("WT","SUN1","SUN2","SUN3","R3")][c(5,1:4)])
  
  ## Linear fit
  lm.fit.orig <- lm(value ~ genotype + experimenter + gender + session + batch + setup + fly + age,
                    metric.df)
  #lm.fit.orig = glm(value ~ age + gender + batch + session + fly, metric.df,family="gaussian")
  lm.fit = summary(lm.fit.orig)
  ## If the model does not fit the data well, just save the original data and continue
  #if(lm.fit$adj.r.squared < 0.6){
  write.table(metric.df,
              paste0("metrics/metric_",ind,".csv"),
              row.names=F,quote=F,sep=",")
  next
  #}
  
  coef = na.omit(lm.fit.orig$coefficients)
  
  model = model.matrix(value ~ genotype + experimenter + gender + session + batch + setup + fly + age, metric.df)
  model = data.matrix(model[,names(coef)])
  
  
  ## Confounding covariates
  coef_ind <- c(#1,## intercept
    grep("experimenter",names(coef)),
    grep("batch",names(coef)),
    grep("setup",names(coef)),
    grep("age",names(coef)),
    grep("gender",names(coef))
    #grep("fly",names(coef))
  )
  uwv <- model[,coef_ind] %*% coef[coef_ind]
  wv <- model[,-coef_ind] %*% coef[-coef_ind]
  
  metric.df$value.uw = uwv
  metric.df$value.w = wv
  
  
  write.table(metric.df,
              paste0("metrics/metric_",ind,".csv"),
              row.names=F,quote=F,sep=",")
  
  print(paste("Finished metric",ind))
  ##print(metrices[ind])
}


write.table(metrices,
            "metrics/list_metrices.csv",
            col.names=F,
            row.names=F,
            quote=F,
            sep=",")


