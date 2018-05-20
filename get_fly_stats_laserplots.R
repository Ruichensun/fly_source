#!/usr/bin/env Rscript

setwd("D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/")
source("get_fly_speed_and_position.R")
##Calculating the duration difference between the duration of one laser punishment and duration of its previous no-punishment's period
one_file_laser <- function(input_file, 
                           framerate
                           )
  {
    fly.file = read.csv(input_file,header=T,stringsAsFactors=F)
    xx = as.numeric(fly.file[[1]])
    yy = as.numeric(fly.file[[2]])
    
    if (is.na(yy[1])==T){
    
      laser_df = NA
      return(laser_df)
      
    }else{
      data_start = 20
      fly_pos = xx[data_start:length(xx)]
      fly_laser = yy[data_start:length(yy)]
      
      if (fly_laser[length(fly_laser)]>0){
        # fly_laser[length(fly_laser)-1]=0
        fly_laser[length(fly_laser)]=0
        }
      
      if (fly_laser[1]>0){
        # fly_laser[length(fly_laser)-1]=0
        fly_laser[1]=0
      }
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
    
      if (laser_OFF[1] == length(fly_laser)){
      laser_df = data.frame(0,0,0,(laser_OFF[1])/framerate,(laser_OFF[1]-0)/framerate,TRUE) 
      
      }else{
       # if(length(which(label_for_laser==1))>length(which(label_for_laser==2))){
       #   #Meaning that when experiment finishes when laser is still ON
       #   laser_OFF_supplemented = c(which(label_for_laser==2),length(fly_laser))
       #   
       #   laser_df = data.frame (which(label_for_laser==1),laser_OFF_supplemented,laser_ON/framerate,(laser_OFF[2:length(laser_OFF)])/framerate,(laser_OFF[2:length(laser_OFF)]-laser_ON)/framerate)
       # }else{
           laser_df = data.frame (which(label_for_laser==1),which(label_for_laser==2),laser_ON/framerate,(laser_OFF[2:length(laser_OFF)])/framerate,(laser_OFF[2:length(laser_OFF)]-laser_ON)/framerate,laser_OFF[2:length(laser_OFF)]>8*60*framerate)
           }
  
      colnames(laser_df) = c("Laser_On","Laser_Off","ON_duration","OFF_duration","Difference","eight_min_OFF")
      return(laser_df)
      }
    }


sessions <- c(
  
  # For all T1 sessions
  "E1T1",
  "E1T1E1T1",
  "E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1T1",
  
  "E1T1E1T1E1T2",
  "E1T1E1T1E1T2E1T2",
  
  "E1T2",
  "E1T2E1T2",
  "E1T2E1T2E1T1",
  "E1T2E1T2E1T1E1T1"
  )

# sessions = unique(sessions)
query.sessions = sessions

# count = 0
# spec = NULL
# fly.names = NULL


## WT
## ofs - one fly statistics

# all_ofs_WT = list()
# for(session in sessions){
#   all_ofs_WT = append(all_ofs_WT,list(c()))
# }


## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp,Framerate

fly.info = read.csv("data/fly_info_CS_preprocessed.csv",header=T,stringsAsFactors=F)

fly.info.trained = fly.info[fly.info$Category=="T",]

pdf("Laser Differences for Trained CS flies_032018.pdf")

for(ind in 1:nrow(fly.info.trained)){
  
    laser_ALL_DF = data.frame()
    laser_Diff_Vector = c()
    
    for(ind.session in 1:4){
        input.file <- list.files(path = paste0("data/",
                                               fly.info.trained$experimenter[ind],
                                              "/CS/"),                             
                                 pattern = paste0("ProcessedData_Fly",fly.info.trained$Fly[ind],
                                                   "_",query.sessions[ind.session],
                                                   "_WT",
                                                   ".csv"),
                                 full.names=T
                                 )
        print(input.file)
        if(length(input.file) == 0){
            # all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(NA))
            next
        }
        
        framerate = fly.info.trained$Framerate[ind]        
        print(framerate)
        # ofs = one_fly_statistics(input.file,framerate=framerate)
        print(one_file_laser(input.file,framerate))
        if (is.na(one_file_laser(input.file,framerate))==F){
        laser_ALL_DF = append(laser_ALL_DF, one_file_laser(input.file,framerate))
        print(laser_ALL_DF)
        laser_Diff_Vector = append (laser_Diff_Vector,one_file_laser(input.file,framerate)$Difference)
        print(laser_Diff_Vector)
        # all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(ofs))
        }
    }
    
    print((length(laser_Diff_Vector)>0)&&(is.na(laser_Diff_Vector[1])==F))
    
    if ((length(laser_Diff_Vector)>0)&&
      (is.na(laser_Diff_Vector[1])==F)
      ){
      
      plot(laser_Diff_Vector, 
           main=paste0("ProcessedData_Fly",fly.info.trained$Fly[ind],
                       "_",fly.info.trained$experimenter[ind], "_T",
                       "_WT"),
           type = "h",
           ylim= c(-200,600),
           xlab='Laser Clicks')
      
    }else{
      
    }
}

dev.off()



#####Note to myself: update the section below! 3/20/2018
#####Note to myself: Update done! 3/22/2018

## Mutants
# all_ofs_mutants = list()
# for(session in sessions){
#   all_ofs_mutants = append(all_ofs_mutants,list(c()))
# }

## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp,Framerate
fly.info = read.csv("data/fly_info_mutants_preprocessed.csv",header=T,stringsAsFactors=F)

fly.info.trained = fly.info[fly.info$Category=="T",]

pdf("Laser Differences for Trained Mutant flies_032218.pdf")


for(ind in 1:nrow(fly.info.trained)){
  
  laser_ALL_DF = data.frame()
  laser_Diff_Vector = c()
  
  for(ind.session in 1:4){
    input.file <- list.files(path = paste0("data/",
                                           fly.info.trained$experimenter[ind],
                                           "/mutants/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info.trained$Fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_",fly.info.trained$Genotype[ind],
                                              ".csv"),
                             full.names=T
    )
    print(input.file)
    if(length(input.file) == 0){
      # all_ofs_mutants[[sessions[ind.session]]] = append(all_ofs_mutants[[sessions[ind.session]]],list(NA))
      next
    }   
    
    framerate = fly.info.trained$Framerate[ind]        
    print(framerate)
    # ofs = one_fly_statistics(input.file,framerate=framerate)
    
    print(one_file_laser(input.file,framerate))
    if (is.na(one_file_laser(input.file,framerate))==F){
      laser_ALL_DF = append(laser_ALL_DF,one_file_laser(input.file,framerate))
      print(laser_ALL_DF)
      laser_Diff_Vector = append(laser_Diff_Vector, one_file_laser(input.file, framerate)$Difference)
      print(laser_Diff_Vector)
      }
    # all_ofs_mutants[[sessions[ind.session]]] = append(all_ofs_mutants[[sessions[ind.session]]],list(ofs))
    }
  print((length(laser_Diff_Vector)>0)&&(is.na(laser_Diff_Vector[1])==F))
  
  if ((length(laser_Diff_Vector)>0)&&
      (is.na(laser_Diff_Vector[1])==F)){
    
    plot(laser_Diff_Vector, 
         main=paste0("ProcessedData_Fly",fly.info.trained$Fly[ind],
                     "_",fly.info.trained$Genotype[ind],"_",fly.info.trained$experimenter[ind], "_T"
                     ),
         type = "h",
         ylim= c(-300,600),
         xlab='Laser Clicks')
  }else{
      
    }
}

dev.off()

# save.image("all_ofs.Rdata")


###############For CS_controls flies

## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp,Framerate

fly.info = read.csv("data/fly_info_CS_controls_preprocessed.csv",header=T,stringsAsFactors=F)

# fly.info.trained = fly.info[(fly.info$Category=="T")&(fly.info$Fly<2001),]
fly.info.trained = fly.info[(fly.info$Category=="T")&(fly.info$Fly>2000),]

# pdf("Laser Differences for Trained CS flies_1000series_040418.pdf")
pdf("Laser Differences for Trained CS flies_2000series_040418.pdf")

for(ind in 1:nrow(fly.info.trained)){
  
  laser_ALL_DF = data.frame()
  laser_Diff_Vector = c()
  ind.session = 1
  # for(ind.session in 1:4){
    input.file <- list.files(path = paste0("data/",
                                           fly.info.trained$experimenter[ind],
                                           "/CS_controls/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info.trained$Fly[ind],
                                              "_",query.sessions[ind.session],
                                              "_WT",
                                              ".csv"),
                             full.names=T
    )
    print(input.file)
    if(length(input.file) == 0){
      # all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(NA))
      next
    }
    
    framerate = fly.info.trained$Framerate[ind]        
    print(framerate)
    # ofs = one_fly_statistics(input.file,framerate=framerate)
    print(one_file_laser(input.file,framerate))
    if (is.na(one_file_laser(input.file,framerate))==F){
      laser_ALL_DF = append(laser_ALL_DF, one_file_laser(input.file,framerate))
      print(laser_ALL_DF)
      laser_Diff_Vector = append (laser_Diff_Vector,one_file_laser(input.file,framerate)$Difference)
      print(laser_Diff_Vector)
      # all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(ofs))
    
  }
  
  print((length(laser_Diff_Vector)>0)&&(is.na(laser_Diff_Vector[1])==F))
  
  if ((length(laser_Diff_Vector)>0)&&
      (is.na(laser_Diff_Vector[1])==F)
  ){
    
    plot(laser_Diff_Vector, 
         main=paste0("ProcessedData_Fly",fly.info.trained$Fly[ind],
                     "_",fly.info.trained$experimenter[ind], "_T",
                     "_WT"),
         type = "h",
         ylim= c(-200,600),
         xlab='Laser Clicks')
    
  }else{
    
  }
}

dev.off()

