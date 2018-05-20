#!/usr/bin/env Rscript
source("get_fly_speed_and_position.R")

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
  "State_transitions (Pause not at the end): pp, pw, ww, wp",#20
  "Transition Probability (Pause not at the end): Pause to Pause",#21
  "Transition Probability (Pause not at the end): Pause to Walking",#22
  "Transition Probability (Pasue not at the end): Walking to Walking",#23
  "Transition Probability (Pause not at the end): Walking to Pause",#24
  "Memory",#25
  "Memory (inverted)",#26
  "Burstiness of Start of Walking (Pause not at the end)",#27 *
  "Burstiness of Start of Pause (Pause not at the end)",#28 *
  "Average Pause Duration (Pause not at the End)",#29 
  "Fraction Time in Pause (Pause not at the End)",#30 
  "Max Pause Duration (Pause not at the End)", #31
  "First Pause Duration (Pause not at the End)" #32
    )

sessions <- c("E1","E1XE1","E1X")

count = 0
spec = NULL
fly.names = NULL


## WT
## ofs - one fly statistics
all_ofs = list()
for(session in sessions){
  all_ofs = append(all_ofs,list(c()))
}
## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp,Framerate
fly.info = read.csv("data/fly_info_Response.csv",header=T,stringsAsFactors=F)
for(ind in 1:nrow(fly.info)){
    query.sessions = gsub("X",fly.info$Category[ind],sessions)
    for(ind.session in 1:length(query.sessions)){
        input.file <- list.files(path = paste0("data/",
                                     fly.info$experimenter[ind],
                                     "/Response/"),                             
                                 pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                     "_",query.sessions[ind.session],
                                     "_",fly.info$Genotype[ind],
                                     ".csv"),
                                 full.names=T
                                 )
        if(length(input.file) == 0){
            all_ofs_WT[[sessions[ind.session]]] = append(all_ofs_WT[[sessions[ind.session]]],list(NA))
            next
        }   
        framerate = fly.info$Framerate[ind]        
        ofs = one_fly_statistics(input.file,framerate=framerate)
        
        all_ofs[[sessions[ind.session]]] = append(all_ofs[[sessions[ind.session]]],list(ofs))
    }
}
save.image("all_ofs_response.Rdata")
