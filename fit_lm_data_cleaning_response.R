#!/usr/bin/env Rscript
source("get_fly_speed_and_position.R")

load("all_ofs_response.Rdata")

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

sessions <- c(      
  "E1",
  "E1X",
  "E1XE1"
)


## Fly info
fly.info = read.csv("data/fly_info_Response.csv",header=T,stringsAsFactors=F)

## Fit linear model for each metric
for(ind in 1:length(metrices)){
    if(ind == 20 | metrices[ind] == "State_transitions (Pause not at the end): pp, pw, ww, wp"){
        next
    }
    
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
                                 )
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
                paste0("metrics_response/metric_",ind,".csv"),
                row.names=F,quote=F,sep=",")
}


write.table(metrices,
            "metrics_response/list_metrices.csv",
            col.names=F,
            row.names=F,
            quote=F,
            sep=",")


