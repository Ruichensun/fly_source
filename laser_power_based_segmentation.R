#Segmentation Project: Start 2018-04-16.
#This code is to get the number of laser clicks and total laser exposure duration a fly experience in a given session

#Step 1: Extracting Number of Clicks and Total Laser Exposure (in seconds) from individual T/R/N file

one_fly_laser_statistics <- function(input_file,
                                     framerate)
{
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  
  fly.position.raw = as.numeric(fly.file[[1]])
  fly.laser.raw = as.numeric(fly.file[[2]])
  
  if (is.na(fly.laser.raw[1]) == T) {
    number_of_laser_clicks = NA
    total_laser_ON = NA
    
    ret = list(number_of_laser_clicks,
               total_laser_ON #in seconds
               )
               
               names(ret) = c("Number of Laser Punishments",
                              #1
                              "Total Laser Exposure in Seconds" #2
                              )
                              
  } else{
    data_start = 21
    
    fly_pos = fly.position.raw[data_start:length(fly.position.raw)]
    fly_laser = fly.laser.raw[data_start:length(fly.laser.raw)]
    
    if (fly_laser[length(fly_laser)] > 0) {
      # fly_laser[length(fly_laser)-1]=0
      fly_laser[length(fly_laser)] = 0
    }
    
    if (fly_laser[1] > 0) {
      # fly_laser[length(fly_laser)-1]=0
      fly_laser[1] = 0
    }
    for (i in 1:length(fly_laser)) {
      if (fly_laser[i] > 0) {
        fly_laser[i] = 1
      }
    }
    laser_ON  = rle(fly_laser)$length[rle(fly_laser)$values == 1]
    laser_OFF = rle(fly_laser)$length[rle(fly_laser)$values == 0]
    
    label_for_laser = rep(0, length(fly_laser))
    for (i in 1:(length(label_for_laser) - 1)) {
      if ((fly_laser[i] == 0) & (fly_laser[i + 1] > 0)) {
        label_for_laser[i + 1] = 1
      }
      if ((fly_laser[i] > 0) & (fly_laser[i + 1] == 0)) {
        label_for_laser[i + 1] = 2
      }
    }
    
    laser_df = data.frame()
    
    if (laser_OFF[1] == length(fly_laser)) {
      laser_df = data.frame(0,
                            0,
                            0,
                            (laser_OFF[1]) / framerate,
                            (laser_OFF[1] - 0) / framerate,
                            TRUE)
      
    } else{
      laser_df = data.frame (
        which(label_for_laser == 1),
        which(label_for_laser == 2),
        laser_ON / framerate,
        (laser_OFF[2:length(laser_OFF)]) / framerate,
        (laser_OFF[2:length(laser_OFF)] - laser_ON) / framerate,
        laser_OFF[2:length(laser_OFF)] > 8 * 60 * framerate
      )
    }
    
    colnames(laser_df) = c(
      "Laser_On",
      "Laser_Off",
      "ON_duration",
      "OFF_duration",
      "Difference",
      "eight_min_OFF"
    )
    
    number_of_laser_clicks = length(laser_df$Laser_On)
    total_laser_ON = sum(laser_df$ON_duration)
    
    if (number_of_laser_clicks == 1) {
      if (laser_df$ON_duration == 0) {
        number_of_laser_clicks = 0
      }
    }
    
    
    ## Return output
    ret = list(number_of_laser_clicks,
               total_laser_ON #in seconds
               )
               
               
               
               names(ret) = c("Number of Laser Punishments",
                              #1
                              "Total Laser Exposure in Seconds" #2
                              )
                              return(ret)
                              
  }
}

#### The above function returns a list with two values, one is `Number of Laser Punishments` (range: 0 - 20, or NA); the other one is `Total Laser Exposure in Seconds` (range: 0 - 2000, or NA)


#Step 2: Run the function from Step 1 on all files.


setwd("D:/Behavioral_project/Behavior Experiment Data/Analysis/")
sessions <- c(
  "E1",
  "E1T1",
  "E1T1E1",
  "E1T1E1T1",
  "E1T1E1T1E1",
  "E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1",
  "E1T1E1T1E1T1E1T1",
  "E1T1E1T1E1T1E1T1E1",
  
  "E1R1",
  "E1R1E1",
  "E1R1E1R1",
  "E1R1E1R1E1",
  "E1R1E1R1E1R1",
  "E1R1E1R1E1R1E1",
  "E1R1E1R1E1R1E1R1",
  "E1R1E1R1E1R1E1R1E1",
  
  "E1N1",
  "E1N1E1",
  "E1N1E1N1",
  "E1N1E1N1E1",
  "E1N1E1N1E1N1",
  "E1N1E1N1E1N1E1",
  "E1N1E1N1E1N1E1N1",
  "E1N1E1N1E1N1E1N1E1"
)

sessions = unique(sessions)

query.sessions = sessions

count = 0
spec = NULL
fly.names = NULL


## WT
## ofls - one fly laser statistics
# all_ofls_WT = list()
all_ofls = list()
for (session in sessions) {
  # all_ofls_WT = append(all_ofls_WT,list(c()))
  all_ofls = append(all_ofls, list(c()))
}
## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp,Framerate
# fly.info = read.csv("data/fly_info_CS_preprocessed.csv",header=T,stringsAsFactors=F)
fly.info.include = fly.info[ind.include, ]

# for(ind in 1:nrow(fly.info)){
for (ind in 1:nrow(fly.info.include)) {
  # query.sessions = gsub("X",fly.info$Category[ind],sessions)
  for (ind.session in 1:length(query.sessions)) {
    if ((fly.info.include[ind, ]$Genotype == "WT") |
        (fly.info.include[ind, ]$Genotype == "CS")) {
      # print("Yes")}else{print("ERROR")}
      input.file <- list.files(
        path = paste0("data/",
                      fly.info.include$experimenter[ind],
                      "/CS/"),
        pattern = paste0(
          "ProcessedData_Fly",
          fly.info.include$Fly[ind],
          "_",
          query.sessions[ind.session],
          "_WT",
          ".csv"
        ),
        full.names = T
      )
    } else{
      input.file <- list.files(
        path = paste0("data/",
                      fly.info.include$experimenter[ind],
                      "/mutants/"),
        pattern = paste0(
          "ProcessedData_Fly",
          fly.info.include$Fly[ind],
          "_",
          query.sessions[ind.session],
          "_",
          fly.info.include$Genotype[ind],
          ".csv"
        ),
        full.names = T
      )
    }
    
    if (length(input.file) == 0) {
      all_ofls[[sessions[ind.session]]] = append(all_ofls[[sessions[ind.session]]], list(NA))
      next
    }
    framerate = fly.info.include$Framerate[ind]
    ofls = one_fly_laser_statistics(input.file, framerate = framerate)
    all_ofls[[sessions[ind.session]]] = append(all_ofls[[sessions[ind.session]]], list(ofls))
  }
}
save.image("all_ofls.Rdata")
###Debugging
#
###
metrics = c("Number of Laser Punishments", #1
            "Total Laser Exposure in Seconds" #2
            )
            
sessions <- c(
              "E1",
              "E1T1",
              "E1T1E1",
              "E1T1E1T1",
              "E1T1E1T1E1",
              "E1T1E1T1E1T1",
              "E1T1E1T1E1T1E1",
              "E1T1E1T1E1T1E1T1",
              "E1T1E1T1E1T1E1T1E1",
              
              "E1R1",
              "E1R1E1",
              "E1R1E1R1",
              "E1R1E1R1E1",
              "E1R1E1R1E1R1",
              "E1R1E1R1E1R1E1",
              "E1R1E1R1E1R1E1R1",
              "E1R1E1R1E1R1E1R1E1",
              
              "E1N1",
              "E1N1E1",
              "E1N1E1N1",
              "E1N1E1N1E1",
              "E1N1E1N1E1N1",
              "E1N1E1N1E1N1E1",
              "E1N1E1N1E1N1E1N1",
              "E1N1E1N1E1N1E1N1E1"
            )
            
            for (ind in 1:length(metrics)) {
              metric.df = NULL
              
              for (session in sessions) {
                print(session)
                metric <- sapply(all_ofls[[session]],
                                 function(x) {
                                   ##if((length(x) == 1) & (is.na(x))){
                                   if (length(x) == 1) {
                                     return(NA)
                                   } else{
                                     return(x[[ind]])
                                   }
                                 })
                
                array.session = rep(session, length(metric))
                # print(array.session)
                for (i in 1:length(array.session)) {
                  array.session[i] = gsub("X", fly.info.include$Category[i], array.session[i])  # Change X to T/R/N
                }
                # print(array.session)
                array.age = fly.info.include$Age
                # array.age[array.age > 5] = 5
                #array.age[array.age < 3] = 3
                
                metric.df <- rbind(
                  metric.df,
                  cbind(
                    metric,
                    fly.info.include$Fly,
                    fly.info.include$Category,
                    fly.info.include$Gender,
                    fly.info.include$Genotype,
                    fly.info.include$experimenter,
                    # array.age, #changed to fly.info$Age on Apr 23
                    fly.info.include$Age,
                    fly.info.include$Setup,
                    array.session
                  )
                  # [ind.include,]
                  # ) #Need to change depending whether there is a need to segment the fly population, in other words, the value of ind.include
                )
              }
              colnames(metric.df) = c(
                "Value",
                "Fly",
                "Category",
                "Gender",
                "Genotype",
                "Experimenter",
                "Age",
                "Setup",
                "Session"
              )
              ## Prepare input dataframe
              metric.df = data.frame(metric.df)
              for (i in 1:ncol(metric.df)) {
                metric.df[, i] = unlist(metric.df[, i])
              }
              
              # metric.df$Setup <- factor(metric.df$Setup,
              #                           levels = levels(metric.df$Setup)[c(2,1,3,4)])
              # metric.df$Age <- factor(metric.df$Age,levels = levels(metric.df$Age)[c(3,1,2,4)])
              
              metric.df$Value = as.numeric(as.character(metric.df$Value))
              
              
              ## NO LM FIT
              write.table(
                metric.df,
                paste0("metric_laser/metric_", ind, ".csv"),
                row.names = F,
                quote = F,
                sep = ","
              )
              
            }
            
            write.table(
              metrics,
              "metric_laser/list_metrics.csv",
              col.names = F,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            ##### The above section calculated all the number of clicks and duration for all flies and all sessions _ updated 4/18/2018
            
            ##### Classifying flies into long-heat group and short-heat group by the duration of heat they received -- Using the "Total laser exposure in Seconds" Metric
            
            input.file = "metric_laser/metric_2.csv"
            # input.file = "metric_laser/metric_1.csv"
            metric.df = read.csv(input.file)
            
            # fly.info.include[fly.info.include$Genotype=="WT",]
            # metric.df[metric.df$genotype=="WT",]
            #
            # metric.df.WT = metric.df[metric.df$Genotype=="WT",] # This can be changed for different genotypes
            
            metric.df.WT = metric.df[(metric.df$Genotype == "WT") &
                                       !(metric.df$Experimenter == "SW"), ] # This can be changed for different genotypes
            # metric.df.WT = metric.df[(metric.df$Genotype=="WT")&(metric.df$Experimenter=="JD"),] # This can be changed for different genotypes
            # metric.df.WT = metric.df[(metric.df$Genotype=="SUN2")&!(metric.df$Experimenter=="SW"),]
            
            
            
            mean_of_Training_session = c(
              mean(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value, na.rm = T),
              mean(metric.df.WT[metric.df.WT$Session ==
                                  "E1T1E1T1", ]$Value, na.rm = T),
              mean(metric.df.WT[metric.df.WT$Session ==
                                  "E1T1E1T1E1T1", ]$Value, na.rm = T),
              mean(metric.df.WT[metric.df.WT$Session ==
                                  "E1T1E1T1E1T1E1T1", ]$Value, na.rm = T)
            )
            
            median_of_Training_session = c(
              median(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value, na.rm = T),
              median(metric.df.WT[metric.df.WT$Session ==
                                    "E1T1E1T1", ]$Value, na.rm = T),
              median(metric.df.WT[metric.df.WT$Session ==
                                    "E1T1E1T1E1T1", ]$Value, na.rm = T),
              median(metric.df.WT[metric.df.WT$Session ==
                                    "E1T1E1T1E1T1E1T1", ]$Value, na.rm = T)
            )
            
            counts_of_Training_session = c(sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                                     "E1T1", ]$Value)),
                                           sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                                     "E1T1E1T1", ]$Value)),
                                           sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                                     "E1T1E1T1E1T1", ]$Value)),
                                           sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                                     "E1T1E1T1E1T1E1T1", ]$Value)))
            
            
            
            metric.df.WT.E1T1 = metric.df.WT[(metric.df.WT$Session == "E1T1") &
                                               (!is.na(metric.df.WT$Value)), ]
            
            longer_heat_1stT1 = metric.df.WT.E1T1[metric.df.WT.E1T1$Value >= median(metric.df.WT.E1T1$Value), ]
            shorter_heat_1stT1 = metric.df.WT.E1T1[metric.df.WT.E1T1$Value < median(metric.df.WT.E1T1$Value), ]
            
            metric.df.WT.E1R1 = metric.df.WT[(metric.df.WT$Session == "E1R1") &
                                               (!is.na(metric.df.WT$Value)), ]
            
            longer_heat_1stR1 = metric.df.WT.E1R1[metric.df.WT.E1R1$Value >= median(metric.df.WT.E1T1$Value), ] ##FLAG: may need tease out the R flies that do not have corresponding T flies in the longer_heat_1stT1 (04/23/2018)
            shorter_heat_1stR1 = metric.df.WT.E1R1[metric.df.WT.E1R1$Value < median(metric.df.WT.E1T1$Value), ] ##FLAG: may need tease out the R flies that do not have corresponding T flies in the shorter_heat_1stT1 (04/23/2018)
            
            # fly.info.First_T1_Above_Median = fly.info.include[
            #                                           (fly.info.include$Genotype%in%longer_heat_1stT1$Genotype)&
            #                                           (fly.info.include$Fly%in%longer_heat_1stT1$Fly)&
            #                                            # (fly.info.include$Genotype==longer_heat_1stT1$genotype) &
            #                                           (fly.info.include$experimenter%in%longer_heat_1stT1$Experimenter),
            #                                         ]
            #
            # fly.info.First_R1_Above_Median = fly.info.include[(fly.info.include$Genotype%in%longer_heat_1stR1$Genotype)&
            #                                                     (fly.info.include$Fly%in%longer_heat_1stR1$Fly)& # (fly.info.include$Genotype==longer_heat_1stT1$genotype) &
            #                                                     (fly.info.include$experimenter%in%longer_heat_1stR1$Experimenter),
            #                                                   ]
            
            # concurrent_sessions = rbind(fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))),],
            #                             fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))-1),],
            #                             fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))+1),],
            #                             fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))+2),]
            #                             )
            #
            
            
            ################################################segmentation based on all the heat received
            
            AllT1 = c()
            AllR1 = c()
            metric.df.WT.AllR1 = data.frame()
            metric.df.WT.AllT1 = data.frame()
            
            # for (i in 1:dim(metric.df.WT.E1T1)[1]){
            #   AllT1 = c(AllT1,sum(metric.df.WT[(metric.df.WT$Fly==metric.df.WT.E1T1[i,]$Fly)&(metric.df.WT$Experimenter==metric.df.WT.E1T1[i,]$Experimenter)&!is.na(metric.df.WT$Value),]$Value))
            # }
            #
            for (i in 1:dim(metric.df.WT.E1T1)[1]) {
              AllT1 = c(AllT1, sum(metric.df.WT[(metric.df.WT$Fly == metric.df.WT.E1T1[i, ]$Fly) &
                                                  (metric.df.WT$Experimenter == metric.df.WT.E1T1[i, ]$Experimenter) &
                                                  !is.na(metric.df.WT$Value), ]$Value[1:5]))
            }
            
            
            # for (i in 1:dim(metric.df.WT.E1R1)[1]){
            #   AllR1 = c(AllR1,sum(metric.df.WT[(metric.df.WT$Fly==metric.df.WT.E1R1[i,]$Fly)&(metric.df.WT$Experimenter==metric.df.WT.E1R1[i,]$Experimenter)&!is.na(metric.df.WT$Value),]$Value))
            # }
            
            for (i in 1:dim(metric.df.WT.E1R1)[1]) {
              AllR1 = c(AllR1, sum(metric.df.WT[(metric.df.WT$Fly == metric.df.WT.E1R1[i, ]$Fly) &
                                                  (metric.df.WT$Experimenter == metric.df.WT.E1R1[i, ]$Experimenter) &
                                                  !is.na(metric.df.WT$Value), ]$Value[1:5]))
            }
            
            
            metric.df.WT.AllT1 = cbind(metric.df.WT.E1T1,
                                       AllT1)
            colnames(metric.df.WT.AllT1) = c(
              "Value",
              "Fly",
              "Category",
              "Gender",
              "Genotype",
              "Experimenter",
              "Age",
              "Setup",
              "Session",
              "All_Heat_Received"
            )
            
            
            longer_heat_allT1 = metric.df.WT.AllT1[metric.df.WT.AllT1$All_Heat_Received >=
                                                     median(metric.df.WT.AllT1$All_Heat_Received), ]
            shorter_heat_allT1 = metric.df.WT.AllT1[metric.df.WT.AllT1$All_Heat_Received <
                                                      median(metric.df.WT.AllT1$All_Heat_Received), ]
            
            
            metric.df.WT.AllR1 = cbind(metric.df.WT.E1R1,
                                       AllR1)
            colnames(metric.df.WT.AllR1) = c(
              "Value",
              "Fly",
              "Category",
              "Gender",
              "Genotype",
              "Experimenter",
              "Age",
              "Setup",
              "Session",
              "All_Heat_Received"
            )
            
            # longer_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received>=median(metric.df.WT.AllT1$All_Heat_Received),]
            # shorter_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received<median(metric.df.WT.AllT1$All_Heat_Received),]
            
            longer_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received %in% longer_heat_allT1$All_Heat_Received, ]
            shorter_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received %in% shorter_heat_allT1$All_Heat_Received, ]
            
            metric.df.WT.T1 = rbind(longer_heat_allT1, shorter_heat_allT1)
            
            metric.df.WT.T1 = subset(metric.df.WT.T1, metric.df.WT.T1$Experimenter !=
                                       "RS")
            
            metric.df.WT.R1 = rbind(longer_heat_allR1, shorter_heat_allR1)
            metric.df.WT.R1 = subset(metric.df.WT.R1, metric.df.WT.R1$Experimenter !=
                                       "RS")
            
            #WT flies
            query.genotype <- c("WT","CS")
            query.fly = fly.info.include[((fly.info.include$Genotype == "WT") |
                                                      (fly.info.include$Genotype == "CS")) &
                                                     (fly.info.include$experimenter!="SW"), ]$Fly
            query.experimenter = fly.info.include[((fly.info.include$Genotype == "WT") |
                                            (fly.info.include$Genotype == "CS")) &
                                           (fly.info.include$experimenter!="SW"), ]$experimenter
            write.table(
              fly.info.include[((fly.info.include$Genotype == "WT") |
                                  (fly.info.include$Genotype == "CS")) &
                                 (fly.info.include$experimenter!="SW"), ],
              "fly_info_include_WT.csv",
              col.names = T,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            #SUN1 flies
            query.genotype <- c("SUN1")
            query.fly = fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                                           (fly.info.include$experimenter!="SW"), ]$Fly
            
            query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                                                    (fly.info.include$experimenter!="SW"), ]$experimenter
            write.table(
              fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                                 (fly.info.include$experimenter!="SW"), ],
              "fly_info_include_SUN1.csv",
              col.names = T,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            #SUN2 flies
            query.genotype <- c("SUN2")
            query.fly = fly.info.include[((fly.info.include$Genotype == "SUN2")) &
                                           (fly.info.include$experimenter!="SW"), ]$Fly
            
            query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN2")) &
                                                    (fly.info.include$experimenter!="SW"), ]$experimenter
            write.table(
              fly.info.include[((fly.info.include$Genotype == "SUN2")) &
                                 (fly.info.include$experimenter!="SW"), ],
              "fly_info_include_SUN2.csv",
              col.names = T,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            #SUN3 flies
            query.genotype <- c("SUN2")
            query.fly = fly.info.include[((fly.info.include$Genotype == "SUN3")) &
                                           (fly.info.include$experimenter!="SW"), ]$Fly
            
            query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN3")) &
                                                    (fly.info.include$experimenter!="SW"), ]$experimenter
            write.table(
              fly.info.include[((fly.info.include$Genotype == "SUN3")) &
                                 (fly.info.include$experimenter!="SW"), ],
              "fly_info_include_SUN3.csv",
              col.names = T,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            
            #R60D05 flies
            query.genotype <- c("R60 D05 x JU30","R60D05 X JU30","R60D05 X JU30")
            query.fly = fly.info.include[((fly.info.include$Genotype == "R60 D05 x JU30")|(fly.info.include$Genotype == "R60D05 X JU30")|(fly.info.include$Genotype =="R60D05 X JU30")),]$Fly
                                           # (fly.info.include$experimenter!="SW"), ]
            
            query.experimenter = fly.info.include[((fly.info.include$Genotype == "R60 D05 x JU30")|(fly.info.include$Genotype == "R60D05 X JU30")|(fly.info.include$Genotype =="R60D05 X JU30")),]$experimenter
                                                    # (fly.info.include$experimenter!="SW"), ]$experimenter
            write.table(
              fly.info.include[((fly.info.include$Genotype == "R60 D05 x JU30")|(fly.info.include$Genotype == "R60D05 X JU30")|(fly.info.include$Genotype =="R60D05 X JU30")),],
              "fly_info_include_R60D05.csv",
              col.names = T,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            #JG17
            query.genotype <- c("JG17 x JU30")
            query.fly = fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),]$Fly
            query.experimenter = fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),]$experimenter
            write.table(
              fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),],
              "fly_info_include_JG17.csv",
              col.names = T,
              row.names = F,
              quote = F,
              sep = ","
            )
            
            # query.fly = c(metric.df.WT.AllT1$Fly)
            # query.experimenter = c(metric.df.WT.AllT1$Experimenter)
            # query.fly <- c(longer_heat_1stT1$Fly,longer_heat_1stR1$Fly)
            # query.experimenter <- c(as.character(longer_heat_1stT1$Experimenter), as.character(longer_heat_1stR1$Experimenter))
            # query.fly <- c(shorter_heat_1stT1$Fly,shorter_heat_1stR1$Fly)
            # query.experimenter <- c(as.character(shorter_heat_1stT1$Experimenter), as.character(shorter_heat_1stR1$Experimenter))
            # query.fly <- c(longer_heat_allT1$Fly,longer_heat_allR1$Fly)
            # query.experimenter <- c(as.character(longer_heat_allT1$Experimenter), as.character(longer_heat_allR1$Experimenter))
            # query.fly <- c(shorter_heat_allT1$Fly,shorter_heat_allR1$Fly)
            # query.experimenter <- c(as.character(shorter_heat_allT1$Experimenter), as.character(shorter_heat_allR1$Experimenter))
            # query.fly <- c(metric.df.WT.T1$Fly,metric.df.WT.R1$Fly)
            # query.experimenter<-c(as.character(metric.df.WT.T1$Experimenter), as.character(metric.df.WT.R1$Experimenter))

            ## Read metric names
            metrices <- read.table("metrics/list_metrices.csv",
                                   stringsAsFactors = F,
                                   sep = "\t")[, 1]
            
            sessions <- c(
              "E1T1E1",
              "E1R1E1",
              "E1N1E1",
              "E1T1E1T1E1",
              "E1R1E1R1E1",
              "E1N1E1N1E1",
              "E1T1E1T1E1T1E1",
              "E1R1E1R1E1R1E1",
              "E1N1E1N1E1N1E1",
              "E1T1E1T1E1T1E1T1E1",
              "E1R1E1R1E1R1E1R1E1",
              "E1N1E1N1E1N1E1N1E1"
            )
            
            ##########Plot the relationship between amount of heat received and the Metric #29 ##########
            #
            # input.file = paste0("metrics/metric_29.csv")
            # metric_of_interest = read.csv(input.file)
            #
            # metric.df.WT.AllT1.Analysis = metric.df.WT.AllT1[(metric.df.WT.AllT1$Experimenter=="JD")|(metric.df.WT.AllT1$Experimenter=="ES"),]
            #
            # relation_of_E1_and_Total_Heat = cbind((metric.df.WT.AllT1.Analysis),
            #                                            metric_of_interest[((metric_of_interest$Fly%in%metric.df.WT.AllT1.Analysis$Fly)&
            #                                                                 (metric_of_interest$Experimenter%in%metric.df.WT.AllT1.Analysis$Experimenter))&
            #                                                                 (metric_of_interest$Genotype=="WT")&
            #                                                                 (metric_of_interest$Session=="E1") ,
            #                                                               ]$Value,
            #                                            metric_of_interest[((metric_of_interest$Fly%in%metric.df.WT.AllT1.Analysis$Fly)&
            #                                                                  (metric_of_interest$Experimenter%in%metric.df.WT.AllT1.Analysis$Experimenter))&
            #                                                                  (metric_of_interest$Genotype=="WT")&
            #                                                                  (metric_of_interest$Session=="E1T1E1"),
            #                                                               ]$Value,
            #                                            metric_of_interest[((metric_of_interest$Fly%in%metric.df.WT.AllT1.Analysis$Fly)&
            #                                                                  (metric_of_interest$Experimenter%in%metric.df.WT.AllT1.Analysis$Experimenter))&
            #                                                                  (metric_of_interest$Genotype=="WT")&
            #                                                                  (metric_of_interest$Session=="E1T1E1T1E1"),
            #                                                               ]$Value
            #                                       )
            #
            #
            # colnames(relation_of_E1_and_Total_Heat) = c("FirstT1_Heat","Fly","Category","Gender","Genotype","Experimenter","Age","Setup","Session","All_Heat_Received","1stE1","2ndE1","3rdE1")
            #
            # SecondT1_Heat = relation_of_E1_and_Total_Heat$All_Heat_Received - relation_of_E1_and_Total_Heat$FirstT1_Heat
            #
            # relation_simplified = data.frame(relation_of_E1_and_Total_Heat$FirstT1_Heat,SecondT1_Heat,relation_of_E1_and_Total_Heat$`1stE1`,relation_of_E1_and_Total_Heat$`2ndE1`,relation_of_E1_and_Total_Heat$`3rdE1`)
            #
            # colnames(relation_simplified) = c("FirstT1_Heat","SecondT1_Heat","1stE1","2ndE1","3rdE1")
            #
            
            
            
            
            # pdf("fly_metric_Less_Clicks_CS_1st_2nd_HeatDuration_3sessions_JD_050418_Updated.pdf",onefile=T,width=10)
            #
            # write.table(longer_heat_allT1, file = "2018_05_09_longer_heat_allT1.csv", append = FALSE, quote = TRUE, sep = ",",
            #             eol = "\n", na = "NA", dec = ".", row.names = TRUE,
            #             col.names = TRUE, qmethod = c("escape", "double"),
            #             fileEncoding = "")
            #
            #
            
            pdf(
              "fly_metric_allmetricdf_SUN3_allflies_Filter1_052118.pdf",
              onefile = T,
              width = 10
            )
            
            p_value_summary = matrix(nrow = 9, ncol = 0)
            for (metric.ind in 1:length(metrices)) {
              input.file = paste0("metrics/metric_", metric.ind, ".csv")
              if (!file.exists(input.file)) {
                next
              }
              
              metric.df = read.csv(input.file)
              
              if (sum(colnames(metric.df) %in% "value.w") == 0) {
                #metric.df$value.w = metric.df$value
                #next;
              }
              ## covariates of interest: genotype, session
              y = list()
              ## E1 data
              session = "E1"
              for (category in c("T", "R")) {
                query.session = gsub("X", category, session)
                ind <- metric.df$Session == query.session &
                  metric.df$Genotype %in% query.genotype &
                  metric.df$Category == category &
                  metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
                
                ind.E1 <- metric.df$Session == "E1" &
                  metric.df$Genotype %in% query.genotype &
                  metric.df$Category == category &
                  metric.df$Fly %in% query.fly&
                  metric.df$Experimenter  %in%  query.experimenter
                
                # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
                z = metric.df[ind, "Value"]
                
                y = append(y, list(na.omit(z)))
              }
              
              for (category in c("N")) {
                query.session = gsub("X", category, session)
                ind <- metric.df$Session == query.session &
                  metric.df$Genotype %in% query.genotype &
                  metric.df$Category == category &
                  metric.df$Fly %in% query.fly&
                  metric.df$Experimenter  %in%  query.experimenter
                
                ind.E1 <- metric.df$Session == "E1" &
                  metric.df$Genotype %in% query.genotype &
                  metric.df$Category == category &
                  metric.df$Fly %in% query.fly&
                  metric.df$Experimenter  %in%  query.experimenter
                
                # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
                z = metric.df[ind, "Value"]
                
                y = append(y, list(na.omit(z)))
              }
              
              
              ## input sessions data
              
              for (session in sessions) {
                if (grepl("N", session) == F) {
                  ind <- metric.df$Session == session &
                    metric.df$Genotype %in% query.genotype &
                    metric.df$Fly %in% query.fly&
                    metric.df$Experimenter  %in%  query.experimenter
                  
                } else{
                  ind <- metric.df$Session == session &
                    metric.df$Genotype %in% query.genotype
                    metric.df$Fly %in% query.fly&
                    metric.df$Experimenter  %in%  query.experimenter
                }
                
                # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
                z = metric.df[ind, "Value"]
                
                y = append(y, list(na.omit(z)))
              }
              y.1 = y
              
              # yrange = c(min(sapply(y,min)),max(sapply(y,max)))
              
              ## special cases
              y_text = c()
              print(metric.ind)
              if (metric.ind == 1) {
                yrange = c(0, 600)
                y_text = 610
              }
              
              if (metric.ind == 2) {
                yrange = c(0, 1)
                y_text = 0.8
              }
              
              if (metric.ind == 3) {
                yrange = c(0, 100)
                y_text = 100
              }
              
              if (metric.ind == 4) {
                yrange = c(0, 600)
                y_text = 600
              }
              
              if (metric.ind == 5) {
                yrange = c(0, 30)
                y_text = 30
              }
              
              if (metric.ind == 6) {
                yrange = c(0, 30)
                y_text = 30
              }
              
              if (metric.ind == 7) {
                yrange = c(0, 10)
                y_text = 8.5
              }
              
              if (metric.ind == 8) {
                yrange = c(0, 10)
                y_text = 8.3
              }
              
              
              if (metric.ind == 9) {
                yrange = c(0, 600)
                y_text = 600
              }
              
              if (metric.ind == 10) {
                yrange = c(0, 200)
                y_text = 160
              }
              
              if (metric.ind == 11) {
                yrange = c(0, 200)
                y_text = 110
              }
              
              if (metric.ind == 12) {
                yrange = c(0, 1)
                y_text = 1
              }
              
              if (metric.ind == 13) {
                yrange = c(-1, 1)
                y_text = 0.7
              }
              
              if (metric.ind == 14) {
                yrange = c(-1, 1)
                y_text = 0.85
              }
              
              if (metric.ind == 15) {
                yrange = c(-1, 1)
                y_text = 0.12
              }
              
              if (metric.ind == 16) {
                yrange = c(-1, 1)
                y_text = 0.42
              }
              
              if (metric.ind == 17) {
                yrange = c(-1, 1)
                y_text = 0.42
              }
              
              if (metric.ind == 18) {
                yrange = c(0, 600)
                y_text = 600
              }
              
              
              if (metric.ind == 19) {
                yrange = c(0, 600)
                y_text = 600
              }
              
              if (metric.ind == 20) {
                yrange = c(0.5, 1)
                y_text = 1.01
              }
              
              
              if (metric.ind == 21) {
                yrange = c(0, 0.5)
                y_text = 0.142
              }
              
              if (metric.ind == 22) {
                yrange = c(0, 1)
                y_text = 1
              }
              
              if (metric.ind == 23) {
                yrange = c(0, 1)
                y_text = 1
              }
              
              if (metric.ind == 24) {
                yrange = c(-0.6, 0.6)
                y_text = 0.6
              }
              
              if (metric.ind == 25) {
                yrange = c(-0.6, 0.6)
                y_text = 0.6
              }
              
              if (metric.ind == 26) {
                yrange = c(-0.4, 0.6)
                y_text = 0.6
              }
              
              if (metric.ind == 27) {
                yrange = c(-0.4, 0.6)
                y_text = 0.6
              }
              
              if (metric.ind == 28) {
                yrange = c(0, 100)
                y_text = 100
              }
              
              
              if (metric.ind == 29) {
                yrange = c(0, 1)
                y_text = 1
              }
              
              if (metric.ind == 30) {
                yrange = c(0, 600)
                y_text = 600
              }
              
              if (metric.ind == 31) {
                yrange = c(0, 2050)
                y_text = 250
              }
              
              print(yrange)
              print(y_text)
              
              
              # extra.title = "Training/Random/Blank"
              # col.pool <- c("indianred3","light blue","grey",
              #               "indianred3","light blue","grey",
              #               "indianred3","light blue","grey",
              #               "indianred3","light blue","grey",
              #               "indianred3","light blue","grey"
              # )
              #
              # input.y = y.1
              
              input.y = y.1[1:9]
              yy.1T = rep("1stE1_1", length(input.y[[1]]))
              yy.1R = rep("1stE1_2", length(input.y[[2]]))
              yy.1N = rep("1stE1_3", length(input.y[[3]]))
              yy.2T = rep("2ndE1_1", length(input.y[[4]]))
              yy.2R = rep("2ndE1_2", length(input.y[[5]]))
              yy.2N = rep("2ndE1_3", length(input.y[[6]]))
              yy.3T = rep("3rdE1_1", length(input.y[[7]]))
              yy.3R = rep("3rdE1_2", length(input.y[[8]]))
              yy.3N = rep("3rdE1_3", length(input.y[[9]]))
              yy.label = c(yy.1T, yy.1R, yy.1N, yy.2T, yy.2R, yy.2N, yy.3T, yy.3R, yy.3N)
              
              input.y_1T = as.numeric(input.y[[1]])
              input.y_1R = as.numeric(input.y[[2]])
              input.y_1N = as.numeric(input.y[[3]])
              input.y_2T = as.numeric(input.y[[4]])
              input.y_2R = as.numeric(input.y[[5]])
              input.y_2N = as.numeric(input.y[[6]])
              input.y_3T = as.numeric(input.y[[7]])
              input.y_3R = as.numeric(input.y[[8]])
              input.y_3N = as.numeric(input.y[[9]])
              input.yy = c(
                input.y_1T,
                input.y_1R,
                input.y_1N,
                input.y_2T,
                input.y_2R,
                input.y_2N,
                input.y_3T,
                input.y_3R,
                input.y_3N
              )
              
              input.y.df = data.frame(input.yy, yy.label)
              colnames(input.y.df) <- c("Value", "Sessions")
              
              
              # boxplot(NUMS ~ GRP, data = ddf, lwd = 2, ylab = 'NUMS')
              # stripchart(NUMS ~ GRP, vertical = TRUE, data = ddf,
              #            method = "jitter", add = TRUE, pch = 20, col = 'blue')
              #
              
              
              col.pool <- c(
                "indianred3",
                "light blue",
                "grey80",
                "indianred3",
                "light blue",
                "grey80",
                "indianred3",
                "light blue",
                "grey80"
              )
              
              boxplot(
                Value ~ Sessions,
                data = input.y.df,
                ylim = yrange,
                outline = F,
                notch = T,
                lwd = 2,
                ylab = metrices[metric.ind],
                xlab = "",
                xaxt = "n",
                col = col.pool
                # main = paste0(query.genotype, "-", metrices[metric.ind], "\n")
              )
              stripchart(
                Value ~ Sessions,
                vertical = TRUE,
                data = input.y.df,
                method = "jitter",
                add = TRUE,
                pch = 15,
                cex = 0.5,
                col =  "grey40"
              )
              
              
              if (any(is.na(yrange)) |
                  any(is.infinite(yrange)) | any(is.nan(yrange))) {
                yrange = c(-1, 1)
                ylim = c(-1, 1)
              }
              
              # boxplot(input.y,outline=F,notch=T,
              #         main=paste0(query.genotype, "-", metrices[metric.ind],"\n"
              #                     # extra.title
              #         ),
              #         col=col.pool,
              #         ylim=yrange,
              #         ylab=metrices[metric.ind],
              #         xaxt='n'
              # )
              text((1:length(input.y)) - 0.1,
                   # yrange[2]*0.6,
                   y_text,
                   paste0(sapply(input.y, length)),
                   xpd = T,
                   srt = 0,
                   adj = 0
              )
              
              n = length(input.y) / length(sessions)
              
              # for(i in c(3,6,9,13)){
              # for(i in c(3,6,9,12)){
              #   #for(i in c(3,6,10)){
              #   lines(c(i,i)+0.5,
              #         c(yrange[1]-1e3,yrange[1]+1e3),
              #         col="light grey",
              #         lty=1)
              # }
              
              for (i in c(3, 6)) {
                lines(c(i, i) + 0.5,
                      c(yrange[1] - 1e3, yrange[1] + 1e3),
                      col = "light grey",
                      lty = 1)
              }
              
              p_value = c(wilcox.test(input.y_1T,input.y_1N)$p.value,
                          wilcox.test(input.y_1R,input.y_1N)$p.value,
                          wilcox.test(input.y_1T,input.y_1R)$p.value,
                          wilcox.test(input.y_2T,input.y_2N)$p.value,
                          wilcox.test(input.y_2R,input.y_2N)$p.value,
                          wilcox.test(input.y_2T,input.y_2R)$p.value,
                          wilcox.test(input.y_3T,input.y_3N)$p.value,
                          wilcox.test(input.y_3R,input.y_3N)$p.value,
                          wilcox.test(input.y_3T,input.y_3R)$p.value
              )
              
              p_value_summary = cbind(p_value_summary,p_value)
            }
            dev.off()
            
            colnames(p_value_summary) = c(1:31)
            rownames(p_value_summary) = c("1T-1N",
                                          "1R-1N",
                                          "1T-1R",
                                          "2T-2N",
                                          "2R-2N",
                                          "2T-2R",
                                          "3T-3N",
                                          "3R-3N",
                                          "3T-3R"
              
            )
            
            write.table(
              p_value_summary,
              "P_VALUE_SUMMARY_SUN3.csv",
              col.names = T,
              row.names = T,
              quote = F,
              sep = ","
            )
            
            ###############################################
            
            #Also, convert "total laser exposure in seconds" into "total heat received".
            #Total heat received(THR) formula: THR = 9.125 (mW) * Total Laser Exposure in Seconds (unit: mille Joules)
            