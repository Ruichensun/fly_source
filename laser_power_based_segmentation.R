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
    for (i in 1:length(array.session)) {
      array.session[i] = gsub("X", fly.info.include$Category[i], array.session[i])  # Change X to T/R/N
    }
    array.age = fly.info.include$Age

    metric.df <- rbind(
      metric.df,
      cbind(
        metric,
        fly.info.include$Fly,
        fly.info.include$Category,
        fly.info.include$Gender,
        fly.info.include$Genotype,
        fly.info.include$experimenter,
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

#Also, convert "total laser exposure in seconds" into "total heat received".
#Total heat received(THR) formula: THR = 9.125 (mW) * Total Laser Exposure in Seconds (unit: mille Joules)
