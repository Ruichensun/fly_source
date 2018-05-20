# To run this file, one needs to first obtain "metric.df.WT.R1" information from the "laser_power_based_segmentation.R" file.

####Testing a few samples####
setwd(
  "D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/data/JD/CS/"
)

# moving_status = function(input_file){
#
#   # a = read.csv("ProcessedData_Fly230_E1T1_WT.csv",header=T,stringsAsFactors=F)
#   a = read.csv(input_file,header=T,stringsAsFactors=F)
#   speed_threshold = 28 # This is determined by quantile(abs(fly_moving_status),c(0.97, 0.975, 0.98)), and the 97.5% corresponds to 28.6
#   framerate = 50
#   ##Finding out the fly's moving status by two criteria: velocity = 0 or velocity much larger than a speed threshold
#   fly_pos = a$fly_pos.framerate.50
#   starting_point = 21
#   fly_pos = fly_pos[starting_point:length(fly_pos)]
#   fly_moving_status = c(diff(c(a$fly_pos.framerate.50[starting_point-1],fly_pos)))
#   fly_moving_status_discretized = replace(fly_moving_status, fly_moving_status>28,0)
#   fly_moving_status_discretized = replace(fly_moving_status_discretized, fly_moving_status_discretized!=0,1)
#
#   # for (i in 2:(length(fly_moving_status_discretized)-1)){
#   #   if ((fly_moving_status_discretized[i-1]==0)&(fly_moving_status_discretized[i+1]==0)){
#   #     fly_moving_status_discretized[i]=0
#   #     }
#   #   if ((fly_moving_status_discretized[i-1]==1)&(fly_moving_status_discretized[i+1]==1)){
#   #   fly_moving_status_discretized[i]=1
#   #     }
#   #   }
#
#
#
#   # for troubleshooting
#   # a = replace(fly_moving_status_discretized,fly_moving_status_discretized==1,fly_moving_status)
#   # a = replace(a,a==0,100)
#
#   ###Labeling the No-Move and Move moments
#
#   # label_for_pause = rep(0, length(fly_moving_status_discretized))
#   #
#   # for (i in 2:length(label_for_pause)){
#   #   if ((fly_moving_status_discretized[i]==0)&(fly_moving_status_discretized[i-1]>0)){
#   #     label_for_pause[i] = 1
#   #   }
#   #   else if ((fly_moving_status_discretized[i]>0)&(fly_moving_status_discretized[i-1]==0)){
#   #     label_for_pause[i] = 2
#   #   }
#   #   # else if ((fly_moving_status_discretized[i]<0)&(fly_moving_status_discretized[i-1]==0)){
#   #   #   label_for_pause[i] = 3
#   #   # }
#   #   # else if ((fly_moving_status_discretized[i]==0)&(fly_moving_status_discretized[i-1]<0)){
#   #   #   label_for_pause[i] = 4
#   #   # }
#   # }
#
#   run_length_movement = rle(fly_moving_status_discretized)
#   Moving = run_length_movement$lengths[run_length_movement$values==1]
#   Pause = run_length_movement$lengths[run_length_movement$values==0]
#   Movement_Difference = c()
#
#   # length(rle(fly_moving_status_discretized)$length[rle(fly_moving_status_discretized)$values==0])
#   run_length_movement$values[length(run_length_movement$values)]
#
#   if ((run_length_movement$values[1]==0)&(run_length_movement$values[length(run_length_movement$values)]==1)){
#     Moving = c(0,Moving)
#     Pause = c(Pause,0)
#     Movement_Difference = Pause - Moving
#     }
#
#   if ((run_length_movement$values[1]==0)&(run_length_movement$values[length(run_length_movement$values)]==0)){
#     Moving = c(0,Moving)
#     Movement_Difference = Pause - Moving
#     }
#
#   if ((run_length_movement$values[1]==1)&(run_length_movement$values[length(run_length_movement$values)]==0)){
#     Movement_Difference = Pause - Moving
#     }
#
#   if ((run_length_movement$values[1]==1)&(run_length_movement$values[length(run_length_movement$values)]==1)){
#     Pause = c(Pause,0)
#     Movement_Difference = Pause - Moving
#     }
#
#   Movement_Difference = Movement_Difference/framerate
#   normalized_x = (1:length(Movement_Difference))/length(Movement_Difference)
#
#   # Movement_Difference3 = list(normalized_x,Movement_Difference)
#   # Movement_Difference2 = list(normalized_x,Movement_Difference)
#   # Movement_Difference1 = list(normalized_x,Movement_Difference)
#   Movement_Difference = list(normalized_x,Movement_Difference)
#   names(Movement_Difference) = c("Pairs", "Duration")
#   # return(Movement_Difference)
#   return(cumsum(fly_moving_status_discretized))
# }

#looping the moving_status function over all sessions

sessions <- c("E1T1",
              "E1T1E1T1",
              
              "E1R1",
              "E1R1E1R1",
              
              "E1N1",
              "E1N1E1N1")

query.sessions = sessions

count = 0
spec = NULL
fly.names = NULL

## WT
## ms - moving_status
all_ms_WT = list()
for (session in sessions) {
  all_ms_WT = append(all_ms_WT, list(c()))
}

## read the relevant fly infos

fly.info.movement.T = fly.info.include[(fly.info.include$Genotype %in% metric.df.WT.T1$Genotype) &
                                         (fly.info.include$Fly %in% metric.df.WT.T1$Fly) &
                                         (fly.info.include$Genotype == "WT") &
                                         # (fly.info.include$experimenter%in%metric.df.WT.T1$Experimenter),
                                         (fly.info.include$experimenter == "ES"),]

fly.info.movement.T = rbind(fly.info.movement.T,
                            fly.info.include[(fly.info.include$Genotype %in%
                                                metric.df.WT.T1$Genotype) &
                                               (fly.info.include$Fly %in% metric.df.WT.T1$Fly) &
                                               (fly.info.include$Genotype == "WT") &
                                               # (fly.info.include$experimenter%in%metric.df.WT.T1$Experimenter),
                                               (fly.info.include$experimenter == "JD"),])

###This(above) is where I left off at 2pm May 14, 2018


# fly.info.First_T1_Above_Median = fly.info.include[
#                                           (fly.info.include$Genotype%in%longer_heat_1stT1$Genotype)&
#                                           (fly.info.include$Fly%in%longer_heat_1stT1$Fly)&
#                                            # (fly.info.include$Genotype==longer_heat_1stT1$genotype) &
#                                           (fly.info.include$experimenter%in%longer_heat_1stT1$Experimenter),
#                                         ]



for (ind in 1:nrow(fly.info.movement.T)) {
  # query.sessions = gsub("X",fly.info$Category[ind],sessions)
  query.sessions = c("E1T1", "E1T1E1T1")
  for (ind.session in 1:length(query.sessions)) {
    input.file <- list.files(
      path = paste0("data/",
                    fly.info.movement.T$experimenter[ind],
                    "/CS/"),
      pattern = paste0(
        "ProcessedData_Fly",
        fly.info.movement.T$Fly[ind],
        "_",
        query.sessions[ind.session],
        "_WT",
        ".csv"
      ),
      full.names = T
    )
    if (length(input.file) == 0) {
      all_ms_WT[[sessions[ind.session]]] = append(all_ms_WT[[sessions[ind.session]]], list(NA))
      next
    }
    
    # framerate = fly.info.movement.T$Framerate[ind]
    ms = moving_status(input.file)
    
    all_ms_WT[[sessions[ind.session]]] = append(all_ms_WT[[sessions[ind.session]]], list(ms))
  }
}
