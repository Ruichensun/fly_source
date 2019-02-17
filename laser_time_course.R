setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
require(plot_trend.R)

fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]
fly.info.movement.N = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "N") , ]

sessions <- c(
  "E1T1",
  "E1R1",
  "E1N1",
  "E1T1E1T1",
  "E1R1E1R1",
  "E1N1E1N1"
)

input.file = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1_WT.csv"
get_laser_df(input.file)



colnames(pause_df) = c(
  "Start_Index",
  "Start_Position",
  "Start_Type",
  "End_Index",
  "End_Position",
  "End_Type",
  "Pause_Duration"
)


file_path = unlist(strsplit(input_file, "/"))
Experimenter = file_path[2]
file_name = unlist(strsplit(file_path[4], "_"))
file_session = file_name[3]
genotype = unlist(strsplit(file_name[4],".csv"))[1]
fly_num = as.integer(unlist((strsplit(file_name[2], "Fly")))[2])


tryCatch({
  x = read.table(input_file, header = T, sep = ",", stringsAsFactors = F)
}, error = function(e) {
  stop(paste0("Input file is empty!:\n","  Input files is: ", input_file, "\n\n"))
})
if (nrow(x) < 10) {stop(paste0("Input file is empty!:\n", "  Input files is: ", input_file, "\n\n"))}
x = as.numeric(x[[1]])
data_start = 31
fly_pos = x[data_start:min(600 * framerate, length(x))]
set_time = length(fly_pos)
experiment_time = length(fly_pos)

# Remove system noise using speed threshold
fly_speed = diff(c(x[data_start - 1], fly_pos))
for (i in 1:length(fly_pos)) {
  if (abs(fly_speed[i]) >= speed_max_thres) {
    fly_speed[i] = 0
  }
}

pause_df = get_pause_df(fly_pos, fly_speed)


# get_latency = function(input.file, framerate, set_length){
#   fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
#   fly.position.raw = as.numeric(fly.file[[1]])
#   fly.laser.raw = as.numeric(fly.file[[2]])
#   data_start = 31
#   fly_pos = fly.position.raw[data_start:set_length]
#   fly_laser = fly.laser.raw[data_start:set_length]
#   if (fly_laser[length(fly_laser)] > 0) {
#     fly_laser[length(fly_laser)] = 0
#   }
#   if (fly_laser[1] > 0) {
#     fly_laser[1] = 0
#   }
#   for (i in 1:length(fly_laser)) {
#     if (fly_laser[i] > 0) {
#       fly_laser[i] = 1
#     }
#   }
#   laser_df = get_laser_df(fly_laser, framerate)
#   
#   fly_speed = diff(c(fly.position.raw[data_start-1], fly_pos))
#   for (i in 1:length(fly_pos)) {
#     if (abs(fly_speed[i]) >= speed_max_thres) {
#       fly_speed[i] = 0
#     }
#   }
#   pause_df = get_pause_df(fly_pos, fly_speed)
#   
#   for i in 
#   
# }