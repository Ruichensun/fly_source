#Demo files
input.file = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1_WT.csv"
input.file2 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1_WT.csv"
input.file3 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1N1E1_WT.csv"

input.file4 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1_WT.csv"
input.file5 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1E1_WT.csv"
input.file6 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1E1T1E1_WT.csv"


# Some metrics from the Kottler paper: minimal IBI is 0.2 sec (which in my case will be equivalent of 10 frames)

IEI_survival = function(input.file,  speed_max_thres = 90){
  tryCatch({
    x = read.table(input.file, header = T, sep = ",", stringsAsFactors = F)
  }, error = function(e) {
    stop(paste0("Input file is empty!:\n","  Input files is: ", input_file, "\n\n"))
  })
  if (nrow(x) < 10) {stop(paste0("Input file is empty!:\n", "  Input files is: ", input_file, "\n\n"))}
  x = as.numeric(x[[1]])
  data_start = 31
  fly_pos = x[data_start:min(600 * framerate, length(x))]
  experiment_time = length(fly_pos)
  
  # Remove system noise using speed threshold
  fly_speed = diff(c(x[data_start - 1], fly_pos))
  for (i in 1:length(fly_pos)) {
    if (abs(fly_speed[i]) >= speed_max_thres) {
      fly_speed[i] = 0
    }
  }
  
  IE = replace(abs(fly_speed), abs(fly_speed) > 0, 1)
  cs = cumsum(IE)
  s = length(cs)
  cs = 1 + log(1 - (cs/s))
  return(cs)
}

get_all_survival_WT = function(session, fly.info){
  survivals = list()
  for(ind in 1:nrow(fly.info)){
    input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",session, "_WT",".csv"),
                             full.names=T)
    if(length(input.file) == 0){
      next
    }else{
      framerate = fly.info$Framerate[ind]
      survival = IEI_survival(input.file, speed_max_thres = 90)
      survivals = append(survivals, list(survival))
    }
  }
  return(survivals)
}

plot_all_survival_WT = function(survivals){
  plot(1, type="n", xlab="", ylab="", xlim=c(0, 30000), ylim=c(0, 1))
  for (i in (1:length(survivals))){
    lines(survivals[[i]])
  }
}
