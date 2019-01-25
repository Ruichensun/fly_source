# Behavior states analysis - preliminary code
input.file = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1_WT.csv"
input.file2 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1_WT.csv"
input.file3 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1N1E1_WT.csv"

behavioral_state = function(input.file,  span){
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
  
  fly_speed = diff(c(x[data_start - 1], fly_pos))
  states = c()
  state = NULL
  
  bin = floor(experiment_time/span)
  
  for(i in seq(1, (bin - 1) * span , span)){
    a = sum(fly_speed[i: (i + span)])
    if (a <= (span / 2)){
      state = 0
    }else{state = 1}
    states = c(states, state)
  }
  return(states)
}