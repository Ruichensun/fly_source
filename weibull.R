source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

# Some metrics from the Kottler paper: minimal IBI is 0.2 sec (which in my case will be equivalent of 10 frames)

IEI_survival = function(input.file,  speed_max_thres = 90, framerate = 50){
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
  pause_df = get_pause_df(fly_speed)
  real_pause_df = pause_df[pause_df$Pause_Duration>=25,]
  cdf = ecdf(real_pause_df$Pause_Duration/framerate)
  len_cdf = dim(real_pause_df)[1]
  surv = c()
  for (i in 1:len_cdf){
    surv = c(surv, cdf(i))
  }
  return(surv)
}

get_all_survival = function(session, fly.info){
  file_names = c()
  sequence_lengths = c()
  for(ind in 1:nrow(fly.info)){
    if(fly.info$Genotype[ind]=="WT"){
      input.file = list.files(
        path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),
                      pattern = paste0("ProcessedData_Fly", fly.info$Fly[ind], "_", session, "_WT", ".csv"),
                      full.names = T
                     )
    }
    if(fly.info$Genotype[ind]=="CS"){
      input.file = list.files(path = paste0("data/", fly.info$Experimenter[ind], "/mutants/"),
                              pattern = paste0("ProcessedData_Fly", fly.info$Fly[ind], "_", session, "_CS", ".csv"),
                              full.names = T)
    }
    if(length(input.file) == 0){
      next
    }else{
      file_names = c(file_names, input.file)
      sequence = length(IEI_survival(input.file, speed_max_thres = 90, framerate = 50))
      sequence_lengths = c(sequence_lengths, sequence)
    }
  }
  max_sequence_length = max(sequence_lengths)
  survivals = matrix(data = NA, nrow = max_sequence_length, ncol = dim(fly.info)[1])
  for (i in (1:length(file_names))){
    temp = IEI_survival(file_names[i], speed_max_thres = 90, framerate = 50)
    survivals[,i][1:length(temp)] = temp
  }
  return(survivals)
}

fit_weibull <- function(x)
{
  xbar <- mean(x)
  varx <- var(x)
  f <- function(b){return(gamma(1+2/b)/gamma(1+1/b)^2 - 1 - varx/xbar^2)}
  bhat <- uniroot(f,c(0.02,50))$root
  ahat <- xbar/gamma(1+1/bhat)
  return(c(ahat,bhat))
}

E1_survivals = get_all_survival("E1", fly.info)
E1T1E1T1E1_survivals = get_all_survival("E1T1E1T1E1", fly.info)
E1N1E1N1E1_survivals = get_all_survival("E1N1E1N1E1", fly.info)
E1R1E1R1E1_survivals = get_all_survival("E1R1E1R1E1", fly.info)

plot(1, type="n", xlab="", ylab="", xlim=c(0, 100), ylim=c(0.5, 1))
for (i in 1:dim(E1_survivals)[1]){
  lines(E1_survivals[,i],  col = "red")
}
for (i in 1:dim(E1T1E1T1E1_survivals)[1]){
  lines(E1T1E1T1E1_survivals[,i],  col = "purple")
}
for (i in 1:dim(E1R1E1R1E1_survivals)[1]){
  lines(E1R1E1R1E1_survivals[,i],  col = "blue")
}
for (i in 1:dim(E1N1E1N1E1_survivals)[1]){
  lines(E1N1E1N1E1_survivals[,i],  col = "black")
}
# All four groups are the same.

# Also, if median or mean is needed, please see below for scripts.
# E1_survivals_mean = rowMeans(E1_survivals, na.rm = T)
# E1_survivals_median = apply(E1_survivals, 1, function(x) median(x, na.rm = T))
# E1T1E1T1E1_survivals_mean = rowMeans(E1T1E1T1E1_survivals, na.rm = T)
# E1T1E1T1E1_survivals_median = apply(E1T1E1T1E1_survivals, 1, function(x) median(x, na.rm = T))
# E1N1E1N1E1_survivals_mean = rowMeans(E1N1E1N1E1_survivals, na.rm = T)
# E1N1E1N1E1_survivals_median = apply(E1N1E1N1E1_survivals, 1, function(x) median(x, na.rm = T))
# E1R1E1R1E1_survivals_mean = rowMeans(E1R1E1R1E1_survivals, na.rm = T)
# E1R1E1R1E1_survivals_median = apply(E1R1E1R1E1_survivals, 1, function(x) median(x, na.rm = T))
