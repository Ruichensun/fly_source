# Behavior states analysis - preliminary code
library(markovchain)

input.file = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1_WT.csv"
input.file2 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1_WT.csv"
input.file3 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly256_E1N1E1N1E1_WT.csv"

input.file4 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1_WT.csv"
input.file5 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1E1_WT.csv"
input.file6 = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/JD/CS/ProcessedData_Fly254_E1T1E1T1E1_WT.csv"


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
  return (states)
}


## WT
states_WT = data.frame()
fly.info = fly.info.end[fly.info.end$Genotype %in% c("WT", "CS"),]

for(ind in 1:nrow(fly.info)){
  # print(paste0("data/", fly.info$Experimenter[ind], "/CS/", "ProcessedData_Fly",fly.info$Fly[ind]))
  for(ind.session in 1:length(query.sessions)){
    input.file <- list.files(path = paste0("data/", fly.info$Experimenter[ind], "/CS/"),                             
                             pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                             full.names=T)
    if(length(input.file) == 0){next
    }else{
      framerate = fly.info$Framerate[ind]        
      states = behavioral_state(input.file, span=framerate)
      Type = fly.info$Category[ind]
      if (length(states)<550){
        print(paste0("data/", fly.info$Experimenter[ind], "/CS/", "ProcessedData_Fly",fly.info$Fly[ind]))
        next
      }else{
        states = cbind(Type, states[1:550])
        states_WT = rbind(states_WT, states)
        }
    }
  }
}



state_matrix = function(states){
  p11 = 0
  p12 = 0
  p21 = 0
  p22 = 0
  # state 1: pause, state 2: walking
  for (i in 2:length(states)){
    if ((states[i] == 0) & (states[i-1] == 0)){
      p11 = p11 + 1
    }else if ((states[i] == 0) & (states[i-1] == 1)){
      p12 = p12 + 1
    }else if ((states[i] == 1) & (states[i-1] == 0)){
      p21 = p21 + 1
    }else{
      p22 = p22 + 1
    }
  }
  t1 = p11 + p12
  t2 = p21 + p22
  m = rbind(c(p11/t1, p12/t1), c(p21/t2, p22/t2))
  return(m)
}

# count the number of 01, 11, 10, and 00.

matrix.power <- function(A, n) {   # only works for diagonalizable matrices
  e <- eigen(A)
  M <- e$vectors   # matrix for changing basis
  d <- e$values    # eigen values
  return(M %*% diag(d^n) %*% solve(M))
}
