# Behavior states analysis - preliminary code
library(markovchain)
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")


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

get_all_states = function(session, fly.info){
  states = data.frame()
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
      framerate = fly.info$Framerate[ind]        
      states = behavioral_state(input.file, span=framerate)
      if (length(states)<550){
        print(input.file)
        next
      }else{
        states = states[1:550]
        states = data.frame(rbind(states, states))
      }
    }
  }
  colnames(states) =  c(1:550)
  return(states)
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

state_matrix_all = function(states_df, span){
  r = nrow(states_df)
  bin = 550
  p11_lst = c()
  p12_lst = c()
  p21_lst = c()
  p22_lst = c()
  
  for (i in 1:r){
    m = state_matrix(as.numeric(states_df[i, ]))
    p11_lst = c(p11_lst, m[1, 1])
    p12_lst = c(p12_lst, m[1, 2])
    p21_lst = c(p21_lst, m[2, 1])
    p22_lst = c(p22_lst, m[2, 2])
  }
  median_matrix = rbind(c(median(p11_lst, na.rm = T), median(p12_lst, na.rm = T)), c(median(p21_lst, na.rm = T), median(p22_lst, na.rm = T)))
  # print(median_matrix)
  mean_matrix = rbind(c(mean(p11_lst, na.rm = T), mean(p12_lst, na.rm = T)), c(mean(p21_lst, na.rm = T), mean(p22_lst, na.rm = T)))
  # print(mean_matrix)
  matrix = list(median_matrix, mean_matrix)
  return (matrix)
}
