setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/Special_No_Stress")

filename = "RawData_Fly241_E1_F_2015Jul17.csv"

file = read.csv(filename, header = T, stringsAsFactors = F)


lines(file[, 100])
a = c()

for (i in 1:768){
  temp = c()
  for (j in 101:110){
    temp = c(temp, file[[j]][i])
  }
  temp = mean(temp)
  a = c(a, temp)
}
plot(a, type = "l")