setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")

fly.info.end = read.csv("data/fly_info_end.csv", header = T, stringsAsFactors = F)
fly.info.end_WT = fly.info.end[(fly.info.end$genotype=="WT")|(fly.info.end$genotype=="CS"),]
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)
all_ofs_mutants = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)
all_ofs = rbind(all_ofs_WT, all_ofs_mutants)

get_learning_index = function(fly.info.end, all_ofs, ind, ind_of_interest){
  fly = fly.info.end[ind, ]$fly
  geno = fly.info.end[ind, ]$genotype
  experimenter = fly.info.end[ind, ]$experimenter
  cat = fly.info.end[ind, ]$category
  
  fly_ofs = all_ofs[all_ofs$flynum == fly & all_ofs$genotype == geno &
                      all_ofs$experimenter == experimenter & all_ofs$type == cat, 
                    ]
  session_5_name = paste0("E1", cat, "1E1", cat, "1E1")
  learning_index = fly_ofs[fly_ofs$session==session_5_name, ind_of_interest] - 
                    fly_ofs[fly_ofs$session=="E1", ind_of_interest]
  return(learning_index)
}

fly.info.end$LearnIndex = NA
ind_of_interest = 9
for (ind in 1802:nrow(fly.info.end)){
  fly.info.end[ind, ]$LearnIndex = get_learning_index(fly.info.end, all_ofs, ind, ind_of_interest)
  print(fly.info.end[ind, ]$LearnIndex)
}

fly.WT.T = fly.info.end_WT[fly.info.end_WT$category=="T", ]
for (i in 3 : nrow(fly.WT.T)){
  WT.T = fly.WT.T[sample(nrow(fly.WT.T), size = i, replace = TRUE), ]
  WT.R = 
  
}





WT.T.data = data.frame()

for (i in 1:nrow(WT.T)){
  fly = WT.T[i, ]$fly
  geno = WT.T[i, ]$genotype
  cat = WT.T[i, ]$category
  experimenter = WT.T[i, ]$experimenter
  temp = all_ofs_WT[all_ofs_WT$flynum == fly & all_ofs_WT$type == cat &
                      all_ofs_WT$genotype == geno & all_ofs_WT$experimenter == experimenter, ]
  WT.T.data = rbind(WT.T.data, temp)
}

WT.R.data = data.frame()
for (i in 1:nrow(WT.R)){
  fly = WT.R[i, ]$fly
  geno = WT.R[i, ]$genotype
  cat = WT.R[i, ]$category
  experimenter = WT.R[i, ]$experimenter
  temp = all_ofs_WT[all_ofs_WT$flynum == fly & all_ofs_WT$type == cat &
                      all_ofs_WT$genotype == geno & all_ofs_WT$experimenter == experimenter, ]
  WT.R.data = rbind(WT.R.data, temp)
}

WT.N.data = data.frame()
for (i in 1:nrow(WT.N)){
  fly = WT.N[i, ]$fly
  geno = WT.N[i, ]$genotype
  cat = WT.N[i, ]$category
  experimenter = WT.N[i, ]$experimenter
  temp = all_ofs_WT[all_ofs_WT$flynum == fly & all_ofs_WT$type == cat &
                      all_ofs_WT$genotype == geno & all_ofs_WT$experimenter == experimenter, ]
  WT.N.data = rbind(WT.N.data, temp)
}

ses.T.1 = WT.T.data[WT.T.data$session == "E1", ind_of_interest]
ses.R.1 = WT.R.data[WT.R.data$session == "E1", ind_of_interest]
ses.N.1 = WT.N.data[WT.N.data$session == "E1", ind_of_interest]
ses.T.5 = WT.T.data[WT.T.data$session == "E1T1E1T1E1", ind_of_interest]
ses.R.5 = WT.R.data[WT.R.data$session == "E1R1E1R1E1", ind_of_interest]
ses.N.5 = WT.N.data[WT.N.data$session == "E1N1E1N1E1", ind_of_interest]
ret = c(mean(ses.T.1), mean(ses.R.1), mean(ses.N.1), mean(ses.T.5), mean(ses.R.5), mean(ses.N.5))