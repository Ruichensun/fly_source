input_file = "D:/Behavioral_project/behavior_experiment_data/Analysis/data/ES/CS/ProcessedData_Fly402_E1T1E1T1E1_WT.csv"

session5T = data.frame(do.call(rbind, all_ofs_WT[["E1T1E1T1E1"]]))
Tgroup = unlist(session5T[,29])
NewTgroup = Tgroup[!is.na(Tgroup)]


session5R = data.frame(do.call(rbind, all_ofs_WT[["E1R1E1R1E1"]]))

Rgroup = unlist(session5R[,29])
NewRgroup = Rgroup[!is.na(Rgroup)]

session5N = data.frame(do.call(rbind, all_ofs_WT[["E1N1E1N1E1"]]))

Ngroup = unlist(session5N[,29])
NewNgroup = Ngroup[!is.na(Ngroup)]

v1 = rep("NewTGroup", length(NewTgroup))
v2 = rep("NewRGroup", length(NewRgroup))
v3 = rep("NewNGroup", length(NewNgroup))
V = as.factor(c(v1, v2, v3))

DF = as.numeric(c(NewTgroup, NewRgroup, NewNgroup))

df = data.frame(cbind(DF, V))
colnames(df) = c("Value", "Name")

dunn.test(x = as.numeric(df$Value), g = as.factor(df$Name), method = c("bonferroni"))