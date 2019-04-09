fly.info.movement.T = fly.info.end[(fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[(fly.info.end$Category == "R") , ]
fly.info.movement.N = fly.info.end[(fly.info.end$Category == "N") , ]

all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)
all_ofs_mutants = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)
all_ofs = rbind(all_ofs_WT, all_ofs_mutants)
T1 = Hit_by_laser("E1T1", fly.info.movement.T)
T1 = T1[!is.na(T1$Hit_W), ]
T1$Diff = T1$Hit_W - T1$Hit_P
temp = T1[(T1$Diff>=0.2)|(T1$Hit_All==0),]

R1 = Hit_by_laser("E1R1", fly.info.movement.R)
R1 = R1[!is.na(R1$Hit_W), ]
R1$Diff = R1$Hit_W - R1$Hit_P

N1 = Hit_by_laser("E1N1", fly.info.movement.N)
N1 = N1[!is.na(N1$Hit_W), ]
N1$Diff = N1$Hit_W - N1$Hit_P

fly.info.end = rbind(temp, R1, N1)
