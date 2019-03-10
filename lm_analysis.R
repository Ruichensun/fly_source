fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)


fly.info.movement.T = fly.info.end[(fly.info.end$Genotype == "D2R-1 x 51635") & 
                                     (fly.info.end$Category =="T"), ]
fly.info.movement.R = fly.info.end[(fly.info.end$Genotype == "D2R-1 x 51635") & 
                                     (fly.info.end$Category =="R"), ]
all_ofs_WT = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)