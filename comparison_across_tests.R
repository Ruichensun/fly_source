setwd("C:/Users/Ruichen/Desktop/data/csvdata")


##Import the function for obtaining statistics of each experiment
source("get_fly_speed_and_position.R")
source("get_fly_pause_smooth.R")
source("get_fly_pause_nan30.R")
source("smoothening.R")
source("normalization_against_E1.R")
source("one_fly_stats_without_normalization.R")
library(Hmisc)

fly_group_stats_without_normalization <- function(flies, framerate){
  flies_stats = list()
  
  for (i in 1:length(flies)){
    flies_stats = append(flies_stats, 
                         list(one_fly_stats_without_normalization(flies[i],framerate)))
  }
  names(flies_stats) = paste0("Fly_",flies)
  return(flies_stats)
  
}

fly_labels = read.csv("fly_labels.csv",header=T,stringsAsFactors = F)

flies_T = fly_labels$Fly.[fly_labels$Category=="T" & fly_labels$Fly. <=203 & fly_labels$Fly. >= 84]
flies_R = fly_labels$Fly.[fly_labels$Category=="R" & fly_labels$Fly. <=203  & fly_labels$Fly. >= 84]

flies_stats_T = fly_group_stats_without_normalization(flies_T,19)
flies_stats_R = fly_group_stats_without_normalization(flies_R,19)

sessions_T = c("E1","E1T1E1","E1T1E1T1E1","E1T1E1T1E1T1E1")
sessions_R = c("E1","E1R1E1","E1R1E1R1E1","E1R1E1R1E1R1E1")

stats_T = NULL
for (e in sessions_T){##loop over sessions E1 and E1X1E1 (assume all flies have these two)
  x = NULL
  for (j in 1:length(flies_T)){##loop over flies
    if(e %in% names(flies_stats_T[[j]])){
      x = c(x,mean(flies_stats_T[[j]][[e]][[6]],na.rm=T))
    }
    else
    {
      x = c(x,NA)
    }
  }
  stats_T = cbind(stats_T,x)
}

stats_R = NULL
for (e in sessions_R){##loop over sessions E1 and E1X1E1 (assume all flies have these two)
  x = NULL
  for (j in 1:length(flies_R)){##loop over flies
    if(e %in% names(flies_stats_R[[j]])){
      x = c(x,mean(flies_stats_R[[j]][[e]][[6]],na.rm=T))
    }
    else
    {
      x = c(x,NA)
    }
  }
  stats_R = cbind(stats_R,x)
}

colnames(stats_T) = sessions_T
colnames(stats_R) = sessions_R

# stats_T = na.omit(stats_T)
# stats_R = na.omit(stats_R)

pdf("performance_across_tests.pdf")
layout(t(1:2))
#
plot(c(1,ncol(stats_T)),c(0,max(rbind(stats_T,stats_R),na.rm=T)),type='n',
     ylab="Total distance per min",xaxt='n',
     main="T")
for(i in 1:nrow(stats_T)){
  lines(1:ncol(stats_T),stats_T[i,],col="blue")
}
text(1:nrow(stats_T),-500,labels = sessions_T,
     xpd=T,srt=30,adj=1,cex=0.6)
#
plot(c(1,ncol(stats_R)),c(0,max(rbind(stats_T,stats_R),na.rm=T)),type='n',
     ylab="Total distance per min",xaxt='n',
     main="R")
for(i in 1:nrow(stats_R)){
  lines(1:ncol(stats_R),stats_R[i,],col="red")
}
text(1:nrow(stats_R),-500,labels = sessions_R,
     xpd=T,srt=30,adj=1,cex=0.6)
dev.off()

#Test for STD
var.test(stats_T[,1],stats_T[,2])
var.test(stats_R[,1],stats_R[,2])

#Test for Average
t.test(stats_T[,1],stats_T[,2])
t.test(stats_R[,1],stats_R[,2])

