setwd("D:/Behavioral_project/behavior_experiment_data/Analysis")
library(survival)
library(ggplot2)
library(dplyr)
library(ggfortify)

# Follow instructions here: https://rviews.rstudio.com/2017/09/25/survival-analysis-with-r/

fly.info.end = read.csv("data/fly_info_end.csv", header = T, stringsAsFactors = F)
fly.info.end = fly.info.end[fly.info.end$Age!="" & !is.na(fly.info.end$Age),]
Status = rep(1, nrow(fly.info.end))
fly.info.end = data.frame(fly.info.end, Status)


fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category =="T"), ]
T1 = Hit_by_laser("E1T1", fly.info.movement.T)
T1 = T1[!is.na(T1$Hit_W), ]

fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "R") , ]

R1 = Hit_by_laser("E1R1", fly.info.movement.R)
R1 = R1[!is.na(R1$Hit_W), ]

fly.info.movement.N = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                      (fly.info.end$Genotype == "CS")) & 
                                     (fly.info.end$Category == "N") , ]

N1 = Hit_by_laser("E1N1", fly.info.movement.N)
N1 = N1[!is.na(N1$Hit_W), ]

total = data.frame(rbind(T1, R1, N1))
for (ind in 1:nrow(total)){
  if (total[ind, ]$Category == "T"){
    total[ind, ]$Category = "Training Group"
  }else if(total[ind, ]$Category == "R"){
    total[ind, ]$Category = "Yoked Control"
  }else if(total[ind, ]$Category == "N"){
    total[ind, ]$Category = "Blank Control"
  }
}

total_fit = survfit(Surv(as.numeric(Age), Status) ~ Category, data = total)

plot("Survival_plot.pdf",width = 8, height = 8)
graph = autoplot(total_fit, pval = TRUE, CI = TRUE, surv.size = 2, xlab = "", ylab = "") + 
        theme_bw() +
        theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
        theme(text = element_text(size=20),
        axis.text.x = element_text(angle=0, hjust=1))+
        scale_fill_manual(values=c("grey", "indianred3", "light blue")) + 
        scale_color_manual(values=c("black", "red", "blue"))
dev.off()
