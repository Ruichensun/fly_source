setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")

# Segmenting WT flies into long-exposure group and short-exposure group 

# Load  training data and test data
all_ofls_WT = read.csv("all_ofls_WT.csv", header = T, stringsAsFactors = F)
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)

# Some of the samples do not have laser exposure info (denoted as -1)
# Subset the samples with laser exposure recorded. 

ofls = subset(all_ofls_WT, Laser_Exposure > 0)

for (i in 2:nrow(ofls)){
  if ((ofls[i, ]$Fly.Number == ofls[i-1, ]$Fly.Number)&&
      (ofls[i, ]$Experimenter == ofls[i-1, ]$Experimenter)){
    ofls[i, ]$Laser_Exposure = ofls[i, ]$Laser_Exposure + ofls[i-1, ]$Laser_Exposure
    ofls[i, ]$Laser_Count = ofls[i, ]$Laser_Count + ofls[i-1, ]$Laser_Count
  }
}

laser_T = ofls[ofls$Session=="E1T1E1T1" & ofls$Laser_Count < 45, ]
laser_R = ofls[ofls$Session=="E1R1E1R1" & ofls$Laser_Count < 45, ]



# Remember to remove flies that receive more than 45 clicks
med = median(laser_T$Laser_Count)
T_med_abv = laser_T[laser_T$Laser_Count >= med, ]
T_med_bel = laser_T[laser_T$Laser_Count < med, ]
R_med_abv = laser_R[laser_R$Laser_Count >= med, ]
R_med_bel = laser_R[laser_R$Laser_Count < med, ] 

# Get fly activity for the two groups
T_abv_test = subset(all_ofs_WT, 
                    ((all_ofs_WT$Experimenter=="ES") & 
                       (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="ES",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="RS") & 
                         (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="RS",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="JD") & 
                         (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="JD",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="SW") & 
                         (all_ofs_WT$Fly.Number %in% T_med_abv[T_med_abv$Experimenter=="SW",]$Fly.Number))
)

T_bel_test = subset(all_ofs_WT, 
                    ((all_ofs_WT$Experimenter=="ES") & 
                       (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="ES",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="RS") & 
                         (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="RS",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="JD") & 
                         (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="JD",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="SW") & 
                         (all_ofs_WT$Fly.Number %in% T_med_bel[T_med_bel$Experimenter=="SW",]$Fly.Number))
)

R_abv_test = subset(all_ofs_WT, 
                    ((all_ofs_WT$Experimenter=="ES") & 
                       (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="ES",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="RS") & 
                         (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="RS",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="JD") & 
                         (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="JD",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="SW") & 
                         (all_ofs_WT$Fly.Number %in% R_med_abv[R_med_abv$Experimenter=="SW",]$Fly.Number))
)

R_bel_test = subset(all_ofs_WT, 
                    ((all_ofs_WT$Experimenter=="ES") & 
                       (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="ES",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="RS") & 
                         (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="RS",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="JD") & 
                         (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="JD",]$Fly.Number))|
                      ((all_ofs_WT$Experimenter=="SW") & 
                         (all_ofs_WT$Fly.Number %in% R_med_bel[R_med_bel$Experimenter=="SW",]$Fly.Number))
)


# Prepare boxplot 
num = c(length(T_abv_test[T_abv_test$Session=="E1T1E1T1E1", ]$Percentage.Time.Active),
        length(T_bel_test[T_bel_test$Session=="E1T1E1T1E1", ]$Percentage.Time.Active),
        length(R_abv_test[R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active),
        length(R_bel_test[R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active))

m = data.frame(
  factor = c(rep("T-Top50", length(T_abv_test[T_abv_test$Session=="E1T1E1T1E1", ]$Percentage.Time.Active)),
             rep("T-Bottom50", length(T_bel_test[T_bel_test$Session=="E1T1E1T1E1", ]$Percentage.Time.Active)),
             rep("R-Top50", length(R_abv_test[R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active)),
             rep("R-Bottom50", length(R_bel_test[R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active))),
  value = as.numeric(c(
    
    T_abv_test[T_abv_test$Session=="E1T1E1T1E1", ]$Percentage.Time.Active,
    T_bel_test[T_bel_test$Session=="E1T1E1T1E1", ]$Percentage.Time.Active,
    R_abv_test[R_abv_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active,
    R_bel_test[R_bel_test$Session=="E1R1E1R1E1", ]$Percentage.Time.Active
  ))
)


colnames(m) = c("Segment", "Value")
m$Segment = factor(m$Segment, levels = c("T-Top50", "T-Bottom50", "R-Top50", "R-Bottom50"))

a = dunn.test(x = m$Value, g = m$Segment, method = c("bonferroni"))

col.pool = c("indianred2", "indianred3", "light blue", "dark blue")

boxplot(
  Value ~ Segment,
  data = m
)


stripchart(
  Value ~ Segment,
  vertical = TRUE,
  data = m,
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  col.pool
)




