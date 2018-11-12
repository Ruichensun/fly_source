
# Segmenting flies into long-exposure group and short-exposure group by the duration of heat they received -- Using the "Total laser exposure in Seconds" Metric
# CS only

input.file = "metric_laser/metric_2.csv"
metric.df = read.csv(input.file)
metric.df.WT = metric.df[(metric.df$Genotype == "WT"), ] 

#Mean of the laser exposure duration
mean_of_Training_session = c(mean(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value, na.rm = T),
                             mean(metric.df.WT[metric.df.WT$Session == "E1T1E1T1", ]$Value, na.rm = T),
                             mean(metric.df.WT[metric.df.WT$Session == "E1T1E1T1E1T1", ]$Value, na.rm = T),
                             mean(metric.df.WT[metric.df.WT$Session == "E1T1E1T1E1T1E1T1", ]$Value, na.rm = T))

#Median of the laser exposure duration
median_of_Training_session = c(median(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value, na.rm = T),
                               median(metric.df.WT[metric.df.WT$Session == "E1T1E1T1", ]$Value, na.rm = T),
                               median(metric.df.WT[metric.df.WT$Session == "E1T1E1T1E1T1", ]$Value, na.rm = T),
                               median(metric.df.WT[metric.df.WT$Session == "E1T1E1T1E1T1E1T1", ]$Value, na.rm = T)
                              )
#Number of flies in each session (with valid heat exposure duration recorded)
counts_of_Training_session = c(sum(!is.na(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value)),
                               sum(!is.na(metric.df.WT[metric.df.WT$Session == "E1T1E1T1", ]$Value)),
                               sum(!is.na(metric.df.WT[metric.df.WT$Session == "E1T1E1T1E1T1", ]$Value)),
                               sum(!is.na(metric.df.WT[metric.df.WT$Session == "E1T1E1T1E1T1E1T1", ]$Value)))


metric.df.WT.E1T1 = metric.df.WT[(metric.df.WT$Session == "E1T1") & (!is.na(metric.df.WT$Value)), ]
longer_heat_1stT1 = metric.df.WT.E1T1[metric.df.WT.E1T1$Value >= median(metric.df.WT.E1T1$Value), ]
shorter_heat_1stT1 = metric.df.WT.E1T1[metric.df.WT.E1T1$Value < median(metric.df.WT.E1T1$Value), ]
metric.df.WT.E1R1 = metric.df.WT[(metric.df.WT$Session == "E1R1") &(!is.na(metric.df.WT$Value)), ]
longer_heat_1stR1 = metric.df.WT.E1R1[metric.df.WT.E1R1$Value >= median(metric.df.WT.E1T1$Value), ] 
shorter_heat_1stR1 = metric.df.WT.E1R1[metric.df.WT.E1R1$Value < median(metric.df.WT.E1T1$Value), ]
##FLAG: may need tease out the R flies that do not have corresponding T flies in the longer_heat_1stT1 (04/23/2018)

AllT1 = c()
AllR1 = c()
metric.df.WT.AllR1 = data.frame()
metric.df.WT.AllT1 = data.frame()

#Calculating total heat exposure during session #1 to session #5
for (i in 1:dim(metric.df.WT.E1T1)[1]) {
  AllT1 = c(AllT1, 
            sum(metric.df.WT[(metric.df.WT$Fly == metric.df.WT.E1T1[i,]$Fly) & (metric.df.WT$Experimenter == metric.df.WT.E1T1[i,]$Experimenter) & !is.na(metric.df.WT$Value),]$Value[1:5]))
}

for (i in 1:dim(metric.df.WT.E1R1)[1]) {
  AllR1 = c(AllR1, 
            sum(metric.df.WT[(metric.df.WT$Fly == metric.df.WT.E1R1[i, ]$Fly) & (metric.df.WT$Experimenter == metric.df.WT.E1R1[i, ]$Experimenter) & !is.na(metric.df.WT$Value), ]$Value[1:5]))
}

metric.df.WT.AllT1 = cbind(metric.df.WT.E1T1, AllT1)
colnames(metric.df.WT.AllT1) = c("Value",
                                 "Fly",
                                 "Category",
                                 "Gender",
                                 "Genotype",
                                 "Experimenter",
                                 "Age",
                                 "Setup",
                                 "Session",
                                 "Total_Heat_Received"
                                )
longer_heat_allT1 = metric.df.WT.AllT1[metric.df.WT.AllT1$Total_Heat_Received >=median(metric.df.WT.AllT1$Total_Heat_Received), ]
shorter_heat_allT1 = metric.df.WT.AllT1[metric.df.WT.AllT1$Total_Heat_Received < median(metric.df.WT.AllT1$Total_Heat_Received), ]

metric.df.WT.AllR1 = cbind(metric.df.WT.E1R1,AllR1)
colnames(metric.df.WT.AllR1) = c("Value",
                                 "Fly",
                                 "Category",
                                 "Gender",
                                 "Genotype",
                                 "Experimenter",
                                 "Age",
                                 "Setup",
                                 "Session",
                                 "Total_Heat_Received"
                                )
longer_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$Total_Heat_Received %in% longer_heat_allT1$Total_Heat_Received, ]
shorter_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$Total_Heat_Received %in% shorter_heat_allT1$Total_Heat_Received, ]

metric.df.WT.T1 = rbind(longer_heat_allT1, shorter_heat_allT1)
metric.df.WT.T1 = subset(metric.df.WT.T1, metric.df.WT.T1$Experimenter !="RS" & metric.df.WT.T1$Value!=0.00) #Remove the reverse training flies
metric.df.WT.R1 = rbind(longer_heat_allR1, shorter_heat_allR1)
metric.df.WT.R1 = subset(metric.df.WT.R1, metric.df.WT.R1$Experimenter !="RS" & metric.df.WT.R1$Value!=0.00) #Remove the reverse training flies