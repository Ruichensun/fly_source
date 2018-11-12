##### The above section calculated all the number of clicks and duration for all flies and all sessions _ updated 4/18/2018
##### Classifying flies into long-heat group and short-heat group by the duration of heat they received -- Using the "Total laser exposure in Seconds" Metric

input.file = "metric_laser/metric_2.csv"
# input.file = "metric_laser/metric_1.csv"
metric.df = read.csv(input.file)

# fly.info.include[fly.info.include$Genotype=="WT",]
# metric.df[metric.df$genotype=="WT",]
#
# metric.df.WT = metric.df[metric.df$Genotype=="WT",] # This can be changed for different genotypes

metric.df.WT = metric.df[(metric.df$Genotype == "WT") &
                           !(metric.df$Experimenter == "SW"), ] # This can be changed for different genotypes
# metric.df.WT = metric.df[(metric.df$Genotype=="WT")&(metric.df$Experimenter=="JD"),] # This can be changed for different genotypes
# metric.df.WT = metric.df[(metric.df$Genotype=="SUN2")&!(metric.df$Experimenter=="SW"),]



mean_of_Training_session = c(
  mean(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value, na.rm = T),
  mean(metric.df.WT[metric.df.WT$Session ==
                      "E1T1E1T1", ]$Value, na.rm = T),
  mean(metric.df.WT[metric.df.WT$Session ==
                      "E1T1E1T1E1T1", ]$Value, na.rm = T),
  mean(metric.df.WT[metric.df.WT$Session ==
                      "E1T1E1T1E1T1E1T1", ]$Value, na.rm = T)
)

median_of_Training_session = c(
  median(metric.df.WT[metric.df.WT$Session == "E1T1", ]$Value, na.rm = T),
  median(metric.df.WT[metric.df.WT$Session ==
                        "E1T1E1T1", ]$Value, na.rm = T),
  median(metric.df.WT[metric.df.WT$Session ==
                        "E1T1E1T1E1T1", ]$Value, na.rm = T),
  median(metric.df.WT[metric.df.WT$Session ==
                        "E1T1E1T1E1T1E1T1", ]$Value, na.rm = T)
)

counts_of_Training_session = c(sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                         "E1T1", ]$Value)),
                               sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                         "E1T1E1T1", ]$Value)),
                               sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                         "E1T1E1T1E1T1", ]$Value)),
                               sum(!is.na(metric.df.WT[metric.df.WT$Session ==
                                                         "E1T1E1T1E1T1E1T1", ]$Value)))



metric.df.WT.E1T1 = metric.df.WT[(metric.df.WT$Session == "E1T1") &
                                   (!is.na(metric.df.WT$Value)), ]

longer_heat_1stT1 = metric.df.WT.E1T1[metric.df.WT.E1T1$Value >= median(metric.df.WT.E1T1$Value), ]
shorter_heat_1stT1 = metric.df.WT.E1T1[metric.df.WT.E1T1$Value < median(metric.df.WT.E1T1$Value), ]

metric.df.WT.E1R1 = metric.df.WT[(metric.df.WT$Session == "E1R1") &
                                   (!is.na(metric.df.WT$Value)), ]

longer_heat_1stR1 = metric.df.WT.E1R1[metric.df.WT.E1R1$Value >= median(metric.df.WT.E1T1$Value), ] ##FLAG: may need tease out the R flies that do not have corresponding T flies in the longer_heat_1stT1 (04/23/2018)
shorter_heat_1stR1 = metric.df.WT.E1R1[metric.df.WT.E1R1$Value < median(metric.df.WT.E1T1$Value), ] ##FLAG: may need tease out the R flies that do not have corresponding T flies in the shorter_heat_1stT1 (04/23/2018)

# fly.info.First_T1_Above_Median = fly.info.include[
#                                           (fly.info.include$Genotype%in%longer_heat_1stT1$Genotype)&
#                                           (fly.info.include$Fly%in%longer_heat_1stT1$Fly)&
#                                            # (fly.info.include$Genotype==longer_heat_1stT1$genotype) &
#                                           (fly.info.include$experimenter%in%longer_heat_1stT1$Experimenter),
#                                         ]
#
# fly.info.First_R1_Above_Median = fly.info.include[(fly.info.include$Genotype%in%longer_heat_1stR1$Genotype)&
#                                                     (fly.info.include$Fly%in%longer_heat_1stR1$Fly)& # (fly.info.include$Genotype==longer_heat_1stT1$genotype) &
#                                                     (fly.info.include$experimenter%in%longer_heat_1stR1$Experimenter),
#                                                   ]

# concurrent_sessions = rbind(fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))),],
#                             fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))-1),],
#                             fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))+1),],
#                             fly.info[(as.numeric(rownames(fly.info.First_T1_Above_Median))+2),]
#                             )
#


################################################segmentation based on all the heat received

AllT1 = c()
AllR1 = c()
metric.df.WT.AllR1 = data.frame()
metric.df.WT.AllT1 = data.frame()

# for (i in 1:dim(metric.df.WT.E1T1)[1]){
#   AllT1 = c(AllT1,sum(metric.df.WT[(metric.df.WT$Fly==metric.df.WT.E1T1[i,]$Fly)&(metric.df.WT$Experimenter==metric.df.WT.E1T1[i,]$Experimenter)&!is.na(metric.df.WT$Value),]$Value))
# }
#
for (i in 1:dim(metric.df.WT.E1T1)[1]) {
  AllT1 = c(AllT1, sum(metric.df.WT[(metric.df.WT$Fly == metric.df.WT.E1T1[i, ]$Fly) &
                                      (metric.df.WT$Experimenter == metric.df.WT.E1T1[i, ]$Experimenter) &
                                      !is.na(metric.df.WT$Value), ]$Value[1:5]))
}


# for (i in 1:dim(metric.df.WT.E1R1)[1]){
#   AllR1 = c(AllR1,sum(metric.df.WT[(metric.df.WT$Fly==metric.df.WT.E1R1[i,]$Fly)&(metric.df.WT$Experimenter==metric.df.WT.E1R1[i,]$Experimenter)&!is.na(metric.df.WT$Value),]$Value))
# }

for (i in 1:dim(metric.df.WT.E1R1)[1]) {
  AllR1 = c(AllR1, sum(metric.df.WT[(metric.df.WT$Fly == metric.df.WT.E1R1[i, ]$Fly) &
                                      (metric.df.WT$Experimenter == metric.df.WT.E1R1[i, ]$Experimenter) &
                                      !is.na(metric.df.WT$Value), ]$Value[1:5]))
}


metric.df.WT.AllT1 = cbind(metric.df.WT.E1T1,
                           AllT1)
colnames(metric.df.WT.AllT1) = c(
  "Value",
  "Fly",
  "Category",
  "Gender",
  "Genotype",
  "Experimenter",
  "Age",
  "Setup",
  "Session",
  "All_Heat_Received"
)


longer_heat_allT1 = metric.df.WT.AllT1[metric.df.WT.AllT1$All_Heat_Received >=
                                         median(metric.df.WT.AllT1$All_Heat_Received), ]
shorter_heat_allT1 = metric.df.WT.AllT1[metric.df.WT.AllT1$All_Heat_Received <
                                          median(metric.df.WT.AllT1$All_Heat_Received), ]


metric.df.WT.AllR1 = cbind(metric.df.WT.E1R1,
                           AllR1)
colnames(metric.df.WT.AllR1) = c(
  "Value",
  "Fly",
  "Category",
  "Gender",
  "Genotype",
  "Experimenter",
  "Age",
  "Setup",
  "Session",
  "All_Heat_Received"
)

# longer_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received>=median(metric.df.WT.AllT1$All_Heat_Received),]
# shorter_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received<median(metric.df.WT.AllT1$All_Heat_Received),]

longer_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received %in% longer_heat_allT1$All_Heat_Received, ]
shorter_heat_allR1 = metric.df.WT.AllR1[metric.df.WT.AllR1$All_Heat_Received %in% shorter_heat_allT1$All_Heat_Received, ]

metric.df.WT.T1 = rbind(longer_heat_allT1, shorter_heat_allT1)

metric.df.WT.T1 = subset(metric.df.WT.T1, metric.df.WT.T1$Experimenter !=
                           "RS")

metric.df.WT.R1 = rbind(longer_heat_allR1, shorter_heat_allR1)
metric.df.WT.R1 = subset(metric.df.WT.R1, metric.df.WT.R1$Experimenter !=
                           "RS")


##########Plot the relationship between amount of heat received and the Metric #29 ##########
#
# input.file = paste0("metrics/metric_29.csv")
# metric_of_interest = read.csv(input.file)
#
# metric.df.WT.AllT1.Analysis = metric.df.WT.AllT1[(metric.df.WT.AllT1$Experimenter=="JD")|(metric.df.WT.AllT1$Experimenter=="ES"),]
#
# relation_of_E1_and_Total_Heat = cbind((metric.df.WT.AllT1.Analysis),
#                                            metric_of_interest[((metric_of_interest$Fly%in%metric.df.WT.AllT1.Analysis$Fly)&
#                                                                 (metric_of_interest$Experimenter%in%metric.df.WT.AllT1.Analysis$Experimenter))&
#                                                                 (metric_of_interest$Genotype=="WT")&
#                                                                 (metric_of_interest$Session=="E1") ,
#                                                               ]$Value,
#                                            metric_of_interest[((metric_of_interest$Fly%in%metric.df.WT.AllT1.Analysis$Fly)&
#                                                                  (metric_of_interest$Experimenter%in%metric.df.WT.AllT1.Analysis$Experimenter))&
#                                                                  (metric_of_interest$Genotype=="WT")&
#                                                                  (metric_of_interest$Session=="E1T1E1"),
#                                                               ]$Value,
#                                            metric_of_interest[((metric_of_interest$Fly%in%metric.df.WT.AllT1.Analysis$Fly)&
#                                                                  (metric_of_interest$Experimenter%in%metric.df.WT.AllT1.Analysis$Experimenter))&
#                                                                  (metric_of_interest$Genotype=="WT")&
#                                                                  (metric_of_interest$Session=="E1T1E1T1E1"),
#                                                               ]$Value
#                                       )
#
#
# colnames(relation_of_E1_and_Total_Heat) = c("FirstT1_Heat","Fly","Category","Gender","Genotype","Experimenter","Age","Setup","Session","All_Heat_Received","1stE1","2ndE1","3rdE1")
#
# SecondT1_Heat = relation_of_E1_and_Total_Heat$All_Heat_Received - relation_of_E1_and_Total_Heat$FirstT1_Heat
#
# relation_simplified = data.frame(relation_of_E1_and_Total_Heat$FirstT1_Heat,SecondT1_Heat,relation_of_E1_and_Total_Heat$`1stE1`,relation_of_E1_and_Total_Heat$`2ndE1`,relation_of_E1_and_Total_Heat$`3rdE1`)
#
# colnames(relation_simplified) = c("FirstT1_Heat","SecondT1_Heat","1stE1","2ndE1","3rdE1")
#




# pdf("fly_metric_Less_Clicks_CS_1st_2nd_HeatDuration_3sessions_JD_050418_Updated.pdf",onefile=T,width=10)
#
# write.table(longer_heat_allT1, file = "2018_05_09_longer_heat_allT1.csv", append = FALSE, quote = TRUE, sep = ",",
#             eol = "\n", na = "NA", dec = ".", row.names = TRUE,
#             col.names = TRUE, qmethod = c("escape", "double"),
#             fileEncoding = "")
#
#

#Also, convert "total laser exposure in seconds" into "total heat received".
#Total heat received(THR) formula: THR = 9.125 (mW) * Total Laser Exposure in Seconds (unit: mille Joules)
