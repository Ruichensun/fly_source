setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
library(pwr)
library(foreign)
library(lmtest)
library(sandwich)

# Load raw data
fly.info.end = read.csv("data/fly_info_end_final.csv", header = T, stringsAsFactors = F)
fly.info.end_WT = fly.info.end[(fly.info.end$genotype=="WT")|(fly.info.end$genotype=="CS"),]
all_ofs_WT = read.csv("all_ofs_WT.csv", header = T, stringsAsFactors = F)
all_ofs_mutants = read.csv("all_ofs_mutants.csv", header = T, stringsAsFactors = F)
all_ofs = rbind(all_ofs_WT, all_ofs_mutants)

# Get Learning Index (and its log transform)
get_learning_index = function(fly.info.end, all_ofs, ind, ind_of_interest){
  fly = fly.info.end[ind, ]$fly
  geno = fly.info.end[ind, ]$genotype
  experimenter = fly.info.end[ind, ]$experimenter
  cat = fly.info.end[ind, ]$category
  
  fly_ofs = all_ofs[all_ofs$flynum == fly & all_ofs$genotype == geno &
                      all_ofs$experimenter == experimenter & all_ofs$type == cat, 
                    ]
  session_5_name = paste0("E1", cat, "1E1", cat, "1E1")
  post_training = fly_ofs[fly_ofs$session==session_5_name, ind_of_interest]
  learning_index = fly_ofs[fly_ofs$session==session_5_name, ind_of_interest] - 
    fly_ofs[fly_ofs$session=="E1", ind_of_interest]
  ret = cbind(post_training,learning_index)
  colnames(ret) = c("PostTrain", "LearnIndex")
  return(ret)
}

fly.info.end$LearnIndex = NA
fly.info.end$PostTrain = NA
ind_of_interest = 9
for (ind in 1:nrow(fly.info.end)){
    print(ind)
    temp = get_learning_index(fly.info.end, all_ofs, ind, ind_of_interest)
    if(length(temp[2])==0){
      next
    }
    fly.info.end[ind, ]$PostTrain = temp[1]
    fly.info.end[ind, ]$LearnIndex = temp[2]
}
# Plotting WT

pdf("WT_LearnIndex_density_Overall_050619.pdf", width = 10)
plot(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N",]$LearnIndex), 
     col="black", 
     ylim=c(0,5), xlim=c(-2,2),
     main = "WT",
     xlab = "")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T",]$LearnIndex), col="red")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R",]$LearnIndex), col="blue")
text(x = 1, y = 3, paste0("N = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N",]$LearnIndex)))
text(x = 1, y = 3.5, paste0("R = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R",]$LearnIndex)), col = "blue")
text(x = 1, y = 4, paste0("T = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T",]$LearnIndex)), col = "red")

data = data.frame(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS"),]$category,
                  as.numeric(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS"),]$LearnIndex))
colnames(data) = c("category", "LearnIndex")
a = kruskal.test(LearnIndex ~ category, data = data)
text(x = -1, y = 4.5, paste0("KW p-value: ", sprintf("%.5f", a$p.value)))
ap = pairwise.wilcox.test(data$LearnIndex, data$category, p.adjust.method = "BH")
text(x = -1, y = 4, paste0("N-R: ", sprintf("%.5f", ap$p.value[1,1])))
text(x = -1, y = 3.5, paste0("N-T: ", sprintf("%.5f", ap$p.value[2,1])))
text(x = -1, y = 3, paste0("R-T: ", sprintf("%.5f", ap$p.value[2,2])))
dev.off()

pdf("WT_LearnIndex_density_Gender_043019.pdf", width = 10)
par(mfrow=c(2,2))
plot(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N"&fly.info.end$gender=="M",]$LearnIndex), 
     col="black", 
     ylim=c(0,5), xlim=c(-2,2),
     main = "WT - Male",
     xlab = "")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="M",]$LearnIndex), col="red")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="M",]$LearnIndex), col="blue")
text(x = 1, y = 3, paste0("N = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N"&fly.info.end$gender=="M",]$LearnIndex)))
text(x = 1, y = 3.5, paste0("R = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="M",]$LearnIndex)), col = "blue")
text(x = 1, y = 4, paste0("T = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="M",]$LearnIndex)), col = "red")

data = data.frame(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$gender=="M",]$category,
                  as.numeric(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$gender=="M",]$LearnIndex))
colnames(data) = c("category", "LearnIndex")
a = kruskal.test(LearnIndex ~ category, data = data)
text(x = -1, y = 4.5, paste0("KW p-value: ", sprintf("%.5f", a$p.value)))
ap = pairwise.wilcox.test(data$LearnIndex, data$category, p.adjust.method = "BH")
text(x = -1, y = 4, paste0("N-R: ", sprintf("%.5f", ap$p.value[1,1])))
text(x = -1, y = 3.5, paste0("N-T: ", sprintf("%.5f", ap$p.value[2,1])))
text(x = -1, y = 3, paste0("R-T: ", sprintf("%.5f", ap$p.value[2,2])))

plot(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N"&fly.info.end$gender=="F",]$LearnIndex), 
     col="black", ylim=c(0,5), xlim=c(-2,2),
     main = "WT - Female", xlab = "")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="F",]$LearnIndex), col="red")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="F",]$LearnIndex), col="blue")

text(x = 1, y = 3, paste0("N = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N"&fly.info.end$gender=="F",]$LearnIndex)))
text(x = 1, y = 3.5, paste0("R = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="F",]$LearnIndex)), col = "blue")
text(x = 1, y = 4, paste0("T = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="F",]$LearnIndex)), col = "red")

data = data.frame(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$gender=="F",]$category,
                  as.numeric(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$gender=="F",]$LearnIndex))
colnames(data) = c("category", "LearnIndex")
a = kruskal.test(LearnIndex ~ category, data = data)
text(x = -1, y = 4.5, paste0("KW p-value: ", sprintf("%.5f", a$p.value)))
ap = pairwise.wilcox.test(data$LearnIndex, data$category, p.adjust.method = "BH")
text(x = -1, y = 4, paste0("N-R: ", sprintf("%.5f", ap$p.value[1,1])))
text(x = -1, y = 3.5, paste0("N-T: ", sprintf("%.5f", ap$p.value[2,1])))
text(x = -1, y = 3, paste0("R-T: ", sprintf("%.5f", ap$p.value[2,2])))

plot(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="M",]$LearnIndex), 
     col="red", ylim=c(0,2), xlim=c(-2,2),
     main = "WT - T", xlab = "")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="F",]$LearnIndex), col="indianred3")
text(x = 1, y = 1.5, paste0("F = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="F",]$LearnIndex)),
     col = "indianred3")
text(x = 1, y = 1.25, paste0("M = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T"&fly.info.end$gender=="M",]$LearnIndex)),
     col = "red")

data = data.frame(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T",]$gender,
                  as.numeric(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T",]$LearnIndex))
colnames(data) = c("gender", "LearnIndex")
wt = wilcox.test(data[data$gender=="F",]$LearnIndex, 
                 data[data$gender=="M",]$LearnIndex)
text(x = 1, y = 1.75, paste0("P value: ", sprintf("%.5f", wt$p.value)))

plot(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="M",]$LearnIndex), 
     col="blue", ylim=c(0,2), xlim=c(-2,2),
     main = "WT - R", xlab = "")
lines(density(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="F",]$LearnIndex), col="light blue")
text(x = 1, y = 1.5, paste0("F = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="F",]$LearnIndex)),
     col = "light blue")
text(x = 1, y = 1.25, paste0("M = ", length(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R"&fly.info.end$gender=="M",]$LearnIndex)),
     col = "blue")
data = data.frame(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R",]$gender,
                  as.numeric(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R",]$LearnIndex))
colnames(data) = c("gender", "LearnIndex")
wt = wilcox.test(data[data$gender=="F",]$LearnIndex, 
                 data[data$gender=="M",]$LearnIndex)
text(x = 1, y = 1.75, paste0("P value: ", sprintf("%.5f", wt$p.value)))
dev.off()

# Plot Mutant Flies' LearnIndex

LearnIndex_plot = function(geno, fly.info.end){
  fly.info.temp = fly.info.end[fly.info.end$genotype==geno, ]
  pdf(paste0(geno, "_LearnIndex_", Sys.Date(), ".pdf"), width = 10)
  
  par(mfrow=c(2,2))
  plot(density(fly.info.temp[fly.info.temp$category=="N"&fly.info.temp$gender=="M", ]$LearnIndex), 
       col="black", ylim=c(0,5), xlim=c(-2,2),
       main = paste0(geno, " - Male"),
       xlab = "")
  lines(density(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="M", ]$LearnIndex), col="red")
  lines(density(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="M", ]$LearnIndex), col="blue")
  text(x = 1, y = 3, paste0("N = ", length(fly.info.temp[fly.info.temp$category=="N"&fly.info.temp$gender=="M",]$LearnIndex)))
  text(x = 1, y = 3.5, paste0("R = ", length(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="M",]$LearnIndex)), col = "blue")
  text(x = 1, y = 4, paste0("T = ", length(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="M",]$LearnIndex)), col = "red")
  
  data = data.frame(fly.info.temp[fly.info.temp$gender=="M",]$category,
                    as.numeric(fly.info.temp[fly.info.temp$gender=="M",]$LearnIndex))
  colnames(data) = c("category", "LearnIndex")
  a = kruskal.test(LearnIndex ~ category, data = data)
  text(x = -1, y = 4.5, paste0("KW p-value: ", sprintf("%.5f", a$p.value)))
  ap = pairwise.wilcox.test(data$LearnIndex, data$category, p.adjust.method = "BH")
  text(x = -1, y = 4, paste0("N-R: ", sprintf("%.5f", ap$p.value[1,1])))
  text(x = -1, y = 3.5, paste0("N-T: ", sprintf("%.5f", ap$p.value[2,1])))
  text(x = -1, y = 3, paste0("R-T: ", sprintf("%.5f", ap$p.value[2,2])))
  
  plot(density(fly.info.temp[fly.info.temp$category=="N"&fly.info.temp$gender=="F",]$LearnIndex), 
       col="black", ylim=c(0,5), xlim=c(-2,2),
       main = "WT - Female", xlab = "")
  lines(density(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="F",]$LearnIndex), col="red")
  lines(density(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="F",]$LearnIndex), col="blue")
  
  text(x = 1, y = 3, paste0("N = ", length(fly.info.temp[fly.info.temp$category=="N"&fly.info.temp$gender=="F",]$LearnIndex)))
  text(x = 1, y = 3.5, paste0("R = ", length(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="F",]$LearnIndex)), col = "blue")
  text(x = 1, y = 4, paste0("T = ", length(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="F",]$LearnIndex)), col = "red")
  
  data = data.frame(fly.info.temp[fly.info.temp$gender=="F",]$category,
                    as.numeric(fly.info.temp[fly.info.temp$gender=="F",]$LearnIndex))
  colnames(data) = c("category", "LearnIndex")
  a = kruskal.test(LearnIndex ~ category, data = data)
  text(x = -1, y = 4.5, paste0("KW p-value: ", sprintf("%.5f", a$p.value)))
  ap = pairwise.wilcox.test(data$LearnIndex, data$category, p.adjust.method = "BH")
  text(x = -1, y = 4, paste0("N-R: ", sprintf("%.5f", ap$p.value[1,1])))
  text(x = -1, y = 3.5, paste0("N-T: ", sprintf("%.5f", ap$p.value[2,1])))
  text(x = -1, y = 3, paste0("R-T: ", sprintf("%.5f", ap$p.value[2,2])))
  
  plot(density(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="M",]$LearnIndex), 
       col="red", ylim=c(0,2), xlim=c(-2,2),
       main = "WT - T", xlab = "")
  lines(density(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="F",]$LearnIndex), col="indianred3")
  text(x = 1, y = 1.5, paste0("F = ", length(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="F",]$LearnIndex)),
       col = "indianred3")
  text(x = 1, y = 1.25, paste0("M = ", length(fly.info.temp[fly.info.temp$category=="T"&fly.info.temp$gender=="M",]$LearnIndex)),
       col = "red")
  
  data = data.frame(fly.info.temp[fly.info.temp$category=="T",]$gender,
                    as.numeric(fly.info.temp[fly.info.temp$category=="T",]$LearnIndex))
  colnames(data) = c("gender", "LearnIndex")
  wt = wilcox.test(data[data$gender=="F",]$LearnIndex, 
                   data[data$gender=="M",]$LearnIndex)
  text(x = 1, y = 1.75, paste0("P value: ", sprintf("%.5f", wt$p.value)))
  
  plot(density(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="M",]$LearnIndex), 
       col="blue", ylim=c(0,2), xlim=c(-2,2),
       main = "WT - R", xlab = "")
  lines(density(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="F",]$LearnIndex), col="light blue")
  text(x = 1, y = 1.5, paste0("F = ", length(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="F", ]$LearnIndex)),
       col = "light blue")
  text(x = 1, y = 1.25, paste0("M = ", length(fly.info.temp[fly.info.temp$category=="R"&fly.info.temp$gender=="M", ]$LearnIndex)),
       col = "blue")
  data = data.frame(fly.info.temp[fly.info.temp$category=="R", ]$gender,
                    as.numeric(fly.info.temp[fly.info.temp$category=="R", ]$LearnIndex))
  colnames(data) = c("gender", "LearnIndex")
  wt = wilcox.test(data[data$gender=="F",]$LearnIndex, 
                   data[data$gender=="M",]$LearnIndex)
  text(x = 1, y = 1.75, paste0("P value: ", sprintf("%.5f", wt$p.value)))  
  
  dev.off()
}

LearnIndex_plot_FM = function(geno, fly.info.end){
  fly.info.temp = fly.info.end[fly.info.end$genotype==geno, ]
  pdf(paste0(geno, "_LearnIndex_FM_", Sys.Date(), ".pdf"), width = 10)
  plot(density(fly.info.temp[fly.info.temp$category=="N", ]$LearnIndex), 
       col="black", ylim=c(0,5), xlim=c(-2,2),
       main = paste0(geno, " - all"),
       xlab = "")
  lines(density(fly.info.temp[fly.info.temp$category=="T", ]$LearnIndex), col="red")
  lines(density(fly.info.temp[fly.info.temp$category=="R", ]$LearnIndex), col="blue")
  text(x = 1, y = 3, paste0("N = ", length(fly.info.temp[fly.info.temp$category=="N",]$LearnIndex)))
  text(x = 1, y = 3.5, paste0("R = ", length(fly.info.temp[fly.info.temp$category=="R",]$LearnIndex)), col = "blue")
  text(x = 1, y = 4, paste0("T = ", length(fly.info.temp[fly.info.temp$category=="T",]$LearnIndex)), col = "red")
  
  data = data.frame(fly.info.temp$category,
                    as.numeric(fly.info.temp$LearnIndex))
  colnames(data) = c("category", "LearnIndex")
  a = kruskal.test(LearnIndex ~ category, data = data)
  text(x = -1, y = 4.5, paste0("KW p-value: ", sprintf("%.5f", a$p.value)))
  ap = pairwise.wilcox.test(data$LearnIndex, data$category, p.adjust.method = "BH")
  text(x = -1, y = 4, paste0("N-R: ", sprintf("%.5f", ap$p.value[1,1])))
  text(x = -1, y = 3.5, paste0("N-T: ", sprintf("%.5f", ap$p.value[2,1])))
  text(x = -1, y = 3, paste0("R-T: ", sprintf("%.5f", ap$p.value[2,2])))

  dev.off()
}

glist = c("SUN1", 
          "SUN2", 
          "SUN3", 
          "D2R-1 x 51635", 
          "CS x JU30",  
          "Empty-Gal4 x CS",
          "R60D05 x JU30",
          "R60D05 x DopR1-IR",
          "JG17 x JU30",
          "MB009B x JU30", 
          "MB009B x CS",
          "MB009B x DopR1-IR",
          "MB131B x JU30", 
          "MB131B x CS",
          "MB131B x DopR1-IR",
          "MB419B x JU30", 
          "MB419B x CS",
          "MB419B x DopR1-IR",
          "MB607B x JU30", 
          "MB607B x CS",
          "MB607B x DopR1-IR",
          "UAS-DopR1-IR x CS",
          "UAS-DopR1-IR x 51635",
          "51635 x CS"
)


for (i in 1:length(glist)){
  geno = glist[i]
  print(geno)
  LearnIndex_plot(geno, fly.info.end)
}

for (i in 1:length(glist)){
  geno = glist[i]
  print(geno)
  LearnIndex_plot_FM(geno, fly.info.end)
}

# Linear Model WT

fly.info.temp = fly.info.end[fly.info.end$genotype %in% c("WT", "CS"),]
fly.info.temp$pretest = NA
E1_data = all_ofs[all_ofs$session=="E1", ]

for (i in 1:nrow(fly.info.temp)){
  geno = fly.info.temp[i, ]$genotype
  exp = fly.info.temp[i, ]$experimenter
  gender = fly.info.temp[i, ]$gender
  typ = fly.info.temp[i, ]$category
  num = fly.info.temp[i, ]$fly
  
  pta =  E1_data[E1_data$type == typ &
                   E1_data$gender == gender &
                   E1_data$experimenter == exp &
                   E1_data$genotype == geno &
                   E1_data$flynum == num, ]
  fly.info.temp[i, ]$pretest = pta$percentage.time.active
}


fly.info.temp$category = factor(fly.info.temp$category, levels=c("T", "R", "N"))
fly.info.temp$Female = 0
fly.info.temp$month = 0
fly.info.temp$day = 0
fly.info.temp$year = 0

for (i in 1:nrow(fly.info.temp)){
  # genotype - 1
  if (fly.info.temp[i,]$genotype == "CS"){
    fly.info.temp[i,]$genotype = "WT"
  }
  # gender 
  if (fly.info.temp[i, ]$gender == "F"){
    fly.info.temp[i, ]$Female = 1
  }
  date = fly.info.temp[i, ]$exp_date
  date = strsplit(date, "/")[[1]]
  fly.info.temp[i, ]$month = date[1]
  fly.info.temp[i, ]$day = date[2]
  fly.info.temp[i, ]$year = date[3]
}


fly.info.temp$month = factor(fly.info.temp$month, levels=c("1", "2", "3", "4", "5", "6", 
                                                           "7", "8", "9", "10", "11", "12"))
fly.info.temp$year = factor(fly.info.temp$year, levels=c("2017", "2018", "2019"))
fly.info.temp$experimenter = factor(fly.info.temp$experimenter, levels=c("JD", "ES", "SW", "RS", "XC", "LW"))


lm.fit.WT = lm(LearnIndex ~ category + Female + pretest + age + year + experimenter +
                 pretest:category + Female:category, fly.info.temp)


# Linear Model 009B
fly.info.temp = fly.info.end[fly.info.end$genotype %in% c("WT", "CS", "MB009B x CS", "MB009B x JU30", "CS x JU30"),]
fly.info.temp$pretest = NA
E1_data = all_ofs[all_ofs$session=="E1", ]

for (i in 1:nrow(fly.info.temp)){
  geno = fly.info.temp[i, ]$genotype
  exp = fly.info.temp[i, ]$experimenter
  gender = fly.info.temp[i, ]$gender
  typ = fly.info.temp[i, ]$category
  num = fly.info.temp[i, ]$fly
  
  pta =  E1_data[E1_data$type == typ &
                   E1_data$gender == gender &
                   E1_data$experimenter == exp &
                   E1_data$genotype == geno &
                   E1_data$flynum == num, ]
  fly.info.temp[i, ]$pretest = pta$percentage.time.active
}


fly.info.temp$category = factor(fly.info.temp$category, levels=c("T", "R", "N"))
fly.info.temp$Female = 0
fly.info.temp$GAL4 = 0
fly.info.temp$UAS = 0
fly.info.temp$GAL4UAS = 0
fly.info.temp$month = 0
fly.info.temp$day = 0
fly.info.temp$year = 0

for (i in 1:nrow(fly.info.temp)){
  # genotype - 1
  if (fly.info.temp[i,]$genotype == "CS"){
    fly.info.temp[i,]$genotype = "WT"
  }
  # genotype - 2
  if (fly.info.temp[i, ]$genotype == "MB009B x JU30"){
    fly.info.temp[i, ]$GAL4UAS = 1
  }
  if(fly.info.temp[i, ]$genotype == "MB009B x CS"){
    fly.info.temp[i, ]$GAL4 = 1
  }
  if(fly.info.temp[i, ]$genotype == "CS x JU30"){
    fly.info.temp[i, ]$UAS = 1
  }
  # gender 
  if (fly.info.temp[i, ]$gender == "F"){
    fly.info.temp[i, ]$Female = 1
  }
  date = fly.info.temp[i, ]$exp_date
  date = strsplit(date, "/")[[1]]
  fly.info.temp[i, ]$month = date[1]
  fly.info.temp[i, ]$day = date[2]
  fly.info.temp[i, ]$year = date[3]
}


fly.info.temp$month = factor(fly.info.temp$month, levels=c("1", "2", "3", "4", "5", "6", 
                                                           "7", "8", "9", "10", "11", "12"))
fly.info.temp$year = factor(fly.info.temp$year, levels=c("2017", "2018", "2019"))
fly.info.temp$experimenter = factor(fly.info.temp$experimenter, levels=c("JD", "ES", "SW", "RS", "XC", "LW"))


lm.fit.009B = lm(LearnIndex ~ category + Female + GAL4 + UAS + GAL4UAS + pretest + age + category:pretest + Female:pretest + 
                     GAL4:pretest + UAS:pretest + GAL4UAS:pretest + GAL4:category + UAS:category + GAL4UAS:category, fly.info.temp)


fly.info.temp = fly.info.end[fly.info.end$genotype %in% c("WT", "CS", "MB131B x CS", "MB131B x JU30", "CS x JU30"),]
fly.info.temp$pretest = NA
E1_data = all_ofs[all_ofs$session=="E1", ]

for (i in 1:nrow(fly.info.temp)){
  geno = fly.info.temp[i, ]$genotype
  exp = fly.info.temp[i, ]$experimenter
  gender = fly.info.temp[i, ]$gender
  typ = fly.info.temp[i, ]$category
  num = fly.info.temp[i, ]$fly
  
  pta =  E1_data[E1_data$type == typ &
                   E1_data$gender == gender &
                   E1_data$experimenter == exp &
                   E1_data$genotype == geno &
                   E1_data$flynum == num, ]
  fly.info.temp[i, ]$pretest = pta$percentage.time.active
}


fly.info.temp$Train = 0
fly.info.temp$Random = 0
fly.info.temp$Female = 0
fly.info.temp$GAL4 = 0
fly.info.temp$UAS = 0

for (i in 1:nrow(fly.info.temp)){
  # genotype - 1
  if (fly.info.temp[i,]$genotype == "CS"){
    fly.info.temp[i,]$genotype = "WT"
  }
  # genotype - 2
  if (fly.info.temp[i, ]$genotype == "MB131B x JU30"){
    fly.info.temp[i, ]$GAL4 = 1
    fly.info.temp[i, ]$UAS = 1
  }
  if(fly.info.temp[i, ]$genotype == "MB131B x CS"){
    fly.info.temp[i, ]$GAL4 = 1
  }
  if(fly.info.temp[i, ]$genotype == "CS x JU30"){
    fly.info.temp[i, ]$UAS = 1
  }
  # gender 
  if (fly.info.temp[i, ]$gender == "F"){
    fly.info.temp[i, ]$Female = 1
  }
  # Category
  if (fly.info.temp[i, ]$category == "T"){
    fly.info.temp[i, ]$Train = 1
  }
  if (fly.info.temp[i, ]$category == "R"){
    fly.info.temp[i, ]$Random = 1
  }
  
}

lm.fit.131B = lm(LearnIndex ~ Train + Random + Female + GAL4 + UAS + GAL4:UAS + pretest + age, fly.info.temp)


fly.info.temp = fly.info.end[fly.info.end$genotype %in% c("WT", "CS", "MB419B x CS", "MB419B x JU30", "CS x JU30"),]
fly.info.temp$pretest = NA
E1_data = all_ofs[all_ofs$session=="E1", ]

for (i in 1:nrow(fly.info.temp)){
  geno = fly.info.temp[i, ]$genotype
  exp = fly.info.temp[i, ]$experimenter
  gender = fly.info.temp[i, ]$gender
  typ = fly.info.temp[i, ]$category
  num = fly.info.temp[i, ]$fly
  
  pta =  E1_data[E1_data$type == typ &
                   E1_data$gender == gender &
                   E1_data$experimenter == exp &
                   E1_data$genotype == geno &
                   E1_data$flynum == num, ]
  fly.info.temp[i, ]$pretest = pta$percentage.time.active
}


fly.info.temp$Train = 0
fly.info.temp$Random = 0
fly.info.temp$Female = 0
fly.info.temp$GAL4 = 0
fly.info.temp$UAS = 0

for (i in 1:nrow(fly.info.temp)){
  # genotype - 1
  if (fly.info.temp[i,]$genotype == "CS"){
    fly.info.temp[i,]$genotype = "WT"
  }
  # genotype - 2
  if (fly.info.temp[i, ]$genotype == "MB419B x JU30"){
    fly.info.temp[i, ]$GAL4 = 1
    fly.info.temp[i, ]$UAS = 1
  }
  if(fly.info.temp[i, ]$genotype == "MB419B x CS"){
    fly.info.temp[i, ]$GAL4 = 1
  }
  if(fly.info.temp[i, ]$genotype == "CS x JU30"){
    fly.info.temp[i, ]$UAS = 1
  }
  # gender 
  if (fly.info.temp[i, ]$gender == "F"){
    fly.info.temp[i, ]$Female = 1
  }
  # Category
  if (fly.info.temp[i, ]$category == "T"){
    fly.info.temp[i, ]$Train = 1
  }
  if (fly.info.temp[i, ]$category == "R"){
    fly.info.temp[i, ]$Random = 1
  }
  
}

lm.fit.419B = lm(LearnIndex ~ Train + Random + Female + GAL4 + UAS + GAL4:UAS + pretest + age, fly.info.temp)

fly.info.temp = fly.info.end[fly.info.end$genotype %in% c("WT", "CS", "MB607B x CS", "MB607B x JU30", "CS x JU30"),]
fly.info.temp$pretest = NA
E1_data = all_ofs[all_ofs$session=="E1", ]

for (i in 1:nrow(fly.info.temp)){
  geno = fly.info.temp[i, ]$genotype
  exp = fly.info.temp[i, ]$experimenter
  gender = fly.info.temp[i, ]$gender
  typ = fly.info.temp[i, ]$category
  num = fly.info.temp[i, ]$fly
  
  pta =  E1_data[E1_data$type == typ &
                   E1_data$gender == gender &
                   E1_data$experimenter == exp &
                   E1_data$genotype == geno &
                   E1_data$flynum == num, ]
  fly.info.temp[i, ]$pretest = pta$percentage.time.active
}
fly.info.temp$Train = 0
fly.info.temp$Random = 0
fly.info.temp$Female = 0
fly.info.temp$GAL4 = 0
fly.info.temp$UAS = 0

for (i in 1:nrow(fly.info.temp)){
  # genotype - 1
  if (fly.info.temp[i,]$genotype == "CS"){
    fly.info.temp[i,]$genotype = "WT"
  }
  # genotype - 2
  if (fly.info.temp[i, ]$genotype == "MB607B x JU30"){
    fly.info.temp[i, ]$GAL4 = 1
    fly.info.temp[i, ]$UAS = 1
  }
  if(fly.info.temp[i, ]$genotype == "MB607B x CS"){
    fly.info.temp[i, ]$GAL4 = 1
  }
  if(fly.info.temp[i, ]$genotype == "CS x JU30"){
    fly.info.temp[i, ]$UAS = 1
  }
  # gender 
  if (fly.info.temp[i, ]$gender == "F"){
    fly.info.temp[i, ]$Female = 1
  }
  # Category
  if (fly.info.temp[i, ]$category == "T"){
    fly.info.temp[i, ]$Train = 1
  }
  if (fly.info.temp[i, ]$category == "R"){
    fly.info.temp[i, ]$Random = 1
  }
  
}

lm.fit.607B = lm(LearnIndex ~ Train + Random + Female + GAL4 + UAS + GAL4:UAS + pretest + age, fly.info.temp)

# Linear Regression of Dopamine Data
fly.info.temp = fly.info.end[fly.info.end$genotype %in% c("WT", "CS", "SUN1", "SUN2"),]
fly.info.temp$pretest = NA
E1_data = all_ofs[all_ofs$session=="E1", ]

for (i in 1:nrow(fly.info.temp)){
  geno = fly.info.temp[i, ]$genotype
  exp = fly.info.temp[i, ]$experimenter
  gender = fly.info.temp[i, ]$gender
  typ = fly.info.temp[i, ]$category
  num = fly.info.temp[i, ]$fly
  
  pta =  E1_data[E1_data$type == typ &
                   E1_data$gender == gender &
                   E1_data$experimenter == exp &
                   E1_data$genotype == geno &
                   E1_data$flynum == num, ]
  fly.info.temp[i, ]$pretest = pta$percentage.time.active
}
fly.info.temp$Female = 0
fly.info.temp$DopR1 = 0
fly.info.temp$DopR2 = 0
fly.info.temp$DopEcR = 0
fly.info.temp$month = 0
fly.info.temp$day = 0
fly.info.temp$year = 0
fly.info.temp$category = factor(fly.info.temp$category, levels=c("T", "R", "N"))
fly.info.temp$experimenter = factor(fly.info.temp$experimenter, levels=c("JD", "ES", "SW", "RS", "XC", "LW"))
for (i in 1:nrow(fly.info.temp)){
  # genotype - 1
  if (fly.info.temp[i,]$genotype == "CS"){
    fly.info.temp[i,]$genotype = "WT"
  }
  # genotype - 2
  if (fly.info.temp[i, ]$genotype == "SUN1"){
    fly.info.temp[i, ]$DopR1 = 1
  }
  if (fly.info.temp[i, ]$genotype == "SUN2"){
    fly.info.temp[i, ]$DopR2 = 1
  }
  # gender 
  if (fly.info.temp[i, ]$gender == "F"){
    fly.info.temp[i, ]$Female = 1
  }
  date = fly.info.temp[i, ]$exp_date
  date = strsplit(date, "/")[[1]]
  fly.info.temp[i, ]$month = date[1]
  fly.info.temp[i, ]$day = date[2]
  fly.info.temp[i, ]$year = date[3]
}

fly.info.temp$month = factor(fly.info.temp$month, levels=c("1", "2", "3", "4", "5", "6", 
                                                           "7", "8", "9", "10", "11", "12"))
fly.info.temp$year = factor(fly.info.temp$year, levels=c("2017", "2018", "2019"))

# lm.fit.SUN = lm(LearnIndex ~ category + Female + DopR1 +  pretest + 
#                   DopR1:category , fly.info.temp)

lm.fit.SUN.orig = lm(LearnIndex ~ category + Female + DopR1 + DopR2 + age + pretest + 
                       DopR1:category + DopR2:category + pretest:category + 
                       DopR1:pretest + DopR2:pretest + Female:DopR1 + Female:DopR2, fly.info.temp)


lm.fit.SUN.orig = lm(PostTrain ~ category + Female + DopR1 + DopR2 + age + pretest, fly.info.temp)
