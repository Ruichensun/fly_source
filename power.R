setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
library(pwr)
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
for (ind in 1:nrow(fly.info.end)){
  fly.info.end[ind, ]$LearnIndex = get_learning_index(fly.info.end, all_ofs, ind, ind_of_interest)
  print(fly.info.end[ind, ]$LearnIndex)
}

mean(fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R",]$LearnIndex)

pdf("EDvsLI_041919.pdf")
glist = c("SUN1", "SUN2", "SUN3", "D2R-1 x 51635", 
          "CS x JU30",  "Empty-Gal4 x CS",
          "R60D05 x JU30","JG17 x JU30",
          "MB009B x JU30", "MB009B x CS",
          "MB131B x JU30", "MB131B x CS",
          "MB419B x JU30", "MB419B x CS",
          "MB607B x JU30", "MB607B x CS",
          
          )

for (i in 1:length(glist)){
  b = fly.info.end[(fly.info.end$genotype==glist[i])&fly.info.end$category=="R",]
  plot(b$Diff, b$LearnIndex, 
       xlim = c(-1,1), ylim = c(-1,1), col="blue",
       main = paste0(glist[i], " T & R flies"))
  model = lm(formula = b ~ 
               initial_act_laser[initial_act_laser$initial<initial_med,]$Laser_Exposure)
  abline(model$coefficients[[1]], model$coefficients[[2]])
  coef(summary(model))
  text(x = 1000, y = -0.4, paste0("Slope = ",model$coefficients[[2]], ", s.e. = ", coef(summary(model))[2,2]))
  
  }
dev.off()

a = fly.info.end[(fly.info.end$genotype=="MB419B x CS")&fly.info.end$category=="T"&fly.info.end$gender=="M",]
b = fly.info.end[(fly.info.end$genotype=="MB419B x CS")&fly.info.end$category=="R"&fly.info.end$gender=="M",]
c = fly.info.end[(fly.info.end$genotype=="MB419B x CS")&fly.info.end$category=="N"&fly.info.end$gender=="M",]
boxplot(a$LearnIndex, b$LearnIndex, c$LearnIndex)
t.test(a$LearnIndex, b$LearnIndex)


a = fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="T",]
b = fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="R",]
c = fly.info.end[(fly.info.end$genotype=="WT"|fly.info.end$genotype=="CS")&fly.info.end$category=="N",]




f = fly.info.end[(fly.info.end$genotype=="MB009B x CS")&fly.info.end$category=="N",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB009B x JU30")&fly.info.end$category=="N",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB009B x DopR1-IR")&fly.info.end$category=="N",]$LearnIndex


f = fly.info.end[(fly.info.end$genotype=="SUN1")&fly.info.end$category=="N",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="SUN2")&fly.info.end$category=="N",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="SUN3")&fly.info.end$category=="N",]$LearnIndex

f = fly.info.end[(fly.info.end$genotype=="CS x JU30")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="CS x JU30")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="CS x JU30")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

plot(density(f), col = "red", ylim = c(0, 5))
lines(density(g), col = "blue")
lines(density(h), col = "black")

f = fly.info.end[(fly.info.end$genotype=="SUN1")&fly.info.end$category=="T",]
g = fly.info.end[(fly.info.end$genotype=="SUN1")&fly.info.end$category=="R",]
h = fly.info.end[(fly.info.end$genotype=="SUN1")&fly.info.end$category=="N",]
t.test(f,g)
plot(density(f), col = "red", ylim = c(0, 5))
lines(density(g), col = "blue")
lines(density(h), col = "black")

f = fly.info.end[(fly.info.end$genotype=="SUN2")&fly.info.end$category=="T",]
g = fly.info.end[(fly.info.end$genotype=="SUN2")&fly.info.end$category=="R",]
h = fly.info.end[(fly.info.end$genotype=="SUN2")&fly.info.end$category=="N",]
t.test(f,g)
plot(density(f), col = "red", ylim = c(0, 5))
lines(density(g), col = "blue")
lines(density(h), col = "black")


f = fly.info.end[(fly.info.end$genotype=="SUN3")&fly.info.end$category=="T",]
g = fly.info.end[(fly.info.end$genotype=="SUN3")&fly.info.end$category=="R",]
h = fly.info.end[(fly.info.end$genotype=="SUN3")&fly.info.end$category=="N",]
t.test(f,g)


plot(density(f), col = "red", ylim = c(0, 5))
lines(density(g), col = "blue")
lines(density(h), col = "black")

f = fly.info.end[(fly.info.end$genotype=="D2R-1 x 51635")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="D2R-1 x 51635")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="D2R-1 x 51635")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

plot(density(f), col = "red", ylim = c(0, 5), xlim = c(-1, 1))
lines(density(g), col = "blue")
lines(density(h), col = "black")

f = fly.info.end[(fly.info.end$genotype=="51635 x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="51635 x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="51635 x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

plot(density(f), col = "red", ylim = c(0, 5), xlim = c(-1, 1))
lines(density(g), col = "blue")
lines(density(h), col = "black")


f = fly.info.end[(fly.info.end$genotype=="UAS-DopR1-IR x 51635")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="UAS-DopR1-IR x 51635")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="UAS-DopR1-IR x 51635")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

plot(density(f), col = "red", ylim = c(0, 5), xlim = c(-1, 1))
lines(density(g), col = "blue")
lines(density(h), col = "black")


f = fly.info.end[(fly.info.end$genotype=="UAS-DopR1-IR x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="UAS-DopR1-IR x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="UAS-DopR1-IR x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)


f = fly.info.end[(fly.info.end$genotype=="R60D05 x JU30")&fly.info.end$category=="T",]
g = fly.info.end[(fly.info.end$genotype=="R60D05 x JU30")&fly.info.end$category=="R",]
h = fly.info.end[(fly.info.end$genotype=="R60D05 x JU30")&fly.info.end$category=="N",]
t.test(f,g)


f = fly.info.end[(fly.info.end$genotype=="JG17 x JU30")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="JG17 x JU30")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="JG17 x JU30")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)
plot(density(f), col = "red", ylim = c(0, 5), xlim = c(-1, 1))
lines(density(g), col = "blue")
lines(density(h), col = "black")

f = fly.info.end[(fly.info.end$genotype=="MB009B x JU30")&fly.info.end$category=="T",]
g = fly.info.end[(fly.info.end$genotype=="MB009B x JU30")&fly.info.end$category=="R",]
h = fly.info.end[(fly.info.end$genotype=="MB009B x JU30")&fly.info.end$category=="N",]
t.test(f,g)


f = fly.info.end[(fly.info.end$genotype=="MB009B x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB009B x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB009B x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="W1118")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="W1118")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="W1118")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="MB131B x JU30")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB131B x JU30")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB131B x JU30")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)


f = fly.info.end[(fly.info.end$genotype=="MB131B x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB131B x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB131B x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="MB419B x JU30")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB419B x JU30")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB419B x JU30")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="MB419B x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB419B x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB419B x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)


f = fly.info.end[(fly.info.end$genotype=="MB607B x JU30")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB607B x JU30")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB607B x JU30")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="MB607B x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="MB607B x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="MB607B x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="Empty-Gal4 x CS")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="Empty-Gal4 x CS")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="Empty-Gal4 x CS")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)

f = fly.info.end[(fly.info.end$genotype=="Empty-Gal4 x JU30")&fly.info.end$category=="T",]$LearnIndex
g = fly.info.end[(fly.info.end$genotype=="Empty-Gal4 x JU30")&fly.info.end$category=="R",]$LearnIndex
h = fly.info.end[(fly.info.end$genotype=="Empty-Gal4 x JU30")&fly.info.end$category=="N",]$LearnIndex
t.test(f,g)


effectsize = c()
for (i in 3:200){
  temp = pwr.t.test(n = i, sig.level = 0.05 , power = 0.8, type = c("one.sample"))$d
  effectsize = c(effectsize, temp)
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