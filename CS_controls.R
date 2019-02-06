setwd("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter")
source("D:/Behavioral_project/behavior_experiment_data/Analysis/fly_source/utils.R")
path = "D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/"

#Constant_time
type = rep("CS_constant",3)
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

fly.info.constant = read.csv(paste0(path,"fly_info_CS_constant.csv"),header = T,stringsAsFactors = F)
fly.info.constant$Age = as.Date(fly.info.constant$Exp.date, format = '%m/%d/%Y') - as.Date(fly.info.constant$Birth.date, format = '%m/%d/%Y')
fly.info.constant$Fly = as.numeric(fly.info.constant$Fly)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.constant.out = NULL
laser.status = NULL
count = 0
fly.pause = NULL

query.sessions = c(
  "E1",
  "E1T1",
  "E1T1E1",
  "E1T1E1T1",
  "E1T1E1T1E1",
  "E1R1",
  "E1R1E1",
  "E1R1E1R1",
  "E1R1E1R1E1",
  "E1N1",
  "E1N1E1",
  "E1N1E1N1",
  "E1N1E1N1E1"
)

for (ind in 1:nrow(fly.info.constant)) {
  for (session in query.sessions) {
    input.file <-list.files(path = paste0(path, fly.info.constant$Experimenter[ind],"/CS_constant/CSV/"),
                            pattern = paste0("ProcessedData_Fly", fly.info.constant$Fly[ind], "_", session, 
                                             "_", fly.info.constant$Gender[ind],"_.*"), full.names = T)
    print(input.file)
    if (length(input.file) == 0) {
      next
    }else{
      framerate = 50
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        laser.status <- rep(NA, length(fly.pos))#
      } else {file = read.csv(input.file, header = T, stringsAsFactors = F)
      fly.pos <- file[, 1]
      laser.status <- file[, 2]
      }
    }
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
      fly.info.framerate = c(fly.info.framerate, framerate)
      fly.info.constant.out = rbind(fly.info.constant.out, fly.info.constant[ind, ])
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")
    dir.create(
      paste0("data/", fly.info.constant$Experimenter[ind], "/CS/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    output.file <- paste0("data/", fly.info.constant$Experimenter[ind], "/CS/", "ProcessedData_Fly",
                          fly.info.constant$Fly[ind],"_",session,"_WT", ".csv")
    write.table(fly.pos.dat, output.file, row.names = F, quote = F, sep = ',')
  }
}
fly.info.constant.out$Framerate = fly.info.framerate
fly.info.constant.out$Fly.moving.speed = fly.moving.speed
fly.info.constant.out$Fly.pause = fly.pause
write.table(fly.info.constant.out, "data/fly_info_constant.csv", row.names = F,quote = F,sep = ",")



#CS - Controls1
type = c("CS_controls", "CS_controls1", "CS_controls1")
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

#CS - Controls2
type = c("CS_controls", "CS_controls2", "CS_controls2")
experimenter <- c("JD")
combine_flyCSV(experimenter, type)

fly.info.contro11 = read.csv(paste0(path,"fly_info_CS_controls1.csv"),header = T,stringsAsFactors = F)
fly.info.control2 = read.csv(paste0(path,"fly_info_CS_controls2.csv"),header = T,stringsAsFactors = F)
fly.info.control = rbind(fly.info.contro11, fly.info.control2)
fly.info.control$Age = as.Date(fly.info.control$Exp.date, format = '%m/%d/%Y') - as.Date(fly.info.control$Birth.date, format = '%m/%d/%Y')
fly.info.control$Fly = as.numeric(fly.info.control$Fly)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.control.out = NULL
laser.status = NULL
count = 0
fly.pause = NULL

query.sessions = c(
  "E1",
  "E1T1",
  "E1T1E1",
  "E1T1E1T1",
  "E1T1E1T1E1",
  "E1R1",
  "E1R1E1",
  "E1R1E1R1",
  "E1R1E1R1E1",
  "E1N1",
  "E1N1E1",
  "E1N1E1N1",
  "E1N1E1N1E1"
)

for (ind in 1:nrow(fly.info.control)) {
  for (session in query.sessions) {
    input.file <-list.files(path = paste0(path, fly.info.control$Experimenter[ind],"/CS_controls/CSV/"),
                            pattern = paste0("ProcessedData_Fly", fly.info.control$Fly[ind], "_", session, 
                                             "_", fly.info.control$Gender[ind],"_.*"), full.names = T)
    print(input.file)
    if (length(input.file) == 0) {
      next
    }else{
      framerate = 50
      if (dim(read.csv(input.file, header = T, nrow = 1))[2] == 1) {
        fly.pos <- read.csv(input.file, header = T, stringsAsFactors = F)[, 1]
        laser.status <- rep(NA, length(fly.pos))#
      } else {file = read.csv(input.file, header = T, stringsAsFactors = F)
      fly.pos <- file[, 1]
      laser.status <- file[, 2]
      }
    }
    if (session == "E1") {
      count = count + 1
      fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos, framerate))
      fly.pause = c(fly.pause, get_fly_initial_pause(fly.pos, framerate))
      fly.info.framerate = c(fly.info.framerate, framerate)
      fly.info.control.out = rbind(fly.info.control.out, fly.info.control[ind, ])
    }
    fly.pos.dat = data.frame(fly.pos, laser.status)
    colnames(fly.pos.dat) = c(paste0("fly_pos;framerate=", framerate), "laser_status")
    dir.create(
      paste0("data/", fly.info.control$Experimenter[ind], "/CS/"),
      showWarnings = FALSE,
      recursive = TRUE
    )
    output.file <- paste0("data/", fly.info.control$Experimenter[ind], "/CS/", "ProcessedData_Fly",
                          fly.info.control$Fly[ind],"_",session,"_WT", ".csv")
    write.table(fly.pos.dat, output.file, row.names = F, quote = F, sep = ',')
  }
}
fly.info.control.out$Framerate = fly.info.framerate
fly.info.control.out$Fly.moving.speed = fly.moving.speed
fly.info.control.out$Fly.pause = fly.pause
write.table(fly.info.control.out, "data/fly_info_control.csv", row.names = F,quote = F,sep = ",")

shared.info = c("Fly", "Gender", "Category","Setup", "Birth.date", "Exp.date", "Death.date",
                "Age", "Humidity", "E1_loading_time", "Experimenter", "Fly_Exp", "Framerate",
                "Fly.moving.speed", "Fly.pause")

fly.info.all_controls = rbind(fly.info.constant.out[, shared.info], fly.info.control.out[, shared.info])

# Get fly stats 
sessions <- c(
  "E1",
  "E1T1",
  "E1T1E1",
  "E1T1E1T1",
  "E1T1E1T1E1",
  "E1R1",
  "E1R1E1",
  "E1R1E1R1",
  "E1R1E1R1E1",
  "E1N1",
  "E1N1E1",
  "E1N1E1N1",
  "E1N1E1N1E1"
)
sessions = unique(sessions)
query.sessions = sessions

all_ofs_control = data.frame()
for(ind in 1:nrow(fly.info.all_controls)){
  # print(paste0("data/", fly.info$Experimenter[ind], "/CS/", "ProcessedData_Fly",fly.info$Fly[ind]))
  for(ind.session in 1:length(query.sessions)){
    if (ind < 5000){
      input.file <- list.files(path = paste0("data/", fly.info.all_controls$Experimenter[ind], "/CS_controls/"),                             
                               pattern = paste0("ProcessedData_Fly",fly.info.all_controls$Fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                               full.names=T)
    }else{
      input.file <- list.files(path = paste0("data/", fly.info.all_controls$Experimenter[ind], "/CS_constant/"),                             
                                         pattern = paste0("ProcessedData_Fly",fly.info.all_controls$Fly[ind], "_",query.sessions[ind.session], "_WT",".csv"),
                                         full.names=T)}
   
    if(length(input.file) == 0){next
    }else{
      framerate = fly.info.all_controls$Framerate[ind]        
      ofs = one_fly_statistics(input.file, framerate=framerate)
      Type = fly.info.all_controls$Category[ind]
      ofs = cbind(Type, ofs)
      all_ofs_control = rbind(all_ofs_control, ofs)
    }
    
  }
}

write.table(all_ofs_control, file = "all_ofs_control.csv", append = FALSE, col.names = TRUE, sep = ",", row.names = FALSE)

save.image("all_ofs_control.Rdata")

# Plot the results
constant = fly.info.all_controls[as.numeric
                                 (fly.info.all_controls$Fly)>4999, ]

pdf(paste0("Comparison_", genotype, "_", Sys.Date(), ".pdf"),
    onefile = T, width = 16
)
p = c()
metric.df = data.frame()
#Prep data
for (i in 1:length(g_list)){
  m = data.frame(
    factor = c(rep(paste0("E1_T_", g_list[i]), length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
               rep(paste0("E1_R_", g_list[i]), length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
               rep(paste0("E1_N_", g_list[i]), length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
               rep(paste0("E5_T_", g_list[i]), length(all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
               rep(paste0("E5_R_", g_list[i]), length(all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
               rep(paste0("E5_N_", g_list[i]), length(all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]))
    ),
    value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind], 
                         all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                         all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind], 
                         all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]
    )
    )
  )
  colnames(m) = c("Session", "Value")
  m$Session = factor(m$Session, levels=c(paste0("E1_T_", g_list[i]), paste0("E1_R_",g_list[i]), paste0("E1_N_", g_list[i]), 
                                         paste0("E5_T_", g_list[i]), paste0("E5_R_",g_list[i]), paste0("E5_N_", g_list[i])))
  a = test_initial_condition(metric.ind, g_list[i])
  b = test_after_training(metric.ind, g_list[i])
  p = c(p, a$P.adjusted, b$P.adjusted)
  metric.df = rbind(metric.df, m)
}                           

num = as.data.frame(table(metric.df[!is.na(metric.df$Value),]$Session))$Freq

yrange = c(0, 1)
y_text = 1.1

col.pool = rep(c("indianred3", "light blue", "grey80"), length(g_list) * 2)

boxplot(
  Value ~ Session,
  data = metric.df,
  ylim = yrange,
  outline = F,
  notch = F,
  lwd = 1,
  ylab = "",
  xlab = "",
  medlwd = 1,
  xaxt = "n",
  axes=F
)
axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))

stripchart(
  Value ~ Session,
  vertical = TRUE,
  data = metric.df,
  method = "jitter",
  add = TRUE,
  pch = 15,
  cex = 0.5,
  col =  col.pool
)

x_loc = c(1:(length(g_list) * 6))
y_top_base = 1
vertical_gap = 0.01
v_gap = 0.005
y_base_RN = y_top_base - 2.8 * vertical_gap
y_base_TN = y_top_base - 5.6 * vertical_gap
y_base_TR = y_top_base


for (i in 1:length(p)){
  if (p[i] >= 0.05){
    significance = "n.s."
  }else if (p[i] < 0.05 & p[i] >= 0.01){
    significance = "*"
  }else if (p[i] < 0.01 & p[i] >= 0.001){
    significance = "**"
  }else if (p[i] < 0.001 & p[i] >= 0.0001){
    significance = "***"
  }else if (p[i] < 0.0001){
    significance = "****"
  }
  
  # if (i%%3 == 1){
  #   text(((i + 2) + (i + 1))/2, y_base_RN + vertical_gap , significance, xpd = NA)
  #   lines(c(i + 1, i + 2), 
  #         c(y_base_RN, y_base_RN), 
  #         xpd = NA) 
  #   lines(c(i + 1, i + 1), c(y_base_RN, y_base_RN - v_gap), xpd = NA)
  #   lines(c(i + 2, i + 2), c(y_base_RN, y_base_RN - v_gap), xpd = NA)
  #   
  # }else if (i%%3 == 2){
  #   text(i, y_base_TN + vertical_gap, significance, xpd = NA)
  #   lines(c(i - 1, i + 1), 
  #         c(y_base_TN, y_base_TN), 
  #         xpd = NA)
  #   lines(c(i - 1, i - 1), c(y_base_TN, y_base_TN - v_gap), xpd = NA)
  #   lines(c(i + 1, i + 1), c(y_base_TN, y_base_TN - v_gap), xpd = NA)
  #   
  # }else{
  if (i%%3 == 0){
    text(((i - 2) + (i - 1))/2, y_base_TR + vertical_gap, significance, xpd = NA)
    lines(c(i - 2, i - 1), 
          c(y_base_TR,  y_base_TR), 
          xpd = NA)
    lines(c(i - 1, i - 1), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
    lines(c(i - 2, i - 2), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
    
  }
}


text(x = (1:length(num)) - 0.1,
     y = y_text,
     num,
     xpd = T,
     srt = 0,
     adj = 0
)

text(x = seq(1.5, (length(g_list)*6) - 1, by=6),
     y = -0.05,
     rep(c("Before"), length(g_list)),
     xpd = T,
     srt = 0,
     adj = 0
)

text(x = seq(4.75, (length(g_list)*6) - 1, by=6),
     y = -0.05,
     rep(c("After"), length(g_list)),
     xpd = T,
     srt = 0,
     adj = 0
)

text(x = seq(2.5, (length(g_list)*6) - 3.5, by= 6),
     y = -0.1,
     g_list,
     xpd = T,
     srt = 0,
     adj = 0
)

seq_for_lines = seq(3, (length(g_list)*6), by=6)
for (j in seq_for_lines) {
  lines(c(j, j) + 0.5,
        c(yrange[1] - 1e3, yrange[1] + 1e3),
        col = "light grey",
        lty = 1)
}
if (genotype != "WT"){
  seq_for_lines_2 = seq(6, (length(g_list)*6) - 6, by=6)
  for (j in seq_for_lines_2) {
    lines(c(j, j) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "black",
          lty = 1,
          lwd = 2)
  }
}

dev.off()



