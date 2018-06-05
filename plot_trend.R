# Prepare data file names
# data_dir = "D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/data/JD/CS"
setwd("D:/Behavioral_project/Behavior Experiment Data/Analysis/")
library(zoo)
# Finding out when the fly is moving vs not moving
# Input: fly_pos 
# Output: a vector of 0 and 1 of (length of input) - 1 
fly_pos_to_moving_status = function(fly_pos){ 
  # This is determined by quantile(abs(fly_moving_status),c(0.97, 0.975, 0.98)), and the 97.5% corresponds to 28.6
  speed_threshold = 28 
  fly_moving_status = diff(fly_pos)
  # Finding out the fly's moving status by two criteria: velocity = 0 or velocity much larger than a speed threshold
  fly_moving_status_discretized = replace(fly_moving_status, fly_moving_status >
                                            speed_threshold, 0)
  fly_moving_status_discretized = replace(fly_moving_status_discretized,
                                          fly_moving_status_discretized != 0,
                                          1)
  return(fly_moving_status_discretized)
}

moving_status = function(input_file) {
  # a = read.csv("ProcessedData_Fly230_E1T1_WT.csv",header=T,stringsAsFactors=F)
  a = read.csv(input_file, header = T, stringsAsFactors = F)
  fly_pos = a$fly_pos.framerate.50
  fly_moving_status = fly_pos_to_moving_status(fly_pos)
  starting_point = 21
  fly_moving_status = fly_moving_status[(starting_point-1):length(fly_moving_status)]
  
  # for (i in 2:(length(fly_moving_status_discretized)-1)){
  #   if ((fly_moving_status_discretized[i-1]==0)&(fly_moving_status_discretized[i+1]==0)){
  #     fly_moving_status_discretized[i]=0
  #     }
  #   if ((fly_moving_status_discretized[i-1]==1)&(fly_moving_status_discretized[i+1]==1)){
  #   fly_moving_status_discretized[i]=1
  #     }
  #   }
  # for troubleshooting
  # a = replace(fly_moving_status_discretized,fly_moving_status_discretized==1,fly_moving_status)
  # a = replace(a,a==0,100)
  
  ###Labeling the No-Move and Move moments
  
  # label_for_pause = rep(0, length(fly_moving_status_discretized))
  #
  # for (i in 2:length(label_for_pause)){
  #   if ((fly_moving_status_discretized[i]==0)&(fly_moving_status_discretized[i-1]>0)){
  #     label_for_pause[i] = 1
  #   }
  #   else if ((fly_moving_status_discretized[i]>0)&(fly_moving_status_discretized[i-1]==0)){
  #     label_for_pause[i] = 2
  #   }
  #   # else if ((fly_moving_status_discretized[i]<0)&(fly_moving_status_discretized[i-1]==0)){
  #   #   label_for_pause[i] = 3
  #   # }
  #   # else if ((fly_moving_status_discretized[i]==0)&(fly_moving_status_discretized[i-1]<0)){
  #   #   label_for_pause[i] = 4
  #   # }
  # }
  
  # run_length_movement = rle(fly_moving_status_discretized)
  # Moving = run_length_movement$lengths[run_length_movement$values==1]
  # Pause = run_length_movement$lengths[run_length_movement$values==0]
  # Movement_Difference = c()
  #
  ## length(rle(fly_moving_status_discretized)$length[rle(fly_moving_status_discretized)$values==0])
  # run_length_movement$values[length(run_length_movement$values)]
  #
  # if ((run_length_movement$values[1]==0)&(run_length_movement$values[length(run_length_movement$values)]==1)){
  #   Moving = c(0,Moving)
  #   Pause = c(Pause,0)
  #   Movement_Difference = Pause - Moving
  # }
  #
  # if ((run_length_movement$values[1]==0)&(run_length_movement$values[length(run_length_movement$values)]==0)){
  #   Moving = c(0,Moving)
  #   Movement_Difference = Pause - Moving
  # }
  #
  # if ((run_length_movement$values[1]==1)&(run_length_movement$values[length(run_length_movement$values)]==0)){
  #   Movement_Difference = Pause - Moving
  # }
  #
  # if ((run_length_movement$values[1]==1)&(run_length_movement$values[length(run_length_movement$values)]==1)){
  #   Pause = c(Pause,0)
  #   Movement_Difference = Pause - Moving
  # }
  #
  # Movement_Difference = Movement_Difference/framerate
  # normalized_x = (1:length(Movement_Difference))/length(Movement_Difference)
  #
  # Movement_Difference = list(normalized_x,Movement_Difference)
  # names(Movement_Difference) = c("Pairs", "Duration")
  # return(Movement_Difference)
  return(cumsum(fly_moving_status))
}

###Calculating all the flies' cumulated moving status together by types (T/R/N)
plotting_length = c()
get_cumsums_total <- function(file_name_filter, fly.info.movement) {
  # file_names = list.files(data_dir)
  file_names = c()
  for (ind in 1:nrow(fly.info.movement)) {
    input.file <- list.files(
      path = paste0("data/",
                    fly.info.movement$experimenter[ind],
                    # "/CS/"),
                    "/Mutants/"),
      pattern = paste0(
        "ProcessedData_Fly",
        fly.info.movement$Fly[ind],
        "_",
        file_name_filter,
        # "_WT",
        "_",fly.info.movement$Genotype,
        ".csv"
      ),
      full.names = T
    )
    print(input.file)
    file_names = c(file_names, input.file)
  }
  
  # file_names = file_names[grepl(file_name_filter, file_names)]
  # Get min sequence length
  get_sequence_length <- function(file_name) {
    return (length(moving_status(file_name)))
  }
  sequence_lengths = unlist(lapply(file_names, get_sequence_length))
  min_sequence_length = min(sequence_lengths)
  
  # Concat cumsums to matirx
  cumsums = matrix(nrow = min_sequence_length, ncol = 0)
  for (file_name in file_names) {
    cumsums = cbind(cumsums, moving_status(file_name)[1:min_sequence_length])
  }
  
  # cumsums_total = rowMeans(cumsums)
  # cumsums_total = diff(cumsums_total)
  # return (cumsums_total)
  return(cumsums)
}

###Grouping flies by RTN



# fly.info.movement.T = rbind(
#   subset(fly.info.include,(fly.info.include$Genotype=="WT")&(fly.info.include$experimenter=="ES"))[subset(fly.info.include,(fly.info.include$Genotype=="WT")&(fly.info.include$experimenter=="ES"))$Fly%in%(metric.df.WT.T1[metric.df.WT.T1$Experimenter=="ES",]$Fly),],
#   subset(fly.info.include,(fly.info.include$Genotype=="WT")&(fly.info.include$experimenter=="JD"))[subset(fly.info.include,(fly.info.include$Genotype=="WT")&(fly.info.include$experimenter=="JD"))$Fly%in%(metric.df.WT.T1[metric.df.WT.T1$Experimenter=="JD",]$Fly),]
# )

# 
# fly.info.movement.T = fly.info.include[((fly.info.include$Genotype == "WT") |
#                                           (fly.info.include$Genotype == "CS")) &
#                                          (fly.info.include$Category =="T")&
#                                          (fly.info.include$experimenter!="SW"), ]

fly.info.movement.T = fly.info.include[(fly.info.include$Genotype == "SUN2") &
                                         (fly.info.include$Category =="T"), ]

# fly.info.movement.R = fly.info.include[((fly.info.include$Genotype == "WT") |
#                                           (fly.info.include$Genotype == "CS")) &
#                                          (fly.info.include$Category == "R")&
#                                          (fly.info.include$experimenter!="SW"), ]

fly.info.movement.R = fly.info.include[(fly.info.include$Genotype == "SUN2") &
                                         (fly.info.include$Category == "R"), ]

# 
# fly.info.movement.N = fly.info.include[((fly.info.include$Genotype == "WT") |
#                                           (fly.info.include$Genotype == "CS")) &
#                                          (fly.info.include$Category == "N")&
#                                          (fly.info.include$experimenter!="SW"), ]

fly.info.movement.N = fly.info.include[(fly.info.include$Genotype == "SUN2") &
                                         (fly.info.include$Category == "N"), ]

###Including All Relevant Sessions
sessions <- c(
  # "E1T1",
  # "E1R1",
  # "E1N1"

  "E1T1E1T1",
  "E1R1E1R1",
  "E1N1E1N1"
  )

cumsums_total = list()
for (i in 1:3) {
  if (i == 1) {
    fly.info.movement = fly.info.movement.T
  }
  
  if (i == 2) {
    fly.info.movement = fly.info.movement.R
  }
  
  if (i == 3) {
    fly.info.movement = fly.info.movement.N
  }
  
  cumsums = get_cumsums_total(sessions[i], fly.info.movement)
  cumsums_total = append(cumsums_total, list(cumsums))
  
}

##Determine the last common frame across all three groups
min_sequence_length = min(dim(cumsums_total[[1]])[1],
                          dim(cumsums_total[[2]])[1],
                          dim(cumsums_total[[3]])[1])




cumsums_mean = list(rowMeans(cumsums_total[[1]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[2]][1:min_sequence_length, ]),
                    rowMeans(cumsums_total[[3]][1:min_sequence_length, ]))

cumsums_median = list(
  apply(cumsums_total[[1]][1:min_sequence_length, ], 1, median),
  apply(cumsums_total[[2]][1:min_sequence_length, ], 1, median),
  apply(cumsums_total[[3]][1:min_sequence_length, ], 1, median)
)


cumsums_percentile_lower = list(
  apply(cumsums_total[[1]][1:min_sequence_length, ], 1, quantile, c(0.16)),
  apply(cumsums_total[[2]][1:min_sequence_length, ], 1, quantile, c(0.16)),
  apply(cumsums_total[[3]][1:min_sequence_length, ], 1, quantile, c(0.16))
)

cumsums_percentile_higher = list(
  apply(cumsums_total[[1]][1:min_sequence_length, ], 1, quantile, c(0.84)),
  apply(cumsums_total[[2]][1:min_sequence_length, ], 1, quantile, c(0.84)),
  apply(cumsums_total[[3]][1:min_sequence_length, ], 1, quantile, c(0.84))
)


##Preparing plot coordinates
##X axis coordinate
forward_index = c((1:min_sequence_length) / framerate)
reverse_index = rev(forward_index)
index = c(forward_index, reverse_index)

##Y axis coordinate
coordinates = list(
  append(cumsums_percentile_lower[[1]], rev(cumsums_percentile_higher[[1]])),
  append(cumsums_percentile_lower[[2]], rev(cumsums_percentile_higher[[2]])),
  append(cumsums_percentile_lower[[3]], rev(cumsums_percentile_higher[[3]]))
)

######First Training Session Begins######

##Plot learning trends

pdf("First_Training_Session_MB131B_allTRN.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 250),
  ylim = c(0, 150),
  main = "First"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[1]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.8, 0, 0, 0.1)
)
polygon(
  index,
  coordinates[[2]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0, 0, 0.8, 0.1)
)
polygon(
  index,
  coordinates[[3]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.5, 0.5, 0.5, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

lines(
  forward_index,
  cumsums_median[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_median[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_median[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

## Plot learning effect at two timepoint: at the beginning of each training session, and at the end of each traning session
pdf("First_Training_Session_Boxplot_R60D05.pdf",
    onefile = T,
    width = 10)
first_training = list(
  cumsums_total[[1]][1, ] / framerate,
  cumsums_total[[2]][1, ] / framerate,
  cumsums_total[[3]][1, ] / framerate,
  cumsums_total[[1]][min_sequence_length, ] / framerate,
  cumsums_total[[2]][min_sequence_length, ] / framerate,
  cumsums_total[[3]][min_sequence_length, ] / framerate
)

col.pool <- c("indianred3",
              "light blue",
              "grey80",
              # "indianred3","light blue","grey80",
              "indianred3",
              "light blue",
              "grey80")

boxplot(
  first_training,
  ylim = c(0, min_sequence_length / framerate),
  outline = F,
  notch = T,
  lwd = 2,
  ylab = "Cumulated Activity",
  xaxt = "n",
  col = col.pool,
  main = "First Training",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = first_training,
  method = "jitter",
  add = TRUE,
  pch = 20,
  col =  "grey40"
)

text(
  x = (1:length(first_training)) - 0.1,
  y = 150,
  labels = c(
    length(first_training[[1]]),
    length(first_training[[2]]),
    length(first_training[[3]]),
    length(first_training[[4]]),
    length(first_training[[5]]),
    length(first_training[[6]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(3.5, 3.5), c(-11, 351),
      col = "light grey",
      lty = 1)

dev.off()

pdf("First_Training_Session_R60D05_allN.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 200),
  ylim = c(0, 100),
  main = "First"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[3]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.5, 0.5, 0.5, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

pdf("First_Training_Session_R60D05_allR.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 200),
  ylim = c(0, 100),
  main = "First"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[2]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0, 0, 0.8, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

# T
pdf("First_Training_Session_R60D05_allT.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 200),
  ylim = c(0, 100),
  main = "First"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[1]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.8, 0, 0, 0.1)
)


lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

#Second training Rate Approximation
pdf("First Training Session Learning RateMB131B.pdf")
cumsums_mean_diff = diff(cumsums_mean[[1]])
cumsums_mean_diff = c(cumsums_mean_diff, cumsums_mean_diff[length(cumsums_mean_diff)])
cumsums_mean_diff_rm = rollmean(cumsums_mean_diff, 50, fill = NA)
lm.fit.orig <- lm(
  cumsums_mean_diff_rm ~ forward_index,
  data.frame(forward_index, cumsums_mean_diff_rm)
)
plot(
  forward_index,
  cumsums_mean_diff_rm,
  type = 'l',
  col = "red",
  main = "First Training Session",
  ylim = c(0, 1)
)
abline(
  lm.fit.orig$coefficients[[1]],
  lm.fit.orig$coefficients[[2]],
  col = rgb(1, 0, 0, 0.5),
  lty = 1,
  lwd = 2
)
cumsums_mean_diff = diff(cumsums_mean[[2]])
cumsums_mean_diff = c(cumsums_mean_diff, cumsums_mean_diff[length(cumsums_mean_diff)])
cumsums_mean_diff_rm = rollmean(cumsums_mean_diff, 50, fill = NA)
lm.fit.orig <- lm(
  cumsums_mean_diff_rm ~ forward_index,
  data.frame(forward_index, cumsums_mean_diff_rm)
)
lines(forward_index,
      cumsums_mean_diff_rm,
      type = 'l',
      col = "blue")
abline(
  lm.fit.orig$coefficients[[1]],
  lm.fit.orig$coefficients[[2]],
  col = rgb(0, 0, 1, 0.5),
  lty = 1,
  lwd = 2
)
cumsums_mean_diff = diff(cumsums_mean[[3]])
cumsums_mean_diff = c(cumsums_mean_diff, cumsums_mean_diff[length(cumsums_mean_diff)])
cumsums_mean_diff_rm = rollmean(cumsums_mean_diff, 50, fill = NA)
lm.fit.orig <- lm(
  cumsums_mean_diff_rm ~ forward_index,
  data.frame(forward_index, cumsums_mean_diff_rm)
)
lines(forward_index,
      cumsums_mean_diff_rm,
      type = 'l',
      col = "black")
abline(
  lm.fit.orig$coefficients[[1]],
  lm.fit.orig$coefficients[[2]],
  col = rgb(0, 0, 0, 0.5),
  lty = 1,
  lwd = 2
)
dev.off()

######First Training Session Ends######

######Second Training Session Begins######

##Plot learning trends

pdf("Second_Training_Session_MB131B_allTRN.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 250),
  ylim = c(0, 150),
  main = "Second"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[1]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.8, 0, 0, 0.1)
)
polygon(
  index,
  coordinates[[2]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0, 0, 0.8, 0.1)
)
polygon(
  index,
  coordinates[[3]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.5, 0.5, 0.5, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

lines(
  forward_index,
  cumsums_median[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_median[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_median[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

## Plot learning effect at two timepoint: at the beginning of each training session, and at the end of each traning session
pdf(
  "Second_Training_Session_Boxplot_SUN2_allTRN.pdf",
  onefile = T,
  width = 10
)
second_training = list(
  cumsums_total[[1]][1, ] / framerate,
  cumsums_total[[2]][1, ] / framerate,
  cumsums_total[[3]][1, ] / framerate,
  cumsums_total[[1]][min_sequence_length, ] / framerate,
  cumsums_total[[2]][min_sequence_length, ] / framerate,
  cumsums_total[[3]][min_sequence_length, ] / framerate
)

col.pool <- c("indianred3",
              "light blue",
              "grey80",
              # "indianred3","light blue","grey80",
              "indianred3",
              "light blue",
              "grey80")

boxplot(
  second_training,
  ylim = c(0, min_sequence_length / framerate),
  outline = F,
  notch = T,
  lwd = 2,
  ylab = "Cumulated Activity",
  xaxt = "n",
  col = col.pool,
  main = "Second Training",
  ann = FALSE
)
stripchart(
  vertical = TRUE,
  x = second_training,
  method = "jitter",
  add = TRUE,
  pch = 20,
  col =  "grey40"
)

text(
  x = (1:length(second_training)) - 0.1,
  y = 150,
  labels = c(
    length(second_training[[1]]),
    length(second_training[[2]]),
    length(second_training[[3]]),
    length(second_training[[4]]),
    length(second_training[[5]]),
    length(second_training[[6]])
  ),
  xpd = T,
  srt = 0,
  adj = 0
)

lines(c(3.5, 3.5), c(-11, 351),
      col = "light grey",
      lty = 1)

dev.off()

pdf("Second_Training_Session_SUN2_allN.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 200),
  ylim = c(0, 100),
  main = "Second"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[3]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.5, 0.5, 0.5, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

pdf("Second_Training_Session_SUN2_allR.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 200),
  ylim = c(0, 100),
  main = "Second"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[2]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0, 0, 0.8, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

# T
pdf("Second_Training_Session_SUN2_allT.pdf",
    onefile = T,
    width = 10)
plot(
  1,
  type = "n",
  xlab = "",
  ylab = "",
  xlim = c(0, 200),
  ylim = c(0, 100),
  main = "Second"
)

#First: x 250, y 143
#Second: x 350, y 200

polygon(
  index,
  coordinates[[1]] / framerate,
  lty = 2,
  lwd = 2,
  border = NA,
  col = rgb(0.8, 0, 0, 0.1)
)

lines(
  forward_index,
  cumsums_mean[[1]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.8, 0, 0, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[2]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.0, 0, 0.8, 0.5)
)
lines(
  forward_index,
  cumsums_mean[[3]] / framerate,
  lty = 1,
  lwd = 2,
  col = rgb(0.5, 0.5, 0.5, 0.5)
)

dev.off()

#Second training Rate Approximation
pdf("Second Training Session Learning RateMB131B.pdf")
cumsums_mean_diff = diff(cumsums_mean[[1]])
cumsums_mean_diff = c(cumsums_mean_diff, cumsums_mean_diff[length(cumsums_mean_diff)])
cumsums_mean_diff_rm = rollmean(cumsums_mean_diff, 50, fill = NA)
lm.fit.orig <- lm(
  cumsums_mean_diff_rm ~ forward_index,
  data.frame(forward_index, cumsums_mean_diff_rm)
)
plot(
  forward_index,
  cumsums_mean_diff_rm,
  type = 'l',
  col = "red",
  main = "Second Training Session",
  ylim = c(0, 1)
)
abline(
  lm.fit.orig$coefficients[[1]],
  lm.fit.orig$coefficients[[2]],
  col = rgb(1, 0, 0, 0.5),
  lty = 1,
  lwd = 2
)
cumsums_mean_diff = diff(cumsums_mean[[2]])
cumsums_mean_diff = c(cumsums_mean_diff, cumsums_mean_diff[length(cumsums_mean_diff)])
cumsums_mean_diff_rm = rollmean(cumsums_mean_diff, 50, fill = NA)
lm.fit.orig <- lm(
  cumsums_mean_diff_rm ~ forward_index,
  data.frame(forward_index, cumsums_mean_diff_rm)
)
lines(forward_index,
      cumsums_mean_diff_rm,
      type = 'l',
      col = "blue")
abline(
  lm.fit.orig$coefficients[[1]],
  lm.fit.orig$coefficients[[2]],
  col = rgb(0, 0, 1, 0.5),
  lty = 1,
  lwd = 2
)
cumsums_mean_diff = diff(cumsums_mean[[3]])
cumsums_mean_diff = c(cumsums_mean_diff, cumsums_mean_diff[length(cumsums_mean_diff)])
cumsums_mean_diff_rm = rollmean(cumsums_mean_diff, 50, fill = NA)
lm.fit.orig <- lm(
  cumsums_mean_diff_rm ~ forward_index,
  data.frame(forward_index, cumsums_mean_diff_rm)
)
lines(forward_index,
      cumsums_mean_diff_rm,
      type = 'l',
      col = "black")
abline(
  lm.fit.orig$coefficients[[1]],
  lm.fit.orig$coefficients[[2]],
  col = rgb(0, 0, 0, 0.5),
  lty = 1,
  lwd = 2
)
dev.off()
######Second Training Session Ends######
###P value###
pvalue_during_training =  c(
  wilcox.test(first_training[[4]],first_training[[5]])$p.value,
  wilcox.test(first_training[[4]],first_training[[6]])$p.value,
  wilcox.test(first_training[[5]],first_training[[6]])$p.value,
  wilcox.test(second_training[[4]],second_training[[5]])$p.value,
  wilcox.test(second_training[[4]],second_training[[6]])$p.value,
  wilcox.test(second_training[[5]],second_training[[6]])$p.value
)