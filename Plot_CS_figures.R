# Plot Fig 3
setwd("G:/Behavioral_project/behavior_experiment_data/Analysis/")

fly.info.include = fly.info[ind.include, ]
#WT flies
query.genotype <- c("WT","CS")
query.fly = fly.info.include[((fly.info.include$Genotype == "WT") |
                                (fly.info.include$Genotype == "CS")) &
                               (fly.info.include$experimenter!="SW"), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "WT") |
                                         (fly.info.include$Genotype == "CS")) &
                                        (fly.info.include$experimenter!="SW"), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "WT") |
                      (fly.info.include$Genotype == "CS")) &
                     (fly.info.include$experimenter!="SW"), ],
  "fly_info_include_WT.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

metrices <- read.table("metrics/list_metrices.csv",
                       stringsAsFactors = F,
                       sep = "\t")[, 1]
sessions <- c(
  "E1T1E1",
  "E1R1E1",
  "E1N1E1",
  "E1T1E1T1E1",
  "E1R1E1R1E1",
  "E1N1E1N1E1",
  "E1T1E1T1E1T1E1",
  "E1R1E1R1E1R1E1",
  "E1N1E1N1E1N1E1",
  "E1T1E1T1E1T1E1T1E1",
  "E1R1E1R1E1R1E1R1E1",
  "E1N1E1N1E1N1E1N1E1"
)

pdf(paste0("ind_of_interest", query.genotype[1], "_", Sys.Date(), ".pdf"),
    onefile = T, width = 5, height = 5
)

ind_of_interest = c(29, 28, 13)
for (metric.ind in ind_of_interest) {
  input.file = paste0("metrics/metric_", metric.ind, ".csv")
  if (!file.exists(input.file)) {
    next
  }
  
  metric.df = read.csv(input.file)
  y = list()
  
  ## E1 data
  session = "E1"
  for (category in c("T", "R", "N")) {
    query.session = gsub("X", category, session)
    ind <- metric.df$Session == query.session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly &
      metric.df$Experimenter  %in%  query.experimenter
    z = metric.df[ind, "Value"]
    y = append(y, list(na.omit(z)))
  }
  
  ## input sessions data
  
  for (session in sessions) {
    ind <- metric.df$Session == session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    z = metric.df[ind, "Value"]
    
    y = append(y, list(na.omit(z)))
  }
  y.1 = y
  ## special cases
  y_text = c()
  print(metric.ind)
  if (metric.ind == 1) {
    yrange = c(0, 600)
    y_text = 610
  }
  
  if (metric.ind == 2) {
    yrange = c(0, 1)
    y_text = 0.8
  }
  
  if (metric.ind == 3) {
    yrange = c(0, 100)
    y_text = 100
  }
  
  if (metric.ind == 4) {
    yrange = c(0, 600)
    y_text = 600
  }
  
  if (metric.ind == 5) {
    yrange = c(0, 30)
    y_text = 30
  }
  
  if (metric.ind == 6) {
    yrange = c(0, 30)
    y_text = 30
  }
  
  if (metric.ind == 7) {
    yrange = c(0, 10)
    y_text = 8.5
  }
  
  if (metric.ind == 8) {
    yrange = c(0, 10)
    y_text = 8.3
  }
  
  
  if (metric.ind == 9) {
    yrange = c(0, 600)
    y_text = 600
  }
  
  if (metric.ind == 10) {
    yrange = c(0, 200)
    y_text = 160
  }
  
  if (metric.ind == 11) {
    yrange = c(0, 200)
    y_text = 110
  }
  
  if (metric.ind == 12) {
    yrange = c(0, 1)
    y_text = 1
  }
  
  if (metric.ind == 13) {
    yrange = c(-1, 1)
    y_text = 0.7
  }
  
  if (metric.ind == 14) {
    yrange = c(-1, 1)
    y_text = 0.85
  }
  
  if (metric.ind == 15) {
    yrange = c(-1, 1)
    y_text = 0.12
  }
  
  if (metric.ind == 16) {
    yrange = c(-1, 1)
    y_text = 0.42
  }
  
  if (metric.ind == 17) {
    yrange = c(-1, 1)
    y_text = 0.42
  }
  
  if (metric.ind == 18) {
    yrange = c(0, 600)
    y_text = 600
  }
  
  
  if (metric.ind == 19) {
    yrange = c(0, 600)
    y_text = 600
  }
  
  if (metric.ind == 20) {
    yrange = c(0.5, 1)
    y_text = 1.01
  }
  
  
  if (metric.ind == 21) {
    yrange = c(0, 0.5)
    y_text = 0.142
  }
  
  if (metric.ind == 22) {
    yrange = c(0, 1)
    y_text = 1
  }
  
  if (metric.ind == 23) {
    yrange = c(0, 0.5)
    y_text = 1
  }
  
  if (metric.ind == 24) {
    yrange = c(-0.6, 0.6)
    y_text = 0.6
  }
  
  if (metric.ind == 25) {
    yrange = c(-0.6, 0.6)
    y_text = 0.6
  }
  
  if (metric.ind == 26) {
    yrange = c(-0.4, 0.6)
    y_text = 0.6
  }
  
  if (metric.ind == 27) {
    yrange = c(-0.4, 0.6)
    y_text = 0.6
  }
  
  if (metric.ind == 28) {
    yrange = c(0, 100)
    y_text = 100
  }
  
  
  if (metric.ind == 29) {
    yrange = c(0, 1)
    y_text = 1
  }
  
  if (metric.ind == 30) {
    yrange = c(0, 600)
    y_text = 600
  }
  
  if (metric.ind == 31) {
    yrange = c(0, 2050)
    y_text = 250
  }
  input.y = y.1[1:3]
  input.y = append(input.y, y.1[7:9])
  yy.1T = rep("1stE1_1", length(input.y[[1]]))
  yy.1R = rep("1stE1_2", length(input.y[[2]]))
  yy.1N = rep("1stE1_3", length(input.y[[3]]))
  yy.3T = rep("3rdE1_1", length(input.y[[4]]))
  yy.3R = rep("3rdE1_2", length(input.y[[5]]))
  yy.3N = rep("3rdE1_3", length(input.y[[6]]))
  yy.label = c(yy.1T, yy.1R, yy.1N, yy.3T, yy.3R, yy.3N)
  
  input.y_1T = as.numeric(input.y[[1]])
  input.y_1R = as.numeric(input.y[[2]])
  input.y_1N = as.numeric(input.y[[3]])
  input.y_3T = as.numeric(input.y[[4]])
  input.y_3R = as.numeric(input.y[[5]])
  input.y_3N = as.numeric(input.y[[6]])
  input.yy = c(
    input.y_1T,
    input.y_1R,
    input.y_1N,
    input.y_3T,
    input.y_3R,
    input.y_3N
  )
  
  input.y.df = data.frame(input.yy, yy.label)
  colnames(input.y.df) <- c("Value", "Sessions")
  col.pool <- c(
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80"
  )
  
  boxplot(
    Value ~ Sessions,
    data = input.y.df,
    ylim = yrange,
    outline = F,
    notch = T,
    lwd = 2,
    ylab = metrices[metric.ind],
    xlab = "",
    xaxt = "n",
    col = col.pool
    # main = paste0(query.genotype, "-", metrices[metric.ind], "\n")
  )
  stripchart(
    Value ~ Sessions,
    vertical = TRUE,
    data = input.y.df,
    method = "jitter",
    add = TRUE,
    pch = 15,
    cex = 0.5,
    col =  "grey40"
  )
  
  if (any(is.na(yrange)) |
      any(is.infinite(yrange)) | any(is.nan(yrange))) {
    yrange = c(-1, 1)
    ylim = c(-1, 1)
  }
  
  text((1:length(input.y)) - 0.1,
       # yrange[2]*0.6,
       y_text,
       paste0(sapply(input.y, length)),
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  n = length(input.y) / length(sessions)
  
  for (i in c(3, 6)) {
    lines(c(i, i) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "light grey",
          lty = 1)
  }
}
dev.off()


