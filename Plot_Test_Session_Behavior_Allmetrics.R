#This script is for after fit_lm_data_cleaning.R

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

#MB009B x DopR1-IR flies

query.genotype <- c("MB009B x DopR1-IR")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB009B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB009B x DopR1-IR")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB009B x DopR1-IR")), ],
  "fly_info_include_009BxDopR1-IR.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)


#MB419B x DopR1-IR flies

query.genotype <- c("MB419B x DopR1-IR")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB419B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB419B x DopR1-IR")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB419B x DopR1-IR")), ],
  "fly_info_include_419BxDopR1-IR.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)


#MB607B x DopR1-IR flies

query.genotype <- c("MB607B x DopR1-IR")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB607B x DopR1-IR")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB607B x DopR1-IR")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB607B x DopR1-IR")), ],
  "fly_info_include_607BxDopR1-IR.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)



#SUN1 flies
query.genotype <- c("SUN1")
query.fly = fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                               (fly.info.include$experimenter!="SW"), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                                        (fly.info.include$experimenter!="SW"), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                     (fly.info.include$experimenter!="SW"), ],
  "fly_info_include_SUN1.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#SUN2 flies
query.genotype <- c("SUN2")
query.fly = fly.info.include[((fly.info.include$Genotype == "SUN2")) &
                               (fly.info.include$experimenter!="SW"), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN2")) &
                                        (fly.info.include$experimenter!="SW"), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "SUN2")) &
                     (fly.info.include$experimenter!="SW"), ],
  "fly_info_include_SUN2.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#SUN3 flies
query.genotype <- c("SUN3")
query.fly = fly.info.include[((fly.info.include$Genotype == "SUN3")) &
                               (fly.info.include$experimenter!="SW"), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN3")) &
                                        (fly.info.include$experimenter!="SW"), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "SUN3")) &
                     (fly.info.include$experimenter!="SW"), ],
  "fly_info_include_SUN3.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#"MB607B x JU30" flies
query.genotype <- c("MB607B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB607B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB607B x JU30")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB607B x JU30")), ],
  "fly_info_include_607B.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#"MB009B x JU30" flies
query.genotype <- c("MB009B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB009B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB009B x JU30")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB009B x JU30")), ],
  "fly_info_include_009B.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#"MB131B x JU30" flies
query.genotype <- c("MB131B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB131B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB131B x JU30")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB131B x JU30")), ],
  "fly_info_include_131B.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#"MB419B x JU30" flies
query.genotype <- c("MB419B x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB419B x JU30")), ]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB419B x JU30")), ]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB419B x JU30")), ],
  "fly_info_include_419B.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#R60D05 x JU30 flies
query.genotype <- c("R60D05 x JU30")
query.fly = fly.info.include[(fly.info.include$Genotype == "R60D05 x JU30"),]$Fly
query.experimenter = fly.info.include[(fly.info.include$Genotype == "R60D05 x JU30"),]$experimenter
write.table(
  fly.info.include[(fly.info.include$Genotype == "R60D05 x JU30"),],
  "fly_info_include_R60D05xJU30.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#JG17 x JU30
query.genotype <- c("JG17 x JU30")
query.fly = fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "JG17 x JU30")),],
  "fly_info_include_JG17.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#R60D05 x PKCi 
query.genotype <- c("R60D05 x PKCi", "R60D05 x PKCI")
query.fly = fly.info.include[((fly.info.include$Genotype == "R60D05 x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "R60D05 x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "R60D05 x PKCi")),],
  "fly_info_include_R60D05xPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#JG17 x PKCi
query.genotype <- c("JG17 x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "JG17 x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "JG17 x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "JG17 x PKCi")),],
  "fly_info_include_JG17xPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#CS x PKCi
query.genotype <- c("CS x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "CS x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "CS x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "JG17 x PKCi")),],
  "fly_info_include_CSxPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#MB009B x PKCi 
query.genotype <- c("MB009B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB009B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB009B x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB009B x PKCi")),],
  "fly_info_include_MB009BxPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#MB131B x PKCi
query.genotype <- c("MB131B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB131B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB131B x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB131B x PKCi")),],
  "fly_info_include_MB131BxPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#MB419B x PKCi
query.genotype <- c("MB419B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB419B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB419B x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB419B x PKCi")),],
  "fly_info_include_MB419BxPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

#MB607B x PKCi
query.genotype <- c("MB607B x PKCi")
query.fly = fly.info.include[((fly.info.include$Genotype == "MB607B x PKCi")),]$Fly
query.experimenter = fly.info.include[((fly.info.include$Genotype == "MB607B x PKCi")),]$experimenter
write.table(
  fly.info.include[((fly.info.include$Genotype == "MB607B x PKCi")),],
  "fly_info_include_MB607BxPKCi.csv",
  col.names = T,
  row.names = F,
  quote = F,
  sep = ","
)

## Read metric names
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

pdf(
  # "fly_metric_allmetricdf_JG17xPKCi_allflies_Filter1_063018.pdf",
  "fly_metric_allmetricdf_CS_allflies_Filter1_121418.pdf",
  # "fly_metric_allmetricdf_R60D05xJU30_allflies_Filter1_102518.pdf",
  # "fly_metric_allmetricdf_JG17xJU30_allflies_Filter1_102518.pdf",
  # "fly_metric_allmetricdf_MB009BxJU30_allflies_Filter1_102518.pdf",
  # "fly_metric_allmetricdf_MB131BxJU30_allflies_Filter1_102518.pdf",
  # "fly_metric_allmetricdf_MB419BxJU30_allflies_Filter1_102518.pdf",
  # "fly_metric_allmetricdf_MB607BxJU30_allflies_Filter1_102518.pdf",
  # "fly_metric_allmetricdf_R60D05xPKCi_allflies_Filter1_063018.pdf",
  # "fly_metric_allmetricdf_MB419BxDopR1IR_allflies_Filter1_111918.pdf",
  # "fly_metric_allmetricdf_MB009BxDopR1IR_allflies_Filter1_111918.pdf",
  # "fly_metric_allmetricdf_MB607BxDopR1IR_allflies_Filter1_111918.pdf",
  # "fly_metric_allmetricdf_SUN3_allflies_Filter1_102518.pdf",
  onefile = T,
  width = 10
)

p_value_summary = matrix(nrow = 9, ncol = 0)

for (metric.ind in 1:length(metrices)) {
  input.file = paste0("metrics/metric_", metric.ind, ".csv")
  if (!file.exists(input.file)) {
    next
  }
  
  metric.df = read.csv(input.file)
  
  if (sum(colnames(metric.df) %in% "value.w") == 0) {
    #metric.df$value.w = metric.df$value
    #next;
  }
  ## covariates of interest: genotype, session
  y = list()
  
  ## E1 data
  session = "E1"
  for (category in c("T", "R")) {
    query.session = gsub("X", category, session)
    
    ind <- metric.df$Session == query.session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly &
      metric.df$Experimenter  %in%  query.experimenter
    
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly &
      metric.df$Experimenter  %in%  query.experimenter
    
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    z = metric.df[ind, "Value"]
    
    y = append(y, list(na.omit(z)))
  }
  
  for (category in c("N")) {
    query.session = gsub("X", category, session)
    ind <- metric.df$Session == query.session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly &
      metric.df$Experimenter  %in%  query.experimenter
    
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    z = metric.df[ind, "Value"]
    
    y = append(y, list(na.omit(z)))
  }
  
  
  ## input sessions data
  
  for (session in sessions) {
    # if (grepl("N", session) == F) {
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
  
  print(yrange)
  print(y_text)
  
  input.y = y.1[1:9]
  yy.1T = rep("1stE1_1", length(input.y[[1]]))
  yy.1R = rep("1stE1_2", length(input.y[[2]]))
  yy.1N = rep("1stE1_3", length(input.y[[3]]))
  yy.2T = rep("2ndE1_1", length(input.y[[4]]))
  yy.2R = rep("2ndE1_2", length(input.y[[5]]))
  yy.2N = rep("2ndE1_3", length(input.y[[6]]))
  yy.3T = rep("3rdE1_1", length(input.y[[7]]))
  yy.3R = rep("3rdE1_2", length(input.y[[8]]))
  yy.3N = rep("3rdE1_3", length(input.y[[9]]))
  yy.label = c(yy.1T, yy.1R, yy.1N, yy.2T, yy.2R, yy.2N, yy.3T, yy.3R, yy.3N)
  
  input.y_1T = as.numeric(input.y[[1]])
  input.y_1R = as.numeric(input.y[[2]])
  input.y_1N = as.numeric(input.y[[3]])
  input.y_2T = as.numeric(input.y[[4]])
  input.y_2R = as.numeric(input.y[[5]])
  input.y_2N = as.numeric(input.y[[6]])
  input.y_3T = as.numeric(input.y[[7]])
  input.y_3R = as.numeric(input.y[[8]])
  input.y_3N = as.numeric(input.y[[9]])
  input.yy = c(
    input.y_1T,
    input.y_1R,
    input.y_1N,
    input.y_2T,
    input.y_2R,
    input.y_2N,
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
  
  p_value = c(wilcox.test(input.y_1T,input.y_1N)$p.value,
              wilcox.test(input.y_1R,input.y_1N)$p.value,
              wilcox.test(input.y_1T,input.y_1R)$p.value,
              wilcox.test(input.y_2T,input.y_2N)$p.value,
              wilcox.test(input.y_2R,input.y_2N)$p.value,
              wilcox.test(input.y_2T,input.y_2R)$p.value,
              wilcox.test(input.y_3T,input.y_3N)$p.value,
              wilcox.test(input.y_3R,input.y_3N)$p.value,
              wilcox.test(input.y_3T,input.y_3R)$p.value
  )
  
  p_value_summary = cbind(p_value_summary,p_value)
}
dev.off()

colnames(p_value_summary) = c(1:31)
rownames(p_value_summary) = c("1T-1N",
                              "1R-1N",
                              "1T-1R",
                              "2T-2N",
                              "2R-2N",
                              "2T-2R",
                              "3T-3N",
                              "3R-3N",
                              "3T-3R"
                              
)

write.table(
  p_value_summary,
  # "P_VALUE_SUMMARY_CS_111918.csv",
  # "P_VALUE_SUMMARY_MB419BxDopR1-IR_111918.csv",
  # "P_VALUE_SUMMARY_MB009BxDopR1-IR_111918.csv",
  "P_VALUE_SUMMARY_MB607BxDopR1-IR_111918.csv",
  # "P_VALUE_SUMMARY_R60D05xJU30_111918.csv",
  # "P_VALUE_SUMMARY_JG17xJU30.csv",
  # "P_VALUE_SUMMARY_MB009BxJU30.csv",
  # "P_VALUE_SUMMARY_MB131BxJU30.csv",
  # "P_VALUE_SUMMARY_MB419BxJU30.csv",
  # "P_VALUE_SUMMARY_MB607BxJU30.csv",
  # "P_VALUE_SUMMARY_R60D05xPKCi.csv",
  # "P_VALUE_SUMMARY_JG17xPKCi.csv",
  # "P_VALUE_SUMMARY_SUN3.csv",
  col.names = T,
  row.names = T,
  quote = F,
  sep = ","
)