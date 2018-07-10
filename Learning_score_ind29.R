#We define learning index as (3rd E1's performance - 1st E1's performance)/(1st E1's performance), calculated at an individual level.
#This script follows the laser_power_based_segmentation.R, and cannot be run prior to running laser_power_based_segmentation.R

input.y.df = data.frame()

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
          fly_genotype = "CS"

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
          fly_genotype = "SUN1"

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
          fly_genotype = "SUN2"

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
          fly_genotype = "SUN3"

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
          fly_genotype = "MB607BxJU30"
          
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
          fly_genotype = "MB009BxJU30"

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
          fly_genotype = "MB131BxJU30"

    #R60D05xJU30 flies
          query.genotype <- c("R60 D05 x JU30","R60D05 X JU30","R60D05 X JU30")
          query.fly = fly.info.include[((fly.info.include$Genotype == "R60 D05 x JU30")|(fly.info.include$Genotype == "R60D05 X JU30")|(fly.info.include$Genotype =="R60D05 X JU30")),]$Fly
          query.experimenter = fly.info.include[((fly.info.include$Genotype == "R60 D05 x JU30")|(fly.info.include$Genotype == "R60D05 X JU30")|(fly.info.include$Genotype =="R60D05 X JU30")),]$experimenter
          write.table(
            fly.info.include[((fly.info.include$Genotype == "R60 D05 x JU30")|(fly.info.include$Genotype == "R60D05 X JU30")|(fly.info.include$Genotype =="R60D05 X JU30")),],
            "fly_info_include_R60D05.csv",
            col.names = T,
            row.names = F,
            quote = F,
            sep = ","
          )
          fly_genotype = "R60D05xJU30"

    #JG17xJU30
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
          fly_genotype = "JG17xJU30"

    #R60 D05 x PKCi 
          query.genotype <- c("R60 D05 x PKCi")
          query.fly = fly.info.include[((fly.info.include$Genotype == "R60 D05 x PKCi")),]$Fly
          query.experimenter = fly.info.include[((fly.info.include$Genotype == "R60 D05 x PKCi")),]$experimenter
          write.table(
            fly.info.include[((fly.info.include$Genotype == "R60 D05 x PKCi")),],
            "fly_info_include_R60D05xPKCi.csv",
            col.names = T,
            row.names = F,
            quote = F,
            sep = ","
          )
          fly_genotype = "R60D05xPKCi"
  
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
          fly_genotype = "JG17xPKCi"

#########Plotting Different Genotypes together#########
          
   
          
          
          metric.ind = 29
          
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
              metric.df$Fly %in% query.fly&
              metric.df$Experimenter  %in%  query.experimenter
            ind.E1 <- metric.df$Session == "E1" &
              metric.df$Genotype %in% query.genotype &
              metric.df$Category == category &
              metric.df$Fly %in% query.fly&
              metric.df$Experimenter  %in%  query.experimenter
            z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
            y = append(y, list(na.omit(z)))
          }
          for (category in c("N")) {
            query.session = gsub("X", category, session)
            ind <- metric.df$Session == query.session &
              metric.df$Genotype %in% query.genotype &
              metric.df$Category == category &
              metric.df$Fly %in% query.fly&
              metric.df$Experimenter  %in%  query.experimenter
            ind.E1 <- metric.df$Session == "E1" &
              metric.df$Genotype %in% query.genotype &
              metric.df$Category == category &
              metric.df$Fly %in% query.fly&
              metric.df$Experimenter  %in%  query.experimenter
            z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
            y = append(y, list(na.omit(z)))
          }
          ## input sessions data
          for (session in sessions) {
            if (grepl("T", session) == T) {
              ind.E1 <- metric.df$Session == "E1" &
                metric.df$Genotype %in% query.genotype &
                metric.df$Category == "T" &
                metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
              ind <- metric.df$Session == session &
                metric.df$Genotype %in% query.genotype &
                metric.df$Category == "T" &
                metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
              z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
              
            } 
            if (grepl("R", session) == T) {
              ind.E1 <- metric.df$Session == "E1" &
                metric.df$Genotype %in% query.genotype &
                metric.df$Category == "R" &
                metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
              ind <- metric.df$Session == session &
                metric.df$Genotype %in% query.genotype &
                metric.df$Category == "R" &
                metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
              z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
            } 
            if (grepl("N", session) == T) {
              ind.E1 <- metric.df$Session == "E1" &
                metric.df$Genotype %in% query.genotype &
                metric.df$Category == "N" &
                metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
              ind <- metric.df$Session == session &
                metric.df$Genotype %in% query.genotype &
                metric.df$Category == "N" &
                metric.df$Fly %in% query.fly&
                metric.df$Experimenter  %in%  query.experimenter
              z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
            }
            
            y = append(y, list(na.omit(z)))
          }
          y.1 = y
          # yrange = c(min(sapply(y,min)),max(sapply(y,max)))
          ## special cases
          y_text = c()
          print(metric.ind)
          print(yrange)
          print(y_text)
          # input.y = y.1[1:9]
          input.y = y.1[7:9]
          
          yy.3T = rep(paste0(fly_genotype,"_3rdE1_1"), length(input.y[[1]]))
          yy.3R = rep(paste0(fly_genotype,"_3rdE1_2"), length(input.y[[2]]))
          yy.3N = rep(paste0(fly_genotype,"_3rdE1_3"), length(input.y[[3]]))
          
          yy.label = c(
            # yy.1T, yy.1R, yy.1N, 
            # yy.2T, yy.2R, yy.2N, 
            yy.3T, yy.3R, yy.3N
          )
          
          input.y_3T = as.numeric(input.y[[1]])
          input.y_3R = as.numeric(input.y[[2]])
          input.y_3N = as.numeric(input.y[[3]])
          
          input.yy = c(
            input.y_3T,
            input.y_3R,
            input.y_3N
          )
          input.y.df.pre = data.frame()
          input.y.df.pre = data.frame(input.yy, yy.label)
          
          input.y.df = rbind(input.y.df, input.y.df.pre)
          colnames(input.y.df) <- c("Value", "Genotype_Sessions")
          
          
          
          
          
          col.pool <- c(
            # "indianred3",
            # "light blue",
            # "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
            "indianred3",
            "light blue",
            "grey80",
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
          
          
          pdf(
            "LearningIndexComparison_070318.pdf",
            onefile = T,
            width = 10
          )
          
          
          boxplot(
            Value ~ Genotype_Sessions,
            data = input.y.df,
            ylim = c(-2,20),
            outline = F,
            notch = T,
            lwd = 2,
            ylab = "Learning Index",
            xlab = "",
            xaxt = "n",
            col = col.pool
            # main = paste0(query.genotype, "-", metrices[metric.ind], "\n")
          )
          stripchart(
            Value ~ Genotype_Sessions,
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
          
          # boxplot(input.y,outline=F,notch=T,
          #         main=paste0(query.genotype, "-", metrices[metric.ind],"\n"
          #                     # extra.title
          #         ),
          #         col=col.pool,
          #         ylim=yrange,
          #         ylab=metrices[metric.ind],
          #         xaxt='n'
          # )
          text((1:length(input.y.df)) - 0.1,
               18,
               paste0(sapply(input.y.df, length)),
               xpd = T,
               srt = 0,
               adj = 0
          )
          
          n = length(input.y) / length(sessions)
          
          # for(i in c(3,6,9,13)){
          # for(i in c(3,6,9,12)){
          #   #for(i in c(3,6,10)){
          #   lines(c(i,i)+0.5,
          #         c(yrange[1]-1e3,yrange[1]+1e3),
          #         col="light grey",
          #         lty=1)
          # }

          for (i in c(3,6,9,12,15,18,21,24,27,30)) {
          # for (i in c(3)) {
            lines(c(i, i) + 0.5,
                  c(yrange[1] - 1e3, yrange[1] + 1e3),
                  col = "light grey",
                  lty = 1)
          }
          
          p_value = c(
            # wilcox.test(input.y_1T,input.y_1N)$p.value,
            # wilcox.test(input.y_1R,input.y_1N)$p.value,
            # wilcox.test(input.y_1T,input.y_1R)$p.value,
            wilcox.test(input.y_2T,input.y_2N)$p.value,
            wilcox.test(input.y_2R,input.y_2N)$p.value,
            wilcox.test(input.y_2T,input.y_2R)$p.value,
            wilcox.test(input.y_3T,input.y_3N)$p.value,
            wilcox.test(input.y_3R,input.y_3N)$p.value,
            wilcox.test(input.y_3T,input.y_3R)$p.value
          )
          # text((1:length(input.y)) - 0.1,
          #      15,
          #      paste0(sapply(input.y, length)),
          #      xpd = T,
          #      srt = 0,
          #      adj = 0
          # )
          
          dev.off()          
          
          
          
          
pdf(
  # "fly_metric_ind29_CS_allflies_Filter1_063018.pdf",
  paste0("fly_metric_ind29_",fly_genotype,"allflies_Filter1_070218.pdf"  ),
  onefile = T,
  width = 10
)

p_value_summary = matrix(nrow = 9, ncol = 0)

metric.ind = 29
  
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
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
    y = append(y, list(na.omit(z)))
  }
  for (category in c("N")) {
    query.session = gsub("X", category, session)
    ind <- metric.df$Session == query.session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
    y = append(y, list(na.omit(z)))
  }
  ## input sessions data
  for (session in sessions) {
    if (grepl("T", session) == T) {
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "T" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind <- metric.df$Session == session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "T" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
    
    } 
    if (grepl("R", session) == T) {
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "R" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind <- metric.df$Session == session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "R" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
    } 
    if (grepl("N", session) == T) {
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "N" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind <- metric.df$Session == session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "N" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      z = (metric.df[ind,"Value"] - metric.df[ind.E1,"Value"])/metric.df[ind.E1,"Value"]
    }
    
    y = append(y, list(na.omit(z)))
  }
  y.1 = y
  # yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  ## special cases
  y_text = c()
  print(metric.ind)
  print(yrange)
  print(y_text)
  # input.y = y.1[1:9]
  input.y = y.1[4:9]
  # yy.1T = rep("1stE1_1", length(input.y[[1]]))
  # yy.1R = rep("1stE1_2", length(input.y[[2]]))
  # yy.1N = rep("1stE1_3", length(input.y[[3]]))
  # yy.2T = rep("2ndE1_1", length(input.y[[4]]))
  # yy.2R = rep("2ndE1_2", length(input.y[[5]]))
  # yy.2N = rep("2ndE1_3", length(input.y[[6]]))
  # yy.3T = rep("3rdE1_1", length(input.y[[7]]))
  # yy.3R = rep("3rdE1_2", length(input.y[[8]]))
  # yy.3N = rep("3rdE1_3", length(input.y[[9]]))
  
  yy.2T = rep("2ndE1_1", length(input.y[[1]]))
  yy.2R = rep("2ndE1_2", length(input.y[[2]]))
  yy.2N = rep("2ndE1_3", length(input.y[[3]]))
  yy.3T = rep("3rdE1_1", length(input.y[[4]]))
  yy.3R = rep("3rdE1_2", length(input.y[[5]]))
  yy.3N = rep("3rdE1_3", length(input.y[[6]]))
  
  yy.label = c(
                # yy.1T, yy.1R, yy.1N, 
                yy.2T, yy.2R, yy.2N, yy.3T, yy.3R, yy.3N
                )
  
  # input.y_1T = as.numeric(input.y[[1]])
  # input.y_1R = as.numeric(input.y[[2]])
  # input.y_1N = as.numeric(input.y[[3]])
  # input.y_2T = as.numeric(input.y[[4]])
  # input.y_2R = as.numeric(input.y[[5]])
  # input.y_2N = as.numeric(input.y[[6]])
  # input.y_3T = as.numeric(input.y[[7]])
  # input.y_3R = as.numeric(input.y[[8]])
  # input.y_3N = as.numeric(input.y[[9]])
  
  # input.y_1T = as.numeric(input.y[[1]])
  # input.y_1R = as.numeric(input.y[[2]])
  # input.y_1N = as.numeric(input.y[[3]])
  
  input.y_2T = as.numeric(input.y[[1]])
  input.y_2R = as.numeric(input.y[[2]])
  input.y_2N = as.numeric(input.y[[3]])
  input.y_3T = as.numeric(input.y[[4]])
  input.y_3R = as.numeric(input.y[[5]])
  input.y_3N = as.numeric(input.y[[6]])
  
  input.yy = c(
    # input.y_1T,
    # input.y_1R,
    # input.y_1N,
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
    # "indianred3",
    # "light blue",
    # "grey80",
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
    ylim = c(-2,20),
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
  
  # boxplot(input.y,outline=F,notch=T,
  #         main=paste0(query.genotype, "-", metrices[metric.ind],"\n"
  #                     # extra.title
  #         ),
  #         col=col.pool,
  #         ylim=yrange,
  #         ylab=metrices[metric.ind],
  #         xaxt='n'
  # )
  text((1:length(input.y)) - 0.1,
       18,
       paste0(sapply(input.y, length)),
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  n = length(input.y) / length(sessions)
  
  # for(i in c(3,6,9,13)){
  # for(i in c(3,6,9,12)){
  #   #for(i in c(3,6,10)){
  #   lines(c(i,i)+0.5,
  #         c(yrange[1]-1e3,yrange[1]+1e3),
  #         col="light grey",
  #         lty=1)
  # }
  
  # for (i in c(3, 6)) {
  for (i in c(3)) {
    lines(c(i, i) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "light grey",
          lty = 1)
  }
  
  p_value = c(
              # wilcox.test(input.y_1T,input.y_1N)$p.value,
              # wilcox.test(input.y_1R,input.y_1N)$p.value,
              # wilcox.test(input.y_1T,input.y_1R)$p.value,
              wilcox.test(input.y_2T,input.y_2N)$p.value,
              wilcox.test(input.y_2R,input.y_2N)$p.value,
              wilcox.test(input.y_2T,input.y_2R)$p.value,
              wilcox.test(input.y_3T,input.y_3N)$p.value,
              wilcox.test(input.y_3R,input.y_3N)$p.value,
              wilcox.test(input.y_3T,input.y_3R)$p.value
  )
  # text((1:length(input.y)) - 0.1,
  #      15,
  #      paste0(sapply(input.y, length)),
  #      xpd = T,
  #      srt = 0,
  #      adj = 0
  # )

dev.off()

write.table(
  p_value,
  paste0("P_VALUE_SUMMARY_learningindex_ind29_",fly_genotype,"_070218.csv"),
  col.names = T,
  row.names = T,
  quote = F,
  sep = ","
)