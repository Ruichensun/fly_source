
learning_score <- function(metric.ind, query.genotype, query.fly, query.experimenter){

    fly_genotype = query.genotype
    if (query.genotype == c("CS")){
      query.genotype = c("WT", "CS")
    }
    
    input.file = paste0("metrics/metric_", metric.ind, ".csv")
    if (!file.exists(input.file)) {
      next
    }
    metric.df = read.csv(input.file)
    ## covariates of interest: genotype, session
    y = list()
    ## E1 data
    session = "E1"
    for (category in c("T", "R", "N")) {
      query.session = gsub("X", category, session)
      ind <- metric.df$Session == query.session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == category &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == category 
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"])/metric.df[ind.E1,"Value"]
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
        z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"])/metric.df[ind.E1,"Value"]
        
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
        z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"])/metric.df[ind.E1,"Value"]
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
        z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"])/metric.df[ind.E1,"Value"]
      }
      y = append(y, list(na.omit(z)))
    }
    
    ## special cases
    input.y = list(y[[7]], y[[15]], y[[23]])
    
    yy.3T = rep(paste0(fly_genotype,"_3rdE1_1"), length(input.y[[1]]))
    yy.3R = rep(paste0(fly_genotype,"_3rdE1_2"), length(input.y[[2]]))
    yy.3N = rep(paste0(fly_genotype,"_3rdE1_3"), length(input.y[[3]]))
    
    yy.label = c(yy.3T, yy.3R, yy.3N)
    
    input.y_3T = as.numeric(input.y[[1]])
    input.y_3R = as.numeric(input.y[[2]])
    input.y_3N = as.numeric(input.y[[3]])
    
    input.yy = c(
      input.y_3T,
      input.y_3R,
      input.y_3N
    )
    input.y.df.pre = data.frame(input.yy, yy.label)
    return(input.y.df.pre)
}


get_query_info<-function(query.genotype){
  if(query.genotype=="CS"){
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
  }else if(query.genotype=="SUN1"){
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
  }else{
    query.fly = fly.info.include[(fly.info.include$Genotype == query.genotype), ]$Fly
    query.experimenter = fly.info.include[(fly.info.include$Genotype == query.genotype), ]$experimenter
    write.table(
      fly.info.include[(fly.info.include$Genotype == query.genotype), ],
      paste0("fly_info_include_",query.genotype,".csv"),
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
  }
  
  return(list(query.fly, query.experimenter))
  }    




p_value_sum = matrix(nrow = 0, ncol = 9)

sessions <- c(                     
  "E1T1",                   #4
  "E1T1E1",                 #5 
  "E1T1E1T1",               #6
  "E1T1E1T1E1",             #7
  "E1T1E1T1E1T1",           #8
  "E1T1E1T1E1T1E1",         #9
  "E1T1E1T1E1T1E1T1",       #10
  "E1T1E1T1E1T1E1T1E1",     #11
  
  "E1R1",                   #12
  "E1R1E1",                 #13
  "E1R1E1R1",               #14
  "E1R1E1R1E1",             #15
  "E1R1E1R1E1R1",           #16
  "E1R1E1R1E1R1E1",         #17
  "E1R1E1R1E1R1E1R1",       #18
  "E1R1E1R1E1R1E1R1E1",     #19
  
  "E1N1",                   #20
  "E1N1E1",                 #21
  "E1N1E1N1",               #22
  "E1N1E1N1E1",             #23
  "E1N1E1N1E1N1",           #24
  "E1N1E1N1E1N1E1",         #25
  "E1N1E1N1E1N1E1N1",       #26
  "E1N1E1N1E1N1E1N1E1"      #27
)

metric.ind = 29  
query.list = c(
  # "CS",
  # "SUN1",
  # "SUN2",
  # "SUN3"
  # "CS x JU30",
  # "MB009B x JU30",
  # "MB131B x JU30",
  # "MB419B x JU30",
  # "MB607B x JU30",
  # "R60D05 x JU30"
  "JG17 x JU30"
  # "R60D05 x PKCi",
  # "JG17 x PKCi",
  # "CS x PKCi",
  # "MB009B x PKCi"
  # "MB131B x PKCi",
  # "MB419B x PKCi",
  # "MB607B x PKCi"
  # "R60D05 x DopR1-IR",
  # "JG17 x DopR1-IR",
  # "MB009B x DopR1-IR",
  # "MB131B x DopR1-IR",
  # "MB419B x DopR1-IR",
  # "MB607B x DopR1-IR"
)

input.y.df = data.frame()
for (i in 1:length(query.list)){
  query.fly = get_query_info(query.list[i])[[1]]
  query.experimenter = get_query_info(query.list[i])[[2]]
  input.y.df.pre = learning_score(metric.ind, query.list[i], query.fly=query.fly, query.experimenter=query.experimenter)
  input.y.df = rbind(input.y.df, input.y.df.pre)
}
colnames(input.y.df) <- c("Value", "Genotype_Sessions")
          

#########Plotting Different Genotypes together########
          

    #segment
    colnames(input.y.df) <- c("Value", "Genotype_Sessions")
    
    col.pool <- c(  "indianred3",
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
      "LearningIndexComparison_DopR_only_112718.pdf",
      onefile = T,
      width = 10
    )
    
    boxplot(
      Value ~ Genotype_Sessions,
      data = input.y.df,
      ylim = c(-2,2), #Value set for metric #29
      outline = F,
      notch = T,
      lwd = 2,
      ylab = "Learning Index",
      xlab = "",
      xaxt = "n",
      col = col.pool
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
    
    number_of_data = c()
    for (i in (1:length(levels(input.y.df$Genotype_Sessions)))){
      number_of_data = c(number_of_data,
                         length(input.y.df$Genotype_Sessions[input.y.df$Genotype_Sessions==levels(input.y.df$Genotype_Sessions)[i]])
                         )
    }
    
    text(x = (1:length(levels(input.y.df$Genotype_Sessions))) - 0.1,
         y = 1.5,
         labels = number_of_data,
         xpd = T,
         srt = 0,
         adj = 0
    )
    # 
    # n = length(input.y) / length(sessions)
    
    # for(i in c(3,6,9,13)){
    # for(i in c(3,6,9,12)){
    #   #for(i in c(3,6,10)){
    #   lines(c(i,i)+0.5,
    #         c(yrange[1]-1e3,yrange[1]+1e3),
    #         col="light grey",
    #         lty=1)
    # }

    for (i in c(3,6,9,12,15,18,21,24,27
                # 27,30,33,36,39,42,45
                )) {
    # for (i in c(3)) {
      lines(c(i, i) + 0.5,
            c(0 - 1e3, 5 + 1e3),
            col = "light grey",
            lty = 1)
    }
    
    # p_value = c(
    #   # wilcox.test(input.y_1T,input.y_1N)$p.value,
    #   # wilcox.test(input.y_1R,input.y_1N)$p.value,
    #   # wilcox.test(input.y_1T,input.y_1R)$p.value,
    #   wilcox.test(input.y_2T,input.y_2N)$p.value,
    #   wilcox.test(input.y_2R,input.y_2N)$p.value,
    #   wilcox.test(input.y_2T,input.y_2R)$p.value,
    #   wilcox.test(input.y_3T,input.y_3N)$p.value,
    #   wilcox.test(input.y_3R,input.y_3N)$p.value,
    #   wilcox.test(input.y_3T,input.y_3R)$p.value
    # )
    # text((1:length(input.y)) - 0.1,
    #      15,
    #      paste0(sapply(input.y, length)),
    #      xpd = T,
    #      srt = 0,
    #      adj = 0
    # )
    
    dev.off()          
      
uni = unique(input.y.df$Genotype_Sessions)    
#CS T R: 0.0009154
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[1],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[2],]$Value)

#MB009 T R 0.391
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[4],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[5],]$Value)
#MB131 T R 0.3115
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[7],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[8],]$Value)

#MB419 T R 0.1797
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[10],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[11],]$Value)
#MB607 T R 0.02296
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[13],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[14],]$Value)
#R60D05 T R 0.002848
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[16],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[17],]$Value)
#189Y T R 0.001539
wilcox.test(input.y.df[input.y.df$Genotype_Sessions==uni[19],]$Value, input.y.df[input.y.df$Genotype_Sessions==uni[20],]$Value)

        
################Plotting Individual Plots#########################          
          
          
pdf(
  # "fly_metric_ind29_CS_allflies_Filter1_063018.pdf",092818.pdf"  ),
  onefile = T,
  width = 10
)

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
  yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  ## special cases
  y_text = c()
  print(metric.ind)
  print(yrange)
  print(y_text)
  input.y = y.1[4:9]
  input.y = list(y.1[[1]], y.1[[2]], y.1[[3]],
                 y.1[[5]], y.1[[13]], y.1[[21]],
                 y.1[[7]], y.1[[15]], y.1[[23]]
                 )
  
  yy.1T = rep("1stE1_1", length(input.y[[1]]))
  yy.1R = rep("1stE1_2", length(input.y[[2]]))
  yy.1N = rep("1stE1_3", length(input.y[[3]]))
  yy.2T = rep("2ndE1_1", length(input.y[[4]]))
  yy.2R = rep("2ndE1_2", length(input.y[[5]]))
  yy.2N = rep("2ndE1_3", length(input.y[[6]]))
  yy.3T = rep("3rdE1_1", length(input.y[[7]]))
  yy.3R = rep("3rdE1_2", length(input.y[[8]]))
  yy.3N = rep("3rdE1_3", length(input.y[[9]]))
  
  yy.label = c(yy.1T, yy.1R, yy.1N, 
               yy.2T, yy.2R, yy.2N, 
               yy.3T, yy.3R, yy.3N
              )
  
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
  

  text((1:length(input.y)) - 0.1,
       18,
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
  
  p_value = c(
              wilcox.test(input.y_1T,input.y_1N)$p.value,
              wilcox.test(input.y_1R,input.y_1N)$p.value,
              wilcox.test(input.y_1T,input.y_1R)$p.value,
              wilcox.test(input.y_2T,input.y_2N)$p.value,
              wilcox.test(input.y_2R,input.y_2N)$p.value,
              wilcox.test(input.y_2T,input.y_2R)$p.value,
              wilcox.test(input.y_3T,input.y_3N)$p.value,
              wilcox.test(input.y_3R,input.y_3N)$p.value,
              wilcox.test(input.y_3T,input.y_3R)$p.value
  )

dev.off()


p_value_sum = rbind(p_value_sum, p_value)





rownames(p_value_sum) = c("WT",
                          "SUN1",
                          "SUN2",
                          "SUN3",
                          "MB009BxJU30",
                          "MB131BxJU30",  
                          "MB419BxJU30",
                          "MB607BxJU30",
                          "R60D05xJU30",
                          "JG17xJU30"
                          )

colnames(p_value_sum) = c(
                          "1TN",
                          "1RN",
                          "1TR",
                          "2TN",
                          "2RN",
                          "2TR",
                          "3TN",
                          "3RN",
                          "3TR"
)

write.table(
              p_value_sum,
              "P_VALUE_LearningIndex29_092818.csv",
              col.names = T,
              row.names = T,
              quote = F,
              sep = ","
            )