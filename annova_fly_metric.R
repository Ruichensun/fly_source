#!/usr/bin/env Rscript

library(nlme)
library(gvlma)

sessions <- c("E1",
              "E1X1E1",
              "E1X1E1X1E1",
              "E1X1E1X1E1X1E1",
              "E1X1E1X1E1X1E1X1E1")

query.genotype <- c("WT", "CS")

## Read metric names
metrices <- read.table("metrics/list_metrices.csv",
                       stringsAsFactors = F,
                       sep = "\t")[, 1]
pdf(
  "frequency_080317_WT_Mar2017-present_filter1_Normalized_Batch1_3.pdf",
  onefile = T,
  width = 10
)
for (metric.ind in 1:length(metrices)) {
  input.file = paste0("metrics/metric_", metric.ind, ".csv")
  if (!file.exists(input.file)) {
    next
  }
  
  metric.df = read.csv(input.file)
  
  y = NULL
  ## E1 data
  for (category in c("T", "R", "N")) {
    for (session in sessions) {
      query.session = gsub("X", category, session)
      ind <- metric.df$session == query.session &
        metric.df$genotype %in% query.genotype &
        metric.df$category == category
      
      z = na.omit(metric.df[ind,])
      abv.session = rep(session, nrow(z))
      z = cbind(z, abv.session)
      y = rbind(y, z)
    }
  }
  df = data.frame(
    metric = as.numeric(y$value),
    fly.id = paste0(y$fly, "_", y$experimenter, "_", y$gender),
    category = factor(y$category),
    session = factor(y$abv.session)
  )
  
  ## test for normality
  layout(matrix(1:6, ncol = 3, byrow = 2))
  for (category in c("T", "R", "N")) {
    ind = df$category == category
    p = shapiro.test(log(abs(df$metric[ind] + exp(-10))))$p.value
    print(p)
    hist(
      log(abs(df$metric[ind])),
      breaks = 50,
      main = paste0(category, " ", metrices[metric.ind], "\np = ", p),
      xlab = "value",
      ylab = "freq"
    )
    
    for (session in sessions) {
      if (sum(df$session == session &
              ind) < 3) {
        plot(c(0, 1), c(0, 1), type = 'n')
        next
      }
      p = shapiro.test(log(abs(df$metric[df$session == session &
                                           ind]) + exp(-10)))$p.value
      hist(
        log(abs(df$metric[df$session == session & ind])),
        breaks = 50,
        main = paste0(category, " ", session, "\np = ", p),
        xlab = "value",
        ylab = "freq"
      )
    }
  }
  ## test for heteroscedasticity
  p = NULL
  for (category in c("T", "R", "N")) {
    model = lm(metric ~ 1 + session,
               data = df[df$category == category,])
    p = c(p, gvlma(model)$GlobalTest$DirectionalStat4$pvalue)
  }
  names(p) = c("T", "R", "N")
  cat(metrices[metric.ind], ": \n")
  print(p)
  cat("\n")
  next
  
  print(metrices[metric.ind])
  ## anova
  ## category only
  model = lme(
    metric ~ 1 + category,
    random = ~ 1 | fly.id,
    data = df,
    method = "REML"
  )
  # print(anova(model))
  
  ## anova
  ## session only
  model = lme(
    metric ~ 1 + session,
    random = ~ 1 | fly.id,
    data = df,
    method = "REML"
  )
  #print(anova(model))
  
  ## anova
  ## category and session
  model = lme(
    metric ~ 1 + session + category + session:category,
    random = ~ 1 | fly.id,
    data = df,
    method = "REML"
  )
  print(anova(model))
  write.csv(anova(model), "annova_normality_test.csv")
}
dev.off()

pdf(
  "frequency_session_only_log_transformed_080317_WT_Mar2017-present_filter1_Normalized_Batch1_3.pdf",
  onefile = T,
  width = 10
)
for (metric.ind in 1:length(metrices)) {
  input.file = paste0("metrics/metric_", metric.ind, ".csv")
  if (!file.exists(input.file)) {
    next
  }
  
  metric.df = read.csv(input.file)
  
  y = NULL
  ## E1 data
  for (category in c("T", "R", "N")) {
    for (session in sessions) {
      query.session = gsub("X", category, session)
      ind <- metric.df$session == query.session &
        metric.df$genotype %in% query.genotype &
        metric.df$category == category
      
      z = na.omit(metric.df[ind,])
      abv.session = rep(session, nrow(z))
      z = cbind(z, abv.session)
      y = rbind(y, z)
    }
  }
  df = data.frame(
    metric = as.numeric(y$value),
    fly.id = paste0(y$fly, "_", y$experimenter, "_", y$gender),
    category = factor(y$category),
    session = factor(y$abv.session)
  )
  
  ## test for normality
  layout(matrix(1:6, ncol = 3, byrow = 2))
  p = shapiro.test(log(abs(df$metric) + exp(-10)))$p.value
  #print(p)
  hist(
    log(abs(df$metric)),
    breaks = 50,
    main = paste0(metrices[metric.ind], "\np = ", p),
    xlab = "value",
    ylab = "freq"
  )
  
  for (session in sessions) {
    p = shapiro.test(log(abs(df$metric[df$session == session]) + exp(-10)))$p.value
    hist(
      log(abs(df$metric[df$session == session])),
      breaks = 50,
      main = paste0(session, "\np = ", p),
      xlab = "value",
      ylab = "freq"
    )
  }
  
  ## test for heteroscedasticity
  p = NULL
  for (category in c("T", "R", "N")) {
    model = lm(metric ~ 1 + session,
               data = df[df$category == category,])
    p = c(p, gvlma(model)$GlobalTest$DirectionalStat4$pvalue)
  }
  names(p) = c("T", "R", "N")
  cat(metrices[metric.ind], ": \n")
  print(p)
  cat("\n")
  next
  
  print(metrices[metric.ind])
  ## anova
  ## category only
  model = lme(
    metric ~ 1 + category,
    random = ~ 1 | fly.id,
    data = df,
    method = "REML"
  )
  # print(anova(model))
  
  ## anova
  ## session only
  model = lme(
    metric ~ 1 + session,
    random = ~ 1 | fly.id,
    data = df,
    method = "REML"
  )
  #print(anova(model))
  
  ## anova
  ## category and session
  model = lme(
    metric ~ 1 + session + category + session:category,
    random = ~ 1 | fly.id,
    data = df,
    method = "REML"
  )
  print(anova(model))
  write.csv(anova(model), "annova_normality_test.csv")
}
dev.off()

######################################################################################################################
###Parallel all the genotypes of the same session
#
# query.genotypes <- c("WT","SUN1","SUN2","SUN3")#,"R3"
#
# for(metric.ind in 1:length(metrices)){
#   input.file = paste0("metrics/metric_",metric.ind,".csv")
#   if(!file.exists(input.file)){next}
#
#   metric.df = read.csv(input.file)
#
#   for(session in sessions){
#     for(category in c("T","R","N")){
#       y = list()
#       for(gt in query.genotypes){
#         ## input sessions data
#         ind <- metric.df$session == session &
#           metric.df$genotype == gt & metric.df$category == category
#
#         ind.E1 <- metric.df$session == "E1" &
#           metric.df$genotype == gt  & metric.df$category == category
#
#         z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
#         y = append(y,list(na.omit(z)))
#       }
#       if(sum(sapply(y,length)>0)==0){
#         # plot(c(0,1),type='n')
#         next
#       }
#       for(j in which(sapply(y,length)==0)){
#         y[[j]] = NA
#       }
#
#       yrange = c(min(sapply(y,min,na.rm=T)),max(sapply(y,max,na.rm=T)))
#       names(y) = query.genotypes
#
#       ## special cases
#       if(metric.ind == 21 | metric.ind == 23){
#         yrange = c(-0.1,0.1)
#
#       }else if(metric.ind == 22 | metric.ind == 24){
#         yrange = c(-0.1,0.1)
#       }
#
#
#       extra.title = paste(query.genotypes,collapse="/")
#       input.y = y
#       if(category == "T")
#       { col_pool = c("red","indianred4","indianred3","indianred2")}
#       else if (category == "R")
#       {col_pool = c("blue","dodgerblue4","dodgerblue3","dodgerblue2")}
#       else { col_pool= c("gray30","gray40","gray50","gray60")}
#
#       boxplot(input.y,outline=F,notch=T,
#               main=paste0(metrices[metric.ind],"\n",extra.title,"\n",session),
#               #col=col.pool,
#               ylim=yrange,
#               ylab=metrices[metric.ind],
#               xaxt='n',
#               col=col_pool
#       )
#       text(1:length(input.y),yrange[2]*1,
#            paste0(sapply(input.y,length)),
#            xpd=T,srt=90,adj=0)
#     }
#   }
# }
