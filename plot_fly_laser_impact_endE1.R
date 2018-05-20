#!/usr/bin/env Rscript


# sessions <- c("E1T1E1","E1R1E1","E1N1E1",
#               "E1T1E1T1E1","E1R1E1R1E1","E1N1E1N1E1",
#               "E1T1E1T1E1T1E1","E1T1E1T1E1T2E1","E1R1E1R1E1R1E1","E1N1E1N1E1N1E1",
#               "E1T1E1T1E1T1E1T1E1","E1T1E1T1E1T2E1T2E1","E1R1E1R1E1R1E1R1E1","E1N1E1N1E1N1E1N1E1")

query.categories <- c("R")#,"R")

col.pool = c( "indianred3", "light blue", "grey" )

query.genotype <- "WT"

## Read Fly laser info
fly.laser <- read.csv("data/fly_laser_info.csv",header=T,stringsAsFactors = F)
fly.laser$fly.id = paste(fly.laser$Fly,fly.laser$Genotype,
                         fly.laser$experimenter)

pdf("fly_laser_stat.pdf",onefile=T,width=10)
for(metric.ind in 1:length(metrices)){
  input.file = paste0("metrics/metric_",metric.ind,".csv")
  if(!file.exists(input.file)){next}
  
  metric.df = read.csv(input.file)
  metric.df = metric.df[!is.na(metric.df$value),]
  rownames(metric.df) = paste(metric.df$fly,metric.df$genotype,
                              metric.df$experimenter,metric.df$session)
  
  query.id = paste(fly.laser$Fly,fly.laser$Genotype,
                   fly.laser$experimenter,fly.laser$session)
  query.id = query.id[query.id %in% rownames(metric.df)]
  metric.df = metric.df[query.id,]
  
  query.fly.id.all = unique(paste(metric.df$fly,metric.df$genotype,
                                  metric.df$experimenter))
  
  ## Calculate laser accumulation
  laser.accum = NULL
  metric = NULL
  data.ind = NULL
  for(category.ind in 1:length(query.categories)){
    query.category = query.categories[category.ind]
    for(query.fly.id in query.fly.id.all){
      l.a = 0
      ind = fly.laser$fly.id == query.fly.id & fly.laser$Category == query.category
      query.sessions = fly.laser$session[ind]
      
      for(query.session in query.sessions){
        #l.a = l.a + fly.laser$time.laser[ind & fly.laser$session == query.session]
        l.a = l.a + fly.laser$num.laser.shot[ind & fly.laser$session == query.session]
      }
      if(length(l.a) == 0){
        laser.accum = c(laser.accum,NA)
      }else{
        laser.accum = c(laser.accum,l.a)
      }
      query.session.metric = query.sessions[length(query.sessions)]
      metric = c(metric,
                 metric.df[paste(query.fly.id,query.session.metric),1] - metric.df[paste(query.fly.id,"E1"),1])
      data.ind = c(data.ind,category.ind)
    }
  }
  
  
  ## Plot
  z = cbind(laser.accum,metric)
  ind.nona = apply(z,1,function(x) return(sum(is.na(x)))) == 0 ## Find rows where NA is present
  z = z[ind.nona,]
  data.ind = data.ind[ind.nona]
  print(cor(z))
  par(mar=c(4,4,4,8))
  plot(z,
       main=paste0(metrices[metric.ind],"\n n = ",nrow(z),";r=",
                   round(cor(z[,1],z[,2],method="spearman"),2)),
       type='n')
  for(query.ind in 1:length(query.categories)){
    points(z[data.ind==query.ind,1],z[data.ind==query.ind,2],col=col.pool[query.ind])  
  }
  legend(max(z[,1])*1.05,max(z[,2]),query.categories,
         fill = col.pool,xpd=T
  )
}
dev.off()