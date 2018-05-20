#!/usr/bin/env Rscript

sessions <- c("E1","E1XE1")
query.categories <- c("L1","L2","L10")
col.pool = c( "grey", "light blue", "indianred3","grey", "light blue", "indianred3" )

metrices <- read.table("metrics_response/list_metrices.csv",
                       stringsAsFactors=F,
                       sep="\t")[,1]
query.genotypes = c("CS","SUN1","SUN2","SUN3","R3","R5")

pdf("fly_laser_stat.pdf",onefile=T,width=10)
layout(matrix(1:6,byrow=T,nrow=3))
for(metric.ind in 1:length(metrices)){
  input.file = paste0("metrics_response/metric_",metric.ind,".csv")
  if(!file.exists(input.file)){next}
  
  metric.df = read.csv(input.file)
  ## covariates of interest: genotype, session
  
  ## E1 data
  y = list()
  for(query.genotype in query.genotypes){
    session = "E1"
    for(query.category in query.categories){
      query.session = gsub("X",query.category,session)
      ind <- metric.df$session == query.session &
        metric.df$genotype == query.genotype &                         
        metric.df$category == query.category
      y = append(y,list(na.omit(metric.df[ind,"value"])))
    }
    
    ## input sessions data
    session = "E1XE1"
    for(query.category in query.categories){
      query.session = gsub("X",query.category,session)
      ind <- metric.df$session == query.session &
        metric.df$genotype == query.genotype
      y = append(y,list(na.omit(metric.df[ind,"value"])))
    }
  }
  yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  
  
  
  
  ## E1 data
  for(query.genotype in query.genotypes){
    y = list()
    session = "E1"
    for(query.category in query.categories){
      query.session = gsub("X",query.category,session)
      ind <- metric.df$session == query.session &
        metric.df$genotype == query.genotype &                         
        metric.df$category == query.category
      y = append(y,list(na.omit(metric.df[ind,"value"])))
    }
    
    ## input sessions data
    session = "E1XE1"
    for(query.category in query.categories){
      query.session = gsub("X",query.category,session)
      ind <- metric.df$session == query.session &
        metric.df$genotype == query.genotype
      y = append(y,list(na.omit(metric.df[ind,"value"])))
    }
    plot(c(1,6),yrange,type='n',
         main=paste(metrices[metric.ind],query.genotype),
         ylab="metric",xaxt='n')
    for(ind in 1:6){
      points(rep(ind,length(y[[ind]])),y[[ind]],col = col.pool[ind],pch=16)
    }
    axis(1,1:6,
         c("E1(L1)","E1(L2)","E1(L10)",
           "E1L1E1","E1L2E1","E1L10E1"
         )
    )
  }
}
dev.off()