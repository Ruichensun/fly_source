#!/usr/bin/env Rscript

cv <- function(x){
  return(sd(x)/mean(x))
}

col.pool <- c("indianred3","light blue","grey")
names(col.pool) = c("T","R","N")

sessions <- c("E1",
              "E1X1E1",
              "E1X1E1X1E1",
              "E1X1E1X1E1X1E1",
              "E1X1E1X1E1X1E1X1E1"
)

# query.genotype <- c("WT","CS")
query.genotype <- c("SUN3")


## Read metric names
metrices <- read.table("metrics/list_metrices.csv",
                       stringsAsFactors=F,
                       sep="\t")[,1]
pdf("CV_080117_SUN3_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
# pdf("CV_080317_WT_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
for(metric.ind in 1:length(metrices)){
  
  print(metric.ind)
 
  input.file = paste0("metrics/metric_",metric.ind,".csv")
  if(!file.exists(input.file)){next}
  
  metric.df = read.csv(input.file)
  
  ## E1 data
  y = list()
  y.n = list()
  for(category in c("T","R","N")){
    x=NULL
    x.n = NULL
    for(session in sessions){
      query.session = gsub("X",category,session)
      ind <- metric.df$session == query.session &
        metric.df$genotype %in% query.genotype &                         
        metric.df$category == category
      
      z = na.omit(metric.df$value[ind])
      if(min(z)<0){next}
      x = c(x,cv(z))
      x.n = c(x.n,length(z))
    }
    if(length(x) == length(sessions)){
      y = append(y,list(x))
      y.n = append(y.n,list(x.n))
    }
  }
  if(length(y)!=3){next}
  names(y) = c("T","R","N")
  names(y.n) = c("T","R","N")
  
  ###Normalization###
  norm_base_y=c()
  
  for(j in 1:3){
    norm_base_y=y[[j]][1]
    for (i in 1:length(y[[j]])){
      y[[j]][i]=y[[j]][i]-norm_base_y
    }
  }
  ###Normalization ends###
  if (metric.ind==30){cv.range=c(-1,1)}
  else if (metric.ind==21){
    cv.range=c(-0.008,0.004)
  }
  else{
    cv.range =c(min(sapply(y,min),na.rm=T),max(sapply(y,max),na.rm=T))}
  
  
    plot(c(1,length(sessions)),
       cv.range,
       type='n',ylab = "CV",
       xlab = NA,
       main = metrices[metric.ind],
      xaxt='n'     
  )
  ## sessions
  text(1:length(sessions),cv.range[1]*0.90,
       paste0((1:length(sessions))-1," test"),
       xpd=T,srt=-45,adj=0)
  
  for(category in c("T","R","N")){
        lines(1:length(sessions),
          y[[category]],
          col=col.pool[category],
          lwd=2
    )
  }
  
  text(1:length(sessions) - 0.25,cv.range[2]*1.04,y.n[["T"]],
       xpd=T,srt=90,adj=0,col=col.pool["T"])
  text(1:length(sessions),cv.range[2]*1.04,y.n[["R"]],
       xpd=T,srt=90,adj=0,col=col.pool["R"])
  text(1:length(sessions) + 0.25,cv.range[2]*1.04,y.n[["T"]],
       xpd=T,srt=90,adj=0,col=col.pool["N"])
  
  
}
dev.off()
