#!/usr/bin/env Rscript

sessions <- c("E1",
              "E1X1E1",
              "E1X1E1X1E1",
              "E1X1E1X1E1X1E1",
              "E1X1E1X1E1X1E1X1E1"              
              )

genotypes <- c("WT","LM1","LM2","LM8"
               )

## Read metric names
metrices <- read.table("metrics/list_metrices.csv",
    stringsAsFactors=F,
    sep="\t")[,1]

pdf("fly_metric_3.pdf",onefile=T,width=3)
layout(1:3)
par(mar=c(4,4,5,6.5))
for(metric.ind in 1:length(metrices)){
    input.file = paste0("metrics/metric_",metric.ind,".csv")
    if(!file.exists(input.file)){next}
    
    metric.df = read.csv(input.file)

    if(sum(colnames(metric.df) %in% "value.w") == 0){
        #metric.df$value.w = metric.df$value
        #next;
    }
    
    ## covariates of interest: genotype, session

    y = list()

    for(session in sessions){
        for(category in c("T","R","N")){
            query.session = gsub("X",category,session)
            ind <- metric.df$session == query.session &
                metric.df$genotype == "WT" &                         
                    metric.df$category == category
            y = append(y,list(na.omit(metric.df[ind,"value"])))
        }
    }
    y.1 = y
    yrange = c(min(sapply(y,min)),max(sapply(y,max)))

    ## Genotype T
    y = list()   
    query.sessions = gsub("X","T",sessions)
    for(session in query.sessions){
        for(genotype in genotypes){
            ind <- metric.df$session == session &
                metric.df$genotype == genotype &
                    metric.df$category == "T"
            y = append(y,list(na.omit(metric.df[ind,"value"])))
        }
    }
    y.2 = y
    yrange[1] = min(min(sapply(y,min)),yrange[1])
    yrange[2] = max(max(sapply(y,max)),yrange[2])
    
    ## Genotype R
    y = list()   
    query.sessions = gsub("X","R",sessions)
    for(session in query.sessions){
        for(genotype in genotypes){
            ind <- metric.df$session == session &
                metric.df$genotype == genotype &
                    metric.df$category == "R"
            y = append(y,list(na.omit(metric.df[ind,"value"])))
        }
    }
    y.3 = y
    yrange[1] = min(min(sapply(y,min)),yrange[1])
    yrange[2] = max(max(sapply(y,max)),yrange[2])


    plot.fly.data <- function(input.y,
                              labels,
                              extra.title,
                              col.pool,
                              notch=F){
        boxplot(input.y,outline=F,notch=notch,
                main=paste0(metrices[metric.ind],"\n",extra.title),
                col=col.pool,
                ylim=yrange,
                ylab=metrices[metric.ind],
                xaxt='n'
                )
        text(1:length(input.y),yrange[2]*1.05,
             paste0(sapply(input.y,length)),
             xpd=T,srt=90,adj=0)

        n = length(input.y)/length(sessions)
        
        for(i in 2:length(sessions)){
            lines(c(n,n)*(i-1)+0.5,
                  c(yrange[1]-1e3,yrange[1]+1e3),
                  col="light grey",
                  lty=1)
        }
            
        axis(1,
             1:length(sessions)*n - (n-1)/2,
             paste0("E1\n(",1:length(sessions)-1,")"),
             tick=F
             )

        legend(length(input.y)*1.1,yrange[2],
               labels,
               fill = col.pool,
               xpd=T)
    }

    ## WT - training/random/blank
    plot.fly.data(y.1,
                  c("Training","Random","Blank"),
                  "CS Training/Random/Blank",
                  c("indianred3","light blue","grey"),
                  notch=T
                  )

    ## WT/mutants - training
    plot.fly.data(y.2,
                  genotypes,
                  "CS/Mutants Training",
                  c("indianred3","light blue","darkolivegreen","grey"),
                  notch=T
                  )
    
    ## WT/mutants - random
    plot.fly.data(y.3,
                  genotypes,
                  "CS/Mutants Random",
                  c("indianred3","light blue","darkolivegreen","grey"),
                  notch=T
                  )
}
dev.off()
