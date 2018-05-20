#!/usr/bin/env Rscript

# sessions <- c("E1",
#               "E1X1E1",
#               "E1X1E1X1E1",
#               "E1X1E1X1E1X1E1",
#               "E1X1E1X1E1X1E1X1E1",
#               "E1X1E1X1E1X2E1",
#               "E1X1E1X1E1X2E1X2E1"
#               
#               
#               #E1T1E1T1E1T1E1-E1R1E1R1E1R1E1
#               #E1T1E1T1E1T2E1-E1R1E1R1E1R1E1
#               
# )

sessions <- c("E1T1E1","E1R1E1","E1N1E1",
              "E1T1E1T1E1","E1R1E1R1E1","E1N1E1N1E1",
              "E1T1E1T1E1T1E1","E1T1E1T1E1T2E1","E1R1E1R1E1R1E1","E1N1E1N1E1N1E1",
              "E1T1E1T1E1T1E1T1E1","E1T1E1T1E1T2E1T2E1","E1R1E1R1E1R1E1R1E1","E1N1E1N1E1N1E1N1E1")

query.genotype <-
  "SUN3"
  # "WT"
#            # "SUN2","SUN3"#,"R3"


## Read metric names
metrices <- read.table("metrics/list_metrices.csv",
                       stringsAsFactors=F,
                       sep="\t")[,1]



# pdf("fly_metric_05302017_CS_July2016-Dec2016_with-R2.pdf",onefile=T,width=10)
# pdf("fly_metric_06272017_LM8_Mar2017-present.pdf",onefile=T,width=10)
# pdf("fly_metric_05302017_CS_Jan-Mar2017_with-R2.pdf",onefile=T,width=10)
# pdf("fly_metric_06262017_CS_Mar2017-present_with-R2.pdf",onefile=T,width=10)
pdf("fly_metric_06272017_SUN3_Mar2017-present_filter1.pdf",onefile=T,width=10)
# par(mar=c(1,1,1,1))
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
  ## E1 data
  session = "E1"
  for(category in c("T","R","N")){
    query.session = gsub("X",category,session)
    ind <- metric.df$session == query.session &
      metric.df$genotype == query.genotype &                         
      metric.df$category == category
    y = append(y,list(na.omit(metric.df[ind,"value"])))
  }
  
  ## input sessions data
  for(session in sessions){
      ind <- metric.df$session == session &
        metric.df$genotype == query.genotype
      y = append(y,list(na.omit(metric.df[ind,"value"])))
    }
  y.1 = y

  yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  
  ## special cases
  if(metric.ind == 21 | metric.ind == 23){
    yrange = c(0.9,1)
    
  }else if(metric.ind == 22 | metric.ind == 24){
    yrange = c(0,0.1)
  }
  
  
   extra.title = "Training/Random/Blank"
  col.pool <- c("indianred3","light blue","grey",
                "indianred3","light blue","grey",
                "indianred3","light blue","grey",
                "indianred3","red","light blue","grey",
                "indianred3","red","light blue","grey")

  input.y = y.1
  boxplot(input.y,outline=F,notch=T,
          main=paste0(query.genotype, "-", metrices[metric.ind],"\n",extra.title),
          col=col.pool,
          ylim=yrange,
          ylab=metrices[metric.ind],
          xaxt='n'
  )
  text(1:length(input.y),yrange[2]*1,
       paste0(sapply(input.y,length)),
       xpd=T,srt=90,adj=0)
  
  n = length(input.y)/length(sessions)
  
  for(i in c(3,6,9,13)){
    lines(c(i,i)+0.5,
          c(yrange[1]-1e3,yrange[1]+1e3),
          col="light grey",
          lty=1)
  }
  
  # axis(1,
  #      1:length(sessions)*n - (n-1)/2,
  #      NA,
  #      #sessions,
  #      #paste0("E1\n(",1:length(sessions)-1,")"),
  #      tick=F
  # )
  
  # legend(length(input.y)*1.1,yrange[2],
  #       legend = c("Training","Rev training","Random","Blank"),
  #        fill = c("indianred3","red","light blue","grey"),
  #        xpd=T)
   # break
}
dev.off()
######################################################################################################################

library(Hmisc)
library(rafalib)
library(ggplot2)


plot_traces<-function(fly.info,ind,genotype,experimenter){
  
  # fly_number<-c()
  # experimenter<-c()
  
  pdf("062617_JD_CS_1_84.pdf")#Change this for mutants or CS
  
  for (i in 1:length(fly.info$Fly[ind & (fly.info$experimenter==experimenter)])){
    fly_number <- fly.info$Fly[ind & (fly.info$experimenter==experimenter)][i]
    #as.numeric(unlist(strsplit(flies[i],'_'))[1])
    # fly_number <- as.numeric(unlist(strsplit(flies_T[58],'_'))[1])
    # experimenter <- unlist(strsplit(flies[i],'_'))[2]
    # experimenter <- unlist(strsplit(flies_T[58],'_'))[2]
    # print(flies[i])
    path_to_file = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",experimenter,"/","CS","/CSV/")
    # pdf("behavior_traces_CS217-224.pdf")
    fly_files = dir(path=path_to_file,pattern = paste0("Fly",fly_number,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE) #import data of a fly
    # fly_files = fly_files[grep("E1_",fly_files)] # Choose only E1 files, excluding all training files
    # fly_files = fly_files[grep("T1_|R1_|T2_|N1_",fly_files)] # Choose all training files
    
    # print(fly_files)
    if(length(fly_files)<1){
      print(paste0('Fly', fly_number,"_",experimenter,'data missing!'))
      next
    }
    mypar(length(fly_files),1)
    for (v in fly_files){
      if (((fly_number<=113) && (experimenter == "JG") && (genotype=="Mutants"))
          ||((experimenter == "JG") && (genotype=="CS")&&(fly_number<=72))
          ||((experimenter=="RS")&& (genotype=="CS")&&(fly_number<=64))
          ||((experimenter=="ES")&& (genotype=="CS") &&(fly_number<=32))
          ||((experimenter=="JE")&&(genotype=="CS")&&(fly_number<=71))
          ||(experimenter=="LM")
      ){
        x = read.table(v,header=T,sep=",",stringsAsFactors = F)
        x<-x$fly.position[seq(1,length(x$fly.position),by=2)] #All data collected after Aug 2, 2016 does not need to skip elements
        
        t<-c(0:length(x))/10
      }else{
        x = read.table(v,header=T,sep=",",stringsAsFactors = F)
        x<-x$fly.position #All data collected after Aug 2, 2016 does not need to skip elements
        t<-c(0:length(x))/50
      }
      temperal_name<-gsub("ProcessedData_","",basename(v))
      temperal_name<- gsub(".csv","",temperal_name)
      
      plot(t[0:(length(t)-1)],x,type='l',xlab="Time (seconds)",ylab="Location",main=paste0(genotype,"_",experimenter,"_",temperal_name))
      print(v)
    }
  }
  dev.off()
  graphics.off()
}







##For 21-24
pdf("CS_Mar-May2017_Pww .pdf")
metric.ind=23
input.file = paste0("metrics/metric_",metric.ind,".csv")
if(!file.exists(input.file)){next}

metric.df = read.csv(input.file)

if(sum(colnames(metric.df) %in% "value.w") == 0){
  #metric.df$value.w = metric.df$value
  #next;
}

## covariates of interest: genotype, session

y = list()
## E1 data
session = "E1"
for(category in c("T","R","N")){
  query.session = gsub("X",category,session)
  ind <- metric.df$session == query.session &
    metric.df$genotype == query.genotype &                         
    metric.df$category == category
  y = append(y,list(na.omit(metric.df[ind,"value"])))
}

## input sessions data
for(session in sessions){
  ind <- metric.df$session == session &
    metric.df$genotype == query.genotype
  y = append(y,list(na.omit(metric.df[ind,"value"])))
}
y.1 = y

yrange = c(0.95,1)


extra.title = "Training/Random/Blank"
col.pool <- c("indianred3","light blue","grey",
              "indianred3","light blue","grey",
              "indianred3","light blue","grey",
              "indianred3","red","light blue","grey",
              "indianred3","red","light blue","grey")

input.y = y.1
boxplot(input.y,outline=F,notch=T,
        main=paste0(query.genotype, "-", metrices[metric.ind],"\n",extra.title),
        col=col.pool,
        ylim=yrange,
        ylab=metrices[metric.ind],
        xaxt='n'
)
text(1:length(input.y),
     1.0022,
     # yrange[2]*1.05,
     paste0(sapply(input.y,length)),
     xpd=T,srt=90,adj=0)

n = length(input.y)/length(sessions)

for(i in c(3,6,9,13)){
  lines(c(i,i)+0.5,
        c(yrange[1]-1e3,yrange[1]+1e3),
        col="light grey",
        lty=1)
}
dev.off()