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

# sessions <- c("E1T1E1",
#               "E1T1E1T1E1",
#               "E1T1E1T1E1T1E1",
#               "E1T1E1T1E1T1E1T1E1",
#               
#               "E1R1E1",
#               "E1R1E1R1E1",
#               "E1R1E1R1E1R1E1",
#               "E1R1E1R1E1R1E1R1E1",
#               
#               "E1N1E1",
#               "E1N1E1N1E1",
#               "E1N1E1N1E1N1E1",
#               "E1N1E1N1E1N1E1N1E1"
#               )
sessions <- c("E1T1E1","E1R1E1","E1N1E1",
              "E1T1E1T1E1","E1R1E1R1E1","E1N1E1N1E1",
              "E1T1E1T1E1T1E1","E1T1E1T1E1T2E1","E1R1E1R1E1R1E1","E1N1E1N1E1N1E1",
              "E1T1E1T1E1T1E1T1E1","E1T1E1T1E1T2E1T2E1","E1R1E1R1E1R1E1R1E1","E1N1E1N1E1N1E1N1E1")



query.genotype <-
  # "SUN2"
  # "SUN3"
  # "R5"
  "WT"
  # "W118"
  # "W1118"
  # "R3"
  # "DOP1R1 X JU30"
  # c("WT","CS")
  # c("R60 D05 X JU30","R60 D05 x JU30", "R60 D05 x JU29")
  # c( "OK107GAL4 x JU30","OK107GAL4 x JU29")
  # c("JG17 x JU30","JG17 X JU30")
  # c("MB607B x JU30","MB607B X JU30")
  # "THGAL4 x JU29" 
#            # "SUN2","SUN3"#,"R3"


## Read metric names
metrices <- read.table("metrics/list_metrices.csv",
                       stringsAsFactors=F,
                       sep="\t")[,1]



# pdf("fly_metric_05302017_CS_July2016-Dec2016_with-R2.pdf",onefile=T,width=10)
# pdf("fly_metric_06272017_LM8_Mar2017-present.pdf",onefile=T,width=10)
# pdf("fly_metric_05302017_CS_Jan-Mar2017_with-R2.pdf",onefile=T,width=10)
# pdf("fly_metric_06262017_CS_Mar2017-present_with-R2.pdf",onefile=T,width=10)
# pdf("fly_metric_10042017_SUN1_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
# pdf("fly_metric_01112018_SUN1_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
# pdf("fly_metric_01162018_JG17_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
# pdf("fly_metric_01192018_R60D05_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
# pdf("fly_metric_02092018_R60D05_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=10)
# pdf("fly_metric_04052018_control_Mar2018_nofilter_no_Normalized_2000series.pdf",onefile=T,width=10)
pdf("fly_metric_04132018_test2_CS_no_Normalized.pdf",onefile=T,width=10)

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
      metric.df$genotype %in% query.genotype &                         
      metric.df$category == category
    
    ind.E1 <- metric.df$session == "E1" &
      metric.df$genotype %in% query.genotype & 
      metric.df$category == category
    
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    z = metric.df[ind,"value"] 
    
    y = append(y,list(na.omit(z)))
  }
  
  ## input sessions data
  for(session in sessions){
    ind <- metric.df$session == session &
      metric.df$genotype %in% query.genotype
    
    ind.E1 <- metric.df$session == "E1" &
      metric.df$genotype %in% query.genotype
    
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    z = metric.df[ind,"value"]
    
    y = append(y,list(na.omit(z)))
  }
  y.1 = y
  
  yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  
  ## special cases
  # if(metric.ind == 21){
  #   yrange = c(-0.02,0.04)
  #   
  # }else if(metric.ind == 22){
  #   yrange = c(-0.04,0.02)
  # }else if(metric.ind == 23){
  #   yrange = c(-0.3,0.05)
  # }else if(metric.ind == 24){
  #   yrange = c(-0.05,0.3)
  # }
  
  
  
  
  # extra.title = "Training/Random/Blank"
  col.pool <- c("indianred3","light blue","grey",
                "indianred3","light blue","grey",
                "indianred3","light blue","grey",
                "indianred3","red","light blue","grey",
                "indianred3","red","light blue","grey"
                )
  
  input.y = y.1
  
  if (any(is.na(yrange))|any(is.infinite(yrange))|any(is.nan(yrange))
      ){
    yrange = c(-1,1)
    ylim = c(-1, 1)
  }
  
  boxplot(input.y,outline=F,notch=T,
          main=paste0(query.genotype, "-", metrices[metric.ind],"\n"
                      # extra.title
                      ),
          col=col.pool,
          ylim=yrange,
          ylab=metrices[metric.ind],
          xaxt='n'
  )
  text(1:length(input.y),yrange[2]*1,
       paste0(sapply(input.y,length)),
       xpd=T,srt=90,adj=0)
  
  n = length(input.y)/length(sessions)
  
  # for(i in c(3,6,9,13)){
  for(i in c(3,6,9,13)){
    #for(i in c(3,6,10)){
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


######################################################################################################
###For only Pre-test, 1st test, and 2nd test###
# pdf("fly_metric_10042017_SUN1_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=6)
# pdf("fly_metric_01162018_R60D05_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=6)

pdf("fly_metric_02092018_CS_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=6)
for(metric.ind in 1:length(metrices)){
  print(metric.ind)
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
      metric.df$genotype %in% query.genotype &                         
      metric.df$category == category
    
    ind.E1 <- metric.df$session == "E1" &
      metric.df$genotype %in% query.genotype & 
      metric.df$category == category
    
    z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    
    y = append(y,list(na.omit(z)))
  }
  
  ## input sessions data
  for(session in sessions[1:6]){
    ind <- metric.df$session == session &
      metric.df$genotype %in% query.genotype
    
    ind.E1 <- metric.df$session == "E1" &
      metric.df$genotype %in% query.genotype
    
    z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    
    y = append(y,list(na.omit(z)))
  }
  y.1 = y
  
  yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  
  # ## special cases
  # if(metric.ind == 21){
  #   yrange = c(-0.02,0.04)
  #   
  # }else if(metric.ind == 22){
  #   yrange = c(-0.04,0.02)
  # }else if(metric.ind == 23){
  #   yrange = c(-0.3,0.05)
  # }else if(metric.ind == 24){
  #   yrange = c(-0.05,0.3)
  # }
  # 
  # if(metric.ind == 28){
  #   yrange=c(-0.4,0.8)
  # }
  # 
  # if(metric.ind == 30){
  #   yrange=c(-1,1)
  # }
  
  print(yrange)
  
  extra.title = "Training/Random/Blank"
  col.pool <- c("indianred3","light blue","grey",
                "indianred3","light blue","grey",
                # rep("red",7),
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
    #for(i in c(3,6,10)){
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
###Parallel all the genotypes of the same session

query.genotypes <- c("WT","SUN1","SUN2","SUN3")#,"R3"

pdf("genotye_080317.pdf",onefile=T,width=10, height =10)
# layout(1:3)
for(metric.ind in 1:length(metrices)){
  input.file = paste0("metrics/metric_",metric.ind,".csv")
  if(!file.exists(input.file)){next}
  
  metric.df = read.csv(input.file)
  
  for(session in sessions){
    for(category in c("T","R","N")){
      y = list()
      for(gt in query.genotypes){
        ## input sessions data
        if(gt == "WT"){
          ind <- metric.df$session == session &
            metric.df$genotype %in% c("WT","CS") & metric.df$category == category
          
          ind.E1 <- metric.df$session == "E1" &
            metric.df$genotype %in% c("WT","CS")  & metric.df$category == category
        }else{
          ind <- metric.df$session == session &
            metric.df$genotype == gt & metric.df$category == category
          
          ind.E1 <- metric.df$session == "E1" &
            metric.df$genotype == gt  & metric.df$category == category
        }
        z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
        y = append(y,list(na.omit(z)))
      }
      if(sum(sapply(y,length)>0)==0){
        # plot(c(0,1),type='n')
        next
      }
      for(j in which(sapply(y,length)==0)){
        y[[j]] = NA
      }
      
      yrange = c(min(sapply(y,min,na.rm=T)),max(sapply(y,max,na.rm=T)))
      names(y) = query.genotypes
      
      ## special cases
      if(metric.ind == 21 | metric.ind == 23){
        yrange = c(-0.1,0.1)
        
      }else if(metric.ind == 22 | metric.ind == 24){
        yrange = c(-0.1,0.1)
      }
      
      
      extra.title = paste(query.genotypes,collapse="/")
      input.y = y
      if(category == "T") 
      { col_pool = c("red","indianred4","indianred3","indianred2")} 
      else if (category == "R") 
      {col_pool = c("blue","dodgerblue4","dodgerblue3","dodgerblue2")} 
      else { col_pool= c("gray30","gray40","gray50","gray60")}
      
      boxplot(input.y,outline=F,notch=T,
              main=paste0(metrices[metric.ind],"\n",extra.title,"\n",session),
              #col=col.pool,
              ylim=yrange,
              ylab=metrices[metric.ind],
              xaxt='n',
              col=col_pool
      )
      text(1:length(input.y),yrange[2]*1,
           paste0(sapply(input.y,length)),
           xpd=T,srt=90,adj=0)
    }
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


plot_traces<-function(fly.info,genotype,experimenter){
  
  # fly_number<-c()
  # experimenter<-c()
  
  pdf("062917_ES_SUN3.pdf")#Change this for mutants or CS
  
  for (i in 1:length(fly.info$Fly[fly.info$Genotype==genotype & (fly.info$experimenter==experimenter)])){
    fly_number <- fly.info$Fly[fly.info$Genotype==genotype & (fly.info$experimenter==experimenter)][i]
    #as.numeric(unlist(strsplit(flies[i],'_'))[1])
    # fly_number <- as.numeric(unlist(strsplit(flies_T[58],'_'))[1])
    # experimenter <- unlist(strsplit(flies[i],'_'))[2]
    # experimenter <- unlist(strsplit(flies_T[58],'_'))[2]
    # print(flies[i])
    path_to_file = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",experimenter,"/","Mutants","/CSV/")
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
      # if (((fly_number<=113) && (experimenter == "JG") && (genotype=="Mutants"))
      #     ||((experimenter == "JG") && (genotype=="CS")&&(fly_number<=72))
      #     ||((experimenter=="RS")&& (genotype=="CS")&&(fly_number<=64))
      #     ||((experimenter=="ES")&& (genotype=="CS") &&(fly_number<=32))
      #     ||((experimenter=="JE")&&(genotype=="CS")&&(fly_number<=71))
      #     ||(experimenter=="LM")
      # ){
      #   x = read.table(v,header=T,sep=",",stringsAsFactors = F)
      #   x<-x$fly.position[seq(1,length(x$fly.position),by=2)] #All data collected after Aug 2, 2016 does not need to skip elements
      #   
      #   t<-c(0:length(x))/10
      # }else{
      x = read.table(v,header=T,sep=",",stringsAsFactors = F)
      x<-x$fly.position #All data collected after Aug 2, 2016 does not need to skip elements
      t<-c(0:length(x))/50
      # }
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


#########Fly metrics with no normalization & Only the pre-test, first test, and second test########

###For only Pre-test, 1st test, and 2nd test###
query.genotype <-
  # "SUN2"
  # "SUN3"
  # "R5"
  # "WT"
  # "W118"
  # "W1118"
  # "R3"
  # "DOP1R1 X JU30"
  c("WT","CS")
  # c("R60 D05 X JU30","R60 D05 x JU30")
# c( "OK107GAL4 x JU30","OK107GAL4 x JU29")
# c("JG17 x JU30","JG17 X JU30")
# c("MB607B x JU30","MB607B X JU30")
# "THGAL4 x JU29" 
#            # "SUN2","SUN3"#,"R3"


# pdf("fly_metric_10042017_SUN1_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=6)
pdf("fly_metric_01222018_WT_Mar2017-present_filter1_Normalized_Batch1_3.pdf",onefile=T,width=6)
for(metric.ind in 1:length(metrices)){
  print(metric.ind)
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
      metric.df$genotype %in% query.genotype &                         
      metric.df$category == category
    
    ind.E1 <- metric.df$session == "E1" &
      metric.df$genotype %in% query.genotype & 
      metric.df$category == category
    
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    z = metric.df[ind,"value"]
    
    y = append(y,list(na.omit(z)))
  }
  
  ## input sessions data
  for(session in sessions[1:6]){
    ind <- metric.df$session == session &
      metric.df$genotype %in% query.genotype
    
    ind.E1 <- metric.df$session == "E1" &
      metric.df$genotype %in% query.genotype
    
    # z = metric.df[ind,"value"] - metric.df[ind.E1,"value"]
    
    z = metric.df[ind,"value"]
    
    y = append(y,list(na.omit(z)))
  }
  y.1 = y
  
  yrange = c(min(sapply(y,min)),max(sapply(y,max)))
  
  ## special cases
  # if(metric.ind == 21){
  #   yrange = c(-0.02,0.04)
  #   
  # }else if(metric.ind == 22){
  #   yrange = c(-0.04,0.02)
  # }else if(metric.ind == 23){
  #   yrange = c(-0.3,0.05)
  # }else if(metric.ind == 24){
  #   yrange = c(-0.05,0.3)
  # }
  # 
  # if(metric.ind == 28){
  #   yrange=c(-0.4,0.8)
  # }
  # 
  # if(metric.ind == 30){
  #   yrange=c(-1,1)
  # }
  # 
  # print(yrange)
  
  if(metric.ind == 23){
       yrange = c(0.9,1)
  }
  if(metric.ind == 24){
    yrange = c(0,0.1)
  }
  
  # extra.title = "Training/Random/Blank"
  col.pool <- c("indianred3","light blue","grey",
                "indianred3","light blue","grey",
                # rep("red",7),
                "indianred3","light blue","grey",
                "indianred3","red","light blue","grey",
                "indianred3","red","light blue","grey")
  
  input.y = y.1
  boxplot(input.y,outline=F,notch=T,
          # main=paste0(query.genotype, "-", metrices[metric.ind],"\n",extra.title),
          main=paste0(query.genotype, "-", metrices[metric.ind],"\n"),
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
    #for(i in c(3,6,10)){
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