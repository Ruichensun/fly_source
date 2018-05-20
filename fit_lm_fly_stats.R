#!/usr/bin/env Rscript
source("get_fly_speed_and_position.R")


new.variable.names <- c("age (2 days)",
                        "age (3 days)",
                        "age (>=5 days)",
                        "male",
                        "setup-1",
                        "setup-3",
                        "setup-4",
                        "batch-2",
                        "batch-3",
                        "loading after 3pm",
                        "blank - 1st test",
                        "blank - 2st test",
                        "blank - 3st test",
                        "blank - 4st test",
                        "random - 1st test",
                        "random - 2st test",
                        "random - 3st test",
                        "random - 4st test",
                        "training - 1st test",
                        "training - 2st test",
                        "training(rev) - 3st test",
                        "training(rev) - 4st test")


names(new.variable.names) <- c("age2",
                               "age3",
                               "age5",
                               "genderM",
                               "setup1",
                               "setup3",
                               "setup4",
                               "batch2",
                               "batch3",
                               "E1_loading_timelate",
                               "sessionE1N1E1",
                               "sessionE1N1E1N1E1",
                               "sessionE1N1E1N1E1N1E1",
                               "sessionE1N1E1N1E1N1E1N1E1",
                               "sessionE1R1E1",
                               "sessionE1R1E1R1E1",
                               "sessionE1R1E1R1E1R1E1",
                               "sessionE1R1E1R1E1R1E1R1E1",
                               "sessionE1T1E1",
                               "sessionE1T1E1T1E1",
                               "sessionE1T1E1T1E1T2E1",
                               "sessionE1T1E1T1E1T2E1T2E1");

## function for number of observations
## from http://stackoverflow.com/questions/15660829/how-to-add-a-number-of-observations-per-group-and-use-group-mean-in-ggplot2-boxp
give.n <- function(x){
    return(c(y = median(x)*1.1, label = length(x)))
    ## experiment with the multiplier to find the perfect position
}

metrices = c(
    "Number of Pause Starts",
    "Fraction Time in Pause",
    "Average Pause Duration",
    "Average Moving Speed ",
    "Average Moving Speed (excluding pause)",
    "Average Speed When Enter Pause",
    "Average Speed When Exit Pause",
    "Moving Distance Per Minute",
    "Number of Turns",
    "Number of Middle Turns",
    "Fration of Middle Turns Out of Total Turns",
    "Burstiness (Pause)",
    "Burstiness (Inter Event Time)",
    "Burstiness (Scrambled)",
    "Burstiness (Walking bouts-thresholding)",
    "Burstiness (Walking events-thresholding)",
    "Beginning pause duration",
    "First pause duration",
    "State_transitions: pp, pw, ww, wp",
    "Transition probability: pause to pause",
    "Transition probability: pause to walking",
    "Transition probability: walking to walking",
    "Transition probability: walking to pause",
    "Memory",
    "Memory inverted",
    "Burstiness of start of walking",
    "Burstiness of start of pause"      
    )

sessions <- c("E1",
              "E1X1E1",
              "E1X1E1X1E1",
              "E1X1E1X1E1X1E1",
              "E1X1E1X1E1X1E1X1E1")

load("all_ofs.Rdata")
all_ofs = list()
for(session in sessions){
  all_ofs = append(all_ofs,list(c(all_ofs_WT[[session]],all_ofs_mutants[[session]])))
}
names(all_ofs) = sessions
 
## Fly info
fly.info.CS = read.csv("data/fly_info_CS.csv",header=T,stringsAsFactors=F)
fly.info.CS$Genotype = "WT"

fly.info.mutants = read.csv("data/fly_info_mutants.csv",header=T,stringsAsFactors=F)
#fly.info.mutants = fly.info.mutants[fly.info.mutants$Genotype %in% c("SUN1","SUN2","SUN3","R3","R5"),]

shared.info = c("Fly","Category","Gender","Genotype","experimenter","Age","Setup")

fly.info = rbind(fly.info.CS[,shared.info],fly.info.mutants[,shared.info])


pdf("coef_lm_fly_stats.pdf",onefile=T)
par(mar=c(4,10,5,4))

for(ind in 1:length(metrices)){
    if(metrices[ind] == "State_transitions: pp, pw, ww, wp"){
        next
    }
  if(ind == 19){next}

    metric.df = NULL;
    for(session in sessions){
        metric <- sapply(all_ofs[[session]],
                         function(x){
                             ##if((length(x) == 1) & (is.na(x))){
                             if(length(x) == 1){
                                 return(NA)
                             }else{
                                 return(x[[ind]])
                             }
                         }
                         )

        array.session = rep(session,length(metric))
        for(i in 1:length(array.session)){
            array.session[i] = gsub("X",fly.info$Category[i],array.session[i])
        }
        
        array.age = fly.info$Age
        array.age[array.age > 5] = 5
        #array.age[array.age < 3] = 3
        
        metric.df <- rbind(metric.df,
                           cbind(metric,
                                 fly.info$Category,
                                 fly.info$Fly,
                                 fly.info$Genotype,
                                 fly.info$experimenter,
                                 fly.info$Gender,
                                 array.age,
                                 fly.info$Setup,
                                 array.session
                                 )
                           )
    }
    colnames(metric.df) = c("value","category","fly","genotype","experimenter","gender","age","setup","session")

    ## Prepare input dataframe
    metric.df = data.frame(metric.df)
    for(i in 1:ncol(metric.df)){
      metric.df[,i] = unlist(metric.df[,i])
    }
    metric.df$setup <- factor(metric.df$setup,
                              levels = levels(metric.df$setup)[c(2,1,3,4)])
    metric.df$age <- factor(metric.df$age,levels = levels(metric.df$age)[c(3,1,2,4)])
    
    metric.df$value = as.numeric(as.character(metric.df$value))
    metric.df$batch = as.numeric(as.character(metric.df$fly))
    
    metric.df$batch[metric.df$batch <= 276] = 1
    metric.df$batch[metric.df$batch >= 277 & metric.df$batch <=400 ] = 2
    metric.df$batch[metric.df$batch > 400] = 3
    metric.df$batch = factor(metric.df$batch)

    metric.df$value[is.infinite(metric.df$value)] = NA

    metric.df = na.omit(metric.df)
    metric.df = metric.df[complete.cases(metric.df),]
    
    metric.df = metric.df[metric.df$genotype %in% c("WT","SUN1","SUN2","SUN3","R3"),]
    metric.df$genotype = factor(metric.df$genotype,
                                levels(metric.df$genotype)[levels(metric.df$genotype) %in% c("WT","SUN1","SUN2","SUN3","R3")][c(5,1:4)])
    
    ## Linear fit
    lm.fit.orig = lm(value ~ genotype + experimenter + gender + batch + setup + session + fly + age, metric.df)
    #lm.fit.orig = glm(value ~ age + gender + batch + session + fly, metric.df,family="gaussian")
    lm.fit = summary(lm.fit.orig)

    coef = lm.fit$coefficients[grep("fly",rownames(lm.fit$coefficients),invert=T),]
    coef = cbind(coef,p.adjust(coef[,4],method="BH"))
    colnames(coef)[5] = "fdr"

    cat(metrices[ind],"\n")
    cat(paste0("R^2 = ",lm.fit$adj.r.squared),"\n")
    print(coef[,c(1,2,4,5)])
    cat("\n\n")

    coef = coef[-1,]
    cint = confint(lm.fit.orig)
    cint = cint[rownames(coef),]
    ## Plot the coefficient
    n = nrow(coef)
    plot(c(0,0),c(0,n+1),type='l',
         xlim=c(min(cint[,1]),max(cint[,2])),
         ylim = c(1,n),
         xlab = "Coefficent", ylab = NA,
         yaxt='n',
         main = paste0(metrices[ind],"\n",
             "R^2 = ", round(lm.fit$adj.r.squared,digits=3)),
         col = "grey"
         )

    ## Axis of variable names
    
    axis(2,1:n,tick=F,
         rownames(coef),
         #new.variable.names[rownames(coef)],
         las=1,adj=1)

    ## Axis of p-values
    fdr.char = rep(NA,n)
    fdr.char[coef[,5] < 0.05] = "*"
    fdr.char[coef[,5] < 0.01] = "**"
    fdr.char[coef[,5] < 0.001] = "***"
    axis(4,1:n,tick=F,
         fdr.char,
         las=1,adj=0)
    
    ## Add error bar and estimated coefficent
    for(i in 1:n){
        lcol = "black"
        if(is.na(fdr.char[i])){lcol="grey"}
        lines(#c(coef[i,1]-coef[i,2],coef[i,1]+coef[i,2]),
            cint[rownames(coef)[i],],
              c(i,i),col=lcol)
    }
    pcol = rep("black",n)
    pcol[is.na(fdr.char)] = "grey"
    points(coef[,1],1:n,pch=16,
           col=pcol
           )

}
dev.off()
