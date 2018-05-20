#!/usr/bin/env Rscript
source("get_fly_speed_and_position.R")

load("all_ofs.Rdata")

sessions <- c("E1",
              "E1X1E1",
              "E1X1E1X1E1",
              "E1X1E1X1E1X1E1",
              "E1X1E1X1E1X1E1X1E1"
              )

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

## End of data import

ind_mutant = fly.info$experimenter == "ES" & fly.info$Fly >= 309 & fly.info$Fly <=372 &
    fly.info$Category == "T" & fly.info$Genotype != "WT"
ind_WT = fly.info$experimenter == "JD" & fly.info$Fly >= 41 & fly.info$Fly <=72 &
    fly.info$Genotype == "WT" & fly.info$Category == "T"


## WT
query.sessions = gsub("X","T",sessions)
count.WT = rep(0,length(query.sessions))
names(count.WT) = query.sessions
for(session in query.sessions){
    for(ind in which(ind_WT)){    
        input.file <- list.files(path = paste0("data/",
                                     fly.info$experimenter[ind],
                                     "/CS/"),                             
                                 pattern = paste0("ProcessedData_Fly",
                                     fly.info$Fly[ind],
                                     "_",session,"_WT.csv"),
                                 full.names=T
                                 )
        if(length(input.file) == 1){
            count.WT[session] = count.WT[session] + 1
        }
    }
}
print(count.WT)

## Mutants
mutants = unique(fly.info$Genotype[ind_mutant])
count.mutants = list()
for(i in 1:length(mutants)){
    x = rep(0,length(query.sessions))
    names(x) = query.sessions
    count.mutants = append(count.mutants,list(x))
}
names(count.mutants) = mutants
for(mutant in mutants){
    for(session in query.sessions){
        for(ind in which(ind_mutant)){    
            input.file <- list.files(path = paste0("data/",
                                         fly.info$experimenter[ind],
                                         "/mutants/"),                             
                                     pattern = paste0("ProcessedData_Fly",
                                         fly.info$Fly[ind],
                                         "_",session,"_",
                                         mutant,
                                         ".csv"),
                                     full.names=T
                                     )
            if(length(input.file) == 1){
                count.mutants[[mutant]][session] = count.mutants[[mutant]][[session]] + 1
            }
        }
    }
}

num = c(count.WT[1])


pdf("line_dropout_rate.pdf")
par(mar=c(8,5,5,5))
plot(1:length(query.sessions),count.WT/count.WT[1],
     type='l',ylim=c(0,1.1),col="black",lwd=3,
     ylab = "1 - dropout rate",xaxt='n',xlab=NA
     )

text(1:length(query.sessions),
     -0.05,
     sessions,
     xpd=T,
     srt=45,
     adj=1
     )


col.pool <- c("firebrick3",
              "dodgerblue4",              
              "darkseagreen",
              "darkkhaki",
              "lightsalmon4",
              "deepskyblue",
              "darkolivegreen",
              "coral","cadetblue3",
              "darkgoldenrod1","cornflowerblue","forestgreen"
              );
col.pool = col.pool[1:length(mutants)]
names(col.pool) = mutants
for(mutant in mutants){
    lines(1:length(query.sessions),count.mutants[[mutant]]/count.mutants[[mutant]][1],
          col=col.pool[mutant],lwd=3)
    num = c(num,count.mutants[[mutant]][1])
}

legend(1,0.3,
       paste(c("WT",mutants),"( n =",num,")"),
       col=c("black",col.pool),lwd=3)

d = dev.off()