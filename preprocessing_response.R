#!/usr/bin/env Rscript

sessions <- c("E1",
              "E1X",
              "E1XE1"
              )


## read fly info
## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp
fly.info = read.csv("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_response.csv",header=T,stringsAsFactors=F)
fly.info$Age = as.Date(fly.info$Exp.date,format='%m/%d/%Y')  - as.Date(fly.info$Birth.date,format='%m/%d/%Y')

fly.info.framerate = NULL
count = 0;
for(ind in 1:nrow(fly.info)){        
    query.sessions = gsub("X",fly.info$Category[ind],sessions)
    for(session in query.sessions){
        input.file <- list.files(path = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",
                                     fly.info$experimenter[ind],
                                     "/Response/CSV/"),
                                 
                                 pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                     "_",session,"_",
                                     fly.info$Gender[ind],"_.*"),
                                 
                                 full.names=T
                                 )

        if(session == "E1" & length(input.file) == 0){
            cat("M",fly.info$Fly[ind],fly.info$Category[ind],input.file,"\n",sep="\t")
          cat(ind,"\n")
            break
        }
        
        if(length(input.file) == 0){next}
        
        framerate = 50
        
        ## Read data
        #fly.pos <- scan(input.file,
        #                skip=1,quiet=T)
        fly.pos <- read.csv(input.file,stringsAsFactors = F)[,1]
        exp.time = length(fly.pos)
        
        fly.pos.dat = data.frame(fly.pos)
        colnames(fly.pos.dat) = paste0("fly_pos;framerate=",framerate)

        dir.create(paste0("data/",fly.info$experimenter[ind],"/Response/"), showWarnings = FALSE, recursive = TRUE)
        
        output.file <- paste0("data/",
                              fly.info$experimenter[ind],
                              "/Response/",
                              "ProcessedData_Fly",
                              fly.info$Fly[ind],
                              "_",session,
                              "_",fly.info$Genotype[ind],
                              ".csv")
                              
                              
        write.table(fly.pos.dat,
                    output.file,
                    row.names=F,quote=F)


        if(session == "E1"){
            fly.info.framerate = c(fly.info.framerate,framerate)
        }
    }
}

fly.info.out = fly.info
fly.info.out$Framerate = fly.info.framerate

write.table(fly.info.out,
            "data/fly_info_Response.csv",
            row.names=F,quote=F,sep=",")


## Fly,Gender,Category,Setup,Birth.date,Exp.date,Death.date,Age,experimenter,Fly_Exp
