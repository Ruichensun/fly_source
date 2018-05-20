get_fly_moving_speed <- function(x,framerate){
  data_start = 20 #changed it to 20 from 10 on Oct 5, 2016
  fly_pos = x[data_start:min(600*framerate,length(x))]
  experiment_time = length(fly_pos)/framerate
  tot_moving_dist = (sum(abs(diff(fly_pos)))*((48.7-1)/768))
  return(tot_moving_dist/experiment_time)
}


fly.info = read.csv("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_CS.csv",header=T,stringsAsFactors=F)

fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.out = NULL
count = 0;

for(ind in 1:nrow(fly.info)){        

        input.file <- list.files(path = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",
                                         fly.info$experimenter[ind],
                                         "/CS/CSV/"),
                                        pattern = paste0("ProcessedData_Fly",fly.info$Fly[ind],
                                        "_E1_",
                                        fly.info$Gender[ind],"_.*"),
                                        full.names=T
                                )
        if(length(input.file) == 0){next}
        
        framerate = 50
        
        fly.pos <- read.csv(input.file,stringsAsFactors = F)[,1]
        exp.time = length(fly.pos)
        
        ## Processing for some special cases
        if(
           ((fly.info$experimenter == "JG")&&(fly.info$Fly<=72))
           ||((fly.info$experimenter=="RS")&&(fly.info$Fly<=64))
           ||((fly.info$experimenter=="ES")&&(fly.info$Fly<=32))
           ||((fly.info$experimenter=="JE")&&(fly.info$Fly<=71))
           ||(fly.info$experimenter=="LM")
        ){
          fly.pos <- fly.pos[seq(1,exp.time,by=2)]
          framerate = 10
        }
        
      
        fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos,framerate))
}
        
pdf("Histogram of Mutants flies moving speed_unfiltered.pdf")
{
  hist(fly.moving.speed,breaks=100, xlab="Average Moving Speed (mm/s)", ylab="Counts", main="Wild type fruit fly average moving speed")
}
dev.off()
graphics.off()
        

fly.info = read.csv("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_mutants.csv",header=T,stringsAsFactors=F)
fly.moving.speed = NULL
fly.info.framerate = NULL
fly.info.out = NULL
count = 0;
genotype="JS19"


for(ind in 1:length(fly.info$Fly[fly.info$Genotype==genotype])){        
  
  input.file <- list.files(path = paste0("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/",
                                         fly.info$experimenter[fly.info$Genotype==genotype][ind],
                                         "/Mutants/CSV/"),
                           pattern = paste0("ProcessedData_Fly",fly.info$Fly[fly.info$Genotype==genotype][ind],
                                            "_E1_",
                                            fly.info$Gender[fly.info$Genotype==genotype][ind],"_.*"),
                           full.names=T
  )
  
  if(length(input.file) == 0){next}
  
  framerate = 50
  
  fly.pos <- read.csv(input.file,stringsAsFactors = F)[,1]
  exp.time = length(fly.pos)
  
  ## Processing for some special cases
  if((fly.info$Fly[fly.info$Genotype==genotype][ind]<=113 & fly.info$experimenter[fly.info$Genotype==genotype][ind] == "JG")
    # ||(fly.info$experimenter=="LM")
    ){
    
    fly.pos <- fly.pos[seq(1,exp.time,by=2)]
    framerate = 10
    # print(framerate)
    }
  
  fly.moving.speed = c(fly.moving.speed, get_fly_moving_speed(fly.pos,framerate))
  # print(paste0(fly.info$Fly_Exp[fly.info$Genotype==genotype][ind]," ",ind," ",framerate," ",get_fly_moving_speed(fly.pos,framerate)))
}

pdf(paste0("Histogram of ",genotype," flies moving speed_unfiltered.pdf"))
{
  hist(fly.moving.speed,breaks=20, xlab="Average Moving Speed (mm/s)", ylab="Counts", main=paste0(genotype, " flies average moving speed"))
}
dev.off()
graphics.off()
