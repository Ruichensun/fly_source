## Parameters

setwd("E:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter")
input_files <- c(list.files()[1],
                 list.files()[2],
                 list.files()[3],
                 list.files()[4],
                 list.files()[5]);

experimenter <- c("JE",
                "Jenny",
                "JG",
                "LM",
                "Ruichen"
                );

output_file = "CS_validation_combined.csv"


## End of parameters

all_info = NULL;
for(i in 1:length(input_files)){
    info = read.csv(input_files[i],header=T,stringsAsFactors=F)
    info$experimenter = experimenter[i]
    info$Fly_Exp = paste(info$Fly,experimenter[i],sep='_')
    all_info = rbind(all_info,info)
}

write.table(all_info,
            output_file,
            quote=F,row.names=F,col.names=T,sep=",")
