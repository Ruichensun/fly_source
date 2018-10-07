## Parameters

setwd("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter")

#Mutants

input_files <- c("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/Mutants/CSV/Behavioral Experiments - Mutants_JD.csv",
                 "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/SW/Mutants/CSV/Behavioral Experiments - Mutants_SW.csv",
                 "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JG/Mutants/CSV/Behavioral Experiments - Mutants_JG.csv",
                 "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/ES/Mutants/CSV/Behavioral Experiments - Mutants_ES.csv")

experimenter <- c("JD",
                  "SW",
                  "JG",
                  "ES"
          );

output_file = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_mutants.csv"

#CS

input_files<-c("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/ES/CS/CSV/Behavioral Experiments - CS_ES.csv",
               "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/RS/CS/CSV/Behavioral Experiments - CS_RS.csv",
               "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/CS/CSV/Behavioral Experiments - CS_JD.csv",
               "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/SW/CS/CSV/Behavioral Experiments - CS_SW.csv"
              )
experimenter <- c(
  "ES",
  "RS",
  "JD",
  "SW"
);
output_file = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_CS.csv"

#CS - Controls


input_files <- c("D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/CS_controls/CSV/Behavioral Experiments - CS_controls1_JD.csv",
                 "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/CS_controls/CSV/Behavioral Experiments - CS_controls2_JD.csv"
                 )

experimenter <- c("JD"
);

output_file = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_CS_controls.csv"

#Response

input_files<-c(
  "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/Response/CSV/Behavioral Experiments - Response_JD.csv",
  "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/ES/Response/CSV/Behavioral Experiments - Response_ES.csv"
)
experimenter <- c(
  "JD",
  "ES"
);
output_file = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_response.csv"

#Constant_time

input_files<-c(
  "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/JD/CS_constant/CSV/Behavioral Experiments - CS_constant_JD.csv"
)
experimenter <- c(
  "JD"
);
output_file = "D:/Behavioral_project/Behavior Experiment Data/Sorted_data_experimenter/fly_info_CS_constant.csv"

#Add unique identifier to the master chart
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
