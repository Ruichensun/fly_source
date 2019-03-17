# setwd("C:/Users/Ruichen/Desktop/data/csvdata")  ##Import the function for obtaining statistics of each experiment
setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")

library(Hmisc)
library(ggplot2)
library(rafalib)

fly.info.end = read.csv("data/fly_info_end.csv", header = T, stringsAsFactors = F)
geno = "MB131B x CS"
pdf(paste0(geno,"_", Sys.Date(), ".pdf"))

for (ind in 1:nrow(fly.info.end[fly.info.end$Genotype == geno, ])) {
  path = paste0("data/",
                fly.info.end[fly.info.end$Genotype == geno, ]$Experimenter[ind],
                "/mutants/")
  input.file = list.files(path)
  dbf.files <- input.file[grep(paste0("Fly", fly.info.end[fly.info.end$Genotype ==geno, ]$Fly[ind], "_"),
                               input.file, fixed = T)]
  print(dbf.files)
  mypar(length(dbf.files), 1)
  for (ind.session in 1:length(dbf.files)) {
    input.file.plotting = read.csv(paste0(path, dbf.files[ind.session]),
                                   header = T, stringsAsFactors = F)
    framerate = 50
    fly_pos = input.file.plotting$fly_pos.framerate.50
    starting_point = 21
    fly_pos = fly_pos[starting_point:length(fly_pos)]
    print(length(fly_pos) / framerate)
    plot((c(1:length(fly_pos) / framerate)),
         fly_pos,
         type = 'l',
         pch = 16,
         cex = 0.4,
         main = paste0(path, dbf.files[ind.session]),
         ylab = "",
         xlab = "Time (sec)",
         ylim = c(0, 800),
         col = "black",
         yaxt = "n"
    )
    axis(2, c(0, 767), labels = c("0", "767"))
  }
}
dev.off()




# For spontaneous behavior's long recording
setwd(
  'D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/data/JD/CS'
)
#For train flies' traces
filename = "ProcessedData_Fly87_E1_WT.csv"
input.file.plotting = read.csv(filename, header = T, stringsAsFactors =
                                 F)
framerate = 20
fly_pos = input.file.plotting$fly.position
starting_point = 21
fly_pos = fly_pos[starting_point:length(fly_pos)]
pdf(
  "D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/Spontaneous_40min_Fly467_2015Nov4.pdf"
)
mypar(6, 1)
plot((c(1:length(fly_pos)) / (framerate * 60)),
     fly_pos,
     type = 'l',
     pch = 16,
     cex = 0.4,
     main = "",
     ylab = "",
     xlab = "Time (min)",
     ylim = c(0, 800),
     col = "black",
     yaxt = "n"
     # xaxt =
)
# lines(t,pos_fly_position_info,col="blue")
axis(2, c(0, 767), labels = c("0", "767"))
dev.off()



filename = "ProcessedData_Fly467_E1_M_2015Nov5.csv"
input.file.plotting = read.csv(filename, header = T, stringsAsFactors =
                                 F)
framerate = 20
fly_pos = input.file.plotting$fly.position
starting_point = 21
fly_pos = fly_pos[starting_point:length(fly_pos)]
pdf(
  "D:/Behavioral_project/Behavior Experiment Data/Analysis/YP_051617/analysis/Spontaneous_40min_Fly467_2015Nov4.pdf"
)
mypar(6, 1)
plot((c(1:length(fly_pos)) / (framerate * 60)),
     fly_pos,
     type = 'l',
     pch = 16,
     cex = 0.4,
     main = "",
     ylab = "",
     xlab = "Time (min)",
     ylim = c(0, 800),
     col = "black",
     yaxt = "n"
     # xaxt =
)
axis(2, c(0, 767), labels = c("0", "767"))
dev.off()


#
#
#
# #plot original traces
#   for (j in fly_number1:fly_number2){
#     fly_files = dir(pattern = paste0("Fly",j,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE);
#     if (length(fly_files)!=0){
#       test_file_number = grep(pattern=("E1_|E2_|E3_"),fly_files)
#       for (i in 1:length(test_file_number)){
#         test_files[i] <- fly_files[as.numeric(test_file_number[i])]
#       }
#       for (i in 1:length(test_files)){
#         pos_fly_position_info = get_fly_position(test_files[i])
#         t = (1:length(pos_fly_position_info))/framerate
#         for (k in 3:(length(t)-2)){
#           if(!is.na(pos_fly_position_info[k])){
#             if(sum(is.na(pos_fly_position_info[c(k-2,k-1,k+1,k+2)]))==4){
#               pos_fly_position_info[k] = NaN
#             }
#           }
#         }
#         for (k in 2:length(t)){
#           if (is.nan(pos_fly_position_info[k])){
#             pos_fly_position_info[k] <- pos_fly_position_info[k-1];
#           }
#         }
#         output_title_0 = gsub("ProcessedData_","",basename(test_files[i]))
#         output_title_1 = gsub(".csv","",output_title_0)
#         plot(t,pos_fly_position_info,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")
#         lines(t,pos_fly_position_info,col="blue")
#       }
#     }
#   }
# dev.off()
# graphics.off()
# }
#
# #plot smoothened traces
# plot_fly_smoothened_test_traces <- function(fly_number1,fly_number2){
#
#   test_file_number = c()
#   test_files = c()
#
# #pdf(paste0("Fly_",fly_number1,"-",fly_number2, "_smoothened_test_traces.pdf"))
#     for (j in fly_number1:fly_number2){
#      fly_files = dir(pattern = paste0("Fly",j,"_.*\\.csv$"), full.names = TRUE, ignore.case = TRUE);
#      if (length(fly_files)!=0){
#        test_file_number = grep(pattern=("E1_|E2_|E3_"),fly_files)
#        for (i in 1:length(test_file_number)){
#          test_files[[j]][i] <- fly_files[as.numeric(test_file_number[i])]
#        }
#
#        for (i in 1:length(test_files[[j]])){
#          pos_fly_position_info_s = get_fly_position_smoothened(test_files[[j]][i])
#          t = (1:length(pos_fly_position_info_s))/framerate
#          output_title_0 = gsub("ProcessedData_","",basename(test_files[[j]][i]))
#          output_title_1 = gsub(".csv","",output_title_0)
#          print(output_title_1)
#          plot(t,pos_fly_position_info_s,type='b',pch=16,cex=0.4,main = output_title_1, ylab = "Position", xlab = "Time (sec)", ylim=c(0,800),col="grey")
#          lines(t,pos_fly_position_info_s,col="blue")
#        }
#      }
#    }
#
# #dev.off()
# graphics.off()
# }
