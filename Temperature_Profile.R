setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")

FILES = "032017Test1-Fly3TempSetup2.csv"
FILE = read.csv(FILES, header = T, stringsAsFactors = F)
length_plot = 12000  

pdf("Laser_Temperature.pdf")
plot((FILE$Time[1:length_plot]/1000),FILE$Channel.1[1:length_plot],type="l",ylab="Temperature (Celsius)", xlab = "Time (sec)")
dev.off()

# Supplemental Material
FILES = "Female1_092518.csv"
FILE = read.csv(FILES, header = T, stringsAsFactors = F)
length_plot = length(FILE$Time)

pdf("Laser_Temperature_supplementary-1.pdf")
plot((FILE$Time[1:length_plot]/1000),FILE$Channel.1[1:length_plot],type="l",ylab="Temperature (Celsius)", xlab = "Time (sec)")
dev.off()