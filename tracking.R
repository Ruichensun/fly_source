setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/Special_No_Stress")

filename = "RawData_Fly241_E1_F_2015Jul17.csv"

file = read.csv(filename, header = T, stringsAsFactors = F)

a = c()

for (i in 1:768){
  temp = c()
  for (j in 101:110){
    temp = c(temp, file[[j]][i])
  }
  temp = mean(temp)
  a = c(a, temp)
}
diff_frame = a - file[, 111]

ma = function(x, size){filter(x, rep(1/size, size), sides=2)}
size = 10
diff_frame_sm = ma(diff_frame, size) 
diff_filtered = diff_frame_sm - 6
diff_filtered = replace(diff_frame_sm, diff_frame_sm < 0, 0)
diff_filtered = ma(diff_filtered, size)
x_cor = c()
y_cor = c()
for (i in 1:length(diff_filtered)){
  if (!is.na(diff_filtered[i])){
    if (diff_filtered[i] > 0){
      x_cor = c(x_cor, i)
    }
  }
}
pdf("raw_data.pdf", width = 10)
plot(file[, 111], type = "l", ylab = "", xlab = "", xlim = c(1, 800), xaxt = "n")
axis(side = 1, at=c(0, 200, 400, 600, 768))
lines(a, col = "red")
dev.off()

pdf("get_fly_location.pdf", width = 10)
plot(diff_frame, type = 'l', ylim = c(-30, 30), ylab = "", xlab = "", xaxt = "n")
axis(side = 1, at=c(0, 200, 400, 600, 768))
lines(diff_filtered, col = "red")
points(x_centroid, 0, col = "red", pch = 19)
dev.off()
