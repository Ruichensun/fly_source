##Demo for testing the 

fly.trace = read.csv("data/ES/CS/ProcessedData_Fly519_E1_WT.csv",header=T,stringsAsFactors=F)


# label_for_pause = rep(0,length(diff(fly.trace$fly_pos.framerate.50)))

label_for_pause = rep(0, length(fly_pos))


# for (i in 2:length(label_for_pause)){
#   if ((diff(fly.trace$fly_pos.framerate.50)[i]==0)&(diff(fly.trace$fly_pos.framerate.50)[i-1]>0)){
#     label_for_pause[i] = 1
#     # pre_pause_speed = c(pre_pause_speed,diff(fly.trace$fly_pos.framerate.50[i]))
#   }
#   else if ((diff(fly.trace$fly_pos.framerate.50)[i]>0)&(diff(fly.trace$fly_pos.framerate.50)[i-1]==0)){
#     label_for_pause[i] = 2
#     # pre_pause_speed = c(pre_pause_speed,diff(fly.trace$fly_pos.framerate.50[i]))
#   }
#   else if ((diff(fly.trace$fly_pos.framerate.50)[i]<0)&(diff(fly.trace$fly_pos.framerate.50)[i-1]==0)){
#     label_for_pause[i] = 3
#   }
#   else if ((diff(fly.trace$fly_pos.framerate.50)[i]==0)&(diff(fly.trace$fly_pos.framerate.50)[i-1]<0)){
#     label_for_pause[i] = 4
#   }
# }


for (i in 2:length(label_for_pause)){
  if ((fly_speed[i]==0)&(fly_speed[i-1]>0)){
    label_for_pause[i] = 1
    # pre_pause_speed = c(pre_pause_speed,diff(fly.trace$fly_pos.framerate.50[i]))
  }
  else if ((fly_speed[i]>0)&(fly_speed[i-1]==0)){
    label_for_pause[i] = 2
    # pre_pause_speed = c(pre_pause_speed,diff(fly.trace$fly_pos.framerate.50[i]))
  }
  else if ((fly_speed[i]<0)&(fly_speed[i-1]==0)){
    label_for_pause[i] = 3
  }
  else if ((fly_speed[i]==0)&(fly_speed[i-1]<0)){
    label_for_pause[i] = 4
  }
}


####Getting the index for the pause start and ends####
starts = c()
ends = c()
is_start = 1

for (i in 1:length(label_for_pause)){
  if (label_for_pause[i]!=0){
    if (is_start == 1){
      if ((label_for_pause[i]==1)|(label_for_pause[i]==4)){
        starts = c(starts, i)
        is_start=0
      }else{}
    }
    else{
      ends = c(ends, i)
      is_start=1
    }
  }
}

# print(is_start)

# pause_df = data.frame()
# end_type = label_for_pause[ends]
# start_type = label_for_pause[starts]
# start_position = fly.trace$fly_pos.framerate.50[starts]
# end_position = fly.trace$fly_pos.framerate.50[ends]

pause_df = data.frame()
end_type = label_for_pause[ends]
start_type = label_for_pause[starts]
start_position = fly_pos[starts-1]
end_position = fly_pos[ends-2]


pause_df = data.frame(starts[1:length(ends)],start_position[1:length(ends)], start_type[1:length(ends)], ends-1,end_position, end_type, ends-starts[1:length(ends)])
colnames(pause_df) = c("Start_Index","Start_Position","Start_Type","End_Index","End_Position","End_Type","Pause_Duration")