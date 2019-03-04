setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
library(zoo)
library(boot)
library(dunn.test)

# For data preprocessing: combining all fly info into one document

metrices = c(
  "Experimenter", 
  "Genotype", 
  "Fly Number", 
  "Session", 
  "Number of Pause", 
  "Number of Middle Pause", 
  "Percentage Time Active", 
  "Percentage Time Active - Pause not at the End",
  "Median Pause Duration",
  "Median Middle Pause Duration",    
  "Max Pause Duration",
  "Max Middle Pause Duration", 
  "First Pause Duration",
  "First Middle Pause Duration", 
  "Average Moving Speed", 
  "Average Moving Speed (excluding pause)",
  "Average Speed When Enter Pause", 
  "Average Speed When Exit Pause",
  "Moving Distance",
  "Number of Turns",
  "Number of Middle Turns",
  "Fraction of Middle Turns Out of Total Turns",
  "Average Pause Duration", 
  "Average Middle Pause Duration"
)

combine_flyCSV = function(experimenter, type){
  all_info = NULL;
  input_files = c()
  for (i in 1:length(experimenter)){
    filename = paste0("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/", 
                      experimenter[i], "/", type[1], "/CSV/Behavioral Experiments - ", 
                      type[2], "_", experimenter[i], ".csv")
    input_files = c(input_files, filename)
  }
  output_file = 
    paste0("D:/Behavioral_project/behavior_experiment_data/Sorted_data_experimenter/fly_info_", 
           type[3], ".csv")
  for(i in 1:length(input_files)){
    info = read.csv(input_files[i],header=T,stringsAsFactors=F)
    info$Experimenter = experimenter[i]
    info$Fly_Exp = paste(info$Fly,experimenter[i],sep='_')
    if (experimenter[i] != "JGLNRS"){
      info$Gap = 0
      info$Exp.date2 = NA
    }else{
      info$Age = NA
    }
    all_info = rbind(all_info,info)
  }
  write.table(all_info,
              output_file,
              quote=F,row.names=F,col.names=T,sep=",")  
}

get_fly_moving_speed = function(x, framerate) {
  data_start = 31 #changed it to 20 from 10 on Oct 5, 2016
  fly_pos = x[data_start:min(600 * framerate, length(x))]
  experiment_time = length(fly_pos) / framerate
  tot_moving_dist = sum(abs(diff(fly_pos)))
  return(tot_moving_dist * (47 / 768) / experiment_time)
}

get_fly_initial_pause = function(x, framerate){
  data_start = 31
  fly_pos = x[data_start:min(600 * framerate, length(x))]
  experiment_time = length(fly_pos) / framerate
  fly_speed = diff(c(x[data_start - 1], fly_pos))
  pause = sum(fly_speed == 0) / framerate
  return(pause / experiment_time)
}

get_pause_df = function(fly_pos, fly_speed){
  label_for_pause = rep(0, length(fly_pos))
  for (i in 2:length(label_for_pause)) {
    if ((fly_speed[i] == 0) & (fly_speed[i - 1] > 0)) {label_for_pause[i] = 1}
    else if ((fly_speed[i] > 0) & (fly_speed[i - 1] == 0)) {label_for_pause[i] = 2}
    else if ((fly_speed[i] < 0) & (fly_speed[i - 1] == 0)) {label_for_pause[i] = 3}
    else if ((fly_speed[i] == 0) & (fly_speed[i - 1] < 0)) {label_for_pause[i] = 4}
  }
  
  # Getting the index for the pause start and ends #
  starts = c()
  ends = c()
  is_start = 1
  if (fly_speed[1] == 0){
    starts = c(starts, 1)
    is_start = 0
  }
  
  for (i in 1:length(label_for_pause)) {
    if (label_for_pause[i] != 0) {
      if (is_start == 1) {
        if ((label_for_pause[i] == 1) | (label_for_pause[i] == 4)) {
          starts = c(starts, i)
          is_start = 0
        }
      }
      else{
        ends = c(ends, i)
        is_start = 1
      }
    }
  }
  
  if (length(starts) < 1) {
    starts = 1
  }
  pause_df = data.frame()
  
  if (length(ends) < 1) {
    start_type = label_for_pause[starts]
    start_position = fly_pos[starts - 1]
    ends = length(fly_pos)
    end_position = fly_pos[ends]
    end_type = label_for_pause[ends]
    pause_df = data.frame(
      starts[1:length(ends)] - 1,
      start_position[1:length(ends)],
      start_type[1:length(ends)],
      ends,
      end_position,
      end_type,
      ends - starts[1:length(ends)]
    )
  } else{
    if (length(starts) - length(ends) == 1){
      ends = c(ends, length(fly_pos))
    }
    end_type = label_for_pause[ends]
    start_type = label_for_pause[starts]
    if (starts[1] == 1){
      start_position = fly_pos[starts]
      startindex = starts[1:length(ends)]
    }else{
      start_position = fly_pos[starts - 1]
      startindex = starts[1:length(ends)] - 1
    }
    end_position = fly_pos[ends]
    pause_df = data.frame(
      startindex,
      start_position[1:length(ends)],
      start_type[1:length(ends)],
      ends - 1,
      end_position,
      end_type,
      ends - starts[1:length(ends)]
    )
  }
  colnames(pause_df) = c(
    "Start_Index",
    "Start_Position",
    "Start_Type",
    "End_Index",
    "End_Position",
    "End_Type",
    "Pause_Duration"
  )
  return(pause_df)
}

one_fly_statistics = function(input_file,
                               framerate = 50,
                               speed_max_thres = 90,
                               speed_zero_thres = 1e-2,
                               pause_frame_thres = 25,
                               chamber_end_thres = 60,
                               chamber_left = 21,
                               chamber_right = 752,
                               special_case = F){
  #  pause_frame_thres - Least of number of frames in a pause
  #  speed_zero_thres - How small the speed is to be treated as not moving (i.e. zero)
  #  speed_max_thres - Maximum speed allowed (The setting for this threshold is as follows:
  #  The distance of centers of adjacent pixels is 63.5um
  #  A fly needs 5-6 seconds to travel from one end of the tube to the other end without pause (in a 50fps setting, 250 - 300 frames ==> 5-6sec)
  #  The tube length is 48.7mm.
  #  So the average walking speed is 48.7mm/5sec = 9.74mm/s or 48.7mm/6sec = 8.11mm/s
  #  Between two adjacent frames (which are 0.02s apart in a 50fps setting), the possible transient speed is 8.11mm/s x 0.02s = 0.1622mm                                                                                                   or 9.74mm/s x 0.02s = 0.1948mm
  #  This transient displacement translates to 0.1622(mm)/63.5um = 2.55px, or 0.1948mm/63.5um = 3.06px
  #  Therefore, the fly typically moves 2-3px per 0.02sec.
  #  Therefore, I set the maximum speed threshold in 0.02s duration to be 30px (10 times the usual speed)
  #  chamber_end_thres - How close (in ) a position to one end of the chamber to be treated as part of the end
  
  # Get File Info
  if (special_case == F){
      file_path = unlist(strsplit(input_file, "/"))
      Experimenter = file_path[2]
      file_name = unlist(strsplit(file_path[4], "_"))
      file_session = file_name[3]
      genotype = unlist(strsplit(file_name[4],".csv"))[1]
      fly_num = as.integer(unlist((strsplit(file_name[2], "Fly")))[2])
  }else if(special_case == T){
      Experimenter = "JGLNRS"
      file_session = unlist(strsplit(input_file, "_"))[3]
      genotype = "WT"
      fly_num = as.integer(unlist(strsplit(unlist(strsplit(input_file, "_"))[2], "Fly"))[2])
  }
  
  # Load data

      tryCatch({
        x = read.table(input_file, header = T, sep = ",", stringsAsFactors = F)
      }, error = function(e) {
        stop(paste0("Input file is empty!:\n","  Input files is: ", input_file, "\n\n"))
      })
      if (nrow(x) < 10) {stop(paste0("Input file is empty!:\n", "  Input files is: ", input_file, "\n\n"))}
      x = as.numeric(x[[1]])
      data_start = 31
      fly_pos = x[data_start:min(600 * framerate, length(x))]
      set_time = length(fly_pos)
      experiment_time = length(fly_pos)
      
      # Remove system noise using speed threshold
      fly_speed = diff(c(x[data_start - 1], fly_pos))
      for (i in 1:length(fly_pos)) {
        if (abs(fly_speed[i]) >= speed_max_thres) {
          fly_speed[i] = 0
        }
      }

  # 1st Metric Group: Pause #
      pause_df = get_pause_df(fly_pos, fly_speed)
      
      # Real pause is the pauses with duration longer than pause_frame_thres
      real_pause_df = subset(pause_df, Pause_Duration >= pause_frame_thres)
  
      # Pauses not at the end
      mid_pause_df = subset(real_pause_df, (Start_Position >= chamber_end_thres) & (Start_Position <= 767 - chamber_end_thres))
      pause_middle_dur = mid_pause_df$Pause_Duration
      
      num_pause = length(real_pause_df$Start_Index)
      num_mid_pause = length(mid_pause_df$Start_Index)
      
      md_pause_dur = median(real_pause_df$Pause_Duration) / framerate
      md_pause_middle_dur = (median(pause_middle_dur)) / framerate
      avg_pause_dur = mean(real_pause_df$Pause_Duration) / framerate
      avg_pause_middle_dur = (mean(pause_middle_dur)) / framerate
      
      frac_pause = sum(real_pause_df$Pause_Duration) / experiment_time 
      frac_pause_middle = (sum(pause_middle_dur)) / experiment_time
      
      max_pause = max(real_pause_df$Pause_Duration) / framerate
      max_pause_middle = (max(pause_middle_dur)) / framerate
      
      # First_pause_duration: first real pause or first real pause not at the end
      first_pause = real_pause_df$Pause_Duration[1] / framerate
      first_pause_middle = (pause_middle_dur[1]) / framerate
      
  # 2nd Metric Group: Speed
      
      # Enter and exit pause speeds (for all pauses)
      fly_speed_at_pause_start = NULL
      fly_speed_at_pause_end = NULL
      window_size = 0.5 * framerate
      if (num_mid_pause >= 2) {
        for (i in 2:num_mid_pause) {
          ps = mid_pause_df$Start_Index[i]
          pe = mid_pause_df$End_Index[i]
          if (ps < window_size || pe > experiment_time - window_size) {
            fly_speed_at_pause_start = c(fly_speed_at_pause_start, NA)
            fly_speed_at_pause_end = c(fly_speed_at_pause_end, NA)
          } else{
            fly_speed_at_pause_start = c(fly_speed_at_pause_start, mean(abs(fly_speed[(ps - window_size):ps])))
            fly_speed_at_pause_end = c(fly_speed_at_pause_end, mean(abs(fly_speed[pe:(pe + window_size)])))
          }
        }
      }

      #Avg fly speed
      avg_fly_speed = mean(abs(fly_speed), na.rm = TRUE)
      avg_fly_speed_not_in_pause = mean(abs(fly_speed[which(abs(fly_speed) > 0)]), na.rm = TRUE)

      if (length(fly_speed[which(abs(fly_speed) > 0)]) < 10) {
        avg_fly_speed_not_in_pause = 0
      }
      if (max(abs(fly_speed)) == 0) {
        avg_fly_speed_not_in_pause = NA
      }

      avg_fly_speed_enter = mean(fly_speed_at_pause_start, na.rm = TRUE)
      avg_fly_speed_exit = mean(fly_speed_at_pause_end, na.rm = TRUE)

      # Convert the speed from px/frame to mm/sec
      # The visible part of the tube is 47 mm
      avg_speed = (avg_fly_speed * 47 / 768) / (1 / framerate)
      avg_speed_moving = (avg_fly_speed_not_in_pause * 47 / 768) / (1 / framerate)
      avg_speed_enter = (avg_fly_speed_enter * 47 / 768) / (1 / framerate)
      avg_speed_exit = (avg_fly_speed_exit * 47 / 768) / (1 / framerate)

      # Total Moving Distance
      tot_moving_dist = sum(abs(fly_speed[1:experiment_time]), na.rm = TRUE)
      dist = tot_moving_dist * 47 / 768

  # 3rd Metric Group: Turns

      # Step 0 - smoothing
      ma = function(x, bin_size){filter(x, rep(1/bin_size, bin_size), sides=2)}
      bin_size = 150
      fly_pos_sm = ma(fly_pos, bin_size)
      fly_speed_sm = diff(fly_pos_sm)

      # Step 1 - get the moving direction (speed sign)
      bin_positive_frac = NULL
      t = bin_size
      while (t < experiment_time) {
        bin_fly_speed = fly_speed_sm[t - 1:bin_size + 1]
        frac = sum(bin_fly_speed > 0, na.rm = T) / sum(bin_fly_speed != 0, na.rm = T)
        bin_positive_frac = c(bin_positive_frac, frac)
        t = t + bin_size
      }

      # Step 2 - get the turns
      turns = find_intersect_points(bin_positive_frac, rep(0.5, length(bin_positive_frac)))
      turns = ceiling(turns * bin_size - bin_size / 2)
      position_turns = (fly_pos[turns] + fly_pos[turns - 1]) / 2 # Use original fly_pos value as the actual position
      mid_turns = turns[position_turns > chamber_end_thres & position_turns < 767 - chamber_end_thres]
      position_mid_turns = position_turns[position_turns > chamber_end_thres & position_turns < 767 - chamber_end_thres]

      # Step 3 - get turn numbers
      num_turns = length(turns)
      num_mid_turns = length(mid_turns)

      if (num_turns == 0){
        frac_mid_turns = 0
      } else{
        frac_mid_turns = num_mid_turns / num_turns
      }

  #Return output
      
      ret = data.frame(cbind(
                              Experimenter,
                              genotype,
                              fly_num,
                              file_session,
                              num_pause,
                              num_mid_pause,
                              1 - frac_pause, #unit: percentage
                              1 - frac_pause_middle,
                              md_pause_dur, 
                              md_pause_middle_dur, 
                              max_pause,
                              max_pause_middle,
                              first_pause,
                              first_pause_middle,
                              avg_speed,
                              avg_speed_moving,
                              avg_speed_enter,
                              avg_speed_exit,
                              dist,
                              num_turns,
                              num_mid_turns,
                              frac_mid_turns,
                              avg_pause_dur,
                              avg_pause_middle_dur
                            ), stringsAsFactors=FALSE)
  
      colnames(ret) = c(
        "Experimenter", #2
        "Genotype", #3
        "Fly Number", #4
        "Session", #5
        "Number of Pause", #6
        "Number of Middle Pause", #7
        "Percentage Time Active", #8
        "Percentage Time Active - Pause not at the End", #9
        "Median Pause Duration",#10
        "Median Middle Pause Duration", #11    
        "Max Pause Duration", #12
        "Max Middle Pause Duration", #13
        "First Pause Duration", #14
        "First Middle Pause Duration", #15
        "Average Moving Speed", #16
        "Average Moving Speed (excluding pause)", #17
        "Average Speed When Enter Pause", #18
        "Average Speed When Exit Pause",#19
        "Moving Distance",#20
        "Number of Turns",#21
        "Number of Middle Turns",#22
        "Fraction of Middle Turns Out of Total Turns",#23
        "Average Pause Duration", #39
        "Average Middle Pause Duration" #40
      )
      return(ret)
}

find_intersect_points = function(x1, x2){
  ##Adapted from code by nograpes
  ##http://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
  ## Find points where x1 is above x2.
  above = x1 > x2
  ## Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points = which(diff(above) != 0)
  ## Find the slopes for each line segment.
  x1.slopes = x1[intersect.points + 1] - x1[intersect.points]
  x2.slopes = x2[intersect.points + 1] - x2[intersect.points]
  ## Find the intersection for each segment.
  x.points = intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes - x2.slopes))
  return(x.points)
}

shuffle_is_pause = function(is_pause) {
  if (max(is_pause) == 0) {
    return(is_pause)
  }
  r = rle(is_pause)
  ## Prepare sequence for shuffling
  folded_is_pause = NULL
  for (i in 1:length(r$values)) {
    if (r$values[i] == 1) {
      ## Pause
      ## Fold
      folded_is_pause = c(folded_is_pause, i)
    } else{
      ## Move
      ## Do not fold
      folded_is_pause = c(folded_is_pause, rep(0, r$lengths[i]))
    }
  }
}

Use_T_find_R = function(fly.info, Tindex){
  if (fly.info[Tindex, ]$Category != "T"){
    return(c())
  }else{
    Rlst = c()
    setup_T = fly.info[Tindex, ]$Setup
    for (i in ((Tindex - setup_T + 1):(Tindex + 4 - setup_T))){
      if ((i < 1) | (i > nrow(fly.info))){
        next
      }
      if (fly.info[i, ]$Category == "R" & 
          fly.info[i, ]$Genotype == fly.info[Tindex, ]$Genotype &
          fly.info[i, ]$Exp.date == fly.info[Tindex, ]$Exp.date &
          fly.info[i, ]$Experimenter == fly.info[Tindex, ]$Experimenter & 
          fly.info[i, ]$Setup != fly.info[Tindex, ]$Setup){
        Rlst = c(Rlst, i)
      }
    }
  return (Rlst)
  }
}

data_filter = function(filter, fly.info){
  # filter 1: filtering flies by walking speed
  if (filter == 1) {
    ind.include = NULL
    for (genotype in unique(fly.info$Genotype)) {
      if (genotype == "CS") {
        next
      }
      else if (genotype == "WT") {
        ind = fly.info$Genotype %in% c("WT", "CS")
      }else{
        ind = fly.info$Genotype == genotype 
      }
      fms = fly.info$Fly.moving.speed[ind]
      rank_fms = rank(fms)
      ind.filter =  rank_fms <= length(fms) * 1.0 & rank_fms >= length(fms) * 0.0 # changed from 0.3 to 0.0
      ind.include = c(ind.include, which(ind)[ind.filter])
    }
  }
  # filter 2: filtering flies by pause
  if (filter == 2) {
    ind.include = NULL
    for (genotype in unique(fly.info$Genotype)) {
      if (genotype == "CS") {
        next
      }
      else if (genotype == "WT") {
        ind = fly.info$Genotype %in% c("WT", "CS")
      }else{
        ind = fly.info$Genotype == genotype
      }
      pause = fly.info$Fly.pause[ind]
      ind.filter =  pause <= 0.9
      ind.include = c(ind.include, which(ind)[ind.filter])
    }
  }
  # filter 3: filtering based on pass_fly_QC code
  if (filter == 3) {
    ind.include = NULL
    session = "E1"
    for (ind in 1:nrow(fly.info)) {
      if (fly.info$Genotype[ind] == "WT") {
        input.file = paste0("data/", fly.info$experimenter[ind], "/CS/", "ProcessedData_Fly",
                             fly.info$Fly[ind], "_",session,"_WT",".csv")
      } else{input.file = paste0("data/", fly.info$experimenter[ind], "/Mutants/", "ProcessedData_Fly",
                                  fly.info$Fly[ind], "_", session, "_", fly.info$Genotype[ind], ".csv")
      }
      framerate =	fly.info$Framerate[ind]
      if (pass_fly_QC(input.file, framerate)) {
        ind.include = c(ind.include, ind)
      }
    }
  }
  return(ind.include)
}

checking_fly_numbers = function(fly.info, filter, filename){
  ind.include = data_filter(filter, fly.info)
  fly.info.include = fly.info[ind.include, ]
  type_of_mutants = length(unique(fly.info.include$Genotype))
  names_of_mutants = unique(fly.info.include$Genotype)
  n = c()
  n_QCed = c()
  for (i in 1:type_of_mutants){
    n[i] = dim(fly.info[fly.info$Genotype==names_of_mutants[i],])[1]
    n_QCed[i] = dim(fly.info.include[fly.info.include$Genotype==names_of_mutants[i],])[1]
  }
  mutant_info = data.frame(names_of_mutants,n, n_QCed)
  colnames(mutant_info) = c("Genotype", "Number of Flies", "Number of Flies QCed")
  write.csv(mutant_info, file = filename , row.names = FALSE)
}

pass_fly_QC = function(input_file,
                        framerate = 50,
                        speed_max_thres = 50,
                        speed_zero_thres = 1e-2,
                        pause_frame_thres = 25,
                        chamber_end_thres = 50) {
  ## read input file
  tryCatch({
    x = read.table(
      input_file,
      header = T,
      sep = ",",
      stringsAsFactors = F
    )
  }, error = function(e) {
    stop(paste0(
      "Input file is empty!:\n",
      "  Input files is: ",
      input_file,
      "\n\n"
    ))
  })
  if (nrow(x) < 10) {
    stop(paste0(
      "Input file is empty!:\n",
      "  Input files is: ",
      input_file,
      "\n\n"
    ))
  }
  x = as.numeric(x[, 1])
  fly_num = sapply(strsplit(input_file, "_"), function(x)
    return(gsub("Fly", "", x[2])))
  data_start = 31 
  fly_pos = x[data_start:min(600 * framerate, length(x))]
  experiment_time = length(fly_pos)
  ##Get the transient speed
  if (data_start > 1) {
    fly_speed = diff(c(x[data_start - 1], fly_pos))
  } else {
    fly_speed = diff(c(NA, fly_pos))
  }
  pause_start = NULL
  pause_end = NULL
  potential_pause_start = 1
  current_zero_length = 0
  is_pause = rep(0, experiment_time)
  if (fly_speed[1] == 0) {
    current_zero_length = 1
  }
  for (t in 2:experiment_time) {
    if (fly_speed[t] == 0) {
      if (current_zero_length == 0) {
        potential_pause_start = t
        current_zero_length = current_zero_length + 1
      } else{
        current_zero_length = current_zero_length + 1
      }
    } else{
      if (current_zero_length >= pause_frame_thres) {
        pause_start = c(pause_start, potential_pause_start)
        pause_end = c(pause_end, t - 1)
        is_pause[potential_pause_start:(t - 1)] = 1
      }
      current_zero_length = 0
    }
  }
  if (current_zero_length >= pause_frame_thres) {
    pause_start = c(pause_start, potential_pause_start)
    pause_end = c(pause_end, experiment_time)
    is_pause[potential_pause_start:experiment_time] = 1
  }
  num_pause = length(pause_start)
  if (max(pause_end - pause_start + 1) >= (20 * framerate)) {
    return(FALSE)
  }
  else {
    return(TRUE)
  }
}


get_laser_df = function(fly_laser, framerate){
  laser_ON  = rle(fly_laser)$length[rle(fly_laser)$values == 1]
  laser_OFF = rle(fly_laser)$length[rle(fly_laser)$values == 0]
  label_for_laser = rep(0, length(fly_laser))
  for (i in 1:(length(label_for_laser) - 1)) {
    if ((fly_laser[i] == 0) & (fly_laser[i + 1] > 0)) {
      label_for_laser[i + 1] = 1
    }
    if ((fly_laser[i] > 0) & (fly_laser[i + 1] == 0)) {
      label_for_laser[i + 1] = 2
    }
  }
  laser_df = data.frame()
  if (laser_OFF[1] == length(fly_laser)) {
    laser_df = data.frame(0,
                          0,
                          0,
                          (laser_OFF[1]) / framerate,
                          (laser_OFF[1] - 0) / framerate,
                          TRUE)
  } else{
    laser_df = data.frame (which(label_for_laser == 1),
                           which(label_for_laser == 2),
                           laser_ON / framerate,
                           (laser_OFF[2:length(laser_OFF)]) / framerate,
                           (laser_OFF[2:length(laser_OFF)] - laser_ON) / framerate,
                           laser_OFF[2:length(laser_OFF)] >= 8 * 60 * framerate
  )
  }
  colnames(laser_df) = c(
    "Laser_On",
    "Laser_Off",
    "ON_duration",
    "OFF_duration",
    "Difference",
    "eight_min_OFF"
  )
  return(laser_df)
}

one_fly_laser_statistics = function(input_file, framerate){
  file_path = unlist(strsplit(input_file, "/"))
  Experimenter = file_path[2]
  file_name = unlist(strsplit(file_path[4], "_"))
  file_session = file_name[3]
  genotype = unlist(strsplit(file_name[4],".csv"))[1]
  fly_num = as.integer(unlist((strsplit(file_name[2], "Fly")))[2])

  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = as.numeric(fly.file[[1]])
  fly.laser.raw = as.numeric(fly.file[[2]])
  if (is.na(fly.laser.raw[1]) == T) {
    number_of_laser_clicks = -1
    total_laser_ON = -1
    ret = data.frame(cbind(
      Experimenter,
      genotype,
      fly_num,
      file_session,
      number_of_laser_clicks,
      total_laser_ON #in seconds
    ))
    colnames(ret) = c(
      "Experimenter", 
      "Genotype", 
      "Fly Number",
      "Session",       
      "Laser_Count",
      "Laser_Exposure"
      )
    return(ret)
  }else{
    data_start = 31
    fly_pos = fly.position.raw[data_start:length(fly.position.raw)]
    fly_laser = fly.laser.raw[data_start:length(fly.laser.raw)]
    if (fly_laser[length(fly_laser)] > 0) {
      fly_laser[length(fly_laser)] = 0
    }
    if (fly_laser[1] > 0) {
      fly_laser[1] = 0
    }
    for (i in 1:length(fly_laser)) {
      if (fly_laser[i] > 0) {
        fly_laser[i] = 1
      }
    }
    laser_df = get_laser_df(fly_laser, framerate)
    number_of_laser_clicks = length(laser_df$Laser_On)
    total_laser_ON = sum(laser_df$ON_duration)
    if (number_of_laser_clicks == 1) {
      if (laser_df$ON_duration == 0) {
        number_of_laser_clicks = 0
      }
    }
    ret = data.frame(cbind(
      Experimenter,
      genotype,
      fly_num,
      file_session,
      number_of_laser_clicks,
      total_laser_ON 
    ))
    colnames(ret) = c(
      "Experimenter", 
      "Genotype", 
      "Fly Number",
      "Session",       
      "Laser_Count",
      "Laser_Exposure")
    return(ret)
  }
}

subset_laser_expo = function(fly.info.end, Random, Training, threshold = 0.2){
  R_sub = data.frame()
  T_sub = data.frame()
  for (i in 1:nrow(fly.info.end)){
    if(fly.info.end[i, ]$Category=="T"){
      Rflies = Use_T_find_R(fly.info.end, i)
      if (length(Rflies) > 0){
        for (k in 1:length(Rflies)){
          experimenter = fly.info.end[Rflies[k], ]$Experimenter
          genotype = fly.info.end[Rflies[k], ]$Genotype
          fly.num = fly.info.end[Rflies[k], ]$Fly
          DIFF = Random[Random$Experimenter == experimenter &
                        Random$Genotype == genotype &
                        Random$Fly == fly.num, ]$Diff 
          if (length(DIFF) > 0 && !is.na(DIFF)){
            if (abs(DIFF) <= threshold){
              R_sub = rbind(R_sub, 
                            Random[Random$Experimenter == experimenter &
                                   Random$Genotype == genotype &
                                   Random$Fly == fly.num, ])
              T_sub = rbind(T_sub,
                            Training[Training$Experimenter == fly.info.end[i, ]$Experimenter &
                                     Training$Genotype == fly.info.end[i, ]$Genotype &
                                     Training$Fly == fly.info.end[i, ]$Fly, ])
            }
          }
        }
      }
    }
  }
  return_Val = rbind(R_sub, T_sub)
  return(return_Val)
}

Delay_of_Laser_On_Off = function(input_file){
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = fly.file$fly_pos.framerate.50
  fly.laser.raw = fly.file$laser_status
  # If laser status not recorded, return NA
  if (is.na(fly.laser.raw[1]) == T) {
    return(c(NA,NA))
  }else{
    fly.moving.status.raw = fly_pos_to_moving_status(fly.position.raw)
    starting_point = 21
    fly.position = fly.position.raw[starting_point:length(fly.position.raw)]
    fly.laser.status = fly.laser.raw[starting_point:length(fly.laser.raw)]
    fly.moving.status = fly.moving.status.raw[(starting_point-1):length(fly.moving.status.raw)] 
  }
  for (i in 1:length(fly.moving.status)){
    if ((fly.position[i]<50)){
      fly.moving.status[i] = 1
    }
    if ((fly.position[i]>717)){
      fly.moving.status[i] = 1
    }
  }
  moving_status_summary = rle(fly.moving.status)
  
  total_frame_moving = sum(moving_status_summary$lengths[moving_status_summary$values!=0])
  total_frame_pause = sum(moving_status_summary$lengths[moving_status_summary$values==0])
  
  moving_laser_status.df = data.frame(fly.moving.status,fly.laser.status)
  
  fly.laser.status.ONSET = replace(fly.laser.status, fly.laser.status > 0, 1)
  laser.moving.status = fly.laser.status.ONSET - fly.moving.status
  
  laser.moving.status_summary = rle(laser.moving.status)
  Laser_On_Delay_by_event = laser.moving.status_summary$lengths[laser.moving.status_summary$values==-1]
  Laser_Off_Delay_by_event = laser.moving.status_summary$lengths[laser.moving.status_summary$values==1]
  Laser_On_Delay = mean(Laser_On_Delay_by_event)
  Laser_Off_Delay = mean(Laser_Off_Delay_by_event)
  return(c(Laser_On_Delay/50, #Framerate is 50
           Laser_Off_Delay/50 #Framerate is 50
  ))
}

#Quantify the delay on/off of laser of all flies
Laser_Delay = function(file_name_filter, fly.info.movement) {
  laser_delays = data.frame()
  for (ind in 1:nrow(fly.info.movement)) {
    if(fly.info.movement$Genotype[ind]=="WT"){
      input.file = list.files(
        path = paste0("data/", fly.info.movement$experimenter[ind], "/CS/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_WT", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    if(fly.info.movement$Genotype[ind]=="CS"){
      input.file = list.files(
        path = paste0("data/", fly.info.movement$experimenter[ind], "/mutants/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_CS", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    laser_delays = rbind(laser_delays, Delay_of_Laser_On_Off(input.file))
  }
  names(laser_delays) = c("Delays of Laser On", "Delays of Laser Off")
  return(laser_delays)
}

#Quantify percentage of time being hit by laser both during walking and during pause
chance_of_being_hit_by_laser = function(input_file){
  
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = fly.file$fly_pos.framerate.50
  fly.laser.raw = fly.file$laser_status
  # If laser status not recorded, return NA
  if (is.na(fly.laser.raw[1]) == T) {
    return(c(NA,NA,NA))
  }else{
    fly.moving.status.raw = fly_pos_to_moving_status(fly.position.raw)
    starting_point = 21
    fly.position = fly.position.raw[starting_point:length(fly.position.raw)]
    fly.laser.status = fly.laser.raw[starting_point:length(fly.laser.raw)]
    fly.moving.status = fly.moving.status.raw[(starting_point-1):length(fly.moving.status.raw)]
    for (i in 1:length(fly.moving.status)){
      if (fly.position[i] < 60){
        fly.moving.status[i] = 1
      }
      if (fly.position[i] > 708){
        fly.moving.status[i] = 1
      }
    }
    
    moving_status_summary = rle(fly.moving.status)
    
    total_frame_moving = sum(moving_status_summary$lengths[moving_status_summary$values!=0])
    total_frame_pause = sum(moving_status_summary$lengths[moving_status_summary$values==0])
    
    moving_laser_status.df = data.frame(fly.moving.status,fly.laser.status)
    moving_status_during_laser_ON = rle(moving_laser_status.df$fly.moving.status[moving_laser_status.df$fly.laser.status!=0])
    
    moving_laser_ON = sum(moving_status_during_laser_ON$lengths[moving_status_during_laser_ON$values==1])
    pause_laser_ON = sum(moving_status_during_laser_ON$lengths[moving_status_during_laser_ON$values==0])
    
    chance_of_being_hit_by_laser_during_moving = moving_laser_ON/total_frame_moving
    chance_of_being_hit_by_laser_during_pause = pause_laser_ON/total_frame_pause
    laser_on_percentage = length(moving_laser_status.df$fly.moving.status[moving_laser_status.df$fly.laser.status!=0])/length(fly.moving.status)
    
    return(c(chance_of_being_hit_by_laser_during_moving,
             chance_of_being_hit_by_laser_during_pause,
             laser_on_percentage))
  }
}

#Calculating all the flies' chance of being hit by types (T/R)
Hit_by_laser = function(file_name_filter, fly.info.movement) {
  laser_chance = data.frame()
  for (ind in 1:nrow(fly.info.movement)) {
    if(fly.info.movement$Genotype[ind]=="WT"){
      input.file = list.files(
        path = paste0("data/",fly.info.movement$Experimenter[ind],"/CS/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_WT", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    if(fly.info.movement$Genotype[ind]=="CS"){
      input.file = list.files(
        path = paste0("data/", fly.info.movement$Experimenter[ind], "/mutants/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_CS", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    laser_profile = chance_of_being_hit_by_laser(input.file)
    temp = cbind(fly.info.movement[ind, ], laser_profile[1], laser_profile[2], laser_profile[3])
    laser_chance = rbind(laser_chance, temp)
  }
  names(laser_chance) = c(names(fly.info.movement[ind, ]), "Hit_W", "Hit_P", "Hit_All")
  return(laser_chance)
}

fly_pos_to_moving_status = function(fly_pos){ 
  # This is determined by quantile(abs(fly_moving_status),c(0.97, 0.975, 0.98)), and the 97.5% 
  # corresponds to 28.6
  speed_threshold = 50
  fly_moving_status = diff(fly_pos)
  # Finding out the fly's moving status by two criteria: velocity = 0 or velocity much larger than 
  # a speed threshold
  fly_moving_status_discretized = replace(fly_moving_status, fly_moving_status >
                                            speed_threshold, 0)
  fly_moving_status_discretized = replace(fly_moving_status_discretized,
                                          fly_moving_status_discretized != 0,
                                          1)
  return(fly_moving_status_discretized)
}

moving_status = function(input_file) {
  a = read.csv(input_file, header = T, stringsAsFactors = F)
  if(is.na(a$laser_status[1])){
    return(NA)
  }else{
    fly_pos = a$fly_pos.framerate.50
    fly_moving_status = fly_pos_to_moving_status(fly_pos)
    starting_point = 31
    fly_moving_status = fly_moving_status[(starting_point-1):length(fly_moving_status)]
    return(cumsum(fly_moving_status))
  }
}

###Get all CS flies' cumulated moving status grouped by types (T/R/N)
get_sequence_length = function(file_name) {
  if (sum(is.na(moving_status(file_name)))==0){
    return (length(moving_status(file_name)))
  }else{return(NA)}
}

get_cumsums_total = function(file_name_filter, fly.info.movement) {
  file_names = c()
  for (ind in 1:nrow(fly.info.movement)) {
      input.file = list.files(
        path = paste0("data/", fly.info.movement$Experimenter[ind], "/CS_constant/CSV"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_",
                         file_name_filter, "_*"),
        full.names = T
      )
    if(length(input.file)==0){next()}
    if (!is.na(get_sequence_length(input.file))){
      file_names = c(file_names, input.file) 
    }else{next()}
  }
  # Get min sequence length
  sequence_lengths = unlist(lapply(file_names, get_sequence_length))
  min_sequence_length = min(sequence_lengths)
  # Concat cumsums to matirx
  cumsums = matrix(nrow = min_sequence_length, ncol = 0)
  for (file_name in file_names) {
    cumsums = cbind(cumsums, moving_status(file_name)[1:min_sequence_length])
  }
  return(cumsums)
}

get_Wald_CI = function(data){
  Mboot = boot(data,
               function(x,i) median(x[i]), 
               R=10000)
  
  CI = boot.ci(Mboot,
               conf = 0.95, 
               type = c("norm") 
               # "basic" ,"perc", "bca")
  )
  return(c(CI$t0, CI$normal[2], CI$normal[3]))
}

get_learning_index_normalized = function(fly.info.end, all_ofs, g_list){
  master_learn = data.frame()
  for (i in 1:length(g_list)){
    learn_list = data.frame()
    if (g_list[i]=="WT"){
      fly.info.temp = fly.info.end[(fly.info.end$Genotype == "WT"|fly.info.end$Genotype=="CS") , ]
    }else{
      fly.info.temp = fly.info.end[fly.info.end$Genotype == g_list[i] , ]
    }
    for (j in 1:nrow(fly.info.temp)){
    # for (j in 1:2){
      if (fly.info.temp[j, ]$Category=="T"){
        Rlst = Use_T_find_R(fly.info.temp, j)
        if (length(Rlst) > 0){
          E1_R = c()
          E5_R = c()
          LI_R = c()
          for (k in 1:length(Rlst)){
            E1_R = c(E1_R, all_ofs[all_ofs$Fly.Number == fly.info.temp[Rlst[k],]$Fly &
                                   all_ofs$Genotype == fly.info.temp[Rlst[k],]$Genotype &
                                   all_ofs$Experimenter == fly.info.temp[Rlst[k],]$Experimenter &
                                   all_ofs$Session == "E1" &
                                   all_ofs$Type == "R", ]$Percentage.Time.Active)
            E5_R = c(E5_R, all_ofs[all_ofs$Fly.Number == fly.info.temp[Rlst[k],]$Fly &
                                     all_ofs$Genotype == fly.info.temp[Rlst[k],]$Genotype &
                                     all_ofs$Experimenter == fly.info.temp[Rlst[k],]$Experimenter &
                                     all_ofs$Session == "E1R1E1R1E1" &
                                     all_ofs$Type == "R", ]$Percentage.Time.Active)
            LI_R = c(LI_R, (E1_R - E5_R) / E1_R)
            }
          E1_R = mean(E1_R)
          E5_R = mean(E5_R)
          # LI_R = mean(LI_R)
          E1_T = all_ofs[all_ofs$Fly.Number== fly.info.temp[j, ]$Fly & 
                           all_ofs$Experimenter == fly.info.temp[j, ]$Experimenter & 
                           all_ofs$Genotype == fly.info.temp[j, ]$Genotype &
                           all_ofs$Session=="E1" &
                           all_ofs$Type=="T", ]$Percentage.Time.Active
          E5_T = all_ofs[all_ofs$Fly.Number== fly.info.temp[j, ]$Fly & 
                           all_ofs$Experimenter == fly.info.temp[j, ]$Experimenter &
                           all_ofs$Genotype == fly.info.temp[j, ]$Genotype &
                           all_ofs$Session=="E1T1E1T1E1", ]$Percentage.Time.Active
          # LI_T = (E1_T - E5_T) / E1_T
          learning_index5 = E5_R - E5_T
          learning_index1 = E1_R - E1_T
          # learn_index = ((E1_T - E5_T)/E1_T - (E1_R - E5_R)/E1_R)
          # learn_index = LI_T - LI_R
          # learn_index = (E1_T - E5_T) / E1_T
          # learn_index = (as.numeric(E1) - as.numeric(E5))/(as.numeric(E1))
          # learn_index = (as.numeric(E5))/(as.numeric(E1))
          learning_index = c(learning_index1, learning_index5)
          learn_list = rbind(learn_list, learning_index)
        }
      }
      }
    name_list = rep(g_list[i], nrow(learn_list))
    m = data.frame(
      factor = name_list,
      value1 = learn_list[,1],
      value2 = learn_list[,2]
    )
    colnames(m) = c("Genotype", "Learning_1", "Learning_5")
    master_learn = rbind(master_learn, m)
  }
  master_learn$Genotype = factor(master_learn$Genotype, levels = g_list)
  return(master_learn)
}

get_learning_index = function(fly.info.end, all_ofs, metric.ind, category, g_list){
  master_learn = data.frame()
  for (i in 1:length(g_list)){
    learn_list = c()
    if (g_list[i]=="WT"){
      fly.info.temp = fly.info.end[(fly.info.end$Genotype == "WT"|fly.info.end$Genotype=="CS") &
                                     fly.info.end$Category == category, ]
      metric.df = all_ofs[(all_ofs$Genotype=="WT"|all_ofs$Genotype=="CS") &
                            all_ofs$Type==category, ]
    }else{
      fly.info.temp = fly.info.end[fly.info.end$Genotype == g_list[i] &
                                     fly.info.end$Category == category , ]
      metric.df = all_ofs[all_ofs$Genotype==g_list[i] &
                            all_ofs$Type==category, ]
    }
    for (j in 1:nrow(fly.info.temp)){
      E1 = metric.df[metric.df$Fly.Number== fly.info.temp[j, ]$Fly &
                     metric.df$Experimenter == fly.info.temp[j, ]$Experimenter &
                     metric.df$Session=="E1", ][, metric.ind]
      E5 = metric.df[metric.df$Fly.Number== fly.info.temp[j, ]$Fly &
                     metric.df$Experimenter == fly.info.temp[j, ]$Experimenter &
                     metric.df$Session==paste0("E1", category, "1E1", category, "1E1"), ][, metric.ind]
      learn_index = as.numeric(E5) - as.numeric(E1)
      learn_list = c(learn_list, learn_index)
    }
    name_list = rep(g_list[i], length(learn_list))
    m = data.frame(
      factor = name_list,
      value = learn_list
    )
    colnames(m) = c("Genotype", "Learning")
    master_learn = rbind(master_learn, m)
  }
  master_learn$Genotype = factor(master_learn$Genotype, levels = g_list)
  return(master_learn)
}

get_query_info = function(query.genotype){
  if(query.genotype %in% c("CS", "WT")){
    query.fly = fly.info.end[((fly.info.end$Genotype == "WT") |
                                    (fly.info.end$Genotype == "CS")), ]$Fly
    query.experimenter = fly.info.end[((fly.info.end$Genotype == "WT") |
                                             (fly.info.end$Genotype == "CS")), ]$Experimenter
  }else{
    query.fly = fly.info.end[(fly.info.end$Genotype == query.genotype), ]$Fly
    query.experimenter = fly.info.end[(fly.info.end$Genotype == query.genotype), ]$Experimenter
    write.table(
      fly.info.end[(fly.info.end$Genotype == query.genotype), ],
      paste0("fly_info_include_",query.genotype,".csv"),
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
  }
  return(data.frame(query.fly, 
              query.experimenter))
}    

test_initial_condition = function(i, query.genotype, all_ofs){
  if (query.genotype == c("CS") || query.genotype == c("WT")){
    metric.df = all_ofs[all_ofs$Genotype=="WT", ]
  }else{
    metric.df = all_ofs[all_ofs$Genotype==query.genotype, ]
  }
  
  metric = data.frame(
    factor = c(rep("E1-T", length(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i])),
               rep("E1-R", length(metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i])),
               rep("E1-N", length(metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i]))),
    value = as.numeric(c(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i], 
                         metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i],
                         metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i]))
    )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c("E1-T", "E1-R", "E1-N"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return (a)
}

test_mid_training = function(i, query.genotype, all_ofs){
  if (query.genotype == c("CS") || query.genotype == c("WT")){
    metric.df = all_ofs[all_ofs$Genotype=="WT", ]
  }else{
    metric.df = all_ofs[all_ofs$Genotype==query.genotype, ]
  }
  metric = data.frame(
    factor = c(rep("E1T1E1", length(metric.df[metric.df$Session=="E1T1E1", ][, i])),
               rep("E1R1E1", length(metric.df[metric.df$Session=="E1R1E1", ][, i])),
               rep("E1N1E1", length(metric.df[metric.df$Session=="E1N1E1", ][, i]))),
    value = as.numeric(c(metric.df[metric.df$Session=="E1T1E1", ][, i],
                         metric.df[metric.df$Session=="E1R1E1", ][, i],
                         metric.df[metric.df$Session=="E1N1E1", ][, i]))
  )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c("E1T1E1", "E1R1E1", "E1N1E1"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return(a)
  
}

test_all = function(i, query.genotype, all_ofs){
  if (query.genotype == c("CS") || query.genotype == c("WT")){
    metric.df = all_ofs[all_ofs$Genotype=="WT" | all_ofs$Genotype=="CS", ]
  }else{
    metric.df = all_ofs[all_ofs$Genotype==query.genotype, ]
  }
  metric = data.frame(
    factor = c(rep("E1-T", length(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i])),
               rep("E1-R", length(metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i])),
               rep("E1-N", length(metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i])),
               rep("E1T1E1T1E1", length(metric.df[metric.df$Session=="E1T1E1T1E1", ][, i])),
               rep("E1R1E1R1E1", length(metric.df[metric.df$Session=="E1R1E1R1E1", ][, i])),
               rep("E1N1E1N1E1", length(metric.df[metric.df$Session=="E1N1E1N1E1", ][, i]))),
    value = as.numeric(c(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i], 
                         metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i],
                         metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i],
                         metric.df[metric.df$Session=="E1T1E1T1E1", ][, i],
                         metric.df[metric.df$Session=="E1R1E1R1E1", ][, i],
                         metric.df[metric.df$Session=="E1N1E1N1E1", ][, i]
                         ))
  )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c("E1-T", "E1-R", "E1-N", "E1T1E1T1E1", "E1R1E1R1E1", "E1N1E1N1E1"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return(a)
}

test_after_training = function(i, query.genotype, all_ofs){
  if (query.genotype == c("CS") || query.genotype == c("WT")){
    metric.df = all_ofs[all_ofs$Genotype=="WT", ]
  }else{
    metric.df = all_ofs[all_ofs$Genotype==query.genotype, ]
  }
  metric = data.frame(
  factor = c(rep("E1T1E1T1E1", length(metric.df[metric.df$Session=="E1T1E1T1E1", ][, i])),
             rep("E1R1E1R1E1", length(metric.df[metric.df$Session=="E1R1E1R1E1", ][, i])),
             rep("E1N1E1N1E1", length(metric.df[metric.df$Session=="E1N1E1N1E1", ][, i]))),
  value = as.numeric(c(metric.df[metric.df$Session=="E1T1E1T1E1", ][, i],
                       metric.df[metric.df$Session=="E1R1E1R1E1", ][, i],
                       metric.df[metric.df$Session=="E1N1E1N1E1", ][, i]))
  )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c("E1T1E1T1E1", "E1R1E1R1E1", "E1N1E1N1E1"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return(a)
}

test_3rd_training = function(i, query.genotype, all_ofs){
  
  if (query.genotype == c("CS") || query.genotype == c("WT")){
    metric.df = all_ofs[all_ofs$Genotype=="WT", ]
  }else{
    metric.df = all_ofs[all_ofs$Genotype==query.genotype, ]
  }
  metric = data.frame(
    factor = c(
      rep("E1T1E1T1E1T1E1", length(metric.df[metric.df$Session=="E1T1E1T1E1T1E1", ][, i])),
      rep("E1R1E1R1E1R1E1", length(metric.df[metric.df$Session=="E1R1E1R1E1R1E1", ][, i])),
      rep("E1N1E1N1E1N1E1", length(metric.df[metric.df$Session=="E1N1E1N1E1N1E1", ][, i]))),
    value = as.numeric(c(
      metric.df[metric.df$Session=="E1T1E1T1E1T1E1", ][, i],
      metric.df[metric.df$Session=="E1R1E1R1E1R1E1", ][, i],
      metric.df[metric.df$Session=="E1N1E1N1E1N1E1", ][, i]))
  )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c(
    "E1T1E1T1E1T1E1",
    "E1R1E1R1E1R1E1",
    "E1N1E1N1E1N1E1"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return(a)
  
}

test_4th_training = function(i, query.genotype, all_ofs){
  if (query.genotype == c("CS") || query.genotype == c("WT")){
    metric.df = all_ofs[all_ofs$Genotype=="WT", ]
  }else{
    metric.df = all_ofs[all_ofs$Genotype==query.genotype, ]
  }
  metric = data.frame(
    factor = c(
      rep("E1T1E1T1E1T1E1T1E1", length(metric.df[metric.df$Session=="E1T1E1T1E1T1E1T1E1", ][, i])),
      rep("E1R1E1R1E1R1E1R1E1", length(metric.df[metric.df$Session=="E1R1E1R1E1R1E1R1E1", ][, i])),
      rep("E1N1E1N1E1N1E1N1E1", length(metric.df[metric.df$Session=="E1N1E1N1E1N1E1N1E1", ][, i]))),
    value = as.numeric(c(
      metric.df[metric.df$Session=="E1T1E1T1E1T1E1T1E1", ][, i],
      metric.df[metric.df$Session=="E1R1E1R1E1R1E1R1E1", ][, i],
      metric.df[metric.df$Session=="E1N1E1N1E1N1E1N1E1", ][, i]))
  )
  colnames(metric) = c("Session", "Value")
  metric$Session = factor(metric$Session, levels=c(
    "E1T1E1T1E1T1E1T1E1",
    "E1R1E1R1E1R1E1R1E1", 
    "E1N1E1N1E1N1E1N1E1"))
  a = dunn.test(x = metric$Value, g = metric$Session, method = c("bonferroni"))
  return(a)
}

hypothesis_testing_E1 = function(i, fly.info, metrices = metrices){
  metric_name = metrices[i]
  
  E1_tests = data.frame()
  
  query.genotype = "WT"
  result = test_initial_condition(i, query.genotype)
  result = result$P.adjusted
  name_results = c(paste0("WT: ", "R-N"), paste0("WT: ", "T-N"), paste0("WT: ", "T-R"))
  E1_tests = data.frame(factor = name_results, value = as.numeric(result))
  
  genotype_list = c("SUN1", "SUN2", "SUN3", "JG17 x JU30", "R60D05 x JU30", "MB009B x JU30", "MB607B x JU30", "MB131B x JU30", "MB419B x JU30",  "UAS-DopR1-IR x 51635", 
                    "UAS-DopR2-RNAi x 51635", "CS x JU30", "MB419B x DopR1-IR", "JG17 x DopR1-IR", "MB009B x DopR1-IR", "R60D05 x DopR1-IR", "MB607B x DopR1-IR", "MB131B x DopR1-IR",
                    "108151 x 51636", "SUN1 x CS", "SUN2 x CS", "SUN3 x CS","Empty-Gal4 x JU30", "Empty-Gal4 x CS", "D2R-1 x 51635")
  
  for (j in genotype_list){
    query.genotype = j
    print(query.genotype)
    if (dim(all_ofs_mutants[all_ofs_mutants$Genotype==query.genotype,])[1] >= 20) {
    result = test_initial_condition(i, query.genotype)
    result = result$P.adjusted
    name_results =  c(paste0(query.genotype, ": R-N"), paste0(query.genotype, ": T-N"), paste0(query.genotype, ": T-R"))
    temp = data.frame(factor = name_results, value = as.numeric(result))
    E1_tests = rbind(E1_tests, temp)
    }else{next}
  }
  E1_tests = cbind(E1_tests, E1_tests$value<0.05)
  return(E1_tests)
}

hypothesis_testing_2ndE1 = function(i, fly.info, metrices = metrices){
  metric_name = metrices[i]
  E3_tests = data.frame()
  query.genotype = "WT"
  result = test_mid_training(i, query.genotype)
  result = result$P.adjusted
  name_results = c(paste0("WT: ", "R-N"), paste0("WT: ", "T-N"), paste0("WT: ", "T-R"))
  E3_tests = data.frame(factor = name_results, value = as.numeric(result))
  genotype_list = c("SUN1", "SUN2", "SUN3", "JG17 x JU30", "R60D05 x JU30", "MB009B x JU30", "MB607B x JU30", "MB131B x JU30", "MB419B x JU30",  "UAS-DopR1-IR x 51635", 
                    "UAS-DopR2-RNAi x 51635", "CS x JU30", "MB419B x DopR1-IR", "JG17 x DopR1-IR", "MB009B x DopR1-IR", "R60D05 x DopR1-IR", "MB607B x DopR1-IR", "MB131B x DopR1-IR",
                    "108151 x 51636",  "SUN1 x CS", "SUN2 x CS", "SUN3 x CS", "Empty-Gal4 x JU30", "Empty-Gal4 x CS", "D2R-1 x 51635")
  for (j in genotype_list){
    query.genotype = j
    print(query.genotype)
    if (dim(all_ofs_mutants[all_ofs_mutants$Genotype==query.genotype,])[1] >= 20) {
      result = test_mid_training(i, query.genotype)
      result = result$P.adjusted
      name_results =  c(paste0(query.genotype, ": R-N"), paste0(query.genotype, ": T-N"), paste0(query.genotype, ": T-R"))
      temp = data.frame(factor = name_results, value = as.numeric(result))
      E3_tests = rbind(E3_tests, temp)
    }else{next}
  }
  E3_tests = cbind(E3_tests, E3_tests$value<0.05)
  return(E3_tests)
}

hypothesis_testing_3rdE1 = function(i, fly.info, metrices = metrices){
  metric_name = metrices[i]
  E5_tests = data.frame()
  query.genotype = "WT"
  result = test_after_training(i, query.genotype)
  result = result$P.adjusted
  name_results = c(paste0("WT: ", "R-N"), paste0("WT: ", "T-N"), paste0("WT: ", "T-R"))
  E5_tests = data.frame(factor = name_results, value = as.numeric(result))
  genotype_list = c("SUN1", "SUN2", "SUN3", "JG17 x JU30", "R60D05 x JU30", "MB009B x JU30", "MB607B x JU30", "MB131B x JU30", "MB419B x JU30",  "UAS-DopR1-IR x 51635", 
                    "UAS-DopR2-RNAi x 51635", "CS x JU30", "MB419B x DopR1-IR", "JG17 x DopR1-IR", "MB009B x DopR1-IR", "R60D05 x DopR1-IR", "MB607B x DopR1-IR", "MB131B x DopR1-IR",
                    "108151 x 51636",  "SUN1 x CS", "SUN2 x CS", "SUN3 x CS", "Empty-Gal4 x JU30", "Empty-Gal4 x CS", "D2R-1 x 51635")
  for (j in genotype_list){
    query.genotype = j
    print(query.genotype)
    if (dim(all_ofs_mutants[all_ofs_mutants$Genotype==query.genotype,])[1] >= 20) {
      result = test_after_training(i, query.genotype)
      result = result$P.adjusted
      name_results =  c(paste0(query.genotype, ": R-N"), paste0(query.genotype, ": T-N"), paste0(query.genotype, ": T-R"))
      temp = data.frame(factor = name_results, value = as.numeric(result))
      E5_tests = rbind(E5_tests, temp)
    }else{next}
  }
  E5_tests = cbind(E5_tests, E5_tests$value<0.05)
  return(E5_tests)
}

# Plot changes before and after training with the gap data taken into account
plot_gap = function(fly.info.end, all_ofs,#remember to use all_ofs_wT
                    metric.ind,  #8
                    gap #2
                    ){
  gap_flies = fly.info.end[fly.info.end$Gap==gap, ]
  m = data.frame(
    factor = c(rep("E1-T", length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1", ][, metric.ind])),
               rep("E1-R", length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1", ][, metric.ind])),
               rep("E1-N", length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" , ][, metric.ind])),
               rep("E1T1E1", length(all_ofs[all_ofs$Session=="E1T1E1" , ][, metric.ind])),
               rep("E1R1E1", length(all_ofs[all_ofs$Session=="E1R1E1", ][, metric.ind])),
               rep("E1N1E1", length(all_ofs[all_ofs$Session=="E1N1E1", ][, metric.ind])),
               rep("E1T1E1T1E1", length(all_ofs[all_ofs$Session=="E1T1E1T1E1", ][, metric.ind])),
               rep("E1R1E1R1E1", length(all_ofs[all_ofs$Session=="E1R1E1R1E1", ][, metric.ind])),
               rep("E1N1E1N1E1", length(all_ofs[all_ofs$Session=="E1N1E1N1E1", ][, metric.ind])),
               rep("E2-T", length(all_ofs[all_ofs$Session=="E2" & all_ofs$Type=="T" &
                                            all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2-R", length(all_ofs[all_ofs$Session=="E2" & all_ofs$Type=="R" &
                                            all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2-N", length(all_ofs[all_ofs$Session=="E2" & all_ofs$Type=="N" &
                                            all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2T2E2", length(all_ofs[all_ofs$Session=="E2T2E2" &
                                              all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2R2E2", length(all_ofs[all_ofs$Session=="E2R2E2" &
                                              all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2N2E2", length(all_ofs[all_ofs$Session=="E2N2E2" &
                                              all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2T2E2T2E2", length(all_ofs[all_ofs$Session=="E2T2E2T2E2" &
                                                  all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2R2E2R2E2", length(all_ofs[all_ofs$Session=="E2R2E2R2E2" &
                                                  all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind])),
               rep("E2N2E2N2E2", length(all_ofs[all_ofs$Session=="E2N2E2N2E2" &
                                                  all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind]))
    ),
    value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1", ][, metric.ind], 
                         all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1", ][, metric.ind],
                         all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1", ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1T1E1", ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1R1E1", ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1N1E1", ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1T1E1T1E1", ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1R1E1R1E1", ][, metric.ind],
                         all_ofs[all_ofs$Session=="E1N1E1N1E1", ][, metric.ind],
                         all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind], 
                         all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Session=="E2T2E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Session=="E2R2E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Session=="E2N2E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Session=="E2T2E2T2E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Session=="E2R2E2R2E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind],
                         all_ofs[all_ofs$Session=="E2N2E2N2E2" &
                                   all_ofs$`Fly Number`%in% gap_flies$Fly, ][, metric.ind]
    ))
  )
  colnames(m) = c("Session", "Value")
  m$Session = factor(m$Session, levels=c("E1-T", "E1-R", "E1-N",
                                         "E1T1E1", "E1R1E1", "E1N1E1",
                                         "E1T1E1T1E1", "E1R1E1R1E1", "E1N1E1N1E1",
                                         "E2-T", "E2-R", "E2-N",
                                         "E2T2E2", "E2R2E2", "E2N2E2",
                                         "E2T2E2T2E2", "E2R2E2R2E2", "E2N2E2N2E2"))
  num = as.data.frame(table(m[!is.na(m$Value),]$Session))$Freq
  pdf(paste0("Gap", fly.info.gap$Gap[1], "_", Sys.Date(), ".pdf"),
      onefile = T, width = 8
  )
  yrange = c(0, 1)
  y_text = 1
  col.pool = c(
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80"
  )
  boxplot(
    Value ~ Session,
    data = m,
    ylim = yrange,
    outline = F,
    notch = F,
    lwd = 1,
    ylab = "",
    xlab = "",
    medlwd = 1,
    # boxwex = 1,
    xaxt = "n"
  )
  stripchart(
    Value ~ Session,
    vertical = TRUE,
    data = m,
    method = "jitter",
    add = TRUE,
    pch = 15,
    cex = 0.5,
    col =  col.pool
  )
  dev.off()
  
}

# Plotting three test sessions' raw data
plot_WT = function(all_ofs, genotype, i){
  if (genotype == "WT"){
    df = all_ofs[all_ofs$Genotype=="WT" | all_ofs$Genotype=="CS", ]
  }else{
    df = all_ofs[all_ofs$Genotype==genotype, ]
  }
  m = data.frame(
    factor = c(rep("E1-T", length(df[df$Type=="T" & df$Session=="E1", ][, i])),
               rep("E1-R", length(df[df$Type=="R" & df$Session=="E1", ][, i])),
               rep("E1-N", length(df[df$Type=="N" & df$Session=="E1", ][, i])),
               rep("E1T1E1", length(df[df$Session=="E1T1E1", ][, i])),
               rep("E1R1E1", length(df[df$Session=="E1R1E1", ][, i])),
               rep("E1N1E1", length(df[df$Session=="E1N1E1", ][, i])),
               rep("E1T1E1T1E1", length(df[df$Session=="E1T1E1T1E1", ][, i])),
               rep("E1R1E1R1E1", length(df[df$Session=="E1R1E1R1E1", ][, i])),
               rep("E1N1E1N1E1", length(df[df$Session=="E1N1E1N1E1", ][, i]))
               
    ),
    value = as.numeric(c(df[df$Type=="T" & df$Session=="E1", ][, i], 
                         df[df$Type=="R" & df$Session=="E1", ][, i],
                         df[df$Type=="N" & df$Session=="E1", ][, i],
                         df[df$Session=="E1T1E1", ][, i],
                         df[df$Session=="E1R1E1", ][, i],
                         df[df$Session=="E1N1E1", ][, i],
                         df[df$Session=="E1T1E1T1E1", ][, i],
                         df[df$Session=="E1R1E1R1E1", ][, i],
                         df[df$Session=="E1N1E1N1E1", ][, i]
                         
    ))
  )
  colnames(m) = c("Session", "Value")
  m$Session = factor(m$Session, levels=c("E1-T", "E1-R", "E1-N",
                                         "E1T1E1", "E1R1E1", "E1N1E1",
                                         "E1T1E1T1E1", "E1R1E1R1E1", "E1N1E1N1E1"
  ))
  s =  c("Pre-test", "Test 1", "Test 2")
  col.pool = c(
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80",
    "indianred3",
    "light blue",
    "grey80"
  )
  a = test_initial_condition(i, genotype, all_ofs)
  b = test_mid_training(i, genotype, all_ofs)
  c = test_after_training(i, genotype, all_ofs)
  p = c(a$P.adjusted, b$P.adjusted, c$P.adjusted)
  num = as.data.frame(table(m[!is.na(m$Value),]$Session))$Freq
  pdf(paste0(genotype, "_", i, "_", Sys.Date(), ".pdf"), onefile = T, width = 8)
  if (i == 9){
    yrange = c(0, 1)
    y_text = 1
  }else if (i == 40){
    yrange = c(0, 120)
    y_text = 120
  }else if (i == 18){
    yrange = c(0, 20)
    y_text = 20
  }else if (i == 7){
    yrange = c(0, 200)
    y_text = 200
  }
  boxplot(
    Value ~ Session,
    data = m,
    ylim = yrange,
    outline = F,
    notch = F,
    lwd = 1,
    ylab = "",
    xlab = "",
    medlwd = 1,
    xaxt = "n",
    axes=F
  )
  if (i == 9){
    axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
    text(x = c(0.8, 1.8, 2.8, 9.85),
         y = 1.1,
         # y_text,
         c(num[1:3], num[10]),
         xpd = T,
         srt = 0,
         adj = 0
    )
    y_top_base = 1
    vertical_gap = 0.01
    v_gap = 0.005
  }
  else if (i == 40){
    axis(side=2, at=c(0, 20, 40, 60, 80, 100, 120))
    text(x = c(0.8, 1.8, 2.8, 9.85),
         y = 125,
         c(num[1:3], num[10]),
         xpd = T,
         srt = 0,
         adj = 0
    )
    y_top_base = 120
    vertical_gap = 2
    v_gap = 1
  }else if (i == 18){
    axis(side=2, at=c(0, 2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
    text(x = c(0.8, 1.8, 2.8, 9.85),
         y = 22,
         # y_text,
         c(num[1:3], num[10]),
         xpd = T,
         srt = 0,
         adj = 0
    )
    y_top_base = 20
    vertical_gap = 0.2
    v_gap = 0.1
  }else if (i == 7){
    axis(side=2, at=c(0, 40, 80,  120, 160, 200))
    text(x = c(0.8, 1.8, 2.8, 9.85),
         y = 210,
         # y_text,
         c(num[1:3], num[10]),
         xpd = T,
         srt = 0,
         adj = 0
    )
    y_top_base = 200
    vertical_gap = 2
    v_gap = 1
  }
  stripchart(
    Value ~ Session,
    vertical = TRUE,
    data = m,
    method = "jitter",
    add = TRUE,
    pch = 15,
    cex = 0.5,
    col =  col.pool
  )
  y_base_RN = y_top_base - 2.8 * vertical_gap
  y_base_TN = y_top_base - 5.6 * vertical_gap
  y_base_TR = y_top_base
  for (i in 1:length(p)){
    if (p[i] >= 0.05){
      significance = "n.s."
    }else if (p[i] < 0.05 & p[i] >= 0.01){
      significance = "*"
    }else if (p[i] < 0.01 & p[i] >= 0.001){
      significance = "**"
    }else if (p[i] < 0.001 & p[i] >= 0.0001){
      significance = "***"
    }else if (p[i] < 0.0001){
      significance = "****"
    }
    if (i%%3==0){
      text(((i - 2) + (i - 1))/2, y_base_TR + vertical_gap, significance, xpd = NA)
      lines(c(i - 2, i - 1), 
            c(y_base_TR,  y_base_TR), 
            xpd = NA)
      lines(c(i - 1, i - 1), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      lines(c(i - 2, i - 2), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      
    }
  }
  seq_for_lines = seq(3, 9, by=3)
  for (j in seq_for_lines) {
    lines(c(j, j) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "light grey",
          lty = 1)
  }
  text(x = c(1.5, 4, 7),
       y = -0.1,
       s,
       xpd = T,
       srt = 0,
       adj = 0
  )
  dev.off()
}

# Plotting all metrices of a given genotype
plot_all_raw_metrics = function(query.genotype, query.fly, query.experimenter, fly.info.end, metrices = metrices){
  if(query.genotype[1] == "WT"){
    write.table(
      fly.info.end[((fly.info.end$Genotype == "WT") |
                      (fly.info.end$Genotype == "CS")), ],
      "fly_info_include_WT.csv",
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
    metric.df = read.table("all_ofs_WT.csv", stringsAsFactors = F, sep = ',', header = T)
  }else{
    write.table(
      fly.info.end[((fly.info.end$Genotype == query.genotype[1])),],
      paste0("fly_info_include_", query.genotype[1], ".csv"),
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
    metric.df = read.table("all_ofs_mutants.csv", stringsAsFactors = F, sep = ',', header = T)
    metric.df = metric.df[metric.df$Genotype==query.genotype[1], ]
  }
  pdf(paste0("all_metric_", query.genotype[1], "_", Sys.Date(), ".pdf"),
      onefile = T, width = 8
  )
  
  for (i in 7:length(metrices)) {
    metric = data.frame(
      factor = c(rep("E1-T", length(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i])),
                 rep("E1-R", length(metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i])),
                 rep("E1-N", length(metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i])),
                 rep("E1T1E1", length(metric.df[metric.df$Session=="E1T1E1", ][, i])),
                 rep("E1R1E1", length(metric.df[metric.df$Session=="E1R1E1", ][, i])),
                 rep("E1N1E1", length(metric.df[metric.df$Session=="E1N1E1", ][, i])),
                 rep("E1T1E1T1E1", length(metric.df[metric.df$Session=="E1T1E1T1E1", ][, i])),
                 rep("E1R1E1R1E1", length(metric.df[metric.df$Session=="E1R1E1R1E1", ][, i])),
                 rep("E1N1E1N1E1", length(metric.df[metric.df$Session=="E1N1E1N1E1", ][, i]))),
      value = as.numeric(c(metric.df[metric.df$Type=="T" & metric.df$Session=="E1", ][, i], 
                           metric.df[metric.df$Type=="R" & metric.df$Session=="E1", ][, i],
                           metric.df[metric.df$Type=="N" & metric.df$Session=="E1", ][, i],
                           metric.df[metric.df$Session=="E1T1E1", ][, i],
                           metric.df[metric.df$Session=="E1R1E1", ][, i],
                           metric.df[metric.df$Session=="E1N1E1", ][, i],
                           metric.df[metric.df$Session=="E1T1E1T1E1", ][, i],
                           metric.df[metric.df$Session=="E1R1E1R1E1", ][, i],
                           metric.df[metric.df$Session=="E1N1E1N1E1", ][, i]))
    )
    colnames(metric) = c("Session", "Value")
    metric$Session = factor(metric$Session, levels=c("E1-T", "E1-R", "E1-N",
                                                     "E1T1E1", "E1R1E1", "E1N1E1",
                                                     "E1T1E1T1E1", "E1R1E1R1E1", "E1N1E1N1E1"))
    num = as.data.frame(table(metric[!is.na(metric$Value),]$Session))$Freq
    
    col.pool = c(
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80",
      "indianred3",
      "light blue",
      "grey80"
    )
    boxplot(
      Value ~ Session,
      data = metric,
      # ylim = yrange,
      outline = F,
      notch = F,
      lwd = 1,
      ylab = metrices[i],
      xlab = "",
      medlwd = 1,
      # boxwex = 1,
      xaxt = "n"
    )
    stripchart(
      Value ~ Session,
      vertical = TRUE,
      data = metric,
      method = "jitter",
      add = TRUE,
      pch = 15,
      cex = 0.5,
      col =  col.pool
    )
    if (any(is.na(yrange)) |
        any(is.infinite(yrange)) | any(is.nan(yrange))) {
      yrange = c(-1, 1)
      ylim = c(-1, 1)
    }
    text(x = (1:length(num)) - 0.1,
         y = y_text,
         # y_text,
         num,
         xpd = T,
         srt = 0,
         adj = 0
    )
    for (j in c(3, 6)) {
      lines(c(j, j) + 0.5,
            c(yrange[1] - 1e3, yrange[1] + 1e3),
            col = "light grey",
            lty = 1)
    }
  }
  dev.off()
}

# Plot only the initial and test 2 behavioral data and test all pairwise combinations
plot_single = function(genotype, metric.ind, all_ofs, fly.info.end){
  g_list = genotype
  if (g_list == "WT"){
    all_ofs = all_ofs[all_ofs$Genotype == "WT" | all_ofs$Genotype == "CS", ]
    fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                          (fly.info.end$Genotype == "CS")) & 
                                         (fly.info.end$Category =="T"), ]
    fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                          (fly.info.end$Genotype == "CS")) & 
                                         (fly.info.end$Category == "R") , ]
    fly.info.movement.N = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                          (fly.info.end$Genotype == "CS")) & 
                                         (fly.info.end$Category == "N") , ]
    R1 = Hit_by_laser("E1R1", fly.info.movement.R)
    T1 = Hit_by_laser("E1T1", fly.info.movement.T)
    N1 = Hit_by_laser("E1N1", fly.info.movement.N)
    RT = rbind(R1, T1, N1)
    RT.include = RT[!is.na(RT$Hit_W), ]
    RT.exclude = RT[is.na(RT$Hit_W), ]
    temp = data.frame()
    for (i in 1:nrow(RT.include)){
      temp = rbind(temp, 
                   all_ofs[all_ofs$Fly.Number == RT.include[i, ]$Fly & 
                             all_ofs$Experimenter==RT.include[i, ]$Experimenter &
                             all_ofs$Genotype == RT.include[i, ]$Genotype, ])
    }
    all_ofs = temp
  }
  pdf(paste0("Single_", genotype, "_", Sys.Date(), ".pdf"),
      onefile = T, width = 8
  )
  p = c()
  metric.df = data.frame()
  #Prep data
  if (g_list == "WT"){
    num = c(num,
            length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1", ][, metric.ind]),
            length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1", ][, metric.ind]),
            length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1", ][, metric.ind])
    )
    m = data.frame(
      factor = c(rep("E1_T_WT", length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1", ][, metric.ind])),
                 rep("E1_R_WT", length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1", ][, metric.ind])),
                 rep("E1_N_WT", length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1", ][, metric.ind])),
                 rep("E5_T_WT", length(all_ofs[all_ofs$Session=="E1T1E1T1E1", ][, metric.ind])),
                 rep("E5_R_WT", length(all_ofs[all_ofs$Session=="E1R1E1R1E1", ][, metric.ind])),
                 rep("E5_N_WT", length(all_ofs[all_ofs$Session=="E1N1E1N1E1", ][, metric.ind]))
      ),
      value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" , ][, metric.ind],
                           all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" , ][, metric.ind],
                           all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" , ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1T1E1T1E1" , ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1R1E1R1E1" , ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1N1E1N1E1", ][, metric.ind]
      )
      )
    )
    colnames(m) = c("Session", "Value")
    m$Session = factor(m$Session, levels=c("E1_T_WT", "E1_R_WT", "E1_N_WT",
                                           "E5_T_WT", "E5_R_WT", "E5_N_WT"))
    a = test_initial_condition(metric.ind, g_list, all_ofs)
    b = test_after_training(metric.ind, g_list, all_ofs)
    
    # comp = test_all(metric.ind, g_list[i], all_ofs)
    # p = c(p, a$P.adjusted, b$P.adjusted)
    # p = c(comp$P.adjusted)
    comp = test_all(metric.ind, g_list, all_ofs)
    p = c(comp$P.adjusted)
    metric.df = rbind(metric.df, m)
  }else{
    for (i in 1:length(g_list)){
      num = c(num,
              length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]),
              length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]),
              length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])
      )
      m = data.frame(
        factor = c(rep(paste0("E1_T_", g_list[i]), length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E1_R_", g_list[i]), length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E1_N_", g_list[i]), length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E5_T_", g_list[i]), length(all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E5_R_", g_list[i]), length(all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E5_N_", g_list[i]), length(all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]))
        ),
        value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]
        )
        )
      )
      colnames(m) = c("Session", "Value")
      m$Session = factor(m$Session, levels=c(paste0("E1_T_", g_list[i]), paste0("E1_R_",g_list[i]), paste0("E1_N_", g_list[i]),
                                             paste0("E5_T_", g_list[i]), paste0("E5_R_",g_list[i]), paste0("E5_N_", g_list[i])))
      a = test_initial_condition(metric.ind, g_list[i], all_ofs)
      b = test_after_training(metric.ind, g_list[i], all_ofs)
      comp = test_all(metric.ind, g_list[i], all_ofs)
      p = c(comp$P.adjusted)
      
      metric.df = rbind(metric.df, m)
    }                           
  }
  yrange = c(0, 1)
  y_text = 1.1
  
  col.pool = rep(c("indianred3", "light blue", "grey80"), length(g_list) * 2)
  
  boxplot(
    Value ~ Session,
    data = metric.df,
    ylim = yrange,
    outline = F,
    notch = F,
    lwd = 1,
    ylab = "",
    xlab = "",
    medlwd = 1,
    xaxt = "n",
    axes=F
  )
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
  
  stripchart(
    Value ~ Session,
    vertical = TRUE,
    data = metric.df,
    method = "jitter",
    add = TRUE,
    pch = 15,
    cex = 0.5,
    col =  col.pool
  )
  
  y_top_base = 1
  vertical_gap = 0.01
  v_gap = 0.01
  y_base_RN = y_top_base - 2.8 * vertical_gap
  y_base_TN = y_top_base - 5.6 * vertical_gap
  y_base_TR = y_top_base
  
  
  for (i in 1:length(p)){
    if (p[i] >= 0.05){
      significance = "n.s."
    }else if (p[i] < 0.05 & p[i] >= 0.01){
      significance = "*"
    }else if (p[i] < 0.01 & p[i] >= 0.001){
      significance = "**"
    }else if (p[i] < 0.001 & p[i] >= 0.0001){
      significance = "***"
    }else if (p[i] < 0.0001){
      significance = "****"
    }
    if (i == 3){
      text(
        1.5, 
        y_base_TR + vertical_gap, 
        significance, 
        xpd = NA)
      lines(c(1, 2), 
            c(y_base_TR,  y_base_TR), 
            xpd = NA)
      lines(c(1, 1), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      lines(c(2, 2), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      
    }else if (i == 15){
      text(
        4.5, 
        y_base_TR + vertical_gap, 
        significance, 
        xpd = NA)
      lines(c(4, 5), 
            c(y_base_TR,  y_base_TR), 
            xpd = NA)
      lines(c(4, 4), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      lines(c(5, 5), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      
    }
    
    else if(i == 13){
      text(
        c(2.5),
        y_base_TR + vertical_gap*15,
        significance,
        xpd = NA)
      lines(c(1, 4),
            c(y_base_TR + vertical_gap*13,  y_base_TR + vertical_gap*13),
            xpd = NA)
      lines(c(1, 1), c(y_base_TR + vertical_gap*13, y_base_TR + vertical_gap*13 - v_gap), xpd = NA)
      lines(c(4, 4), c(y_base_TR + vertical_gap*13, y_base_TR + vertical_gap*13 - v_gap), xpd = NA)
    }
    else if (i == 8){
      text(
        c(3.5),
        y_base_TR + vertical_gap*10,
        significance,
        xpd = NA)
      lines(c(2, 5),
            c(y_base_TR+ vertical_gap*8,  y_base_TR+ vertical_gap*8),
            xpd = NA)
      lines(c(2, 2), c(y_base_TR+ vertical_gap*8, y_base_TR+ vertical_gap*8 - v_gap), xpd = NA)
      lines(c(5, 5), c(y_base_TR+ vertical_gap*8, y_base_TR+ vertical_gap*8 - v_gap), xpd = NA)
    }
    
  }
  
  text(x = seq(0.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(1, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  text(x = seq(1.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(2, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  text(x = seq(2.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(3, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  seq_for_lines = seq(3, (length(g_list)*6), by=6)
  for (j in seq_for_lines) {
    lines(c(j, j) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "light grey",
          lty = 1)
  }
  
  dev.off()
}

# Plot multiple relevant genotypes' data together
plot_comparison = function(genotype, metric.ind, all_ofs){
  if (genotype == "WT"){
    g_list = c("WT")
  }else if (genotype == "R60D05 x JU30"){
    g_list = c("R60D05 x JU30",
               "JG17 x JU30",
               "Empty-Gal4 x CS",
               "CS x JU30")
  }else if(genotype == "JG17 x JU30"){
    g_list = c("JG17 x JU30",
               "Empty-Gal4 x CS",
               "CS x JU30"
    )
  }else if(genotype == "SUN1"){
    g_list = c(
      # "SUN1",
      #  "SUN1 x CS"
      "SUN3",
      "SUN2"
      # "WT"
    )
  }else if(genotype == "SUN2"){
    g_list = c("SUN2",
               "SUN2 x CS"
               # "WT"
    )
  }else if(genotype == "SUN3"){
    g_list = c("SUN3",
               "SUN3 x CS"
               # "WT"
    )
  }else if(genotype == "MB009B x JU30"){
    g_list = c("MB009B x JU30",
               "MB131B x JU30",
               "MB419B x JU30",
               "MB607B x JU30",
               "Empty-Gal4 x CS",
               "CS x JU30")
  }else if(genotype == "MB419B x JU30"){
    g_list = c( "MB419B x JU30",
                "MB607B x JU30",
                "Empty-Gal4 x CS",
                "CS x JU30")
  }else if(genotype == "UAS-DopR1-IR x 51635"){
    g_list = c("SUN1",
               "UAS-DopR1-IR x 51635",
               "MB009B x DopR1-IR",
               "MB131B x DopR1-IR",
               "MB419B x DopR1-IR",
               "MB607B x DopR1-IR",
               "R60D05 x DopR1-IR",
               "JG17 x DopR1-IR"
    )
    
  }else if(genotype == "R60D05 x DopR1-IR"){
    g_list = c("UAS-DopR1-IR x 51635",
               "R60D05 x DopR1-IR",
               "JG17 x DopR1-IR"
    )
    
  }else if(genotype == "CS x PKCi"){
    g_list = c("CS x PKCi",
               "MB009B x PKCi",
               "MB131B x PKCi",
               "MB419B x PKCi",
               "MB607B x PKCi",
               "R60D05 x PKCi",
               "JG17 x PKCi")
  }else if(genotype ==  "THGAL4 x JU29"){
    g_list = c("D2R-1 x 51635",
               "DopR2 x JU30",
               "DopR1 x JU30",
               "THGAL4 x JU29",
               "THGAL4 x JU30",
               "OK107 x JU30",
               "UAS-DopR2-RNAi x 51635",
               "UAS-DopR1-IR x 51635")
  }
  pdf(paste0("Comparison_", genotype, "_", Sys.Date(), ".pdf"),
      onefile = T, width = 8
  )
  p = c()
  metric.df = data.frame()
  #Prep data
  num = c()
  for (i in 1:length(g_list)){
    num = c(num, 
            length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]),
            length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]),
            length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])
    )
    m = data.frame(
      factor = c(rep(paste0("E1_T_", g_list[i]), length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                 rep(paste0("E1_R_", g_list[i]), length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                 rep(paste0("E1_N_", g_list[i]), length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                 rep(paste0("E5_T_", g_list[i]), length(all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                 rep(paste0("E5_R_", g_list[i]), length(all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                 rep(paste0("E5_N_", g_list[i]), length(all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]))
      ),
      value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind], 
                           all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                           all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind], 
                           all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]
      )
      )
    )
    colnames(m) = c("Session", "Value")
    m$Session = factor(m$Session, levels=c(paste0("E1_T_", g_list[i]), paste0("E1_R_",g_list[i]), paste0("E1_N_", g_list[i]), 
                                           paste0("E5_T_", g_list[i]), paste0("E5_R_",g_list[i]), paste0("E5_N_", g_list[i])))
    a = test_initial_condition(metric.ind, g_list[i], all_ofs)
    b = test_after_training(metric.ind, g_list[i], all_ofs)
    p = c(p, a$P.adjusted, b$P.adjusted)
    metric.df = rbind(metric.df, m)
  }                         
  yrange = c(0, 1)
  y_text = 1.1
  
  col.pool = rep(c("indianred3", "light blue", "grey80"), length(g_list) * 2)
  
  boxplot(
    Value ~ Session,
    data = metric.df,
    ylim = yrange,
    outline = F,
    notch = F,
    lwd = 1,
    ylab = "",
    xlab = "",
    medlwd = 1,
    xaxt = "n",
    axes=F
  )
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
  
  stripchart(
    Value ~ Session,
    vertical = TRUE,
    data = metric.df,
    method = "jitter",
    add = TRUE,
    pch = 15,
    cex = 0.5,
    col =  col.pool
  )
  y_top_base = 1
  vertical_gap = 0.01
  v_gap = 0.01
  y_base_RN = y_top_base - 2.8 * vertical_gap
  y_base_TN = y_top_base - 5.6 * vertical_gap
  y_base_TR = y_top_base
  
  for (i in 1:length(p)){
    if (p[i] >= 0.05){
      significance = "n.s."
    }else if (p[i] < 0.05 & p[i] >= 0.01){
      significance = "*"
    }else if (p[i] < 0.01 & p[i] >= 0.001){
      significance = "**"
    }else if (p[i] < 0.001 & p[i] >= 0.0001){
      significance = "***"
    }else if (p[i] < 0.0001){
      significance = "****"
    }
    if (i%%3==0){
      text(((i - 2) + (i - 1))/2, y_base_TR + vertical_gap, significance, xpd = NA)
      lines(c(i - 2, i - 1), 
            c(y_base_TR,  y_base_TR), 
            xpd = NA)
      lines(c(i - 1, i - 1), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      lines(c(i - 2, i - 2), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
    }
  }
  
  text(x = seq(0.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(1, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  text(x = seq(1.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(2, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  text(x = seq(2.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(3, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  text(x = seq(2.5, (length(g_list)*6) - 3.5, by= 6),
       y = -0.1,
       g_list,
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  seq_for_lines = seq(3, (length(g_list)*6), by=6)
  for (j in seq_for_lines) {
    lines(c(j, j) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "light grey",
          lty = 1)
  }
  if (genotype != "WT"){
    seq_for_lines_2 = seq(6, (length(g_list)*6) - 6, by=6)
    for (j in seq_for_lines_2) {
      lines(c(j, j) + 0.5,
            c(yrange[1] - 1e3, yrange[1] + 1e3),
            col = "black",
            lty = 1,
            lwd = 2)
    }
  }
  
  dev.off()
}

# Plot only the initial and test 2 behavioral data and test only T-R pairs
plot_single_15 = function(genotype, metric.ind, all_ofs, fly.info.end){
  g_list = genotype
  if (g_list == "WT"){
    all_ofs = all_ofs[all_ofs$Genotype == "WT" | all_ofs$Genotype == "CS", ]
    fly.info.movement.T = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                          (fly.info.end$Genotype == "CS")) & 
                                         (fly.info.end$Category =="T"), ]
    fly.info.movement.R = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                          (fly.info.end$Genotype == "CS")) & 
                                         (fly.info.end$Category == "R") , ]
    fly.info.movement.N = fly.info.end[((fly.info.end$Genotype == "WT") | 
                                          (fly.info.end$Genotype == "CS")) & 
                                         (fly.info.end$Category == "N") , ]
    R1 = Hit_by_laser("E1R1", fly.info.movement.R)
    T1 = Hit_by_laser("E1T1", fly.info.movement.T)
    N1 = Hit_by_laser("E1N1", fly.info.movement.N)
    RT = rbind(R1, T1, N1)
    RT.include = RT[!is.na(RT$Hit_W), ]
    RT.exclude = RT[is.na(RT$Hit_W), ]
    temp = data.frame()
    for (i in 1:nrow(RT.include)){
      temp = rbind(temp, 
                   all_ofs[all_ofs$Fly.Number == RT.include[i, ]$Fly & 
                             all_ofs$Experimenter == RT.include[i, ]$Experimenter &
                             all_ofs$Genotype == RT.include[i, ]$Genotype,])
    }
    all_ofs = temp
  }
  
  pdf(paste0("15_", genotype, "_", Sys.Date(), ".pdf"),
      onefile = T, width = 8
  )
  p = c()
  metric.df = data.frame()
  
  #Prep data
  num = c()
  if (g_list == "WT"){
    num = c(num, 
            length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1", ][, metric.ind]),
            length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1", ][, metric.ind]),
            length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1", ][, metric.ind])
    )
    m = data.frame(
      factor = c(rep("E1_T_WT", length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1", ][, metric.ind])),
                 rep("E1_R_WT", length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1", ][, metric.ind])),
                 rep("E1_N_WT", length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1", ][, metric.ind])),
                 rep("E5_T_WT", length(all_ofs[all_ofs$Session=="E1T1E1T1E1", ][, metric.ind])),
                 rep("E5_R_WT", length(all_ofs[all_ofs$Session=="E1R1E1R1E1", ][, metric.ind])),
                 rep("E5_N_WT", length(all_ofs[all_ofs$Session=="E1N1E1N1E1", ][, metric.ind]))
      ),
      value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" , ][, metric.ind], 
                           all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" , ][, metric.ind],
                           all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" , ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1T1E1T1E1" , ][, metric.ind], 
                           all_ofs[all_ofs$Session=="E1R1E1R1E1" , ][, metric.ind],
                           all_ofs[all_ofs$Session=="E1N1E1N1E1", ][, metric.ind]
      )
      )
    )
    colnames(m) = c("Session", "Value")
    m$Session = factor(m$Session, levels=c("E1_T_WT", "E1_R_WT", "E1_N_WT", 
                                           "E5_T_WT", "E5_R_WT", "E5_N_WT"))
    a = test_initial_condition(metric.ind, g_list, all_ofs)
    b = test_after_training(metric.ind, g_list, all_ofs)
    
    # comp = test_all(metric.ind, g_list[i], all_ofs)
    p = c(p, a$P.adjusted, b$P.adjusted)
    # p = c(comp$P.adjusted)
    metric.df = rbind(metric.df, m)
  }else{
    for (i in 1:length(g_list)){
      num = c(num, 
              length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]),
              length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]),
              length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])
      )
      m = data.frame(
        factor = c(rep(paste0("E1_T_", g_list[i]), length(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E1_R_", g_list[i]), length(all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E1_N_", g_list[i]), length(all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E5_T_", g_list[i]), length(all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E5_R_", g_list[i]), length(all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind])),
                   rep(paste0("E5_N_", g_list[i]), length(all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]))
        ),
        value = as.numeric(c(all_ofs[all_ofs$Type=="T" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind], 
                             all_ofs[all_ofs$Type=="R" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Type=="N" & all_ofs$Session=="E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Session=="E1T1E1T1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind], 
                             all_ofs[all_ofs$Session=="E1R1E1R1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind],
                             all_ofs[all_ofs$Session=="E1N1E1N1E1" & all_ofs$Genotype==g_list[i], ][, metric.ind]
        )
        )
      )
      colnames(m) = c("Session", "Value")
      m$Session = factor(m$Session, levels=c(paste0("E1_T_", g_list[i]), paste0("E1_R_",g_list[i]), paste0("E1_N_", g_list[i]), 
                                             paste0("E5_T_", g_list[i]), paste0("E5_R_",g_list[i]), paste0("E5_N_", g_list[i])))
      a = test_initial_condition(metric.ind, g_list[i], all_ofs)
      b = test_after_training(metric.ind, g_list[i], all_ofs)
      
      # comp = test_all(metric.ind, g_list[i], all_ofs)
      p = c(p, a$P.adjusted, b$P.adjusted)
      # p = c(comp$P.adjusted)
      metric.df = rbind(metric.df, m)
    }}                         
  yrange = c(0, 1)
  y_text = 1.1
  
  col.pool = rep(c("indianred3", "light blue", "grey80"), length(g_list) * 2)
  
  boxplot(
    Value ~ Session,
    data = metric.df,
    ylim = yrange,
    outline = F,
    notch = F,
    lwd = 1,
    ylab = "",
    xlab = "",
    medlwd = 1,
    xaxt = "n",
    axes=F
  )
  axis(side=2, at=c(0, 0.2, 0.4, 0.6, 0.8, 1.0))
  
  stripchart(
    Value ~ Session,
    vertical = TRUE,
    data = metric.df,
    method = "jitter",
    add = TRUE,
    pch = 15,
    cex = 0.5,
    col =  col.pool
  )
  y_top_base = 1
  vertical_gap = 0.01
  v_gap = 0.01
  y_base_RN = y_top_base - 2.8 * vertical_gap
  y_base_TN = y_top_base - 5.6 * vertical_gap
  y_base_TR = y_top_base
  
  
  for (i in 1:length(p)){
    if (p[i] >= 0.05){
      significance = "n.s."
    }else if (p[i] < 0.05 & p[i] >= 0.01){
      significance = "*"
    }else if (p[i] < 0.01 & p[i] >= 0.001){
      significance = "**"
    }else if (p[i] < 0.001 & p[i] >= 0.0001){
      significance = "***"
    }else if (p[i] < 0.0001){
      significance = "****"
    }
    if (i == 3){
      text(
        1.5, 
        y_base_TR + vertical_gap, 
        significance, 
        xpd = NA)
      lines(c(1, 2), 
            c(y_base_TR,  y_base_TR), 
            xpd = NA)
      lines(c(1, 1), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      lines(c(2, 2), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      
    }else if (i == 6){
      text(
        4.5, 
        y_base_TR + vertical_gap, 
        significance, 
        xpd = NA)
      lines(c(4, 5), 
            c(y_base_TR,  y_base_TR), 
            xpd = NA)
      lines(c(4, 4), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
      lines(c(5, 5), c(y_base_TR, y_base_TR - v_gap), xpd = NA)
    }
  }
  text(x = seq(0.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(1, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  
  text(x = seq(1.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(2, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  text(x = seq(2.85, length(num) * 2, by = 6),
       y = y_text,
       num[seq(3, length(num), by = 3)],
       xpd = T,
       srt = 0,
       adj = 0
  )
  seq_for_lines = seq(3, (length(g_list)*6), by=6)
  for (j in seq_for_lines) {
    lines(c(j, j) + 0.5,
          c(yrange[1] - 1e3, yrange[1] + 1e3),
          col = "light grey",
          lty = 1)
  }
  dev.off()
}

# Plot learning index (difference between Test 2 and Pre-test)
plot_diff = function(gene, fly.info.end, all_ofs){
  pdf(paste0(gene, "_difference_plot.pdf"), width = 8)
  a = get_learning_index(fly.info.end, all_ofs, 9, "T", gene)
  b = get_learning_index(fly.info.end, all_ofs, 9, "R", gene)
  c = get_learning_index(fly.info.end, all_ofs, 9, "N", gene)
  plot(density(a$Learning), col = "red", ylim = c(0, 6), xlim = c(-1, 1), main = gene)
  lines(density(b$Learning), col = "blue")
  lines(density(c$Learning), col = "black")
  dev.off()
}
