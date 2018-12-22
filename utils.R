setwd("D:/Behavioral_project/behavior_experiment_data/Analysis/")
library(zoo)
library(boot)
library(dunn.test)

one_fly_statistics <- function(input_file,
                               framerate = 50,
                               speed_max_thres = 28, #updated from 20 to 30 on Jan 31, 2018, #updated from 30 to 28 on May 14,2018
                               speed_zero_thres = 1e-2,
                               pause_frame_thres = 25,
                               chamber_end_thres = 50){
  #  pause_frame_thres - Least of number of frames in a pause
  #  speed_zero_thres - How small the speed is to be treated as not moving (i.e. zero)
  #  speed_max_thres - Maximum speed allowed (The setting for this threshold is as follows:
  #  The distance of centers of adjacent pixels is 63.5um
  #  A fly needs 5-6 seconds to travel from one end of the tube to the other end without pause (in a 50fps setting, 250 - 300 frames ==> 5-6sec)
  #  The tube length is 48.7mm.
  #  So the average walking speed is 48.7mm/5sec = 9.74mm/s or 48.7mm/6sec = 8.11mm/s
  #  Between two adjacent frames (which are 0.02s apart in a 50fps setting), the possible transient speed is 8.11mm/s x 0.02s = 0.1622mm
  #                                                                                                       or 9.74mm/s x 0.02s = 0.1948mm
  #  This transient displacement translates to 0.1622(mm)/63.5um = 2.55px, or 0.1948mm/63.5um = 3.06px
  #  Therefore, the fly typically moves 2-3px per 0.02sec.
  #  Therefore, I set the maximum speed threshold in 0.02s duration to be 30px (10 times the usual speed)
  ## chamber_end_thres - How close (in ) a position to one end of the chamber to be treated as part of the end
  
  ## Read input file
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
  
  ##Remove the initial XXX points
  x = as.numeric(x[[1]])
  data_start = 21
  fly_pos = x[data_start:length(x)]
  
  if (length(fly_pos) >= 600 * framerate) {
    fly_pos = fly_pos[1:(600 * framerate)]
    set_time = length(fly_pos)
  } else{
    fly_pos = fly_pos
    set_time = length(fly_pos)
  }
  
  experiment_time = length(fly_pos)
  
  ##Get the transient speed
  if (data_start > 1) {
    fly_speed = diff(c(x[data_start - 1], fly_pos))
  } else {
    fly_speed = diff(c(NA, fly_pos))
  }
  
  ## Thresholding the max speed
  for (i in 1:experiment_time) {
    # print(fly_speed[i])#
    if (abs(fly_speed[i]) >= speed_max_thres) {
      fly_speed[i] = 0
    }
  }
  
  ###Generating the Pause data.frame ###
  
  label_for_pause = rep(0, length(fly_pos))
  
  for (i in 2:length(label_for_pause)) {
    if ((fly_speed[i] == 0) & (fly_speed[i - 1] > 0)) {
      label_for_pause[i] = 1
    }
    else if ((fly_speed[i] > 0) & (fly_speed[i - 1] == 0)) {
      label_for_pause[i] = 2
    }
    else if ((fly_speed[i] < 0) & (fly_speed[i - 1] == 0)) {
      label_for_pause[i] = 3
    }
    else if ((fly_speed[i] == 0) & (fly_speed[i - 1] < 0)) {
      label_for_pause[i] = 4
    }
  }
  
  
  ###Getting the index for the pause start and ends###
  starts = c()
  ends = c()
  is_start = 1
  
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
  end_type = label_for_pause[ends]
  start_type = label_for_pause[starts]
  start_position = fly_pos[starts - 1]
  end_position = fly_pos[ends - 2]
  
  if (length(ends) < 1) {
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
    pause_df = data.frame(
      starts[1:length(ends)] - 1,
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
  
  #  Get the time spans when fly paused
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
  
  # Pause not at the end is between [50,717])
  is_pause_middle = is_pause
  for (i in 1:experiment_time) {
    if (is_pause[i] == 1) {
      if (fly_pos[i] < 50) {
        is_pause_middle[i] = 0
      }
      else if (fly_pos[i] > 717) {
        is_pause_middle[i] = 0
      }
      else{
        is_pause_middle[i] = 1
      }
    }
  }
  ## Current pause designation: pause duration longer than 25, and pause position is between [50,717]
  pause_middle_dur <-subset(pause_df,(Pause_Duration >= 25) & (Start_Position >= 50) & (Start_Position <= 717))$Pause_Duration
  avg_pause_middle_dur <- (mean(pause_middle_dur)) / framerate
  frac_pause_middle <- (sum(pause_middle_dur)) / experiment_time
  max_pause_middle <- (max(pause_middle_dur)) / framerate
  first_pause_middle <- (pause_middle_dur[1]) / framerate
  
  ### First_pause_duration (for all pauses)###
  
  first_pause_duration = c()
  if (is_pause[1] == 1) {
    first_pause_duration = (pause_end[1]) - (pause_start[1])
  } else{
    first_pause_duration = 0
  }
  first_pause_duration_all = pause_df$Pause_Duration[1]
  
  ## Enter and exit pause speeds (for all pauses) and average of three frames before/after pause
  fly_speed_at_pause_start = NULL
  fly_speed_at_pause_end = NULL
  window_size = 0.5 * framerate
  if (num_pause != 0) {
    for (i in 1:num_pause) {
      ps = pause_start[i]
      pe = pause_end[i]
      if (ps < window_size ||
          pe > experiment_time - window_size) {
        fly_speed_at_pause_start = c(fly_speed_at_pause_start, NA)
        fly_speed_at_pause_end = c(fly_speed_at_pause_end, NA)
      } else{
        fly_speed_at_pause_start = c(fly_speed_at_pause_start,
                                     mean(abs(fly_speed[(ps - window_size):ps])))
        fly_speed_at_pause_end = c(fly_speed_at_pause_end,
                                   mean(abs(fly_speed[pe:(pe + window_size)])))
      }
    }
  }
  
  ## Turns
  ## Step 1 - get the moving direction (speed sign)
  bin_size = framerate * 0.5
  bin_positive_frac = NULL
  t = bin_size
  while (t < experiment_time) {
    bin_fly_speed = fly_speed[t - 1:bin_size + 1]
    frac = sum(bin_fly_speed > 0, na.rm = T) / sum(bin_fly_speed != 0, na.rm =
                                                     T)
    bin_positive_frac = c(bin_positive_frac, frac)
    t = t + bin_size
  }
  ## Step 2 - get the turns
  turns = find_intersect_points(bin_positive_frac, rep(0.5, length(bin_positive_frac)))
  turns = ceiling(turns * bin_size - bin_size / 2)
  position_turns = (fly_pos[turns] + fly_pos[turns - 1]) / 2
  mid_turns = turns[position_turns > chamber_end_thres &
                      position_turns < 767 - chamber_end_thres]
  position_mid_turns = position_turns[position_turns > chamber_end_thres &
                                        position_turns < 767 - chamber_end_thres]
  
  ## Calculating burstiness - inter-event time is pause defined previously ##
  
  num_pause = length(subset(pause_df, (Pause_Duration >= 25))$Pause_Duration)
  
  Pause_duration = subset(pause_df, (Pause_Duration >= 25))$Pause_Duration
  if (num_pause <= 10) {
    burstiness_pause = NA
  } else{
    burstiness_pause = (sd(Pause_duration, na.rm = T) - mean(Pause_duration, na.rm = T)) /
      (sd(Pause_duration, na.rm = T) + mean(Pause_duration, na.rm = T))
  }
  
  ## Calculating burstiness - inter-event time is frames with zero velocity ##
  burstiness_inter_event <-
    replace(abs(fly_speed), abs(fly_speed) > 0, 1)
  
  inter_event_time <-
    rle(burstiness_inter_event)$length[rle(burstiness_inter_event)$values ==
                                         0]
  if (length(inter_event_time) <= 10) {
    Burst_inter_event = NA
  } else{
    Burst_inter_event = (sd((inter_event_time), na.rm = T) - mean((inter_event_time), na.rm = T)) /
      (sd((inter_event_time), na.rm = T) + mean((inter_event_time), na.rm = T))
  }
  
  ## Calculating scrambled burstiness 
  burstiness_inter_event_scrambled <- sample(burstiness_inter_event)
  inter_event_time_scrambled <-
    rle(burstiness_inter_event_scrambled)$length[rle(burstiness_inter_event_scrambled)$values ==
                                                   0]
  Burst_inter_event_scrambled <- c()
  if (length(inter_event_time_scrambled) <= 10) {
    Burst_inter_event_scrambled = NA
  } else{
    Burst_inter_event_scrambled = (sd((inter_event_time_scrambled), na.rm = T) -
                                     mean((inter_event_time_scrambled), na.rm = T)) / (sd((inter_event_time_scrambled), na.rm = T) +
                                                                                         mean((inter_event_time_scrambled), na.rm = T))
  }
  
  ## Inverted burstiness with no thresholding
  burstiness_inter_event_inverted <-
    rep(0, length(burstiness_inter_event))
  burstiness_inter_event_inverted <-
    replace(burstiness_inter_event_inverted,
            burstiness_inter_event == 0,
            1)
  inter_event_time_inverted <-
    rle(burstiness_inter_event_inverted)$length[rle(burstiness_inter_event_inverted)$values ==
                                                  0]
  if (length(inter_event_time_inverted) <= 10) {
    Burst_inter_event_inverted = NA
  } else{
    Burst_inter_event_inverted = (sd((inter_event_time_inverted), na.rm = T) -
                                    mean((inter_event_time_inverted), na.rm = T)) / (sd((inter_event_time_inverted), na.rm = T) +
                                                                                       mean((inter_event_time_inverted), na.rm = T))
  }
  
  ## Inverted burstiness_with thresholding
  if (num_pause <= 10) {
    burstiness_pause_inverted = NA
  } else{
    pause_start_late <- pause_start[2:length(pause_start)]
    pause_end_late <- pause_end[1:(length(pause_end) - 1)]
    
    burstiness_pause_inverted = (sd((pause_start_late - pause_end_late), na.rm = T) -
                                   mean((pause_start_late - pause_end_late), na.rm = T)) / (sd((pause_start_late - pause_end_late), na.rm = T) +
                                                                                              mean((pause_start_late - pause_end_late), na.rm = T))
  }
  
  ## Behavioral states
  fly_pos_original = burstiness_inter_event[1:(length(burstiness_inter_event) -
                                                 1)]
  fly_pos_lag = burstiness_inter_event[2:length(burstiness_inter_event)]
  fly_pos_sum = (fly_pos_original) * 1 + 2 * fly_pos_lag
  
  ## Get Behavioral State (all) 
  
  p_to_p = sum(pause_df$Pause_Duration - 1)
  if (num_pause < 2) {
    w_to_w = 0
  } else{
    w_to_w = sum(pause_df$Start_Index[2:length(pause_df$Start_Index)] - pause_df$End_Index[1:(length(pause_df$Start_Index) -
                                                                                                1)] - 1)
  }
  
  p_to_w = length(pause_df$Start_Index)
  w_to_p = length(pause_df$End_Index)
  
  p_p2p <- c()
  p_p2w <- c()
  p_w2p <- c()
  p_w2w <- c()
  if (p_to_p + p_to_w == 0) {
    p_p2p = NA
    p_p2w = NA
  } else{
    p_p2p = p_to_p / (p_to_p + p_to_w)
    p_p2w = p_to_w / (p_to_p + p_to_w)
  }
  
  if ((w_to_p) + (w_to_w) == 0) {
    p_w2p = NA
    p_w2w = NA
  } else if (is.na((w_to_p) + (w_to_w))) {
    p_w2p = NA
    p_w2w = NA
  } else{
    p_w2p = w_to_p / (w_to_p + w_to_w)
    p_w2w = w_to_w / (w_to_p + w_to_w)
  }
  
  ## Behavioral states for pauses not in the middle
  
  fly_pos_sum_middle = (is_pause_middle[2:(length(is_pause_middle) - 1)]) *
    1 + 2 * (is_pause_middle[3:length(is_pause_middle)]) + 4 * (is_pause_middle[1:(length(is_pause_middle) -
                                                                                     2)])
  
  ## Behavioral states for pauses not at the end & not bumping to the wall
  pause_middle_nobump_df = subset(pause_df,
                                  (Start_Position >= 50)& 
                                    (Start_Position <= 717)&
                                    (((Start_Type == 1) &
                                        (End_Type == 2)) |
                                       ((Start_Type == 4) &
                                          (End_Type == 3))))
  pause_middle_bump_df = subset(pause_df,
                                (Start_Position >= 50)
                                & (Start_Position <= 717)
                                &
                                  (((Start_Type == 1) &
                                      (End_Type == 3)) |
                                     ((Start_Type == 4) &
                                        (End_Type == 2))))
  pause_end_df = subset(pause_df,
                        (Start_Position < 50)
                        | (Start_Position > 717))
  
  p_to_p_end = sum(pause_end_df$Pause_Duration - 1)
  p_to_w_end = length(pause_end_df$Start_Index)
  w_to_p_end = length(pause_end_df$End_Index)
  p_to_p_middle_bump = sum(pause_middle_bump_df$Pause_Duration - 1)
  p_to_w_middle_bump = length(pause_middle_bump_df$Start_Index)
  w_to_p_middle_bump = length(pause_middle_bump_df$End_Index)
  
  p_to_p_middle_nobump = sum(pause_middle_nobump_df$Pause_Duration - 1)
  
  num_pause_middle_nobump = nrow(pause_middle_nobump_df)
  num_pause_middle_bump = nrow(pause_middle_bump_df)
  num_pause_end = nrow(pause_end_df)
  
  if (num_pause < 2) {
    w_to_w_middle_nobump = 0
  } else{
    if (num_pause_middle_nobump < 2) {
      w_to_w_middle_nobump = 0
    } else{
      w_to_w_middle_nobump = sum(pause_middle_nobump_df$Start_Index[2:length(pause_middle_nobump_df$Start_Index)] -
                                   pause_middle_nobump_df$End_Index[1:(length(pause_middle_nobump_df$Start_Index) -
                                                                         1)] - 1) - sum(
                                                                           p_to_p_end,
                                                                           p_to_w_end,
                                                                           w_to_p_end,
                                                                           p_to_p_middle_bump,
                                                                           p_to_w_middle_bump,
                                                                           w_to_p_middle_bump
                                                                         )
    }
  }
  
  p_to_w_middle_nobump = length(pause_middle_nobump_df$Start_Index)
  w_to_p_middle_nobump = length(pause_middle_nobump_df$End_Index)
  
  p_p2p_middle <- c()
  p_p2w_middle <- c()
  p_w2p_middle <- c()
  p_w2w_middle <- c()
  
  if (p_to_p_middle_nobump + p_to_w_middle_nobump == 0) {
    p_p2p_middle = NA
    p_p2w_middle = NA
  } else{
    p_p2p_middle = p_to_p_middle_nobump / (p_to_p_middle_nobump + p_to_w_middle_nobump)
    p_p2w_middle = p_to_w_middle_nobump / (p_to_p_middle_nobump + p_to_w_middle_nobump)
  }
  
  if (is.na(w_to_w_middle_nobump)) {
    w_to_w_middle_nobump = 0
  }
  
  if (w_to_p_middle_nobump + w_to_w_middle_nobump == 0) {
    p_w2p_middle = NA
    p_w2w_middle = NA
  } else{
    p_w2p_middle = w_to_p_middle_nobump / (w_to_p_middle_nobump + w_to_w_middle_nobump)
    p_w2w_middle = w_to_w_middle_nobump / (w_to_p_middle_nobump + w_to_w_middle_nobump)
  }
  
  ## Burstiness of start of walking (event) -- Pause not at the end
  
  start_walking_middle = replace(fly_pos_sum_middle, fly_pos_sum_middle !=
                                   4, 0)
  start_walk_burst_middle = rle(start_walking_middle)$length[rle(start_walking_middle)$values ==
                                                               0]
  
  if (length(start_walk_burst_middle) <= 10) {
    Burst_start_walking_middle = NA
  } else{
    Burst_start_walking_middle = (sd((start_walk_burst_middle), na.rm = T) -
                                    mean((start_walk_burst_middle), na.rm = T)) / (sd((start_walk_burst_middle), na.rm = T) +
                                                                                     mean((start_walk_burst_middle), na.rm = T))
  }
  ## Burstiness of start of pause (event) -- Pause not at the end
  start_pause_middle = replace(fly_pos_sum_middle, fly_pos_sum_middle != 3, 0)
  start_pause_burst_middle = rle(start_pause_middle)$length[rle(start_pause_middle)$values ==
                                                              0]
  
  if (length(start_pause_burst_middle) <= 10) {
    Burst_start_pause_middle = NA
  } else{
    Burst_start_pause_middle = (sd((start_pause_burst_middle), na.rm = T) -
                                  mean((start_pause_burst_middle), na.rm = T)) / (sd((start_pause_burst_middle), na.rm = T) +
                                                                                    mean((start_pause_burst_middle), na.rm = T))
  }
  
  ## Memory - pause as inter event and walking as event
  memory <- 0
  if (length(inter_event_time) < 2) {
    memory = NA
  } else{
    m1 <-
      mean(inter_event_time[1:(length(inter_event_time) - 1)]) #mean of inter-event time from 1 to n-1
    m2 <-
      mean(inter_event_time[2:(length(inter_event_time))]) #mean of inter-event time from 2 to n
    std1 <- sd(inter_event_time[1:(length(inter_event_time) - 1)])
    std2 <- sd(inter_event_time[2:(length(inter_event_time))])
    
    for (i in 1:(length(inter_event_time) - 1)) {
      memory <-
        memory + ((inter_event_time[i] - m1) * (inter_event_time[i + 1] - m2) /
                    (std1 * std2))
    }
    memory <- (1 / (length(inter_event_time) - 1)) * memory
  }
  
  ## #Inverted burstiness' memory
  memory_inverted <- 0
  if (length(inter_event_time_inverted) < 2) {
    memory_inverted = NA
  } else{
    m3 <-
      mean(inter_event_time_inverted[1:(length(inter_event_time_inverted) - 1)])
    m4 <-
      mean(inter_event_time_inverted[2:(length(inter_event_time_inverted))])
    std3 <-
      sd(inter_event_time_inverted[1:(length(inter_event_time_inverted) - 1)])
    std4 <-
      sd(inter_event_time_inverted[2:(length(inter_event_time_inverted))])
    
    for (i in 1:(length(inter_event_time_inverted) - 1)) {
      memory_inverted <-
        memory_inverted + ((inter_event_time_inverted[i] - m3) * (inter_event_time_inverted[i +
                                                                                              1] - m4) / (std3 * std4)
        )
    }
    memory_inverted <-
      (1 / (length(inter_event_time_inverted) - 1)) * memory_inverted
  }
  
  
  
  ##  Middle-nobump pauses
  num_pause = length(pause_middle_nobump_df$Start_Index)
  frac_pause = sum((pause_middle_nobump_df$Pause_Duration), na.rm =
                     TRUE) / experiment_time
  avg_pause_dur = mean((pause_middle_nobump_df$Pause_Duration), na.rm = TRUE)#unit: px/frame
  avg_fly_speed = mean(abs(fly_speed), na.rm = TRUE)
  avg_fly_speed_not_in_pause = mean(abs(fly_speed[which(abs(fly_speed) >
                                                          0)]), na.rm = TRUE)
  if (length(fly_speed[which(abs(fly_speed) > 0)]) < 10) {
    avg_fly_speed_not_in_pause = 0
  }
  if (max(abs(fly_speed)) == 0) {
    avg_fly_speed_not_in_pause = NA
  }
  
  avg_fly_speed_enter = mean(fly_speed_at_pause_start, na.rm = TRUE)
  avg_fly_speed_exit = mean(fly_speed_at_pause_end, na.rm = TRUE)
  
  tot_moving_dist = sum(abs(fly_speed[1:set_time]), na.rm = TRUE)
  
  num_turn = length(turns)
  num_mid_turns = length(mid_turns)
  frac_mid_turns = num_mid_turns / num_turn
  
  ## Return output
  ret = list(
    num_pause,
    1-frac_pause, #unit: percentage
    avg_pause_dur / (framerate), # max(pause_end - pause_start) / framerate,
    max(pause_middle_nobump_df$Pause_Duration) / framerate,
    (avg_fly_speed * (48.7 - 1) / 768) / (1 / framerate),
    (avg_fly_speed_not_in_pause * (48.7 - 1) / 768) / (1 / framerate),
    (avg_fly_speed_enter * (48.7 - 1) / 768) / (1 / framerate),
    (avg_fly_speed_exit * (48.7 - 1) / 768) / (1 / framerate),
    (((tot_moving_dist) * (48.7 - 1) / 768) / experiment_time) *
      framerate * 60,
    num_turn,#NU
    num_mid_turns,#NU
    frac_mid_turns,#NU
    burstiness_pause,
    Burst_inter_event,#NU
    Burst_inter_event_scrambled,#NU
    burstiness_pause_inverted, #NU
    Burst_inter_event_inverted, #NU
    first_pause_duration / (framerate),#NU
    first_pause_duration_all / (framerate), #NU
    # state_transition_middle,#Removed Jan31, 2018
    p_p2p_middle,
    p_p2w_middle,
    p_w2w_middle,
    p_w2p_middle,
    memory,#NU
    memory_inverted,#NU
    Burst_start_walking_middle,#NU
    Burst_start_pause_middle,#NU
    avg_pause_middle_dur,
    1-frac_pause_middle,
    max_pause_middle,
    first_pause_middle
  )
  
  names(ret) = c(
    "Number of Pause Starts",#1
    "Percentage Time Active",#2
    "Average Pause Duration",#3
    "Max Pause Duration",#4
    "Average Moving Speed ",#5
    "Average Moving Speed (excluding pause)",#6
    "Average Speed When Enter Pause",#7
    "Average Speed When Exit Pause",#8
    "Moving Distance Per Minute",#9
    "Number of Turns",#10
    "Number of Middle Turns",#11
    "Fration of Middle Turns Out of Total Turns",#12
    "Burstiness (Pause)",#13
    "Burstiness (Inter Event Time)",#14
    "Burstiness (Scrambled)",#15
    "Burstiness (Walking bouts-thresholding)",#16
    "Burstiness (Walking events-thresholding)",#17
    "Beginning Pause Duration",#18
    "First Pause Duration",#19
    "Transition Probability (Pause not at the end): Pause to Pause", #20
    "Transition Probability (Pause not at the end): Pause to Walking", #21
    "Transition Probability (Pause not at the end): Walking to Walking", #22
    "Transition Probability (Pause not at the end): Walking to Pause", #23
    "Memory", #24
    "Memory (inverted)", #25
    "Burstiness of Start of Walking (Pause not at the end)", #26 *
    "Burstiness of Start of Pause (Pause not at the end)", #27 *
    "Average Pause Duration (Pause not at the End)",#28
    "Percentage Time Active (Pause not at the End)",#29
    "Max Pause Duration (Pause not at the End)", #30
    "First Pause Duration (Pause not at the End)" #31
  )
  return(ret)
}

find_intersect_points <- function(x1, x2){
  ##Adapted from code by nograpes
  ##http://stackoverflow.com/questions/20519431/finding-point-of-intersection-in-r
  ## Find points where x1 is above x2.
  above <- x1 > x2
  ## Points always intersect when above=TRUE, then FALSE or reverse
  intersect.points <- which(diff(above) != 0)
  ## Find the slopes for each line segment.
  x1.slopes <- x1[intersect.points + 1] - x1[intersect.points]
  x2.slopes <- x2[intersect.points + 1] - x2[intersect.points]
  ## Find the intersection for each segment.
  x.points <-
    intersect.points + ((x2[intersect.points] - x1[intersect.points]) / (x1.slopes -
                                                                           x2.slopes))
  return(x.points)
}

shuffle_is_pause <- function(is_pause) {
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
  ## unfold
}

pass_fly_QC <- function(input_file,
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
  data_start = 21 
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

data_filter <- function(filter, fly.info){
  if (filter == 1) {
    ind.include = NULL
    for (genotype in unique(fly.info$Genotype)) {
      if (genotype == "CS") {
        next
      }
      if (genotype == "WT") {
        ind = fly.info$Genotype %in% c("WT", "CS") &
          !(1:nrow(fly.info) %in% ind.excl)
      }else{
        ind = fly.info$Genotype == genotype &
          !(1:nrow(fly.info) %in% ind.excl)
      }
      fms <- fly.info$Fly.moving.speed[ind]
      rank_fms = rank(fms)
      ind.filter =  rank_fms <= length(fms) * 1.0 & rank_fms >= length(fms) * 0.0 # changed from 0.3 to 0.0
      ind.include = c(ind.include, which(ind)[ind.filter])
    }
  }
  if (filter == 2) {
    ind.include = NULL
    session = "E1"
    for (ind in 1:nrow(fly.info)) {
      if (fly.info$Genotype[ind] == "WT") {
        input.file <- paste0(
          "data/",
          fly.info$experimenter[ind],
          "/CS/",
          "ProcessedData_Fly",
          fly.info$Fly[ind],
          "_",
          session,
          "_WT",
          ".csv"
        )
      } else{
        input.file <- paste0(
          "data/",
          fly.info$experimenter[ind],
          "/Mutants/",
          "ProcessedData_Fly",
          fly.info$Fly[ind],
          "_",
          session,
          "_",
          fly.info$Genotype[ind],
          ".csv"
        )
      }
      framerate =	fly.info$Framerate[ind]
      if (pass_fly_QC(input.file, framerate)) {
        ind.include = c(ind.include, ind)
      }
    }
  }
  return(ind.include)
}

checking_fly_numbers <- function(fly.info.include, filename){
  
  type_of_mutants = length(unique(fly.info.include$Genotype))
  number_of_mutants = c()
  names_of_mutants = unique(fly.info.include$Genotype)
  
  for (i in 1:type_of_mutants){
    number_of_mutants[i] = dim(fly.info.include[fly.info.include$Genotype==names_of_mutants[i],])[1]
  }
  
  mutant_info = data.frame(names_of_mutants,number_of_mutants)
  
  colnames(mutant_info) = c("Mutant Genotype", "Number of Flies")
  
  write.csv(mutant_info, file = filename , row.names = FALSE)
}

one_fly_laser_statistics <- function(input_file, framerate){
  fly.file = read.csv(input_file, header = T, stringsAsFactors = F)
  fly.position.raw = as.numeric(fly.file[[1]])
  fly.laser.raw = as.numeric(fly.file[[2]])
  if (is.na(fly.laser.raw[1]) == T) {
    number_of_laser_clicks = NA
    total_laser_ON = NA
    ret = list(number_of_laser_clicks,
               total_laser_ON #in seconds
    )
    names(ret) = c("Number of Laser Punishments", "Total Laser Exposure in Seconds")
  }else{
    data_start = 21
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
    } else{laser_df = data.frame (which(label_for_laser == 1),
                                  which(label_for_laser == 2),
                                  laser_ON / framerate,
                                  (laser_OFF[2:length(laser_OFF)]) / framerate,
                                  (laser_OFF[2:length(laser_OFF)] - laser_ON) / framerate,
                                  laser_OFF[2:length(laser_OFF)] > 8 * 60 * framerate
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
    number_of_laser_clicks = length(laser_df$Laser_On)
    total_laser_ON = sum(laser_df$ON_duration)
    if (number_of_laser_clicks == 1) {
      if (laser_df$ON_duration == 0) {
        number_of_laser_clicks = 0
      }
    }
    ret = list(number_of_laser_clicks,
               total_laser_ON 
    )
    names(ret) = c("Number of Laser Punishments","Total Laser Exposure in Seconds")
    return(ret)
  }
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
Laser_Delay <- function(file_name_filter, fly.info.movement) {
  laser_delays = data.frame()
  for (ind in 1:nrow(fly.info.movement)) {
    if(fly.info.movement$Genotype[ind]=="WT"){
      input.file <- list.files(
        path = paste0("data/", fly.info.movement$experimenter[ind], "/CS/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_WT", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    if(fly.info.movement$Genotype[ind]=="CS"){
      input.file <- list.files(
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
total_chance_of_being_hit_by_laser <- function(file_name_filter, fly.info.movement) {
  laser_chance = data.frame()
  for (ind in 1:nrow(fly.info.movement)) {
    if(fly.info.movement$Genotype[ind]=="WT"){
      input.file <- list.files(
        path = paste0("data/",fly.info.movement$experimenter[ind],"/CS/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_WT", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    if(fly.info.movement$Genotype[ind]=="CS"){
      input.file <- list.files(
        path = paste0("data/", fly.info.movement$experimenter[ind], "/mutants/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_CS", ".csv"),
        full.names = T)
      if(length(input.file)==0){next()}
    }
    laser_chance = rbind(laser_chance, chance_of_being_hit_by_laser(input.file))
  }
  names(laser_chance) = c("Chances of being hit during walking", "Chances of being hit during pause ", "Laser ON duration percentage")
  return(laser_chance)
}

fly_pos_to_moving_status = function(fly_pos){ 
  # This is determined by quantile(abs(fly_moving_status),c(0.97, 0.975, 0.98)), and the 97.5% 
  # corresponds to 28.6
  speed_threshold = 28 
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
    starting_point = 21
    fly_moving_status = fly_moving_status[(starting_point-1):length(fly_moving_status)]
    return(cumsum(fly_moving_status))
  }
}

###Get all CS flies' cumulated moving status grouped by types (T/R/N)
get_sequence_length <- function(file_name) {
  if (sum(is.na(moving_status(file_name)))==0){
    return (length(moving_status(file_name)))
  }else{return(NA)}
}

get_cumsums_total <- function(file_name_filter, fly.info.movement) {
  file_names = c()
  for (ind in 1:nrow(fly.info.movement)) {
    if(fly.info.movement$Genotype[ind]=="WT"){
      input.file <- list.files(
        path = paste0("data/", fly.info.movement$experimenter[ind], "/CS/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_",
                         file_name_filter, "_WT", ".csv"),
        full.names = T
      )
    }
    if(fly.info.movement$Genotype[ind]=="CS"){
      input.file <- list.files(
        path = paste0("data/", fly.info.movement$experimenter[ind], "/mutants/"),
        pattern = paste0("ProcessedData_Fly", fly.info.movement$Fly[ind], "_", file_name_filter, "_CS", ".csv"),
        full.names = T)
    }
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


learning_score <- function(metric.ind, query.genotype, query.fly, query.experimenter){
  
  fly_genotype = query.genotype
  if (query.genotype == c("CS")){
    query.genotype = c("WT", "CS")
  }
  
  input.file = paste0("metrics/metric_", metric.ind, ".csv")
  if (!file.exists(input.file)) {
    next
  }
  metric.df = read.csv(input.file)
  ## covariates of interest: genotype, session
  y = list()
  ## E1 data
  session = "E1"
  for (category in c("T", "R", "N")) {
    query.session = gsub("X", category, session)
    ind <- metric.df$Session == query.session &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category &
      metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    ind.E1 <- metric.df$Session == "E1" &
      metric.df$Genotype %in% query.genotype &
      metric.df$Category == category
    metric.df$Fly %in% query.fly&
      metric.df$Experimenter  %in%  query.experimenter
    # z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"]) /metric.df[ind.E1,"Value"]
    # z = - (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"]) /metric.df[ind.E1,"Value"]
    z = log(metric.df[ind,"Value"]) - log(metric.df[ind.E1,"Value"])
    y = append(y, list(na.omit(z)))
  }
  
  ## input sessions data
  for (session in sessions) {
    if (grepl("T", session) == T) {
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "T" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind <- metric.df$Session == session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "T" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      # z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"]) /metric.df[ind.E1,"Value"]
      # z = - (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"])/metric.df[ind.E1,"Value"]
      z = log(metric.df[ind,"Value"]) - log(metric.df[ind.E1,"Value"])
      }
    if (grepl("R", session) == T) {
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "R" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind <- metric.df$Session == session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "R" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      # z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"]) /metric.df[ind.E1,"Value"]
      # z = -(metric.df[ind.E1,"Value"] - metric.df[ind,"Value"]) /metric.df[ind.E1,"Value"]
      z = log(metric.df[ind,"Value"]) - log(metric.df[ind.E1,"Value"])
      }
    if (grepl("N", session) == T) {
      ind.E1 <- metric.df$Session == "E1" &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "N" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      ind <- metric.df$Session == session &
        metric.df$Genotype %in% query.genotype &
        metric.df$Category == "N" &
        metric.df$Fly %in% query.fly&
        metric.df$Experimenter  %in%  query.experimenter
      # z = (metric.df[ind.E1,"Value"] - metric.df[ind,"Value"])/metric.df[ind.E1,"Value"]
      # z = -(metric.df[ind.E1,"Value"] - metric.df[ind,"Value"]) /metric.df[ind.E1,"Value"]
      z = log(metric.df[ind,"Value"]) - log(metric.df[ind.E1,"Value"])
    }
    y = append(y, list(na.omit(z)))
  }
  
  ## special cases
  input.y = list(y[[7]], y[[15]], y[[23]])
  
  yy.3T = rep(paste0("E11_", query.genotype[1]), length(input.y[[1]]))
  yy.3R = rep(paste0("E12_", query.genotype[1]), length(input.y[[2]]))
  yy.3N = rep(paste0("E13_", query.genotype[1]), length(input.y[[3]]))
  
  yy.label = c(yy.3T, yy.3R, yy.3N)
  # yy.label = c(yy.3T, yy.3R)
  
  input.y_3T = as.numeric(input.y[[1]])
  input.y_3R = as.numeric(input.y[[2]])
  input.y_3N = as.numeric(input.y[[3]])
  
  input.yy = c(
    input.y_3T,
    input.y_3R,
    input.y_3N
  )
  input.y.df.pre = data.frame(input.yy, yy.label)
  return(input.y.df.pre)
}

get_query_info<-function(query.genotype){
  if(query.genotype=="CS"){
    query.fly = fly.info.include[((fly.info.include$Genotype == "WT") |
                                    (fly.info.include$Genotype == "CS")) &
                                   (fly.info.include$experimenter!="SW"), ]$Fly
    query.experimenter = fly.info.include[((fly.info.include$Genotype == "WT") |
                                             (fly.info.include$Genotype == "CS")) &
                                            (fly.info.include$experimenter!="SW"), ]$experimenter
    write.table(
      fly.info.include[((fly.info.include$Genotype == "WT") |
                          (fly.info.include$Genotype == "CS")) &
                         (fly.info.include$experimenter!="SW"), ],
      "fly_info_include_WT.csv",
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
  }else if(query.genotype=="SUN1"){
    query.genotype <- c("SUN1")
    query.fly = fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                                   (fly.info.include$experimenter!="SW"), ]$Fly
    
    query.experimenter = fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                                            (fly.info.include$experimenter!="SW"), ]$experimenter
    write.table(
      fly.info.include[((fly.info.include$Genotype == "SUN1")) &
                         (fly.info.include$experimenter!="SW"), ],
      "fly_info_include_SUN1.csv",
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
  }else{
    query.fly = fly.info.include[(fly.info.include$Genotype == query.genotype), ]$Fly
    query.experimenter = fly.info.include[(fly.info.include$Genotype == query.genotype), ]$experimenter
    write.table(
      fly.info.include[(fly.info.include$Genotype == query.genotype), ],
      paste0("fly_info_include_",query.genotype,".csv"),
      col.names = T,
      row.names = F,
      quote = F,
      sep = ","
    )
  }
  
  return(list(query.fly, 
              query.experimenter))
}    
