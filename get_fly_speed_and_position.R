# the one_fly_statistics function can only calculate the metrics from E1 sessions (all spontaneous locomotion monitoring sessions) accurately.
# This function cannot be applied to T1/R1/N1 sessions, because it only analyze the first 10 mins of recording data of any given file.
# Some T1/R1/N1 files inadvertantly exceeded that 10 mins limit and applying this function to those files will truncate the data.
# To account for this issue, one needs to remove that time limit commands when analyzing those affected files.

one_fly_statistics <- function(input_file,
                               framerate = 50,
                               speed_max_thres = 28, #updated from 20 to 30 on Jan 31, 2018, #updated from 30 to 28 on May 14,2018
                               speed_zero_thres = 1e-2,
                               pause_frame_thres = 25,
                               chamber_end_thres = 50)
{
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

find_intersect_points <- function(x1, x2)
{
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
