## Script for processing Qualisys motion capture data for Row Perfect 3 (RP3) ergometer rowing
## This code shows how to check variables for missing data and calculate kinematic variables

## Made by Henrik Herrebroden
## Contact information: henrikh@henrikh.no

## To run this script, you should have a folder in your working directory with csv data files
## The CSV files should contain information for markers of interest, such as (in my case): 
## shoulder and hip on the rower; seat, handle, and the front on the ergometer
## I consistently use right side markers for shoulder, hip, and handle, 
## Left/right seat marker is chosen based on data tracking (least missing data)

# Set these values prior to running the script ----------------------------------------------------

setwd("C:/Users/henrher/OneDrive - Universitetet i Oslo/Tests&Data/")
library(dplyr) # needed for time filtering
library(stringr) # needed for str_sub function
library(prospectr) # needed for the Savitzky-Golay function. 

sg.w <- 11 # Used in Savitzky-Golay filter (window size); increase/decrease for more/less smoothing

path.data <- "PhD Data Rowing/QTM Processed Files/mocap_data_files/" # Subfolder within working directory containing RP3 data exports
path.plots <- "PhD Data Rowing/QTM Processed Files/Plots/" # Subfolder to receive exported plots
# List the files that are currently in path.data and choose one to process
mocap.list <-  data.frame(list.files(path = path.data, pattern = "mocap.csv", recursive = T)) # Create a list of files in path.data
colnames(mocap.list) <- "files" # Label the column of filenames as "files"
mocap.list # Run this to see the list of files in your folder

#Choose a file to get going
i <- mocap.list$files[2]

## Set up standardization results (to be added later)
    #Explanation: calibrations and the rowers' body sizes differ between trials
      #Thus, x axis (horizontal) variables of interest need standardization to get comparable values

results.s <- data.frame(rsh_x_min = 0,
                        rsh_x_max = 0,
                        rsh_z_min = 0,
                        rsh_z_max = 0,
                        rhi_x_min = 0,
                        rhi_x_max = 0,
                        new_rha_x_min = 0,
                        new_rha_x_max = 0,
                        pfr_x_min = 0,
                        pfr_x_max = 0,
                        info = 0, 
                        id = 0)


# Set up results for missing data and variables of interest (to be added later)

results <- data.frame(percent.na.rsh_x.overall = 0,
                      percent.na.rsh_x.drive = 0,
                      percent.na.rsh_x.drive.seq.area = 0,
                      percent.na.rsh_x.recovery = 0,
                      percent.na.rsh_x.rockover.area = 0,
                      percent.na.rsh_x.catch = 0,
                      percent.na.rsh_x.finish = 0,
                      percent.na.rsh_z.overall = 0,
                      percent.na.rsh_z.drive = 0,
                      percent.na.rsh_z.drive.seq.area = 0,
                      percent.na.rsh_z.recovery = 0,
                      percent.na.rsh_z.rockover.area = 0,
                      percent.na.rsh_z.catch = 0,
                      percent.na.rsh_z.finish = 0,
                      percent.na.rhi_x.overall = 0,
                      percent.na.rhi_x.drive = 0,
                      percent.na.rhi_x.recovery = 0,
                      percent.na.rhi_x.catch = 0,
                      percent.na.rhi_x.finish = 0,
                      percent.na.new_rha_x.overall = 0,
                      percent.na.new_rha_x_sg.overall = 0,
                      percent.na.new_rha_x.drive = 0,
                      percent.na.new_rha_x.drive.seq.area = 0,
                      percent.na.new_rha_x.recovery = 0,
                      percent.na.new_rha_x.rockover.area = 0,
                      percent.na.new_rha_x.catch = 0,
                      percent.na.new_rha_x.finish = 0,
                      percent.na.pfr_x.overall = 0,
                      percent.na.pfr_x.drive = 0,
                      percent.na.pfr_x.drive.seq.area = 0,
                      percent.na.pfr_x.recovery = 0,
                      percent.na.pfr_x.rockover.area = 0,
                      percent.na.pfr_x.catch = 0,
                      percent.na.pfr_x.finish = 0,
                      percent.na.chosen.seat_z = 0,
                      percent.na.chosen_seat_z_sg = 0,
                      max_pfr_x_check = 0,
                      mean_shoulder_location_x_drive_seq = 0,
                      mean_hip_location_x_drive_seq = 0,
                      mean_shoulder_location_x_rockover = 0, 
                      mean_hip_location_x_rockover = 0,
                      mean_standardized_leg_travel_at_drive_seq = 0,
                      mean_standardized_leg_travel_at_drive_seq_percent = 0,
                      sd_standardized_leg_travel_at_drive_seq = 0,
                      min_standardized_leg_travel_at_drive_seq = 0,
                      max_standardized_leg_travel_at_drive_seq = 0,
                      mean_standardized_leg_travel_at_rockover = 0,
                      mean_standardized_leg_travel_at_rockover_percent = 0,
                      sd_standardized_leg_travel_at_rockover = 0,
                      min_standardized_leg_travel_at_rockover = 0,
                      max_standardized_leg_travel_at_rockover = 0,
                      mean_standardized_handle_travel_at_drive_seq = 0,
                      sd_standardized_handle_travel_at_drive_seq = 0,
                      min_standardized_handle_travel_at_drive_seq = 0,
                      max_standardized_handle_travel_at_drive_seq = 0,
                      mean_standardized_handle_travel_at_rockover = 0,
                      sd_standardized_handle_travel_at_rockover = 0,
                      min_standardized_handle_travel_at_rockover = 0,
                      max_standardized_handle_travel_at_rockover = 0,
                      mean_seat_location_z = 0,
                      sd_seat_location_z = 0,
                      mean_seat_jerk_z_sg = 0,
                      sd_seat_jerk_z_sg = 0,
                      mean_velocity_sg_handle_overall = 0,
                      mean_acceleration_sg_handle_overall = 0,
                      mean_jerk_sg_handle_overall = 0,
                      mean_jerk_sg_handle_drive = 0,
                      mean_jerk_sg_handle_recovery = 0,
                      mean_jerk_sg_handle_catch = 0,
                      mean_jerk_sg_handle_finish = 0,
                      mean_handle_distance_check_negative = 0,
                      min_and_max_left_right_handle_distance_check = 0,
                      mean_sh_distance_check_negative = 0, 
                      min_and_max_left_right_sh_distance_check = 0,
                      mean_hip_distance_check_negative = 0, 
                      min_and_max_left_right_hip_distance_check = 0,
                      mean_seat_distance_check_negative = 0,
                      min_and_max_left_right_seat_distance_check = 0,
                      check_who_switched_handle_mocap = 0,
                      mean_shoulder_location_vertical_overall = 0,  
                      sd_shoulder_location_vertical_overall = 0,
                      mean_shoulder_location_vertical_overall_standardized = 0,  
                      sd_shoulder_location_vertical_overall_standardized = 0,
                      mean_time_between_catches = 0,
                      sd_time_between_catches = 0,
                      min_time_between_catches = 0,
                      max_time_between_catches = 0,
                      stroke_count = 0,
                      mean_stroke_length = 0,
                      sd_stroke_length = 0,
                      min_stroke_length = 0,
                      max_stroke_length = 0,
                      count_drives = 0,
                      count_recoverys = 0,
                      count_catches = 0,
                      count_finish = 0,
                      count_rockover_points = 0,
                      count_drive_seq_points = 0,
                      info = 0, 
                      id = 0, 
                      cond_full = 0,
                      cond_order = 0,
                      eliteness = 0,
                      check.max.x = 0)

#Run all files to find values for standardization

for (i in mocap.list$files) {
  
  #Read data
  data.mocap <- read.csv(paste(path.data, i, sep = ""), sep = ",")
  
  #read time list (when rowing trial starts and stops)
  timelist <- read.csv("C:/Users/henrher/OneDrive - Universitetet i Oslo/Tests&Data/PhD Data Rowing/QTM Processed Files/timedata_mocap.csv")
  
  #trim data based on filename and time list
  
  i.filename <- gsub(".*/","",i)
  
  crop.times <- timelist[grepl(i.filename, timelist$Filename),]
  
  mocap.data.short <- data.mocap[data.mocap$time >= crop.times$Start_Analysis & data.mocap$time <= crop.times$Stop_Analysis,]
  
  #Replace zero values (missing data) with NA
  mocap.data.short[mocap.data.short == 0] <- NA
  
  
  ## REVERSING OF VALUES IN CERTAIN TRIALS
    # For some trials (not all), the Qualisys model confuses right and left handle marker
    #For these trials, switch right/left handle values
  
  ## First, calculate absolute proximity, depthwise, from right handle (rha_y) to right shoulder (rsh_y)
  
  mocap.data.short["right.handle.prox.rsh_y"] <- mocap.data.short$rha_y - mocap.data.short$rsh_y
  mocap.data.short$right.handle.prox.rsh_y <- abs(mocap.data.short$right.handle.prox.rsh_y)
  
  mocap.data.short["left.handle.prox.rsh_y"] <- mocap.data.short$lha_y - mocap.data.short$rsh_y
  mocap.data.short$left.handle.prox.rsh_y <- abs(mocap.data.short$left.handle.prox.rsh_y)
  
  ## Calculate which handle marker is closer to right shoulder
  mocap.data.short["left.right.handle.distance.to.rha_y"] <- mocap.data.short$right.handle.prox.rsh_y - mocap.data.short$left.handle.prox.rsh_y
  
  mocap.data.short["left.right.handle.distance.to.rha_y.mean"] <- mean(mocap.data.short$right.handle.prox.rsh_y, na.rm = T) - mean(mocap.data.short$left.handle.prox.rsh_y, na.rm = T)
  
  ## Switch if left handle values are closer to right shoulder
  mocap.data.short["new_rha_x"] <- mocap.data.short$rha_x
  mocap.data.short$new_rha_x <- with(mocap.data.short, ifelse(left.right.handle.distance.to.rha_y.mean > 0, lha_x, new_rha_x))

  mocap.data.short["new_rha_y"] <- mocap.data.short$rha_y
  mocap.data.short$new_rha_y <- with(mocap.data.short, ifelse(left.right.handle.distance.to.rha_y.mean > 0, lha_y, new_rha_y))
  
  mocap.data.short["new_rha_z"] <- mocap.data.short$rha_z
  mocap.data.short$new_rha_z <- with(mocap.data.short, ifelse(left.right.handle.distance.to.rha_y.mean > 0, lha_z, new_rha_z))
  
  ## Due to calibration differences, the x axis (horizontal) values are reversed for a few trials
  ## For these trials, the x variables' values should be reversed, so that the same travel direction leads to the same value change for all trials
  
  ## Specifically: Switch direction for key values on x axis if max new_rha_x value is closer to the ergometer front than minimum new_rha_x value 
  
  mocap.data.short["reversing"] <- ifelse(mean(mocap.data.short$pfr_x, na.rm = T) > mean(mocap.data.short$new_rha_x, na.rm = T), 1, 0)
  
  mocap.data.short$new_rha_x <- with(mocap.data.short, ifelse(reversing == 1, (new_rha_x * -1), new_rha_x))
  mocap.data.short$pfr_x <- with(mocap.data.short, ifelse(reversing == 1, (pfr_x * -1), pfr_x))
  mocap.data.short$rsh_x <- with(mocap.data.short, ifelse(reversing == 1, (rsh_x * -1), rsh_x))
  mocap.data.short$rhi_x <- with(mocap.data.short, ifelse(reversing == 1, (rhi_x * -1), rhi_x))
  mocap.data.short$lse_x <- with(mocap.data.short, ifelse(reversing == 1, (lse_x * -1), lse_x))
  mocap.data.short$rse_x <- with(mocap.data.short, ifelse(reversing == 1, (rse_x * -1), rse_x))
  
  #Helper for identifying condition (task load), will be extracted at the end of results below
  cond.helper <- sub("^[^_]*_", "", i)
  
  #Helper for identifying ID number (see results below)
  id = as.numeric(gsub(".*?([0-9]+).*", "\\1", i))
  
## add results

to.add.s <- data.frame(rsh_x_min = min(mocap.data.short$rsh_x, na.rm = T),
                       rsh_x_max = max(mocap.data.short$rsh_x, na.rm = T),
                       rsh_z_min = min(mocap.data.short$rsh_z, na.rm = T),
                       rsh_z_max = max(mocap.data.short$rsh_z, na.rm = T),
                       rhi_x_min = min(mocap.data.short$rhi_x, na.rm = T),
                       rhi_x_max = max(mocap.data.short$rhi_x, na.rm = T),
                       new_rha_x_min = min(mocap.data.short$new_rha_x, na.rm = T),
                       new_rha_x_max = max(mocap.data.short$new_rha_x, na.rm = T),
                       pfr_x_min = min(mocap.data.short$pfr_x, na.rm = T),
                       pfr_x_max = max(mocap.data.short$pfr_x, na.rm = T),
                       info = paste(i), 
                       id = paste(id))

results.s <- rbind(results.s, to.add.s)

}

results.s <- results.s[-1, ] # Remove the first empty observation in results.s       
results.s #check results.s
## Run all files in a loop for variable extraction

for (i in mocap.list$files) {
 
  #Read data
  data.mocap <- read.csv(paste(path.data, i, sep = ""), sep = ",")
  
  #read time list (when rowing trial starts and stops)
  timelist <- read.csv("C:/Users/henrher/OneDrive - Universitetet i Oslo/Tests&Data/PhD Data Rowing/QTM Processed Files/timedata_mocap.csv")
 
  #trim data based on filename and time list
  
  i.filename <- gsub(".*/","",i)

  crop.times <- timelist[grepl(i.filename, timelist$Filename),]

  mocap.data.short <- data.mocap[data.mocap$time >= crop.times$Start_Analysis & data.mocap$time <= crop.times$Stop_Analysis,]
  
  #Replace zero values (missing data) with NA
  mocap.data.short[mocap.data.short == 0] <- NA
  
 
  ## REVERSING OF VALUES IN CERTAIN TRIALS
    # For some trials (not all), my Qualisys model confuses right and left handle marker
    # For these trials, switch right/left handle values
  
  ## First, calculate absolute proximity, depthwise, from right handle (rha_y) to right shoulder (rsh_y)
  
  mocap.data.short["right.handle.prox.rsh_y"] <- mocap.data.short$rha_y - mocap.data.short$rsh_y
  mocap.data.short$right.handle.prox.rsh_y <- abs(mocap.data.short$right.handle.prox.rsh_y)
  
  mocap.data.short["left.handle.prox.rsh_y"] <- mocap.data.short$lha_y - mocap.data.short$rsh_y
  mocap.data.short$left.handle.prox.rsh_y <- abs(mocap.data.short$left.handle.prox.rsh_y)
  
  ## Calculate which handle marker is closer to right shoulder
    #and set up for a check to see if left/right switches during trial (see results section at the bottom)
  mocap.data.short["left.right.handle.distance.to.rha_y"] <- mocap.data.short$right.handle.prox.rsh_y - mocap.data.short$left.handle.prox.rsh_y
  
  mocap.data.short["left.right.handle.distance.to.rha_y.mean"] <- mean(mocap.data.short$right.handle.prox.rsh_y, na.rm = T) - mean(mocap.data.short$left.handle.prox.rsh_y, na.rm = T)
  
  ## Switch if left handle values are closer to right shoulder
  mocap.data.short["new_rha_x"] <- mocap.data.short$rha_x
  mocap.data.short$new_rha_x <- with(mocap.data.short, ifelse(left.right.handle.distance.to.rha_y.mean > 0, lha_x, new_rha_x))
 
  mocap.data.short["new_rha_y"] <- mocap.data.short$rha_y
  mocap.data.short$new_rha_y <- with(mocap.data.short, ifelse(left.right.handle.distance.to.rha_y.mean > 0, lha_y, new_rha_y))
  
  mocap.data.short["new_rha_z"] <- mocap.data.short$rha_z
  mocap.data.short$new_rha_z <- with(mocap.data.short, ifelse(left.right.handle.distance.to.rha_y.mean > 0, lha_z, new_rha_z))
  
  
  
  ## To check that hip/seat/shoulder marker did not get mixed during trials, add check of this as well (similar to the approach for handle)
  
  #Shoulder check against seat
  
  mocap.data.short["right.sh.prox.rse_y"] <- mocap.data.short$rsh_y - mocap.data.short$rse_y
  mocap.data.short$right.sh.prox.rse_y <- abs(mocap.data.short$right.sh.prox.rse_y)
  
  mocap.data.short["left.sh.prox.rse_y"] <- mocap.data.short$lsh_y - mocap.data.short$rse_y
  mocap.data.short$left.sh.prox.rse_y <- abs(mocap.data.short$left.sh.prox.rse_y)
  
  mocap.data.short["left.right.sh.distance.to.rse_y"] <- mocap.data.short$right.sh.prox.rse_y - mocap.data.short$left.sh.prox.rse_y
  
  #Hip check against shoulder
  
  mocap.data.short["right.hip.prox.rsh_y"] <- mocap.data.short$rhi_y - mocap.data.short$rsh_y
  mocap.data.short$right.hip.prox.rsh_y <- abs(mocap.data.short$right.hip.prox.rsh_y)
  
  mocap.data.short["left.hip.prox.rsh_y"] <- mocap.data.short$lhi_y - mocap.data.short$rsh_y
  mocap.data.short$left.hip.prox.rsh_y <- abs(mocap.data.short$left.hip.prox.rsh_y)
  
  mocap.data.short["left.right.hip.distance.to.rsh_y"] <- mocap.data.short$right.hip.prox.rsh_y - mocap.data.short$left.hip.prox.rsh_y
  
  #Seat check against hip
  
  mocap.data.short["right.seat.prox.rhi_y"] <- mocap.data.short$rse_y - mocap.data.short$rhi_y
  mocap.data.short$right.seat.prox.rhi_y <- abs(mocap.data.short$right.seat.prox.rhi_y)
  
  mocap.data.short["left.seat.prox.rhi_y"] <- mocap.data.short$lse_y - mocap.data.short$rhi_y
  mocap.data.short$left.seat.prox.rhi_y <- abs(mocap.data.short$left.seat.prox.rhi_y)
  
  mocap.data.short["left.right.seat.distance.to.rhi_y"] <- mocap.data.short$right.seat.prox.rhi_y - mocap.data.short$left.seat.prox.rhi_y
 
  ## Due to calibration differences, the x axis (horizontal) values are reversed for a few trials
  ## For these trials, the x variables' values should be reversed, so that the same travel direction leads to the same value change for all trials
  
  ## Specifically: Switch direction for key values on x axis if 
    #mean ergometer front value is greater than mean handle value
    
  
  mocap.data.short["reversing"] <- ifelse(mean(mocap.data.short$pfr_x, na.rm = T) > mean(mocap.data.short$new_rha_x, na.rm = T), 1, 0)
  
  mocap.data.short$new_rha_x <- with(mocap.data.short, ifelse(reversing == 1, (new_rha_x * -1), new_rha_x))
  mocap.data.short$pfr_x <- with(mocap.data.short, ifelse(reversing == 1, (pfr_x * -1), pfr_x))
  mocap.data.short$rsh_x <- with(mocap.data.short, ifelse(reversing == 1, (rsh_x * -1), rsh_x))
  mocap.data.short$rhi_x <- with(mocap.data.short, ifelse(reversing == 1, (rhi_x * -1), rhi_x))
  mocap.data.short$lse_x <- with(mocap.data.short, ifelse(reversing == 1, (lse_x * -1), lse_x))
  mocap.data.short$rse_x <- with(mocap.data.short, ifelse(reversing == 1, (rse_x * -1), rse_x))
 
  ## SMOOTHNESS (JERK) CALCULATIONS
  
  ## Choose seat to be included for analysis (by number of NAs in z axis)
  
  percent.na.lse_z.overall <- (sum(is.na(mocap.data.short$lse_z))/nrow(mocap.data.short)*100)
  percent.na.rse_z.overall <- (sum(is.na(mocap.data.short$rse_z))/nrow(mocap.data.short)*100)
  
  mocap.data.short["seat_choice"] <- ifelse(percent.na.lse_z.overall < percent.na.rse_z.overall, "left", "right")
  
  mocap.data.short["chosen_seat_z"] <- ifelse(mocap.data.short$seat_choice == "left", mocap.data.short$lse_z, mocap.data.short$rse_z) 
 
  ## Calculate velocity, acceleration, and jerk for chosen seat_z
  
  ## Savitzky-Golay filter
  sg.s <- savitzkyGolay(mocap.data.short$chosen_seat_z, m = 0, p = 3, w = sg.w)
  
  mocap.data.short["chosen_seat_z_sg"] <- c(rep(NA, (sg.w-1)/2), sg.s, rep(NA, (sg.w-1)/2))
  
  mocap.data.short$chosen_seat_z_sg <- abs(mocap.data.short$chosen_seat_z_sg)
  
  diff_seat_sg <- ave(mocap.data.short$chosen_seat_z_sg, FUN = function(x) c(NA, diff(x)))
  
  diff_time <- ave(mocap.data.short$time, FUN = function(x) c(NA, diff(x)))
  
  mocap.data.short["seat_velo_sg"] <- diff_seat_sg / diff_time
  
  diff_seat_velo_sg <- ave(mocap.data.short$seat_velo_sg, FUN = function(x) c(NA, diff(x)))
  
  mocap.data.short["seat_acceleration_sg"] <- diff_seat_velo_sg/diff_time
  
  diff_seat_acceleration_sg <- ave(mocap.data.short$seat_acceleration_sg, FUN = function(x) c(NA, diff(x)))
  
  mocap.data.short["seat_jerk_z_sg"] <- diff_seat_acceleration_sg/diff_time
  
  ## Smooth handle data, then calculate velocity, acceleration, and jerk for handle

  ## Savitzky-Golay filter
  sg.h <- savitzkyGolay(mocap.data.short$new_rha_x, m = 0, p = 3, w = sg.w)
  
  mocap.data.short["new_rha_x_sg"] <- c(rep(NA, (sg.w-1)/2), sg.h, rep(NA, (sg.w-1)/2))
 
  mocap.data.short$new_rha_x_sg <- abs(mocap.data.short$new_rha_x_sg)
  
  diff_handle_sg <- ave(mocap.data.short$new_rha_x_sg, FUN = function(x) c(NA, diff(x)))
 
  diff_time <- ave(mocap.data.short$time, FUN = function(x) c(NA, diff(x)))
  
  mocap.data.short["handle_velo_sg"] <- diff_handle_sg / diff_time
  
  diff_handle_velo_sg <- ave(mocap.data.short$handle_velo_sg, FUN = function(x) c(NA, diff(x)))
 
  mocap.data.short["handle_acceleration_sg"] <- diff_handle_velo_sg/diff_time
  
  diff_handle_acceleration_sg <- ave(mocap.data.short$handle_acceleration_sg, FUN = function(x) c(NA, diff(x)))
  
  mocap.data.short["handle_jerk_sg"] <- diff_handle_acceleration_sg/diff_time
  
  ## IDENTIFY ROWING PHASES

  ## First, find the catch/finish, where travel speed in handle slows and direction changes
  
  mocap.data.short["handle_direction"] <- ave(mocap.data.short$new_rha_x, FUN = function(x) c(NA, diff(x)))
  
  mocap.data.short["prev_handle_direction"] <- c(mocap.data.short$handle_direction[-1], NA)
  
  mocap.data.short["shift"] <- mocap.data.short$handle_direction * mocap.data.short$prev_handle_direction

  mocap.data.short["shift_yes_no"] <- ifelse(mocap.data.short$shift <= 0, 0, 1)
  
  mocap.data.short["shift_by_handle"] <- mocap.data.short$shift_yes_no * mocap.data.short$handle_direction
  
  mocap.data.short$shift_by_handle <- abs(mocap.data.short$shift_by_handle)
  
  ## Some rowers change direction several times near the catch/finish phase
  ## Thus, add the shift value to the 25 previous rows in the data set, to identify as same shift

  mocap.data.short["prev1.shift_by_handle"] <- c(mocap.data.short$shift_by_handle[-1], NA)
  mocap.data.short["prev2.shift_by_handle"] <- c(mocap.data.short$prev1.shift_by_handle[-1], NA)
  mocap.data.short["prev3.shift_by_handle"] <- c(mocap.data.short$prev2.shift_by_handle[-1], NA)
  mocap.data.short["prev4.shift_by_handle"] <- c(mocap.data.short$prev3.shift_by_handle[-1], NA)
  mocap.data.short["prev5.shift_by_handle"] <- c(mocap.data.short$prev4.shift_by_handle[-1], NA)
  
  mocap.data.short["prev6.shift_by_handle"] <- c(mocap.data.short$prev5.shift_by_handle[-1], NA)
  mocap.data.short["prev7.shift_by_handle"] <- c(mocap.data.short$prev6.shift_by_handle[-1], NA)
  mocap.data.short["prev8.shift_by_handle"] <- c(mocap.data.short$prev7.shift_by_handle[-1], NA)
  mocap.data.short["prev9.shift_by_handle"] <- c(mocap.data.short$prev8.shift_by_handle[-1], NA)
  mocap.data.short["prev10.shift_by_handle"] <- c(mocap.data.short$prev9.shift_by_handle[-1], NA)
  
  mocap.data.short["prev11.shift_by_handle"] <- c(mocap.data.short$prev10.shift_by_handle[-1], NA)
  mocap.data.short["prev12.shift_by_handle"] <- c(mocap.data.short$prev11.shift_by_handle[-1], NA)
  mocap.data.short["prev13.shift_by_handle"] <- c(mocap.data.short$prev12.shift_by_handle[-1], NA)
  mocap.data.short["prev14.shift_by_handle"] <- c(mocap.data.short$prev13.shift_by_handle[-1], NA)
  mocap.data.short["prev15.shift_by_handle"] <- c(mocap.data.short$prev14.shift_by_handle[-1], NA)
  
  mocap.data.short["prev16.shift_by_handle"] <- c(mocap.data.short$prev15.shift_by_handle[-1], NA)
  mocap.data.short["prev17.shift_by_handle"] <- c(mocap.data.short$prev16.shift_by_handle[-1], NA)
  mocap.data.short["prev18.shift_by_handle"] <- c(mocap.data.short$prev17.shift_by_handle[-1], NA)
  mocap.data.short["prev19.shift_by_handle"] <- c(mocap.data.short$prev18.shift_by_handle[-1], NA)
  mocap.data.short["prev20.shift_by_handle"] <- c(mocap.data.short$prev19.shift_by_handle[-1], NA)
  
  mocap.data.short["prev21.shift_by_handle"] <- c(mocap.data.short$prev20.shift_by_handle[-1], NA)
  mocap.data.short["prev22.shift_by_handle"] <- c(mocap.data.short$prev21.shift_by_handle[-1], NA)
  mocap.data.short["prev23.shift_by_handle"] <- c(mocap.data.short$prev22.shift_by_handle[-1], NA)
  mocap.data.short["prev24.shift_by_handle"] <- c(mocap.data.short$prev23.shift_by_handle[-1], NA)
  mocap.data.short["prev25.shift_by_handle"] <- c(mocap.data.short$prev24.shift_by_handle[-1], NA)
  
  ## Decide on catch/finish based on handle distance traveled (less than 1 mm since last frame (in current frame and last 25 frames)) and proximity to the front of the ergometer
 
   mocap.data.short["distance_handle_front"] <- mocap.data.short$new_rha_x - mocap.data.short$pfr_x
  
  mocap.data.short["catch_finish_area"] <- ifelse(mocap.data.short$shift_by_handle <= 1 | mocap.data.short$prev1.shift_by_handle <= 1 | mocap.data.short$prev2.shift_by_handle <= 1 | mocap.data.short$prev3.shift_by_handle <= 1 | mocap.data.short$prev4.shift_by_handle <= 1 | mocap.data.short$prev5.shift_by_handle <= 1 | mocap.data.short$prev6.shift_by_handle <= 1 | mocap.data.short$prev7.shift_by_handle <= 1 | mocap.data.short$prev8.shift_by_handle <= 1 | mocap.data.short$prev9.shift_by_handle <= 1 | mocap.data.short$prev10.shift_by_handle <= 1 | mocap.data.short$prev11.shift_by_handle <= 1 | mocap.data.short$prev12.shift_by_handle <= 1 | mocap.data.short$prev13.shift_by_handle <= 1 | mocap.data.short$prev14.shift_by_handle <= 1 | mocap.data.short$prev15.shift_by_handle <= 1 | mocap.data.short$prev16.shift_by_handle <= 1 | mocap.data.short$prev17.shift_by_handle <= 1 | mocap.data.short$prev18.shift_by_handle <= 1 | mocap.data.short$prev19.shift_by_handle <= 1 | mocap.data.short$prev20.shift_by_handle <= 1 | mocap.data.short$prev21.shift_by_handle <= 1 | mocap.data.short$prev21.shift_by_handle <= 1 | mocap.data.short$prev22.shift_by_handle <= 1 | mocap.data.short$prev23.shift_by_handle <= 1 | mocap.data.short$prev24.shift_by_handle <= 1 | mocap.data.short$prev25.shift_by_handle <= 1, 1, 0)
  mocap.data.short["catch"] <- ifelse(mocap.data.short$catch_finish_area == 1 & mocap.data.short$distance_handle_front < 300, 1, 0)
  mocap.data.short["finish"] <- ifelse(mocap.data.short$catch_finish_area == 1 & mocap.data.short$distance_handle_front > 1000, 1, 0)

  ##Identify drive/recovery phases based on travel direction, in frames not part of a shift (catch/finish) phase
  
  mocap.data.short["direction"]  <-  ave(mocap.data.short$new_rha_x, FUN = function(x) c(NA, diff(x)))
  mocap.data.short["drive"] <- ifelse(mocap.data.short$direction > 0 & mocap.data.short$catch == 0 & mocap.data.short$finish == 0, 1, 0)
  mocap.data.short["recovery"] <- ifelse(mocap.data.short$direction < 0 & mocap.data.short$catch == 0 & mocap.data.short$finish == 0, 1, 0)
 

##Filter to datasets for main phases
  
  drive.data <- mocap.data.short %>% filter(drive == 1)
  recovery.data <- mocap.data.short %>% filter(recovery == 1)
  
  catch.data <- mocap.data.short %>% filter(catch == 1)
  finish.data <- mocap.data.short %>% filter(finish == 1)
 
 ## VALUE STANDARDIZATION 
 ## Explanation: there are inter-individual differences in body size, and calibration differences
  ##Thus, x axis variables of interest should be standardized to get comparable values
 ## Give variables of interest standardized values (0 to 1 per trial)
  
  #Identify ID number 
  id.n <- as.numeric(gsub(".*?([0-9]+).*", "\\1", i))
 
  #Filter out results for current subject
  results.s.id <- results.s %>% filter(id == id.n)
  
 # Calculate standardized values for rsh_x

 max.x <- max(results.s.id$rsh_x_max, na.rm = T)
 min.x <- min(results.s.id$rsh_x_min, na.rm = T)
 sub.x <- max.x - min.x
 mocap.data.short["rsh_x_standardized"] <- mocap.data.short$rsh_x
 mocap.data.short$rsh_x_standardized <- (mocap.data.short$rsh_x_standardized - min.x)
 mocap.data.short$rsh_x_standardized <- mocap.data.short$rsh_x_standardized / sub.x
 
 
 #Calculate standardized values for rsh_z
 max.x <- max(results.s.id$rsh_z_max, na.rm = T)
 min.x <- min(results.s.id$rsh_z_min, na.rm = T)
 sub.x <- max.x - min.x
 mocap.data.short["rsh_z_standardized"] <- mocap.data.short$rsh_z
 mocap.data.short$rsh_z_standardized <- (mocap.data.short$rsh_z_standardized - min.x)
 mocap.data.short$rsh_z_standardized <- mocap.data.short$rsh_z_standardized / sub.x

 #Calculate standardized values for rhi_x
 max.x <- max(results.s.id$rhi_x_max, na.rm = T)
 min.x <- min(results.s.id$rhi_x_min, na.rm = T)
 sub.x <- max.x - min.x
 mocap.data.short["rhi_x_standardized"] <- mocap.data.short$rhi_x
 mocap.data.short$rhi_x_standardized <- (mocap.data.short$rhi_x_standardized - min.x)
 mocap.data.short$rhi_x_standardized <- mocap.data.short$rhi_x_standardized / sub.x
 
 #Calculate standardized values for rha_x
 max.x <- max(results.s.id$new_rha_x_max, na.rm = T)
 min.x <- min(results.s.id$new_rha_x_min, na.rm = T)
 sub.x <- max.x - min.x
 mocap.data.short["new_rha_x_standardized"] <- mocap.data.short$new_rha_x
 mocap.data.short$new_rha_x_standardized <- (mocap.data.short$new_rha_x_standardized - min.x)
 mocap.data.short$new_rha_x_standardized <- mocap.data.short$new_rha_x_standardized / sub.x
 
 #Calculate standardized values for pfr_x
 max.x <- max(results.s.id$pfr_x_max, na.rm = T)
 min.x <- min(results.s.id$pfr_x_min, na.rm = T)
 sub.x <- max.x - min.x
 mocap.data.short["pfr_x_standardized"] <- mocap.data.short$pfr_x
 mocap.data.short$pfr_x_standardized <- (mocap.data.short$pfr_x_standardized - min.x)
 mocap.data.short$pfr_x_standardized <- mocap.data.short$pfr_x_standardized / sub.x

 ## Calculate shoulder to hip, and handle to shoulder, variables
 mocap.data.short["sh_to_hip"] <- mocap.data.short$rsh_x - mocap.data.short$rhi_x
 mocap.data.short["ha_to_sh"] <- mocap.data.short$new_rha_x - mocap.data.short$rsh_x
 ## Calculate standardized shoulder to hip, and handle to shoulder, variables
 mocap.data.short["sh_to_hip_standardized"] <- mocap.data.short$rsh_x_standardized - mocap.data.short$rhi_x_standardized
 mocap.data.short["ha_to_sh_standardized"] <- mocap.data.short$new_rha_x_standardized - mocap.data.short$rsh_x_standardized
 
  ## TEMPORAL COUPLING CALCULATION
  ## Calculate the timing/sequencing of leg extension and trunk rotation in drive and recovery (rockover)
  
 # First: Calculate passing point for sh to hip (where shoulder passes hip)
  
  mocap.data.short["prev_sh_to_hip"] <- c(mocap.data.short$sh_to_hip[-1], NA)
  mocap.data.short["passing_sh_hip"] <- mocap.data.short$sh_to_hip * mocap.data.short$prev_sh_to_hip
  mocap.data.short["rockover_point"] <- ifelse(mocap.data.short$passing_sh_hip < 0 & mocap.data.short$recovery == 1, 1, 0)
  mocap.data.short["drive_point"] <- ifelse(mocap.data.short$passing_sh_hip < 0 & mocap.data.short$drive == 1, 1, 0)
  
  ## Create data sets for passing points (for drive and recovery (rockover))
  drive.seq.data <- mocap.data.short %>% filter(drive_point == 1)
  rockover.data <- mocap.data.short %>% filter(rockover_point == 1)
  
 
  ##Count the occurence of different phases of the rowing cycle
  #create diff for calculation of the amount of phases (count will be added in the Results section)
  diff_drive <- ave(mocap.data.short$drive, FUN = function(x) c(NA, diff(x)))
  diff_recovery <- ave(mocap.data.short$recovery, FUN = function(x) c(NA, diff(x)))
  diff_catch <- ave(mocap.data.short$catch, FUN = function(x) c(NA, diff(x)))
  diff_finish <- ave(mocap.data.short$finish, FUN = function(x) c(NA, diff(x)))
 
  
  ##STROKE RATE CONSISTENCY CALCULATION
  ##Identify time between catch phases
 
  #Filter out catch data which is part of same catch
  catch.data["timediff"]  <-  ave(catch.data$time, FUN = function(x) c(NA, diff(x)))

  catch.data.short <- catch.data %>% filter(timediff > .3)

  ##STROKE LENGTHS
  ##Note: These can be extracted from the RP3 ergometer
  ##This code shows how to calculate it from mocap recordings
  #for stroke length, calculate handle distance traveled between catch and finish
  
 #first, filter out data where handle direction shifts
  shift.area <- mocap.data.short %>% filter(shift < 0)
  
  #Filter out data part of same direction shift
  shift.area["timediff"]  <-  ave(shift.area$time, FUN = function(x) c(NA, diff(x)))
 
  shift.area.short <- shift.area %>% filter(timediff > .3)
  
  #make stroke length variable
  #Calculate distance between shift areas (catch and finish)
  shift.area.short["stroke.length"] <- ave(shift.area.short$new_rha_x, FUN = function(x) c(NA, diff(x)))
  stroke.length.data <- shift.area.short %>% filter(stroke.length < 0)
  stroke.length.data$stroke.length <- abs(stroke.length.data$stroke.length)
  
  #Helper for identifying condition (task load) and order, will be extracted at the end of results below
  cond.helper <- sub("^[^_]*_", "", i)
  
  order.helper <- sub("_mocap.csv.*", "", i)  
  trial.order.n <- str_sub(order.helper, -1)
  

## add results

to.add <- data.frame(percent.na.rsh_x.overall = (sum(is.na(mocap.data.short$rsh_x))/nrow(mocap.data.short)*100),
                     percent.na.rsh_x.drive = (sum(is.na(drive.data$rsh_x))/nrow(drive.data)*100),
                     percent.na.rsh_x.drive.seq.area = (sum(is.na(drive.seq.data$rsh_x))/nrow(drive.seq.data)*100),
                     percent.na.rsh_x.recovery = (sum(is.na(recovery.data$rsh_x))/nrow(recovery.data)*100),
                     percent.na.rsh_x.rockover.area = (sum(is.na(rockover.data$rsh_x))/nrow(rockover.data)*100),
                     percent.na.rsh_x.catch = (sum(is.na(catch.data$rsh_x))/nrow(catch.data)*100),
                     percent.na.rsh_x.finish = (sum(is.na(finish.data$rsh_x))/nrow(finish.data)*100),
                     percent.na.rsh_z.overall = (sum(is.na(mocap.data.short$rsh_z))/nrow(mocap.data.short)*100),
                     percent.na.rsh_z.drive = (sum(is.na(drive.data$rsh_z))/nrow(drive.data)*100),
                     percent.na.rsh_z.drive.seq.area = (sum(is.na(drive.seq.data$rsh_z))/nrow(drive.seq.data)*100),
                     percent.na.rsh_z.recovery = (sum(is.na(recovery.data$rsh_z))/nrow(recovery.data)*100),
                     percent.na.rsh_z.rockover.area = (sum(is.na(rockover.data$rsh_z))/nrow(rockover.data)*100),
                     percent.na.rsh_z.catch = (sum(is.na(catch.data$rsh_z))/nrow(catch.data)*100),
                     percent.na.rsh_z.finish = (sum(is.na(finish.data$rsh_z))/nrow(finish.data)*100),
                     percent.na.rhi_x.overall = (sum(is.na(mocap.data.short$rhi_x))/nrow(mocap.data.short)*100),
                     percent.na.rhi_x.drive = (sum(is.na(drive.data$rhi_x))/nrow(drive.data)*100),
                     percent.na.rhi_x.recovery = (sum(is.na(recovery.data$rhi_x))/nrow(recovery.data)*100),
                     percent.na.rhi_x.catch = (sum(is.na(catch.data$rhi_x))/nrow(catch.data)*100),
                     percent.na.rhi_x.finish = (sum(is.na(finish.data$rhi_x))/nrow(finish.data)*100),
                     percent.na.new_rha_x.overall = (sum(is.na(mocap.data.short$new_rha_x))/nrow(mocap.data.short)*100),
                     percent.na.new_rha_x_sg.overall = (sum(is.na(mocap.data.short$new_rha_x_sg))/nrow(mocap.data.short)*100),
                     percent.na.new_rha_x.drive = (sum(is.na(drive.data$new_rha_x))/nrow(drive.data)*100),
                     percent.na.new_rha_x.drive.seq.area = (sum(is.na(drive.seq.data$new_rha_x))/nrow(drive.seq.data)*100),
                     percent.na.new_rha_x.recovery = (sum(is.na(recovery.data$new_rha_x))/nrow(recovery.data)*100),
                     percent.na.new_rha_x.rockover.area = (sum(is.na(rockover.data$new_rha_x))/nrow(rockover.data)*100),
                     percent.na.new_rha_x.catch = (sum(is.na(catch.data$new_rha_x))/nrow(catch.data)*100),
                     percent.na.new_rha_x.finish = (sum(is.na(finish.data$new_rha_x))/nrow(finish.data)*100),
                     percent.na.pfr_x.overall = (sum(is.na(mocap.data.short$pfr_x))/nrow(mocap.data.short)*100),
                     percent.na.pfr_x.drive = (sum(is.na(drive.data$pfr_x))/nrow(drive.data)*100),
                     percent.na.pfr_x.drive.seq.area = (sum(is.na(drive.seq.data$pfr_x))/nrow(drive.seq.data)*100),
                     percent.na.pfr_x.recovery = (sum(is.na(recovery.data$pfr_x))/nrow(recovery.data)*100),
                     percent.na.pfr_x.rockover.area = (sum(is.na(rockover.data$pfr_x))/nrow(rockover.data)*100),
                     percent.na.pfr_x.catch = (sum(is.na(catch.data$pfr_x))/nrow(catch.data)*100),
                     percent.na.pfr_x.finish = (sum(is.na(finish.data$pfr_x))/nrow(finish.data)*100),
                     percent.na.chosen.seat_z = (sum(is.na(mocap.data.short$chosen_seat_z))/nrow(mocap.data.short)*100),
                     percent.na.chosen_seat_z_sg = (sum(is.na(mocap.data.short$chosen_seat_z_sg))/nrow(mocap.data.short)*100),
                     max_pfr_x_check = max(mocap.data.short$pfr_x, na.rm = T),
                      mean_shoulder_location_x_drive_seq = mean(drive.seq.data$rsh_x, na.rm = T),  
                      mean_hip_location_x_drive_seq = mean(drive.seq.data$rhi_x, na.rm = T), 
                      mean_shoulder_location_x_rockover = mean(rockover.data$rsh_x, na.rm = T),  
                      mean_hip_location_x_rockover = mean(rockover.data$rhi_x, na.rm = T),  
                      mean_standardized_leg_travel_at_drive_seq = mean(drive.seq.data$pfr_x_standardized, na.rm = T),
                      mean_standardized_leg_travel_at_drive_seq_percent = ((1-mean(drive.seq.data$pfr_x_standardized, na.rm = T))*100),
                      sd_standardized_leg_travel_at_drive_seq = sd(drive.seq.data$pfr_x_standardized, na.rm = T),
                      min_standardized_leg_travel_at_drive_seq = min(drive.seq.data$pfr_x_standardized, na.rm = T),
                      max_standardized_leg_travel_at_drive_seq = max(drive.seq.data$pfr_x_standardized, na.rm = T),
                      mean_standardized_leg_travel_at_rockover = mean(rockover.data$pfr_x_standardized, na.rm = T),
                      mean_standardized_leg_travel_at_rockover_percent = ((1-mean(rockover.data$pfr_x_standardized, na.rm = T))*100),
                      sd_standardized_leg_travel_at_rockover = sd(rockover.data$pfr_x_standardized, na.rm = T),
                      min_standardized_leg_travel_at_rockover = min(rockover.data$pfr_x_standardized, na.rm = T),
                      max_standardized_leg_travel_at_rockover = max(rockover.data$pfr_x_standardized, na.rm = T),
                      mean_standardized_handle_travel_at_drive_seq = mean(drive.seq.data$new_rha_x_standardized, na.rm = T),
                      sd_standardized_handle_travel_at_drive_seq = sd(drive.seq.data$new_rha_x_standardized, na.rm = T),
                      min_standardized_handle_travel_at_drive_seq = min(drive.seq.data$new_rha_x_standardized, na.rm = T),
                      max_standardized_handle_travel_at_drive_seq = max(drive.seq.data$new_rha_x_standardized, na.rm = T),
                      mean_standardized_handle_travel_at_rockover = mean(rockover.data$new_rha_x_standardized, na.rm = T),
                      sd_standardized_handle_travel_at_rockover = sd(rockover.data$new_rha_x_standardized, na.rm = T),
                      min_standardized_handle_travel_at_rockover = min(rockover.data$new_rha_x_standardized, na.rm = T),
                      max_standardized_handle_travel_at_rockover = max(rockover.data$new_rha_x_standardized, na.rm = T),
                      mean_seat_location_z = mean(mocap.data.short$chosen_seat_z, na.rm = T), 
                      sd_seat_location_z = sd(mocap.data.short$chosen_seat_z, na.rm = T),
                      mean_seat_jerk_z_sg = mean(abs(mocap.data.short$seat_jerk_z_sg), na.rm = T), 
                      sd_seat_jerk_z_sg = sd(abs(mocap.data.short$seat_jerk_z_sg), na.rm = T),
                      mean_velocity_sg_handle_overall = mean(abs(mocap.data.short$handle_velo_sg), na.rm = T),
                      mean_acceleration_sg_handle_overall = mean(abs(mocap.data.short$handle_acceleration_sg), na.rm = T),
                      mean_jerk_sg_handle_overall = mean(abs(mocap.data.short$handle_jerk_sg), na.rm = T),
                      mean_jerk_sg_handle_drive = mean(abs(drive.data$handle_jerk_sg), na.rm = T),
                      mean_jerk_sg_handle_recovery = mean(abs(recovery.data$handle_jerk_sg), na.rm = T),
                      mean_jerk_sg_handle_catch = mean(abs(catch.data$handle_jerk_sg), na.rm = T),
                      mean_jerk_sg_handle_finish = mean(abs(finish.data$handle_jerk_sg), na.rm = T),
                      mean_handle_distance_check_negative = mean(mocap.data.short$right.handle.prox.rsh_y,  na.rm = T) - mean(mocap.data.short$left.handle.prox.rsh_y,  na.rm = T),
                      min_and_max_left_right_handle_distance_check = max(mocap.data.short$left.right.handle.distance.to.rha_y, na.rm = T) * min(mocap.data.short$left.right.handle.distance.to.rha_y,  na.rm = T),
                      mean_sh_distance_check_negative = mean(mocap.data.short$right.hip.prox.rsh_y,  na.rm = T) - mean(mocap.data.short$left.hip.prox.rsh_y,  na.rm = T), 
                      min_and_max_left_right_sh_distance_check = max(mocap.data.short$left.right.sh.distance.to.rse_y, na.rm = T) * min(mocap.data.short$left.right.sh.distance.to.rse_y,  na.rm = T),
                      mean_hip_distance_check_negative = mean(mocap.data.short$right.hip.prox.rsh_y,  na.rm = T) - mean(mocap.data.short$left.hip.prox.rsh_y,  na.rm = T), 
                      min_and_max_left_right_hip_distance_check = max(mocap.data.short$left.right.hip.distance.to.rsh_y, na.rm = T) * min(mocap.data.short$left.right.hip.distance.to.rsh_y,  na.rm = T),
                      mean_seat_distance_check_negative = mean(mocap.data.short$right.seat.prox.rhi_y,  na.rm = T) - mean(mocap.data.short$left.seat.prox.rhi_y,  na.rm = T),
                      min_and_max_left_right_seat_distance_check = max(mocap.data.short$left.right.seat.distance.to.rhi_y, na.rm = T) * min(mocap.data.short$left.right.seat.distance.to.rhi_y,  na.rm = T),
                      check_who_switched_handle_mocap = mean(abs(mocap.data.short$rha_x), na.rm = T) - mean(abs(mocap.data.short$new_rha_x), na.rm = T),
                      mean_shoulder_location_vertical_overall = mean(mocap.data.short$rsh_z, na.rm = T),  
                      sd_shoulder_location_vertical_overall = sd(mocap.data.short$rsh_z, na.rm = T),
                      mean_shoulder_location_vertical_overall_standardized = mean(mocap.data.short$rsh_z_standardized, na.rm = T),  
                      sd_shoulder_location_vertical_overall_standardized = sd(mocap.data.short$rsh_z_standardized, na.rm = T),
                      mean_time_between_catches = mean(catch.data.short$timediff, na.rm = T),
                      sd_time_between_catches = sd(catch.data.short$timediff, na.rm = T),
                      min_time_between_catches = min(catch.data.short$timediff, na.rm = T),
                      max_time_between_catches = max(catch.data.short$timediff, na.rm = T),
                      stroke_count = nrow(stroke.length.data),
                      mean_stroke_length = mean(stroke.length.data$stroke.length, na.rm = T),
                      sd_stroke_length = sd(stroke.length.data$stroke.length, na.rm = T),
                      min_stroke_length = min(stroke.length.data$stroke.length, na.rm = T),
                      max_stroke_length = max(stroke.length.data$stroke.length, na.rm = T),
                      count_drives = length(which(diff_drive == -1)),
                      count_recoverys = length(which(diff_recovery==-1)),
                      count_catches = length(which(diff_catch==-1)),
                      count_finish = length(which(diff_finish==-1)),
                      count_rockover_points = length(which(mocap.data.short$rockover_point==1)),
                      count_drive_seq_points = length(which(mocap.data.short$drive_point==1)),
                      info = paste(i), 
                      id = as.numeric(gsub(".*?([0-9]+).*", "\\1", i)), 
                      cond_full = substr(cond.helper, 1, 4), 
                      cond_order = paste(trial.order.n),
                      eliteness = ifelse(id <= 9, "elite", "non-elite"),
                      check.max.x = paste(max.x))
                     
results <- rbind(results, to.add)

}

results.mocap <- results[-1, ] # Remove the first empty observation after running the loop         
results.mocap # Check results
#Write your results to preferred location
write.table(results.mocap, file='C:/Users/henrher/OneDrive - Universitetet i Oslo/Tests&Data/PhD Data Rowing/QTM Processed Files/New_Dataset_Mocap_Feb24_2023.txt')

##BAR GRAPHS
##Below I have adopted code from http://www.cookbook-r.com/Graphs/
  #to create bar graphs displaying leg extension across cognitive load and skill levels,
    #when shoulder passed hips in drive and recovery, respectively

##First, install/load relevant packages
library(ggplot2)
install.packages("ggplot2")
install.packages("Rmisc")
library(Rmisc)

##Upload your results data in long format
long.kinematics.data <- read.csv("C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and analysis/Pupil and Analysis/Results and Datasets 2022/mocap/dataset for jasp/Long_Kinematics_Data_for_JASP.csv")
long.kinematics.data # Check data

#If you want, you can add variables to this dataset
  #In my case, i wanted to name low/high mental load as a and, respectively
long.kinematics.data["Mental_Load"] <- long.kinematics.data$cond_full
long.kinematics.data$Mental_Load <- with(long.kinematics.data, ifelse(cond_full == "plml", "a", Mental_Load))
long.kinematics.data$Mental_Load <- with(long.kinematics.data, ifelse(cond_full == "phml", "a", Mental_Load))
long.kinematics.data$Mental_Load <- with(long.kinematics.data, ifelse(cond_full == "plmh", "b", Mental_Load))
long.kinematics.data$Mental_Load <- with(long.kinematics.data, ifelse(cond_full == "phmh", "b", Mental_Load))

#Create plot for data during the recovery (rockover) phase
lkd_ci <- summarySE(long.kinematics.data, measurevar="mean_standardized_leg_travel_at_rockover_percent", groupvars=c("Mental_Load","eliteness"))
lkd_ci
ggplot(lkd_ci, aes(x=Mental_Load, y=mean_standardized_leg_travel_at_rockover_percent, fill=eliteness)) + 
  geom_bar(position=position_dodge(), stat="identity") + ylim(0,85) + coord_cartesian(ylim = c(60, 90)) +
  geom_errorbar(aes(ymin=mean_standardized_leg_travel_at_rockover_percent-ci, ymax=mean_standardized_leg_travel_at_rockover_percent+ci),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9))

#Create plot for data during the drive phase
lkd_drive_ci <- summarySE(long.kinematics.data, measurevar="mean_standardized_leg_travel_at_drive_seq_percent", groupvars=c("Mental_Load","eliteness"))
lkd_drive_ci
ggplot(lkd_drive_ci, aes(x=Mental_Load, y=mean_standardized_leg_travel_at_drive_seq_percent, fill=eliteness)) + 
  geom_bar(position=position_dodge(), stat="identity") + ylim(0,85) + coord_cartesian(ylim = c(60, 90)) +
  geom_errorbar(aes(ymin=mean_standardized_leg_travel_at_drive_seq_percent-ci, ymax=mean_standardized_leg_travel_at_drive_seq_percent+ci),
                width=.4,                    # Width of the error bars
                position=position_dodge(.9))
