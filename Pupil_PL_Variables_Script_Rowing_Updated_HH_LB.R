## Script for processing blinks and pupil data from Pupil Player output
## 
## This script is made by, Henrik Herrebroden, henrikh@henrikh.no,
## and Laura Bishop, laura.bishop@imv.uio.no.

## To run this script, you should have a folder in your working directory with
## csv files exported from Pupil Player. 

# Set these values prior to running the script ----------------------------------------------------

setwd("C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and Analysis/Pupil and Analysis/") # Change this to your local working directory
library(prospectr) # Install & load this library, which you need for the Savitzky-Golay function. 

path.data <- "TestData_PL/" # Subfolder within working directory containing Pupil Lab csv exports

# List the files that are currently in path.data and choose one to process
pupils.list <-  data.frame(list.files(path = path.data, pattern = ".csv", recursive = T)) # Create a list of files in path.data
colnames(pupils.list) <- "files" # Label the column of filenames as "files"
pupils.list # Run this to see the list of files in your folder

# Distinguish between baseline and condition trials
baseline.trials <- pupils.list[grepl("_b", pupils.list$files) & !grepl("DoNotUse", pupils.list$files),]
condition.trials <- pupils.list[grepl("_c", pupils.list$files) & !grepl("DoNotUse", pupils.list$files),]

# These parameters are used for the filtering function & can be changed if needed
choose.resample <- "T" # Do you want to downsample to 60 Hz? T or F
outlier.value <- 2 # Used for defining outlier velocities
low.threshold.pupil <- 2 # Used for cutting off pupil values that are too far below trial mean
high.threshold.pupil <- 3 # Used for cutting off pupil values that are too far above trial mean
sg.w <- 15 # Used in Savitzky-Golay filter (window size); reduce for less smoothing

i <- as.character(pupils.list$files[11]) # Choose a csv file from your list to process

# Read timelist with start/stop times for each trial
timelist <- read.csv("TestData_PL/File information PL HH PhD Rowing.csv")

# Set up results (to be added later)
results <- data.frame(row.loss.cond.right = 0,
                      row.loss.cond.left = 0,
                      row.loss.bl.right = 0,
                      row.loss.bl.left = 0,
                      rows.in.pupil.data.cond.right = 0,
                      rows.in.pupil.data.cond.left = 0,
                      rows.in.pupil.data.bl.right = 0,
                      rows.in.pupil.data.bl.left = 0,
                      pupil.data.percentage.cond.right = 0,
                      pupil.data.percentage.cond.left = 0,
                      pupil.data.percentage.bl.right = 0,
                      pupil.data.percentage.bl.left = 0,
                      count.NA.xmm.sg.cond.left = 0, 
                      count.NA.xmm.sg.cond.right = 0, 
                      count.NA.xmm.sg.bl.left = 0, 
                      count.NA.xmm.sg.bl.right = 0,
                      count.rows.bl.right = 0,
                      count.rows.bl.left = 0,
                      count.rows.cond.right = 0,
                      count.rows.cond.left = 0,
                      max.timegap.bl.right = 0,
                      max.timegap.bl.left = 0,
                      max.timegap.cond.right = 0, 
                      max.timegap.cond.left = 0,
                      min.timegap.bl.right = 0,
                      min.timegap.bl.left = 0,
                      min.timegap.cond.right = 0, 
                      min.timegap.cond.left = 0,
                      mean.timegap.bl.right = 0,
                      mean.timegap.bl.left = 0,
                      mean.timegap.cond.right = 0, 
                      mean.timegap.cond.left = 0,
                      percent.NA.sg.cond.left = 0,  
                      percent.NA.sg.cond.right = 0, 
                      percent.NA.sg.bl.left = 0,  
                      percent.NA.sg.bl.right = 0, 
                      mean.diameter.sg.bl.left = 0, 
                      mean.diameter.sg.bl.right = 0, 
                      sd.diameter.sg.bl.left = 0,
                      sd.diameter.sg.bl.right = 0,
                      mean.diameter.raw.bl.left = 0, 
                      mean.diameter.raw.bl.right = 0, 
                      mean.diameter.sg.cond.left = 0, 
                      mean.diameter.sg.cond.right = 0,
                      sd.diameter.sg.cond.left = 0,
                      sd.diameter.sg.cond.right = 0,
                      mean.diameter.raw.cond.left = 0, 
                      mean.diameter.raw.cond.right = 0,
                      mean.dilation.sg.left = 0,
                      mean.dilation.sg.right = 0,
                      blink.count.cond.left = 0,
                      blink.count.cond.right = 0,
                      blink.count.bl.left = 0,
                      blink.count.bl.right = 0,
                      filename = 0,
                      start.time.cond = 0,
                      end.time.cond = 0,
                      start.time.bl = 0,
                      end.time.bl = 0,
                      ID = 0)

# Filtering function ------------------------------------------------------------------------------

filtering <- function(f) {

  ## Resample down to 60 Hz using a linear interpolation
  if (choose.resample == "T") {
    trial <- with(f, approx(newtime, diameter_3d, xout = seq(min(newtime), max(newtime), by = 1/60)))
    trial <- data.frame(trial)
    trial.posx <- with(f, approx(newtime, norm_pos_x, xout = seq(min(newtime), max(newtime), by = 1/60)))
    trial.posx <- data.frame(trial.posx)
    trial$xposition <- trial.posx$y
    trial.posy <- with(f, approx(newtime, norm_pos_y, xout = seq(min(newtime), max(newtime), by = 1/60)))
    trial.posy <- data.frame(trial.posy)
    trial$yposition <- trial.posy$y
    trial.conf <- with(f, approx(newtime, confidence, xout = seq(min(newtime), max(newtime), by = 1/60)))
    trial.conf <- data.frame(trial.conf)
    trial$confidence <- trial.conf$y
    colnames(trial) <- c("newtime", "diameter_3d", "xposition", "yposition", "confidence")
  } else {
    trial <- trial.0
  }

  ## Calculate cutoff velocity value
  trial["xmm_vel"] <- with(trial, 
                           c(NA, diff(diameter_3d))/c(NA, diff(newtime))) # Derive velocity
  out.bound <- outlier.value * sd(trial$xmm_vel, na.rm = T) # Calculate max allowed velocity
  
  ## Create new variable & omit instances where diameter = 0
  trial["xmm_p"] <- trial$diameter_3d
  trial$xmm_p <- with(trial, ifelse(xmm_p == 0, NA, xmm_p))
  
  ## Create new variable & omit extreme velocities
  trial["xmm_pp"] <- with(trial, ifelse(abs(xmm_vel) > out.bound, NA, xmm_p))
  
  ## Calculate lowest allowed pupil value & remove pupil values below this threshold
  low.bound.pupil <- mean(trial$xmm_pp, na.rm = T) - low.threshold.pupil * sd(trial$xmm_pp, na.rm = T)
  trial$xmm_ppp <- with(trial, ifelse(xmm_pp < low.bound.pupil, NA, xmm_pp))
  
  ## Calculate highest allowed value & remove pupil values above this threshold
  high.bound.pupil <- mean(trial$xmm_pp, na.rm = T) + high.threshold.pupil * sd(trial$xmm_pp, na.rm = T)
  trial$xmm_ppp <- with(trial, ifelse(xmm_pp > high.bound.pupil, NA, xmm_ppp))
  
  ## Savitzky-Golay filter
  sg <- savitzkyGolay(trial$xmm_ppp, m = 0, p = 3, w = sg.w)
  trial["xmm_sg"] <- c(rep(NA, (sg.w-1)/2), sg, rep(NA, (sg.w-1)/2))
  
  trial
}

# Load and process data ---------------------------------------------------------------------------

for (i in condition.trials) {
  i.filename <- matrix(unlist(strsplit(i,"/")),nrow=1)
  i.filename <- gsub(".csv", "", i.filename[,2])
  player.output <- read.csv(paste(path.data, i, sep = ""), sep = ",") # Load csv file
                            
  player.3d <- player.output[player.output$method == "pye3d 0.0.4 real-time" |
                               player.output$method == "3d c++", ]
  
  eye0 <- player.3d[player.3d$eye_id == 0, ] # Isolate data for each eye
  eye1 <- player.3d[player.3d$eye_id == 1, ]
  
  ## Right (eye = 0)
  eye0.ts <- aggregate(diameter_3d ~ pupil_timestamp, data = eye0, mean) # Collapse over timestamp values
  eye0.ts["newtime"] <- with(eye0.ts, 
                             round((pupil_timestamp - pupil_timestamp[1]), 3)) # Create new timestamp starting at 0
  eye0.conf <- aggregate(confidence ~ pupil_timestamp, data = eye0, mean)
  eye0.ts["confidence"] <- eye0.conf$confidence
  eye0.posx <- aggregate(norm_pos_x ~ pupil_timestamp, data = eye0, mean)
  eye0.ts["norm_pos_x"] <- eye0.posx$norm_pos_x
  eye0.posy <- aggregate(norm_pos_y ~ pupil_timestamp, data = eye0, mean)
  eye0.ts["norm_pos_y"] <- eye0.posy$norm_pos_y
  
  ## Left eye (eye = 1)
  eye1.ts <- aggregate(diameter_3d ~ pupil_timestamp, data = eye1, mean) # Collapse over timestamp values
  eye1.ts["newtime"] <- with(eye1.ts, 
                             round((pupil_timestamp - pupil_timestamp[1]), 3)) # Create new timestamp starting at 0
  eye1.conf <- aggregate(confidence ~ pupil_timestamp, data = eye1, mean)
  eye1.ts["confidence"] <- eye1.conf$confidence
  eye1.posx <- aggregate(norm_pos_x ~ pupil_timestamp, data = eye1, mean)
  eye1.ts["norm_pos_x"] <- eye1.posx$norm_pos_x
  eye1.posy <- aggregate(norm_pos_y ~ pupil_timestamp, data = eye1, mean)
  eye1.ts["norm_pos_y"] <- eye1.posy$norm_pos_y
  
  ## Crop to start and end times of individual trials
  crop.times <- timelist[grepl(i.filename, timelist$Filename),]
  eye0.ts <- eye0.ts[eye0.ts$newtime >= crop.times$Start & eye0.ts$newtime <= crop.times$Stop,]
  eye1.ts <- eye1.ts[eye1.ts$newtime >= crop.times$Start & eye1.ts$newtime <= crop.times$Stop,]
  
  ## Run filtering function and write as separate text files for each eye
  filtered.condition.eye0 <- filtering(eye0.ts)
  filtered.condition.eye1 <- filtering(eye1.ts)
  
  ## Count blinks, condition, right
  blink.data.right <- subset(filtered.condition.eye0, diameter_3d == 0)
  timegaps.right <- diff(blink.data.right$newtime)
  blink.count.prel.right <- length(timegaps.right[timegaps.right > 0.4])
  min.value.cond.right <- min(filtered.condition.eye0$diameter_3d)
  blink.count.cond.right <- ifelse(min.value.cond.right > 0, blink.count.prel.right, blink.count.prel.right+1)

  ## Count blinks, condition, left
  blink.data.left <- subset(filtered.condition.eye1, diameter_3d == 0)
  timegaps.left <- diff(blink.data.left$newtime)
  blink.count.prel.left <- length(timegaps.left[timegaps.left > 0.4])
  min.value.cond.left <- min(filtered.condition.eye1$diameter_3d)
  blink.count.cond.left <- ifelse(min.value.cond.left > 0, blink.count.prel.left, blink.count.prel.left+1)
  
  baseline.name <- gsub("_c", "_bl", i)
  baseline <- read.csv(paste(path.data, baseline.name, sep = ""), sep = ",") # Load csv file
  baseline.3d <- baseline[baseline$method == "pye3d 0.0.4 real-time", ]
  eye0 <- baseline.3d[baseline.3d$eye_id == 0, ] # Isolate data for each eye
  eye1 <- baseline.3d[baseline.3d$eye_id == 1, ]
  
  ## Right (eye = 0)
  eye0.ts <- aggregate(diameter_3d ~ pupil_timestamp, data = eye0, mean) # Collapse over timestamp values
  eye0.ts["newtime"] <- with(eye0.ts, 
                             round((pupil_timestamp - pupil_timestamp[1]), 3)) # Create new timestamp starting at 0
  eye0.conf <- aggregate(confidence ~ pupil_timestamp, data = eye0, mean)
  eye0.ts["confidence"] <- eye0.conf$confidence
  eye0.posx <- aggregate(norm_pos_x ~ pupil_timestamp, data = eye0, mean)
  eye0.ts["norm_pos_x"] <- eye0.posx$norm_pos_x
  eye0.posy <- aggregate(norm_pos_y ~ pupil_timestamp, data = eye0, mean)
  eye0.ts["norm_pos_y"] <- eye0.posy$norm_pos_y
  
  ## Left eye (eye = 1)
  eye1.ts <- aggregate(diameter_3d ~ pupil_timestamp, data = eye1, mean) # Collapse over timestamp values
  eye1.ts["newtime"] <- with(eye1.ts, 
                             round((pupil_timestamp - pupil_timestamp[1]), 3)) # Create new timestamp starting at 0
  eye1.conf <- aggregate(confidence ~ pupil_timestamp, data = eye1, mean)
  eye1.ts["confidence"] <- eye1.conf$confidence
  eye1.posx <- aggregate(norm_pos_x ~ pupil_timestamp, data = eye1, mean)
  eye1.ts["norm_pos_x"] <- eye1.posx$norm_pos_x
  eye1.posy <- aggregate(norm_pos_y ~ pupil_timestamp, data = eye1, mean)
  eye1.ts["norm_pos_y"] <- eye1.posy$norm_pos_y
  baseline.name.short <- gsub("_c", "_bl", i.filename)
  
  ## Crop baseline to start/end times
  crop.times <- timelist[grepl(baseline.name.short, timelist$Filename),]
  eye0.ts <- eye0.ts[eye0.ts$newtime >= crop.times$Start & eye0.ts$newtime <= crop.times$Stop,]
  eye1.ts <- eye1.ts[eye1.ts$newtime >= crop.times$Start & eye1.ts$newtime <= crop.times$Stop,]
  
  ## Run filtering function and write as separate text files for each eye
  filtered.baseline.eye0 <- filtering(eye0.ts)
  filtered.baseline.eye1 <- filtering(eye1.ts)
  
  ## Count blinks, baseline, right
  blink.data.bl.right <- subset(filtered.baseline.eye0, diameter_3d == 0)
  timegaps.bl.right <- diff(blink.data.bl.right$newtime)
  blink.count.prel.bl.right <- length(timegaps.bl.right[timegaps.bl.right > 0.4])
  min.value.bl.right <- min(filtered.baseline.eye0$diameter_3d)
  blink.count.bl.right <- ifelse(min.value.bl.right > 0, blink.count.prel.bl.right, blink.count.prel.bl.right+1)
  
  ## Count blinks, baseline, left
  blink.data.bl.left <- subset(filtered.baseline.eye1, diameter_3d == 0)
  timegaps.bl.left <- diff(blink.data.bl.left$newtime)
  blink.count.prel.bl.left <- length(timegaps.bl.left[timegaps.bl.left > 0.4])
  min.value.bl.left <- min(filtered.baseline.eye1$diameter_3d)
  blink.count.bl.left <- ifelse(min.value.bl.left > 0, blink.count.prel.bl.left, blink.count.prel.bl.left+1)
  
  ## Gather time gaps column (newtime diff)
  filtered.condition.eye1["newtime_diff"] <- with(filtered.condition.eye1, c(NA, diff(newtime)))
  filtered.condition.eye0["newtime_diff"] <- with(filtered.condition.eye0, c(NA, diff(newtime)))
  filtered.baseline.eye1["newtime_diff"] <- with(filtered.baseline.eye1, c(NA, diff(newtime)))
  filtered.baseline.eye0["newtime_diff"] <- with(filtered.baseline.eye0, c(NA, diff(newtime)))
 
  ## Add data loss info
  row.loss.cond.right = (9600 - nrow(filtered.condition.eye0))
  row.loss.cond.left = (9600 - nrow(filtered.condition.eye1))
  row.loss.bl.right = (600 - nrow(filtered.baseline.eye0))
  row.loss.bl.left = (600 - nrow(filtered.baseline.eye1))
  rows.in.pupil.data.cond.right = (9600 - (row.loss.cond.right) - sum(is.na(filtered.condition.eye0$xmm_sg)))
  rows.in.pupil.data.cond.left = (9600 - (row.loss.cond.left) - sum(is.na(filtered.condition.eye1$xmm_sg)))
  rows.in.pupil.data.bl.right = (600 - (row.loss.bl.right) - sum(is.na(filtered.baseline.eye0$xmm_sg)))
  rows.in.pupil.data.bl.left = (600 - (row.loss.bl.left) - sum(is.na(filtered.baseline.eye1$xmm_sg)))
  pupil.data.percentage.cond.right = (rows.in.pupil.data.cond.right/9600 *100)
  pupil.data.percentage.cond.left = (rows.in.pupil.data.cond.left/9600 *100)
  pupil.data.percentage.bl.right = (rows.in.pupil.data.bl.right/600 *100)
  pupil.data.percentage.bl.left = (rows.in.pupil.data.bl.left/600 *100)
  
  ## Add results
  to.add <- data.frame(row.loss.cond.right = (9600 - nrow(filtered.condition.eye0)),
                       row.loss.cond.left = (9600 - nrow(filtered.condition.eye1)),
                       row.loss.bl.right = (600 - nrow(filtered.baseline.eye0)),
                       row.loss.bl.left = (600 - nrow(filtered.baseline.eye1)),
                       rows.in.pupil.data.cond.right = (9600 - (row.loss.cond.right) - sum(is.na(filtered.condition.eye0$xmm_sg))),
                       rows.in.pupil.data.cond.left = (9600 - (row.loss.cond.left) - sum(is.na(filtered.condition.eye1$xmm_sg))),
                       rows.in.pupil.data.bl.right = (600 - (row.loss.bl.right) - sum(is.na(filtered.baseline.eye0$xmm_sg))),
                       rows.in.pupil.data.bl.left = (600 - (row.loss.bl.left) - sum(is.na(filtered.baseline.eye1$xmm_sg))),
                       pupil.data.percentage.cond.right = (rows.in.pupil.data.cond.right/9600 *100),
                       pupil.data.percentage.cond.left = (rows.in.pupil.data.cond.left/9600 *100),
                       pupil.data.percentage.bl.right = (rows.in.pupil.data.bl.right/600 *100),
                       pupil.data.percentage.bl.left = (rows.in.pupil.data.bl.left/600 *100),
                       count.NA.xmm.sg.cond.left = sum(is.na(filtered.condition.eye1$xmm_sg)),
                       count.NA.xmm.sg.cond.right = sum(is.na(filtered.condition.eye0$xmm_sg)),
                       count.NA.xmm.sg.bl.left = sum(is.na(filtered.baseline.eye1$xmm_sg)),
                       count.NA.xmm.sg.bl.right = sum(is.na(filtered.baseline.eye0$xmm_sg)),
                       count.rows.bl.right = nrow(filtered.baseline.eye0),
                       count.rows.bl.left = nrow(filtered.baseline.eye1),
                       count.rows.cond.right = nrow(filtered.condition.eye0),
                       count.rows.cond.left = nrow(filtered.condition.eye1),
                       max.timegap.bl.right = max(filtered.baseline.eye0$newtime_diff, na.rm = T),
                       max.timegap.bl.left = max(filtered.baseline.eye1$newtime_diff, na.rm = T),
                       max.timegap.cond.right = max(filtered.condition.eye0$newtime_diff, na.rm = T),
                       max.timegap.cond.left = max(filtered.condition.eye1$newtime_diff, na.rm = T),
                       min.timegap.bl.right = min(filtered.baseline.eye0$newtime_diff, na.rm = T),
                       min.timegap.bl.left = min(filtered.baseline.eye1$newtime_diff, na.rm = T),
                       min.timegap.cond.right = min(filtered.condition.eye0$newtime_diff, na.rm = T),
                       min.timegap.cond.left = min(filtered.condition.eye1$newtime_diff, na.rm = T),
                       mean.timegap.bl.right = mean(filtered.baseline.eye0$newtime_diff, na.rm = T),
                       mean.timegap.bl.left = mean(filtered.baseline.eye1$newtime_diff, na.rm = T),
                       mean.timegap.cond.right = mean(filtered.condition.eye0$newtime_diff, na.rm = T),
                       mean.timegap.cond.left = mean(filtered.condition.eye1$newtime_diff, na.rm = T),
                       percent.NA.sg.cond.left = (sum(is.na(filtered.condition.eye1$xmm_sg)) / nrow(filtered.condition.eye1[filtered.condition.eye1$xmm_sg, ])),
                       percent.NA.sg.cond.right = (sum(is.na(filtered.condition.eye0$xmm_sg)) / nrow(filtered.condition.eye0[filtered.condition.eye0$xmm_sg, ])),
                       percent.NA.sg.bl.left = (sum(is.na(filtered.baseline.eye1$xmm_sg)) / nrow(filtered.baseline.eye1[filtered.baseline.eye1$xmm_sg, ])),
                       percent.NA.sg.bl.right = (sum(is.na(filtered.baseline.eye0$xmm_sg)) / nrow(filtered.baseline.eye0[filtered.baseline.eye0$xmm_sg, ])),
                       mean.diameter.sg.bl.left = mean(filtered.baseline.eye1$xmm_sg, na.rm = T),
                       mean.diameter.sg.bl.right = mean(filtered.baseline.eye0$xmm_sg, na.rm = T),
                       sd.diameter.sg.bl.left = sd(filtered.baseline.eye1$xmm_sg, na.rm = T),
                       sd.diameter.sg.bl.right = sd(filtered.baseline.eye0$xmm_sg, na.rm = T),
                       mean.diameter.raw.bl.left = mean(filtered.baseline.eye1$diameter_3d, na.rm = T),
                       mean.diameter.raw.bl.right = mean(filtered.baseline.eye0$diameter_3d, na.rm = T),
                       mean.diameter.sg.cond.left = mean(filtered.condition.eye1$xmm_sg, na.rm = T),
                       mean.diameter.sg.cond.right = mean(filtered.condition.eye0$xmm_sg, na.rm = T),
                       sd.diameter.sg.cond.left = sd(filtered.condition.eye1$xmm_sg, na.rm = T),
                       sd.diameter.sg.cond.right = sd(filtered.condition.eye0$xmm_sg, na.rm = T),
                       mean.diameter.raw.cond.left = mean(filtered.condition.eye1$diameter_3d, na.rm = T),
                       mean.diameter.raw.cond.right = mean(filtered.condition.eye0$diameter_3d, na.rm = T),
                       mean.dilation.sg.left = mean(filtered.condition.eye1$xmm_sg, na.rm = T) - mean(filtered.baseline.eye1$xmm_sg, na.rm = T),
                       mean.dilation.sg.right = mean(filtered.condition.eye0$xmm_sg, na.rm = T) - mean(filtered.baseline.eye0$xmm_sg, na.rm = T),
                       blink.count.cond.left = paste(blink.count.cond.left),
                       blink.count.cond.right = paste(blink.count.cond.right),
                       blink.count.bl.left = paste(blink.count.bl.left),
                       blink.count.bl.right = paste(blink.count.bl.right),
                       filename = gsub(".*/","", i),
                       start.time.cond = paste(filtered.condition.eye1[1, "newtime"]),
                       end.time.cond = with(filtered.condition.eye0, tail(filtered.condition.eye0$newtime, n=1)),
                       start.time.bl = paste(filtered.baseline.eye0[1, "newtime"]),
                       end.time.bl = with(filtered.baseline.eye1, tail(filtered.baseline.eye1$newtime, n=1)),
                       ID = as.numeric(gsub(".*?([0-9]+).*", "\\1", i)))
                          
  results <- rbind(results, to.add)

}
results # Check your results
results <- results[-1, ] # Remove the first empty observation after running the loop  

# Write table with results to your destination of choice
write.table(results, file='C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and analysis/Pupil and Analysis/Results and Datasets 2022//results_PL_newdata_after_revision.txt')






