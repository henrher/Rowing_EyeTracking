## Script for processing blinks and pupil data from SMI/BeGaze output
## 
## The final version of this script is made by me, Henrik Herrebrøden, henrikh@henrikh.no
## The code is based on work by colleague Laura Bishop, laura.bishop@imv.uio.no
##
## To run this script, you should have a folder in your working directory with
## csv files exported from SMI/BeGaze. 


# Set these values prior to running the script ----------------------------------------------------

setwd("C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and analysis/Pupil and Analysis/") # Change this to your local working directory
library(prospectr) # Install & load this library, which you need for the Savitzky-Golay function. 
library(robfilter) # Install & load this library

path.data <- "TestData_SMI/" # Subfolder within working directory containing Pupil Lab csv exports

# List the files that are currently in path.data and choose one to process
pupils.list <-  data.frame(list.files(path = path.data, pattern = ".csv", recursive = T)) # Create a list of files in path.data
colnames(pupils.list) <- "files" # Label the column of filenames as "files"
pupils.list # Run this to see the list of files in your folder
#Distinguish between baseline and condition trials
baseline.trials <- pupils.list[grepl("_b", pupils.list$files) & !grepl("DoNotUse", pupils.list$files),]
condition.trials <- pupils.list[grepl("_c", pupils.list$files) & !grepl("DoNotUse", pupils.list$files),]

i <- pupils.list$files[2] # Choose a csv file from your list to process

#Read timelist with start/stop times for each trial
timelist <- read.csv("TestData_SMI/File information SMI HH PhD Rowing.csv")

# These parameters are used for the filtering function & can be changed if needed
outlier.value <- 2 # Used for defining outlier velocities
low.threshold.pupil <- 2 # Used for cutting off pupil values that are too far below trial mean
high.threshold.pupil <- 3 # Used for cutting off pupil values that are too far above trial mean
sg.w <- 15 # Used in Savitzky-Golay filter (window size); reduce for less smoothing

#Set up results (to be added later)
results <- data.frame(count.NA.xmm.sg.left = 0, 
                      count.NA.xmm.sg.right = 0, 
                      count.NA.xmm.sg.bl.left = 0, 
                      count.NA.xmm.sg.bl.right = 0, 
                      percent.NA.sg.cond.left = 0,  
                      percent.NA.sg.cond.right = 0, 
                      percent.NA.sg.bl.left = 0,  
                      percent.NA.sg.bl.right = 0, 
                      mean.diameter.sg.bl.left = 0, 
                      mean.diameter.sg.bl.right = 0, 
                      mean.diameter.raw.bl.left = 0, 
                      mean.diameter.raw.bl.right = 0, 
                      mean.diameter.sg.cond.left = 0, 
                      mean.diameter.sg.cond.right = 0, 
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
                      end.time.bl = 0, 
                      start.time.bl = 0, 
                      ID = 0)


filtering.right <- function(f) {

##Create dataframe with variables for Right eye 

rower.smi.right <- rower.smi[, c("newtime", "Pupil.Diameter.Right..mm.", "Gaze.Vector.Right.X", "Gaze.Vector.Right.Y")]

#Rename variables to match PL script/output
names(rower.smi.right)[2] <- "diameter_3d"
names(rower.smi.right)[3] <- "xposition"
names(rower.smi.right)[4] <- "yposition"

#Create more variables for Right eye
rower.smi.right["confidence"] <- rower.smi$Tracking.Ratio....
rower.smi.right["xmm_vel"] <- with(rower.smi.right, 
                                   c(NA, diff(diameter_3d))/c(NA, diff(newtime))) # Derive velocity
out.bound <- outlier.value * sd(rower.smi.right$xmm_vel, na.rm = T) # Calculate max allowed velocity

## Create new variable & omit instances where diameter = 0
rower.smi.right["xmm_p"] <- rower.smi.right$diameter_3d
rower.smi.right$xmm_p <- with(rower.smi.right, ifelse(xmm_p == 0, NA, xmm_p))

## Create new variable & omit extreme velocities - Right
rower.smi.right["xmm_pp"] <- with(rower.smi.right, ifelse(abs(xmm_vel) > out.bound, NA, xmm_p))

## Calculate lowest allowed value & remove values below this threshold - Right
low.bound.pupil <- mean(rower.smi.right$xmm_pp, na.rm = T) - low.threshold.pupil * sd(rower.smi.right$xmm_pp, na.rm = T)
rower.smi.right$xmm_ppp <- with(rower.smi.right, ifelse(xmm_pp < low.bound.pupil, NA, xmm_p))

## Calculate highest allowed value & remove pupil values above this threshold
high.bound.pupil <- mean(rower.smi.right$xmm_pp, na.rm = T) + high.threshold.pupil * sd(rower.smi.right$xmm_pp, na.rm = T)
rower.smi.right$xmm_ppp <- with(rower.smi.right, ifelse(xmm_pp > high.bound.pupil, NA, xmm_ppp))

## Savitzky-Golay filter
sg <- savitzkyGolay(rower.smi.right$xmm_ppp, m = 0, p = 3, w = sg.w)
rower.smi.right["xmm_sg"] <- c(rep(NA, (sg.w-1)/2), sg, rep(NA, (sg.w-1)/2))


rower.smi.right
}

filtering.left <- function(f) {
#Same as above, for left eye
##Create dataframe with variables for left eye 
  
  rower.smi.left <- rower.smi[, c("newtime", "Pupil.Diameter.Left..mm.", "Gaze.Vector.Left.X", "Gaze.Vector.Left.Y")]
  
  #Rename variables to match PL script/output
  names(rower.smi.left)[2] <- "diameter_3d"
  names(rower.smi.left)[3] <- "xposition"
  names(rower.smi.left)[4] <- "yposition"
  
  #Create more variables for left eye
  rower.smi.left["confidence"] <- rower.smi$Tracking.Ratio....
  rower.smi.left["xmm_vel"] <- with(rower.smi.left, 
                                     c(NA, diff(diameter_3d))/c(NA, diff(newtime))) # Derive velocity
  out.bound <- outlier.value * sd(rower.smi.left$xmm_vel, na.rm = T) # Calculate max allowed velocity
  
  ## Create new variable & omit instances where diameter = 0
  rower.smi.left["xmm_p"] <- rower.smi.left$diameter_3d
  rower.smi.left$xmm_p <- with(rower.smi.left, ifelse(xmm_p == 0, NA, xmm_p))
  
  ## Create new variable & omit extreme velocities - left
  rower.smi.left["xmm_pp"] <- with(rower.smi.left, ifelse(abs(xmm_vel) > out.bound, NA, xmm_p))
  
  ## Calculate lowest allowed value & remove values below this threshold - left
  low.bound.pupil <- mean(rower.smi.left$xmm_pp, na.rm = T) - low.threshold.pupil * sd(rower.smi.left$xmm_pp, na.rm = T)
  rower.smi.left$xmm_ppp <- with(rower.smi.left, ifelse(xmm_pp < low.bound.pupil, NA, xmm_p))
  
  ## Calculate highest allowed value & remove pupil values above this threshold
  high.bound.pupil <- mean(rower.smi.left$xmm_pp, na.rm = T) + high.threshold.pupil * sd(rower.smi.left$xmm_pp, na.rm = T)
  rower.smi.left$xmm_ppp <- with(rower.smi.left, ifelse(xmm_pp > high.bound.pupil, NA, xmm_ppp))
  
  ## Savitzky-Golay filter
  sg <- savitzkyGolay(rower.smi.left$xmm_ppp, m = 0, p = 3, w = sg.w)
  rower.smi.left["xmm_sg"] <- c(rep(NA, (sg.w-1)/2), sg, rep(NA, (sg.w-1)/2))
  
  
  rower.smi.left
}

filtering.bl.right <- function(f) {
  
  ##Create dataframe with variables for Right eye 
  
  rower.smi.bl.right <- rower.smi.bl[, c("newtime", "Pupil.Diameter.Right..mm.", "Gaze.Vector.Right.X", "Gaze.Vector.Right.Y")]
  
  #Rename variables to match PL script/output
  names(rower.smi.bl.right)[2] <- "diameter_3d"
  names(rower.smi.bl.right)[3] <- "xposition"
  names(rower.smi.bl.right)[4] <- "yposition"
  
  #Create more variables for Right eye
  rower.smi.bl.right["confidence"] <- rower.smi.bl$Tracking.Ratio....
  rower.smi.bl.right["xmm_vel"] <- with(rower.smi.bl.right, 
                                     c(NA, diff(diameter_3d))/c(NA, diff(newtime))) # Derive velocity
  out.bound <- outlier.value * sd(rower.smi.bl.right$xmm_vel, na.rm = T) # Calculate max allowed velocity
  
  ## Create new variable & omit instances where diameter = 0
  rower.smi.bl.right["xmm_p"] <- rower.smi.bl.right$diameter_3d
  rower.smi.bl.right$xmm_p <- with(rower.smi.bl.right, ifelse(xmm_p == 0, NA, xmm_p))
  
  ## Create new variable & omit extreme velocities - Right
  rower.smi.bl.right["xmm_pp"] <- with(rower.smi.bl.right, ifelse(abs(xmm_vel) > out.bound, NA, xmm_p))
  
  ## Calculate lowest allowed value & remove values below this threshold - Right
  low.bound.pupil <- mean(rower.smi.bl.right$xmm_pp, na.rm = T) - low.threshold.pupil * sd(rower.smi.bl.right$xmm_pp, na.rm = T)
  rower.smi.bl.right$xmm_ppp <- with(rower.smi.bl.right, ifelse(xmm_pp < low.bound.pupil, NA, xmm_p))
  
  ## Calculate highest allowed value & remove pupil values above this threshold
  high.bound.pupil <- mean(rower.smi.bl.right$xmm_pp, na.rm = T) + high.threshold.pupil * sd(rower.smi.bl.right$xmm_pp, na.rm = T)
  rower.smi.bl.right$xmm_ppp <- with(rower.smi.bl.right, ifelse(xmm_pp > high.bound.pupil, NA, xmm_ppp))
  
  ## Savitzky-Golay filter
  sg <- savitzkyGolay(rower.smi.bl.right$xmm_ppp, m = 0, p = 3, w = sg.w)
  rower.smi.bl.right["xmm_sg"] <- c(rep(NA, (sg.w-1)/2), sg, rep(NA, (sg.w-1)/2))
  
  rower.smi.bl.right
}

filtering.bl.left <- function(f) {
  #Same as above, for left eye
  ##Create dataframe with variables for left eye 
  
  rower.smi.bl.left <- rower.smi.bl[, c("newtime", "Pupil.Diameter.Left..mm.", "Gaze.Vector.Left.X", "Gaze.Vector.Left.Y")]
  
  #Rename variables to match PL script/output
  names(rower.smi.bl.left)[2] <- "diameter_3d"
  names(rower.smi.bl.left)[3] <- "xposition"
  names(rower.smi.bl.left)[4] <- "yposition"
  
  #Create more variables for left eye
  rower.smi.bl.left["confidence"] <- rower.smi.bl$Tracking.Ratio....
  rower.smi.bl.left["xmm_vel"] <- with(rower.smi.bl.left, 
                                    c(NA, diff(diameter_3d))/c(NA, diff(newtime))) # Derive velocity
  out.bound <- outlier.value * sd(rower.smi.bl.left$xmm_vel, na.rm = T) # Calculate max allowed velocity
  
  ## Create new variable & omit instances where diameter = 0
  rower.smi.bl.left["xmm_p"] <- rower.smi.bl.left$diameter_3d
  rower.smi.bl.left$xmm_p <- with(rower.smi.bl.left, ifelse(xmm_p == 0, NA, xmm_p))
  
  ## Create new variable & omit extreme velocities - left
  rower.smi.bl.left["xmm_pp"] <- with(rower.smi.bl.left, ifelse(abs(xmm_vel) > out.bound, NA, xmm_p))
  
  ## Calculate lowest allowed value & remove values below this threshold - left
  low.bound.pupil <- mean(rower.smi.bl.left$xmm_pp, na.rm = T) - low.threshold.pupil * sd(rower.smi.bl.left$xmm_pp, na.rm = T)
  rower.smi.bl.left$xmm_ppp <- with(rower.smi.bl.left, ifelse(xmm_pp < low.bound.pupil, NA, xmm_p))
  
  ## Calculate highest allowed value & remove pupil values above this threshold
  high.bound.pupil <- mean(rower.smi.bl.left$xmm_pp, na.rm = T) + high.threshold.pupil * sd(rower.smi.bl.left$xmm_pp, na.rm = T)
  rower.smi.bl.left$xmm_ppp <- with(rower.smi.bl.left, ifelse(xmm_pp > high.bound.pupil, NA, xmm_ppp))
  
  ## Savitzky-Golay filter
  sg <- savitzkyGolay(rower.smi.bl.left$xmm_ppp, m = 0, p = 3, w = sg.w)
  rower.smi.bl.left["xmm_sg"] <- c(rep(NA, (sg.w-1)/2), sg, rep(NA, (sg.w-1)/2))
 
  rower.smi.bl.left
}


#Load and process data
for (i in condition.trials) {

  rower.output <- read.csv(paste(path.data, i, sep = ""), sep = ",") # Load csv file

rower.smi <- rower.output[, c("Pupil.Diameter.Right..mm.", 
                              "Pupil.Diameter.Left..mm.", 
                              "Gaze.Vector.Right.X", 
                              "Gaze.Vector.Right.Y", 
                              "Gaze.Vector.Left.X", 
                              "Gaze.Vector.Left.Y", 
                              "RecordingTime..ms.", 
                              "Tracking.Ratio....")]

#Make timestamp and use data within the right timeframe
rower.smi["newtime"] <- with(rower.smi, 
                             round((RecordingTime..ms. - RecordingTime..ms.[1]) /1000, 3)) # Create new timestamp starting at 0
i.filename <- gsub(".*/","", i)
crop.times.start <-timelist$Start[which(timelist$Filename == i.filename)]
crop.times.stop <-timelist$Stop[which(timelist$Filename == i.filename)]
rower.smi <- rower.smi[rower.smi$newtime >= crop.times.start & rower.smi$newtime <= crop.times.stop,]

rower.smi.filtered.right <- filtering.right(rower.smi)
rower.smi.filtered.left <- filtering.left(rower.smi)


##Baseline...
baseline.name <- gsub("_c", "_bl", i)

baseline <- read.csv(paste(path.data, baseline.name, sep = ""), sep = ",") # Load csv file
rower.smi.bl <- baseline[, c("Pupil.Diameter.Right..mm.", "Pupil.Diameter.Left..mm.", "Gaze.Vector.Right.X", "Gaze.Vector.Right.Y", "Gaze.Vector.Left.X", "Gaze.Vector.Left.Y", "RecordingTime..ms.", "Tracking.Ratio....")]

#Make timestamp and use data within the right timeframe
rower.smi.bl["newtime"] <- with(rower.smi.bl, 
                             round((RecordingTime..ms. - RecordingTime..ms.[1]) /1000, 3)) # Create new timestamp starting at 0
baseline.name.short <- gsub("_c", "_bl", i.filename)

crop.times <- timelist[grepl(baseline.name.short, timelist$Filename),]

crop.times.start <-timelist$Start[which(timelist$Filename == baseline.name.short)]
crop.times.stop <-timelist$Stop[which(timelist$Filename == baseline.name.short)]

rower.smi.bl <- rower.smi.bl[rower.smi.bl$newtime >= crop.times.start & rower.smi.bl$newtime <= crop.times.stop,]

rower.smi.bl.left <- filtering.bl.left(rower.smi.bl)
rower.smi.bl.right <- filtering.bl.right(rower.smi.bl)


#Count blinks, condition, right
blink.data.cond.right <- subset(rower.smi.filtered.right, diameter_3d == 0)

timegaps.cond.right <- diff(blink.data.cond.right$newtime)

blink.count.prel.cond.right <- length(timegaps.cond.right[timegaps.cond.right > 0.4])

min.value.cond.right <- min(rower.smi.filtered.right$diameter_3d)

blink.count.cond.right <- ifelse(min.value.cond.right > 0, blink.count.prel.cond.right, blink.count.prel.cond.right+1)

#Count blinks, condition, left

blink.data.cond.left <- subset(rower.smi.filtered.left, diameter_3d == 0)

timegaps.cond.left <- diff(blink.data.cond.left$newtime)

blink.count.prel.cond.left <- length(timegaps.cond.left[timegaps.cond.left > 0.4])

min.value.cond.left <- min(rower.smi.filtered.left$diameter_3d)

blink.count.cond.left <- ifelse(min.value.cond.left > 0, blink.count.prel.cond.left, blink.count.prel.cond.left+1)

#Count blinks, baseline, right
blink.data.bl.right <- subset(rower.smi.bl.right, diameter_3d == 0)

timegaps.bl.right <- diff(blink.data.bl.right$newtime)

blink.count.prel.bl.right <- length(timegaps.bl.right[timegaps.bl.right > 0.4])

min.value.bl.right <- min(rower.smi.bl.right$diameter_3d)

blink.count.bl.right <- ifelse(min.value.bl.right > 0, blink.count.prel.bl.right, blink.count.prel.bl.right+1)

#Count blinks, baseline, left

blink.data.bl.left <- subset(rower.smi.bl.left, diameter_3d == 0)

timegaps.bl.left <- diff(blink.data.bl.left$newtime)

blink.count.prel.bl.left <- length(timegaps.bl.left[timegaps.bl.left > 0.4])

min.value.bl.left <- min(rower.smi.bl.left$diameter_3d)

blink.count.bl.left <- ifelse(min.value.bl.left > 0, blink.count.prel.bl.left, blink.count.prel.bl.left+1)


##Add results

to.add <- data.frame(count.NA.xmm.sg.left = nrow(rower.smi.filtered.left[rower.smi.filtered.left$xmm_sg == "NA", ]),
                     count.NA.xmm.sg.right = nrow(rower.smi.filtered.right[rower.smi.filtered.right$xmm_sg == "NA", ]),
                     count.NA.xmm.sg.bl.left = nrow(rower.smi.bl.left[rower.smi.filtered.left$xmm_sg == "NA", ]),
                     count.NA.xmm.sg.bl.right = nrow(rower.smi.bl.right[rower.smi.filtered.right$xmm_sg == "NA", ]),
                     percent.NA.sg.cond.left = (nrow(rower.smi.filtered.left[rower.smi.filtered.left$xmm_sg == "NA", ]) / 9600),
                     percent.NA.sg.cond.right = (nrow(rower.smi.filtered.right[rower.smi.filtered.right$xmm_sg == "NA", ]) / 9600),
                     percent.NA.sg.bl.left = (nrow(rower.smi.bl.left[rower.smi.bl.left$xmm_sg == "NA", ]) / 600),
                     percent.NA.sg.bl.right = (nrow(rower.smi.bl.right[rower.smi.bl.right$xmm_sg == "NA", ]) / 600),
                     mean.diameter.sg.bl.left = mean(rower.smi.bl.left$xmm_sg, na.rm = T),
                     mean.diameter.sg.bl.right = mean(rower.smi.bl.right$xmm_sg, na.rm = T),
                     mean.diameter.raw.bl.left = mean(rower.smi.bl.left$diameter_3d, na.rm = T),
                     mean.diameter.raw.bl.right = mean(rower.smi.bl.right$diameter_3d, na.rm = T),
                     mean.diameter.sg.cond.left = mean(rower.smi.filtered.left$xmm_sg, na.rm = T),
                     mean.diameter.sg.cond.right = mean(rower.smi.filtered.right$xmm_sg, na.rm = T),
                     mean.diameter.raw.cond.left = mean(rower.smi.filtered.left$diameter_3d, na.rm = T),
                     mean.diameter.raw.cond.right = mean(rower.smi.filtered.right$diameter_3d, na.rm = T),
                     mean.dilation.sg.left = mean(rower.smi.filtered.left$xmm_sg, na.rm = T) - mean(rower.smi.bl.left$xmm_sg, na.rm = T),
                     mean.dilation.sg.right = mean(rower.smi.filtered.right$xmm_sg, na.rm = T) - mean(rower.smi.bl.right$xmm_sg, na.rm = T),
                     blink.count.cond.left = paste(blink.count.cond.left),
                     blink.count.cond.right = paste(blink.count.cond.right),
                     blink.count.bl.left = paste(blink.count.bl.left),
                     blink.count.bl.right = paste(blink.count.bl.right),
                     filename = gsub(".*/","", i),
                     start.time.cond = paste(rower.smi.filtered.left[1, "newtime"]),
                     end.time.cond = with(rower.smi.filtered.right, tail(rower.smi.filtered.right$newtime, n=1)),
                     start.time.bl = paste(rower.smi.bl.right[1, "newtime"]),
                     end.time.bl = with(rower.smi.bl.left, tail(rower.smi.bl.left$newtime, n=1)),
                     ID = as.numeric(gsub(".*?([0-9]+).*", "\\1", i)))

results <- rbind(results, to.add)

}

results
results <- results[-1, ] # Remove the first empty observation after running the loop         
write.table(results, file='C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and analysis/Pupil and Analysis/Results and Datasets 2022//results_SMI_test.txt')



