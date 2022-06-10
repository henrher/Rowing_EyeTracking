## Script for processing Row Perfect 3 (RP3) rowing ergometer data
## 
## This script is made by me, Henrik Herrebrøden, henrikh@henrikh.no

## To run this script, you should have a folder in your working directory 
## with csv files with your RP3 erg data

# Set these values prior to running the script ----------------------------------------------------

setwd("C:/Users/henrher/OneDrive - Universitetet i Oslo/Tests&Data/")
library(dplyr) # needed for time filtering
library(stringr) # needed for str_sub function
path.data <- "PhD Data Rowing/" # Subfolder within working directory containing RP3 data exports
# List the files that are currently in path.data and choose one to process
rp3.list <-  data.frame(list.files(path = path.data, pattern = ".csv", recursive = T)) # Create a list of files in path.data
colnames(rp3.list) <- "files" # Label the column of filenames as "files"
rp3.list # Run this to see the list of files in your folder
#Filter out any CSV files you do not want to use
condition.trials.9subjects <- rp3.list[!grepl("warmup", rp3.list$files) & !grepl("Oral responses", rp3.list$files) & !grepl("Double check", rp3.list$files) & !grepl("kopi", rp3.list$files) & !grepl("Questionnaires", rp3.list$files) & !grepl("_hr_", rp3.list$files) & !grepl("incomplete", rp3.list$files) & !grepl("Both", rp3.list$files) & !grepl("test2", rp3.list$files) & !grepl("Test2", rp3.list$files) & !grepl("DoNotUse", rp3.list$files) & !grepl("id21", rp3.list$files) & !grepl("id22", rp3.list$files) & !grepl("id23", rp3.list$files) & !grepl("id15", rp3.list$files) & !grepl("id16", rp3.list$files),]
condition.trials.9subjects # Check that the correct files are there
i <- rp3.list$files[13]

#Read sheet with target splits for each subject and trial 
splits.row <- read.csv("C:/Users/henrher/OneDrive - Universitetet i Oslo/Tests&Data/PhD Data Rowing/RP3_Split_sheets & Double check/rp3_splits.csv")

#Set up results (to be added later)
results <- data.frame(meanSplit_raw_180secs = 0,  
                      meanSplit_dev_180secs = 0, 
                      sdSplit_raw_180secs = 0, 
                      meanSplit_raw_160secs = 0,  
                      meanSplit_dev_160secs = 0, 
                      sdSplit_raw_160secs = 0, 
                      meanSplitDev_abs_160sec = 0, 
                      sdSplitDev_abs_160sec = 0, 
                      meanSplitDev_abs_180sec = 0, 
                      sdSplitDev_abs_180sec = 0, 
                      info = 0, 
                      condition.order.rp3 = 0, 
                      id = 0, cond_full = 0, 
                      mental_load = 0, 
                      mental_load_number = 0, 
                      phys_load = 0, 
                      phys_load_number = 0, 
                      eliteness = 0)

# for (i in rp3.list) 
for (i in condition.trials.9subjects) {
  
  rower.rp3 <- read.csv(paste(path.data, i, sep = ""), sep = ",") # Load csv file
 
  #filter out data where time restarts (after the 180 sec trial is finished)
  
  low <- rower.rp3$ref[which(rower.rp3$time >= 180 & rower.rp3$time < 180.1)]
  rower.rp3.trial <- rower.rp3[rower.rp3$ref < low, ]
  
  ## Time filter (use only mid 160 secs out of the 180 secs trials) 

rower.rp3.short <- rower.rp3.trial[rower.rp3$time >= 10 & rower.rp3$time <= 170, ] 

#Extract trial order, ID number, condition, and loads from file name
trial.order <- sub("\\.csv.*", "", i)
trial.order.n <- str_sub(trial.order, -1)
id <- as.numeric( sub("\\D*(\\d+).*", "\\1", i) )
cond.helper <- str_sub(trial.order,1,nchar(trial.order)-2)
cond.full <- str_sub(cond.helper, -4)

phys.load <- substr(cond.full,1,nchar(cond.full)-2)
mental.load <- str_sub(cond.full, -2)

#Add info about skill-level, based on ID number
eliteness <- ifelse(id <= 9, 1, 2 )
#Find the right split based on split sheet
split.plan <- splits.row$total_secs[which(splits.row$info == i)]

#Create variable with absolute split deviation (from target), mid 160 secs
rower.rp3.short["split_subtr"] <- rower.rp3.short$estimated_500m_time - split.plan
rower.rp3.short$split_subtr <- abs(rower.rp3.short$split_subtr)

#Create variable with absolute split deviation (from target), full 180 secs
rower.rp3.trial["split_subtr"] <- rower.rp3.trial$estimated_500m_time - split.plan
rower.rp3.trial$split_subtr <- abs(rower.rp3.trial$split_subtr)

to.add <- data.frame(meanSplit_raw_180secs = mean(rower.rp3.trial$estimated_500m_time, na.rm = T),
                     meanSplit_dev_180secs = mean(rower.rp3.trial$estimated_500m_time, na.rm = T) - split.plan,
                     sdSplit_raw_180secs = sd(rower.rp3.trial$estimated_500m_time, na.rm = T),
                     meanSplit_raw_160secs = mean(rower.rp3.short$estimated_500m_time, na.rm = T),
                     meanSplit_dev_160secs = mean(rower.rp3.short$estimated_500m_time, na.rm = T) - split.plan,
                     sdSplit_raw_160secs = sd(rower.rp3.short$estimated_500m_time, na.rm = T),
                     meanSplitDev_abs_160sec = mean(rower.rp3.short$split_subtr, na.rm = T),
                     sdSplitDev_abs_160sec = sd(rower.rp3.short$split_subtr, na.rm = T),
                     meanSplitDev_abs_180sec = mean(rower.rp3.trial$split_subtr, na.rm = T),
                     sdSplitDev_abs_180sec = sd(rower.rp3.trial$split_subtr, na.rm = T),
                     info = paste(i),
                     condition.order.rp3 = paste(trial.order.n),
                     id = paste(id),
                     cond_full = paste(cond.full),
                     mental_load = paste(mental.load),
                     mental_load_number = paste(mental.load.number),
                     phys_load = paste(phys.load),
                     phys_load_number = paste(phys.load.number),
                     eliteness = paste(eliteness))
                     
results <- rbind(results, to.add)

}

results.rp3 <- results[-1, ] # Remove the first empty observation after running the loop         

#Write your results 
write.table(results.rp3, file='C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and analysis/Pupil and Analysis/Results and Datasets 2022/test.RP3.18subj.rp3.txt')
