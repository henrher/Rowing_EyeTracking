## Script for processing data output from an Row Perfect 3 (RP3) rowing ergometer
## 
## This script is made by me, Henrik Herrebroden, henrikh@henrikh.no

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
rp3.trials <- rp3.list[!grepl("warmup", rp3.list$files) & !grepl("mocap", rp3.list$files) & !grepl("MM", rp3.list$files) & !grepl("mm", rp3.list$files) & !grepl("Oral responses", rp3.list$files) & !grepl("Double check", rp3.list$files) & !grepl("kopi", rp3.list$files) & !grepl("Questionnaires", rp3.list$files) & !grepl("_hr_", rp3.list$files) & !grepl("incomplete", rp3.list$files) & !grepl("Both", rp3.list$files) & !grepl("test2", rp3.list$files) & !grepl("Test2", rp3.list$files) & !grepl("DoNotUse", rp3.list$files) & !grepl("id21", rp3.list$files) & !grepl("id22", rp3.list$files) & !grepl("id23", rp3.list$files) & !grepl("id15", rp3.list$files) & !grepl("id16", rp3.list$files) & !grepl("data.script.test", rp3.list$files),]
rp3.trials # Check that the correct files are there
i <- rp3.list$files[137]

#Set up results of interest (to be added later)
results <- data.frame(average_stroke_length_160secs = 0,
                      sd_stroke_length_160secs = 0,
                      min_stroke_length_160secs = 0,
                      max_stroke_length_160secs = 0,
                      average_stroke_rate_160secs = 0,
                      sd_stroke_rate_160secs = 0,
                      min_stroke_rate_160secs = 0,
                      max_stroke_rate_160secs = 0,
                      average_split_160secs = 0,
                      sd_split_160secs = 0,
                      min_split_160secs = 0,
                      max_split_160secs = 0,
                      stroke_count_160secs = 0,
                      info = 0, 
                      cond_full = 0,
                      condition.order.rp3 = 0, 
                      id = 0)

# for (i in rp3.list) 
for (i in rp3.trials) {
  
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
cond.full[cond.full == "PLML"] <- "plml"
cond.full[cond.full == "PLMH"] <- "plmh"
cond.full[cond.full == "PHML"] <- "phml"
cond.full[cond.full == "PHMH"] <- "phmh"

phys.load <- substr(cond.full,1,nchar(cond.full)-2)
mental.load <- str_sub(cond.full, -2)

#Add info about skill-level, based on ID number
eliteness <- ifelse(id <= 9, 1, 2 )

to.add <- data.frame(average_stroke_length_160secs = mean(rower.rp3.short$stroke_length, na.rm = T),
                     sd_stroke_length_160secs = sd(rower.rp3.short$stroke_length, na.rm = T),
                     min_stroke_length_160secs = min(rower.rp3.short$stroke_length, na.rm = T),
                     max_stroke_length_160secs = max(rower.rp3.short$stroke_length, na.rm = T),
                     average_stroke_rate_160secs = mean(rower.rp3.short$stroke_rate, na.rm = T),
                     sd_stroke_rate_160secs = sd(rower.rp3.short$stroke_rate, na.rm = T),
                     min_stroke_rate_160secs = min(rower.rp3.short$stroke_rate, na.rm = T),
                     max_stroke_rate_160secs = max(rower.rp3.short$stroke_rate, na.rm = T),
                     average_split_160secs = mean(rower.rp3.short$estimated_500m_time, na.rm = T),
                     sd_split_160secs = sd(rower.rp3.short$estimated_500m_time, na.rm = T),
                     min_split_160secs = min(rower.rp3.short$estimated_500m_time, na.rm = T),
                     max_split_160secs = max(rower.rp3.short$estimated_500m_time, na.rm = T),
                     stroke_count_160secs = sum(!is.na(rower.rp3.short$time)),
                     info = paste(i),
                     cond_full = paste(cond.full),
                     condition.order.rp3 = paste(trial.order.n),
                     id = paste(id))
                     
results <- rbind(results, to.add)

}

results.rp3 <- results[-1, ] # Remove the first empty observation after running the loop         

#Write your results 
write.table(results.rp3, file='C:/Users/henrher/OneDrive - Universitetet i Oslo/Documents/R and analysis/Pupil and Analysis/Results and Datasets 2022/Dataset_RP3_Feb25_2023.txt')

