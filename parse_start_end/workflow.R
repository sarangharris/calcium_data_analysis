setwd("/Users/sarang/Programming/Projects/R/startAndEnd")
source("Mvmt_start_times_from_DLC.R")

# creating empty df that will become df with all movements
allMovements <- data.frame(
  time=numeric(),
  action=character(),
  bodypart=character(),
  diff=numeric(),
  date=numeric(),
  active=logical(),
  subj_num=numeric(),
  type=character(),
  stringsAsFactors=FALSE
)

# looping through all files in a directory
experiment <- "experimentData"
path = "/Users/sarang/Programming/Projects/R/startAndEnd/DLC_Google_files/"

file.names <- dir(path, pattern =".csv")

for(i in 1:length(file.names)) {

file_location <- paste(path,file.names[i], sep="")

# Create column with subj number and date
subj_num <- subj_num_fx("Subject(.*?)-", file_location)

# Create column with subj number and date
subj_date <- subj_date_fx("Restraint-(.*?)_Subject", file_location)

# set threshold for confidence level 
confThreshold <- 0.1

# Read to tibble
pos_data <- read_to_tibble(file_location, subj_num, subj_date, confThreshold)

# Create data offset by 1 frame to calc movement between data frames.
data_offset <- add_row(pos_data, .before = 1)

# Calc speed for each marker
pos_data <- mutate(pos_data,
               Green_Tape_speed = Pythat_ther(pos_data$Green_Tape_x,data_offset$Green_Tape_x, pos_data$Green_Tape_y, data_offset$Green_Tape_y),
               White_Black_Interface_speed = Pythat_ther(pos_data$White_Black_Interface_x, data_offset$White_Black_Interface_x, pos_data$White_Black_Interface_y, data_offset$White_Black_Interface_y),
               Tip_Tail_speed = Pythat_ther(pos_data$Tip_Tail_x, data_offset$Tip_Tail_x,  pos_data$Tip_Tail_y, data_offset$Tip_Tail_y),
               seconds = pos_data$frame_num *.1
              )

# Replace NA's with 0's
pos_data[is.na(pos_data)] <- 0

# Calc standard deviation for each marker
pos_data <- mutate(pos_data,
               GT_speed_sd = sd(pos_data$Green_Tape_speed),
               WB_speed_sd = sd(pos_data$White_Black_Interface_speed),
               Tail_speed_sd = sd(pos_data$Tip_Tail_speed)
)


# set threshold for sd 
sdThreshold <- 1.0

# Create column with movement (true or false)
pos_data$Green_mvnt <-  isMovement(pos_data$Green_Tape_speed, pos_data$GT_speed_sd, sdThreshold) 
pos_data$WB_mvmt <- isMovement(pos_data$White_Black_Interface_speed, pos_data$WB_speed_sd, sdThreshold)
pos_data$Tail_mvmt <- isMovement(pos_data$Tip_Tail_speed, pos_data$Tail_speed_sd, sdThreshold)


# set maximum gap to define movements
maxgap <- .7

# clean start and stop times for each marker
cleanGreen <- raw_to_clean(vel_to_raw(pos_data$Green_mvnt), maxgap, subj_date, "Green_Tape")
cleanWB <- raw_to_clean(vel_to_raw(pos_data$WB_mvmt), maxgap, subj_date, "WB")
cleanTail <- raw_to_clean(vel_to_raw(pos_data$Tail_mvmt), maxgap, subj_date, "Tail_Tip")

# pull active intervals
green_active <- active_only(cleanGreen)
WB_active <- active_only(cleanWB)
tail_active <- active_only(cleanTail)

# pull inactive intervals
green_inactive <- inactive_only(cleanGreen)
WB_inactive <- inactive_only(cleanWB)
tail_inactive <- inactive_only(cleanTail)

# clean data from each marker appended into one df
allClean <- bind_rows(
  cleanGreen,
  cleanWB,
  cleanTail
)

# adds labels; clean but not tidy yet
allClean <- addLabels(allClean, subj_num)

# this labelled df will be added to "allMovements"
tidiedAllClean <- tidyCleanData(allClean)

# filter types of movements and pull start times for each type
largeMovements <- filterLarge(tidiedAllClean)
largeStart <- grabLargeStarts(largeMovements)

smallMovements <- filterSmall(tidiedAllClean)
smallStart <- grabSingleStarts(smallMovements)

tailOnlyMovements <- filterTailOnly(tidiedAllClean)
tailOnlyStart <- grabSingleStarts(tailOnlyMovements)


# append to large df
allMovements <- rbind(allMovements, tidiedAllClean)




####### writing files #######
fileStem <- paste('/Users/sarang/Programming/Projects/R/startAndEnd/', experiment, sep="") %>% file.path()

# ensures directory exists 
checkFilePath(fileStem, subj_date, subj_num)

fileLarge <-  paste(fileStem, '/Restraint-', subj_date, '/Subject-', subj_num, '-', subj_date, '/whole_body_mvmt_start_times.csv', sep="") %>% file.path()
fileSmall <- paste(fileStem, '/Restraint-', subj_date, '/Subject-', subj_num, '-', subj_date, '/head_only_mvmt_start_times.csv', sep="") %>% file.path()
fileTail <-  paste(fileStem, '/Restraint-', subj_date, '/Subject-', subj_num, '-', subj_date, '/tail_only_mvmt_start_times.csv', sep="") %>% file.path()

write.table(largeStart, sep = ",", file = fileLarge, col.names=FALSE, row.names = FALSE)
write.table(smallStart, sep = ",", file = fileSmall, col.names=FALSE, row.names = FALSE)
write.table(tailOnlyStart, sep = ",", file = fileTail, col.names=FALSE, row.names = FALSE)

}
