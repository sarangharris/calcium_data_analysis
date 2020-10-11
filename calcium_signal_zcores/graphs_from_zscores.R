library(tidyverse)
library(ggforce)
library(viridis)
library(prospectr)
library(scales)

# Get subj number and date
# subj_num_pattern = the pattern surounding the subject number (.*?) indicates any 
# charicrar string of anylenght will be used. place the pattern that comes before and after the subj number on either side of the (.*?). [e.g. "Subject(.*?)-"]
subj_num_fx <- function(subj_num_pattern, file_path){
  regmatches(file_path, regexec(subj_num_pattern,file_path))[[1]][2]
  
}

# Get subj number and date
# file_path = the path for the file which will be used to pull out the naming info. Same rules as subj_num_fx [e.g. "Restraint-(.*?)_Subject"]
subj_date_fx <- function(subj_date_pattern, file_path){
  regmatches(file_path, regexec(subj_date_pattern,file_path))[[1]][2]
  
}


# Get movement type
# file_path = the path for the file which will be used to pull out the naming info. Same rules as subj_num_fx [e.g. "Restraint-(.*?)_Subject"]
movement_type_fx <- function(movement_type_pattern, movement_type_pattern2, file_path){
  temp2 <- regmatches(file_path, regexec(movement_type_pattern,file_path))[[1]][2]
    regmatches(temp2, regexec(movement_type_pattern2,temp2))[[1]][2]
  
}


not_all_na <- function(x) {!all(is.na(x))}

read_alignments <- function(filePath, subj_num, movementType, date) {
    temp <- read_csv(filePath, col_names = FALSE, cols(.default = col_double())) %>% 
    select_if(not_all_na) %>%
    mutate(Time = seq(from = -5, to = 5, by = .001)) %>%
    group_by(Time = cut(Time, breaks=100)) %>%
    summarize_all(list(mean)) %>%
    mutate(Time = seq(from = -4.95, to = 4.95, by = .1)) %>%
    gather(key = "boutNum", value = "Z_score", -Time) %>% 
    mutate(boutNum = boutNum %>% str_sub(2) %>% str_trim() %>% as.numeric()) %>%
    arrange(Time, boutNum) %>%
    mutate(subj_num = subj_num, movementType = movementType, date = date) %>%
    select(subj_num, movementType, date, everything())
}

