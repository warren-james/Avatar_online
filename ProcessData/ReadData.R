# Read in the data from the Online version of the experiment 
# This is for the controlled condition 

#### Library #### 
library(tidyverse)
library(gridExtra)
library(progress)
library(beepr)

#### Functions ####
# function for sorting columns we want 
get_cols <- function(df, string_to_check){
  output <- c()
  for(check in colnames(df)){
    if(grepl(string_to_check, check)){
      output <- c(output, check)
    }
  }
  return(output)
}

# load in some functions we might need
source("Functions/0_functions.R")

#### Constants ####
excess <- "please enter your prolific ID here"

#### Read in data ####
results_files <- c("data/Manual/", "data/Automatic/")

# sort out frames 
df_trials <- tibble()
df_demo <- tibble()
df_params <- tibble()
df_check <- tibble()
df_demographics <- tibble()

# read files 
print("reading files")

for(DIR in results_files){
  print(DIR)
  n = length(unique(dir(DIR)))
  pb <- progress_bar$new(total = n)
  for(f in dir(DIR)){
    # print(f)
    # only read .csv files 
    pb$tick()
    if(grepl("pilot", tolower(f))) {
      next
    }
    if(!grepl(".csv", f)){
      next
    }
    # remove our pilot data
    if(grepl("pilot", tolower(f))){
      next
    }
    extracted <- strsplit(f, "_")
    
    participant <- extracted[[1]][1]
    # if(grepl(excess, participant)){
    #   temp <- strsplit(participant, excess)
    #   for(a in temp[[1]]){
    #     if(nchar(a) == 24){
    #       participant = a
    #     } else {
    #       participant = NA
    #     }
    #   }
    # }
    if(participant == excess){
      next
    }
    
    
    # check file size 
    size <- file.info(paste(DIR, f, sep = ""))$size
    if(size < 20){
      next
    }
    d <- read.csv(paste(DIR, f, sep = ""))
    # temporary since people should put in the prol id
    participant <- unique(d$PROLIFIC_PID)
    d$participant <- participant
    
    split <- strsplit(f, "_")
    
    date <- split[[1]][3]
    time <- strsplit(split[[1]][4], "[.]")[[1]][1]
    Browser <- unique(d$Browser)[1]
    OS <- unique(d$OS)
    exp <- split[[1]][2]
    phone <- "PhoneDetected" %in% colnames(d)
    screenSmall <- "ScreenTooSmall" %in% colnames(d)
    
    if(is_empty(Browser)){
      Browser = NA
      OS = NA
    }
    
    if(nrow(d) < 95){
      crashed = TRUE
    } else {
      crashed = FALSE
    }
    
    df_check <- rbind(df_check, tibble(participant = participant,
                                       condition = exp,
                                       date = date,
                                       time = time,
                                       OS = OS,
                                       phone = phone,
                                       screenSmall = screenSmall,
                                       Browser = Browser,
                                       crashed = crashed))
    
    if(crashed == TRUE){
      next
    }

    # skip if they disagreed
    if("Disagreed" %in% unique(d$params_consent)){
      next
    }

    # get the data we want
    # decision data
    d_trial <- d %>%
      select(get_cols(d, "Decision_"), participant, Trial_duration) %>%
      # mutate(Decision_RTms = ifelse(is.na(Decision_RTms), "No response", Decision_RTms)) %>%
      drop_na() %>% 
      mutate(Condition = exp,
             Condition= ifelse(Condition == "AvatarAutomaticProl", "Automatic", "Manual"))
    # parameters
    d_param <- d %>%
      select(get_cols(d, "Params_"),true_x_res, true_y_res, participant) %>%
      drop_na()
    # demo phase
    d_demo <- d %>%
      select(get_cols(d, "Demo_"), participant, Trial_duration) %>%
      # mutate(Demo_RTms = ifelse(is.na(Demo_RTms), "No response", Demo_RTms)) %>%
      drop_na() %>% 
      mutate(Condition = exp,
             Condition= ifelse(Condition == "AvatarAutomaticProl", "Automatic", "Manual"))
    d_demographics <- d %>% 
      select(Gender, Age, participant) %>% 
      group_by(participant) %>% 
      summarise(Gender = unique(Gender),
                Age = unique(Age))

    df_trials <- rbind(df_trials, d_trial)
    df_params <- rbind(df_params, d_param)
    df_demo <- rbind(df_demo, d_demo)
    df_demographics <- rbind(df_demographics, d_demographics)
  }
}

# tidy 
rm(f, results_files, d, d_demo, d_trial, d_param, d_demographics, time, date, Browser, OS, crashed, participant, split, exp, phone, DIR, extracted, excess, pb, size, screenSmall)
print("reading complete")
beep()





