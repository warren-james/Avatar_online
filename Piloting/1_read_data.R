# Read in the data from the Online version of the experiment 
# This is for the controlled condition 

#### Library #### 
library(tidyverse)
library(gridExtra)

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
source("0_functions.R")

#### Read in data ####
results_files <- c("../data/Pilot1/Manual/")

# sort out frames rm(list = )
df_trials <- tibble()
df_demo <- tibble()
df_params <- tibble()
df_check <- tibble()
df_demographics <- tibble()

# read files 
print("reading files")

for(f in dir(results_files)){
  print(f)
  # only read .csv files 
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
  d <- read_csv(paste(results_files, f, sep = ""), col_types = cols())
  # temporary since people should put in the prol id 
  split <- strsplit(f, "_")
  participant <- split[[1]][1]
  d$participant <- participant
  date <- split[[1]][3]
  time <- strsplit(split[[1]][4], "[.]")[[1]][1]
  Browser <- unique(d$Browser)[1]
  OS <- unique(d$OS)
  exp <- split[[1]][2]
  phone <- "PhoneDetected" %in% colnames(d)
  
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
                                     date = date,
                                     time = time,
                                     OS = OS,
                                     phone = phone,
                                     Browser = Browser,
                                     crashed = crashed))
  
  # break out if they crashed
  if(crashed == TRUE | exp != "AvatarManual"){
    next
  }
  
  # skip if they disagreed 
  if("Disagreed" %in% unique(d$params_consent)){
    next
  }
  
  # get the data we want
  # decision data
  d_trial <- d %>% 
    select(get_cols(d, "Decision_"), participant) %>% 
    drop_na()
  # parameters
  d_param <- d %>% 
    select(get_cols(d, "Params_"),true_x_res, true_y_res, participant) %>% 
    drop_na() 
  # demo phase 
  d_demo <- d %>% 
    select(get_cols(d, "Demo_"), participant) %>% 
    mutate(Demo_speedCond = unique(Demo_speedCond)[!is.na(unique(Demo_speedCond))]) %>%
    drop_na()
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

# tidy 
rm(f, results_files, d, d_demo, d_trial, d_param, d_demographics, time, date, Browser, OS, crashed, participant, split, exp, phone)
print("reading complete")

# checking for crashes 
# test <- df_check %>% 
#   filter(!participant %in% c("PilotWJ1",
#                              "PilotWJ2",
#                              "PilotWJ3",
#                              "Pilot1.2",
#                              "Pilot4",
#                              "Pilot1",
#                              "TestBrowser2",
#                              "TestBrowser3",
#                              "TestBrowser4",
#                              "TestingBrowserCheck")) %>% 
#   mutate(Browser = ifelse(is.na(Browser), "undefined", Browser),
#          OS = ifelse(is.na(OS), "undefined", OS))
# 
# test %>% 
#   filter(OS != "iPhone") %>%
#   # group_by(Browser, OS, crashed) %>% 
#   # summarise(n = n()) %>% 
#   ggplot(aes(crashed,
#              colour = Browser,
#              fill = Browser)) + 
#   geom_bar(position = "dodge") + 
#   facet_wrap(~OS)




