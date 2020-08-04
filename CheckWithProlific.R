# check against prolific files 
# get R file 
source("ProcessData/ReadData.R")

# get list of participants
participants <- unique(df_trials$participant)
results <- "data/ProlificCheck/"

# read in prolific data 
df_prol_comp <- tibble()
for(f in dir(results)){
  d <- read.csv(paste(results, f, sep = ""))
  d <- d %>%
    select(participant_id, status, Sex, age, Country.of.Birth) %>% 
    mutate(Condition = strsplit(f, ".csv")[[1]][1])
  
  df_prol_comp <- rbind(df_prol_comp, d)
  # d <-d %>% 
  #   select(participant_id)
}

# tidy 
rm(d, f, results)


# #phone_detected
phone <- df_check[df_check$phone == TRUE,]
phone_participants <- unique(phone$participant)
# screen small
screen_small <-df_check[df_check$screenSmall == TRUE,]
small_participants <- unique(screen_small$participant)

# check if we have a file for them
file_present <- unique(df_check$participant)

# checking who is in there 
df_prol_comp <- df_prol_comp %>% 
  # rowwise() %>% 
  mutate(
    phone = ifelse(participant_id %in% phone_participants, "True", "False"),
    screenSmall = ifelse(participant_id %in% small_participants, "True", "False"),
    file_present = ifelse(participant_id %in% file_present, "True", "False"),
    full_set = ifelse(participant_id %in% participants, "True", "False"),
    checked = ifelse(full_set == "True", "All good", 
                     ifelse(phone == "True", "Phone",
                            ifelse(screenSmall == "True", "Screen too small",
                                   ifelse(file_present == "False", "No File", "Crashed"))))
    ) %>% 
  arrange(Condition, participant_id)

write.csv(df_prol_comp, file = "scratch/prol_checks.csv", row.names = F)


