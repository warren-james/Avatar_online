# process the data from the online experiment

#### Library ####
library(tidyverse)

#### source ####
# get datasets and old functions 
source("1_read_data.R")

#### Functions ####
# load in bot to get chance
source("0_driving_function.R")

#### Process data ####
df_params <- df_params %>% 
  filter(participant != "Pilot1.2") %>%
  mutate(Params_boxHeight = 50,
         Params_boxWidth = 50,
         Params_ReachX = Params_Reach,
         Params_ReachY = Params_Reach)

df_trials <- df_trials %>% 
  filter(participant != "Pilot1.2") %>%
  merge(df_params) 

# sort out the dataset for our uses now 
df_avatar <- df_trials %>%
  rowwise() %>%
  mutate(move_time = sum(str_to_num(Decision_Click)),
         # Driving = as_tibble_col(str_to_list(Decision_Driving)),
         right_move = sum(str_to_list(Decision_Driving) %in% c("right", "\"right\"")),
         left_move = sum(str_to_list(Decision_Driving) %in% c("left", "\"left\"")),
         down_move = sum(str_to_list(Decision_Driving) %in% c("down", "\"down\"")),
         up_move = sum(str_to_list(Decision_Driving) %in% c("up", "\"up\"")),
         prop_right = right_move/Params_TravelTime,
         prop_left = left_move/Params_TravelTime,
         prop_up = up_move/Params_TravelTime,
         prop_down = down_move/Params_TravelTime,
         prop = move_time/Params_TravelTime,
         distance = ellipse_dist(Decision_finalX, Decision_finalY, Decision_xposTarget, Decision_yposTarget, Params_ReachX, Params_ReachY),
         rough_RTframe = order(str_to_list(Decision_Click))[!duplicated(sort(str_to_list(Decision_Click)))][2],
         rough_RT = rough_RTframe * (1/Params_Hz)) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(switch_delay = sum(str_to_list(Decision_Click)[rough_RTframe:length(str_to_list(Decision_Click))] %in% "0"),
         remaining_frames = length(str_to_num(Decision_Click)[rough_RTframe:length(str_to_num(Decision_Click))]),
         Travel_time_afterRT = sum(str_to_num(Decision_Click)[rough_RTframe:length(str_to_num(Decision_Click))]),
         prop_afterRT = Travel_time_afterRT/remaining_frames) %>% 
  select(-c(Decision_Click, Decision_Driving)) %>% 
  mutate(Condition = "Manual") %>% 
  ungroup()

df_demo <- df_demo %>% 
  rowwise() %>% 
  mutate(right_move = sum(str_to_list(Demo_motion) %in% c("right", "\"right\"")),
         left_move = sum(str_to_list(Demo_motion) %in% c("left", "\"left\"")),
         down_move = sum(str_to_list(Demo_motion) %in% c("down", "\"down\"")),
         up_move = sum(str_to_list(Demo_motion) %in% c("up", "\"up\"")),
         Demo_FinalXFixed = (right_move * Demo_AvSpeed) - (left_move * Demo_AvSpeed),
         Demo_FinalYFixed = 100 + (up_move * Demo_AvSpeed) - (down_move * Demo_AvSpeed)) %>% 
  merge(df_params)

#### Add in chance performance ####
df_acc <- tibble(participant = character(),
                 dist = numeric(),
                 acc = numeric())

# get distances to check... 
df_avatar <- df_avatar %>% 
  # select(participant, Decision_xposTarget, Decision_placement) %>% 
  mutate(delta = abs(Decision_xposTarget),
         distance1 = abs(delta - Decision_placement),
         distance2 = abs(delta + Decision_placement))

dists <- c(unique(df_avatar$distance1), unique(df_avatar$distance2))
dists <- unique(dists)

# now loop and make predictions
print("calculating chance")
count <- 1
n <- length(unique(df_demo$participant))
for(p in unique(df_demo$participant)){
  ss <- df_demo[df_demo$participant == p,] %>% 
    mutate(dist = abs(Demo_xposTarget))
  m <- glm(Demo_Success ~ dist,
           family = "binomial",
           data = ss)
  
  temp <- tibble(participant = unique(ss$participant),
                 dist = dists) %>% 
    rowwise() %>% 
    mutate(acc = predict(m, data.frame(dist = dist), type = "response"))
  
  df_acc <- rbind(df_acc, temp)
  print(paste(count, " of ", n, " complete", sep = ""))
  count <- count + 1
}

# tidy 
rm(m, ss, temp, count, dists, n, p)

print("complete")

# bind this data to the trials data so we have chance performance
df_acc1 <- df_acc %>% 
  mutate(distance1 = dist,
         acc1 = acc) %>% 
  select(-c(dist, acc))
df_acc2 <- df_acc %>% 
  mutate(distance2 = dist,
         acc2 = acc) %>% 
  select(-c(dist, acc))

df_avatar <- merge(df_avatar, df_acc1) %>% 
  merge(df_acc2) %>% 
  mutate(chance = (acc1 + acc2)/2) %>% 
  select(-c(distance1, distance2, acc1, acc2, delta))

# tidy 
rm(df_acc1, df_acc2)


