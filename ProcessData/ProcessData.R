# process the data from the online experiment

#### Library ####
library(tidyverse)
library(data.table)

#### source ####
# get datasets and old functions 
source("ProcessData/ReadData.R")

#### Functions ####
# load in bot to get chance
source("Functions/0_driving_function.R")

#### Process data ####
df_params <- df_params %>% 
  mutate(Params_boxHeight = 50,
         Params_boxWidth = 50,
         Params_ReachX = Params_Reach,
         Params_ReachY = Params_Reach)

df_trials <- df_trials %>% 
  merge(df_params) 

# sort out the dataset for our uses now 
df_avatar <- df_trials %>%
  rowwise() %>%
  mutate(elip_distance = ellipse_dist(Decision_finalX, Decision_finalY, Decision_xposTarget, Decision_yposTarget, Params_ReachX, Params_ReachY)) 


df_demo <- df_demo %>% 
  merge(df_params) 


#### Add in chance performance ####
# get distances to check... 
df_avatar <- df_avatar %>% 
  # select(participant, Decision_xposTarget, Decision_placement) %>% 
  mutate(delta = abs(Decision_xposTarget),
         distance1 = abs(delta - Decision_placement),
         distance2 = abs(delta + Decision_placement),
         abs_norm_place = abs(Decision_placement)/delta,
         dist_type = ifelse(Decision_Delta < 400, "Close", "Far"))

dists <- c(unique(df_avatar$distance1), unique(df_avatar$distance2), unique(df_avatar$delta))
dists <- unique(dists)

df_acc <- tibble(participant = character(),
                 Condition = character(),
                 dist = numeric(),
                 acc = numeric())

# df_accparams <- tibble(participant = character(), 
#                        condition = character(),
#                        A = numeric(),
#                        B = numeric())

n <- length(unique(df_demo$participant))

df_accparams <- data.table(participant = rep("", n),
                           Condition = rep("",n),
                           A = rep(0, n),
                           B = rep(0,n))

# now loop and make predictions
print("calculating chance")

count <- 1
pb <- progress_bar$new(total = n)
for(p in unique(df_demo$participant)){
  ss <- df_demo[df_demo$participant == p,] %>% 
    mutate(dist = abs(Demo_xposTarget))
  m <- glm(Demo_Success ~ dist,
           family = "binomial",
           data = ss)
  subj = unique(ss$participant)
  Cond = unique(ss$Condition)
  
  temp <- tibble(participant = subj,
                 Condition = Cond,
                 dist = dists) %>% 
    rowwise() %>% 
    mutate(acc = predict(m, data.frame(dist = dist), type = "response"))
  
  df_acc <- rbind(df_acc, temp)
  # df_accparams <- rbind(df_accparams, temp_params)
  df_accparams[count, participant := subj]
  df_accparams[count, Condition := Cond]
  df_accparams[count, A := m$coefficients[1]]
  df_accparams[count, B := m$coefficients[2]]
  
  pb$tick()
  count <- count + 1
}

# tidy 
rm(m, ss, temp, temp_params, participant, Cond, dists, n, p, pb, count)

print("complete")
beep()

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

#### Replace #### 
# check the manual condition for people who have poor accuracy 
limit <- .85
Manual_participants <- as_tibble(df_accparams) %>% 
  filter(Condition == "Manual") %>%
  mutate(dist_100 = 100,
         dist_200 = 200,
         dist_300 = 300,
         dist_400 = 400) %>% 
  gather(dist_100:dist_400,
         key = "remove",
         value = "distance") %>% 
  select(-remove) %>% 
  mutate(accuracy = boot::inv.logit(A + (distance * B)),
         exclude = ifelse(boot::inv.logit(A + (100 * B)) < limit, "exclude", "keep"))

Manual_keep <- Manual_participants %>% 
  filter(exclude == "keep")
Manual_keep = unique(Manual_keep$participant)
Manual_exclude <- Manual_participants %>% 
  filter(exclude == "exclude")
Manual_exclude = unique(Manual_exclude$participant)

# add in column to see who's being excluded
df_avatar <- df_avatar %>% 
  mutate(exclude = ifelse(participant %in% Manual_exclude, TRUE, FALSE))

plt_Manual_acc <- Manual_participants %>%
  ggplot(aes(distance, accuracy,
             colour = exclude)) +
  geom_point() +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = F) +
  facet_wrap(~participant)

# compare closest and furthest acc 
plt_acc_check <- df_acc %>%
  filter(dist %in% c(100,400)) %>%
  filter(!participant %in% Manual_exclude) %>% 
  ggplot(aes(acc, fill = Condition)) + 
  geom_density(alpha = .3) +
  facet_wrap(~dist, scales = "free")

# tidy 
rm(limit)

#### tidy up data frames ####
df_avatar <- df_avatar %>% 
  select(-c(Decision_Driving, Decision_Click)) %>% 
  mutate(Condition = as.factor(Condition),
         Condition = factor(Condition, levels = c("Manual", "Automatic")))

df_demo <- df_demo %>% 
  select(-c(Demo_Click, Demo_motion, Demo_speedCond)) %>% 
  mutate(Condition = as.factor(Condition),
         Condition = factor(Condition, levels = c("Manual", "Automatic")))

