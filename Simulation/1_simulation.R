#### Simulated data set ####
# this could in a way be used as a power analysis? 
# not sure... 
# This script will use the data from some previous experiments to simulate performance on this 
# task in the direction that we might expect. i.e., manual will be better than automatic 
# All we need to do is get the placement positions and then from there, simulate a trial 
# Do this a bunch of times and then we'll have a data set to play with 

#### Library ####
library(tidyverse)
library(data.table)

#### Functions #### 
# probably need access to some of the other functions we've defined elsewhere 
source("../0_functions.R")

# function to drive the bot
source("0_driving_function.R")

#### Load data ####
# the data we'll be sampling from
load("data/df_part2_Throw")
load("data/df_Aberdeen_decisions")

# data for the bot's parameters 
load("../scratch/bot_params")

# naming convention 
df_man <- df_part2_Throw
df_aut <- df_Aberdeen_decisions

# tidy 
rm(df_Aberdeen_decisions, df_part2_Throw)

#### processing ####
# need to extract only the variables we care about... 
# in this case it's the distance and position (in normalised terms)
# maybe only look at the variable condition in the automatic version? Since this is most comparable? 
# Also keep participant so we have a bit of variation? 
df_aut <- df_aut %>% 
  filter(truck_perf == "Variable") %>%
  mutate(Driving = "Automatic") %>% 
  group_by(participant) %>% 
  mutate(Delta = as.numeric(as.factor(delta)),
         norm_place = placed_x/delta,
         abs_place = abs(norm_place)) %>% 
  select(participant, Driving, Delta, norm_place, success) %>% 
  filter(norm_place <= 1,
         norm_place >= -1)

df_man <- df_man %>% 
  mutate(Driving = "Manual",
         participant = Participant) %>% 
  group_by(participant) %>% 
  mutate(Delta = as.numeric(as.factor(HoopDelta)),
         norm_place = Subject_Position/HoopDelta,
         abs_place = abs(norm_place),
         success = Accuracy) %>% 
  select(participant, Driving, Delta, norm_place, success) %>% 
  filter(norm_place <= 1,
         norm_place >= -1)

# combine 
df <- rbind(df_aut, df_man) %>% 
  mutate(Delta = (as.numeric(Delta) + 1) * 100,
         ypos = 100,
         xpos = Delta - ypos,
         reach = 50,
         speed = 3.5)

# tidy 
rm(df_aut, df_man)

#### make a plot to check #### 
# x11()
df %>% 
  ggplot(aes(abs(norm_place), colour = Driving, 
             fill = Driving)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~Delta)

df %>% 
  ggplot(aes(Delta, success, 
             colour = Driving)) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"))


# this looks good
# so let's move on

#### simulating dataset ####
# to do this, we'll sample participants at random
# simulate the truck being driven 
# (since the bot is doing this and the bot is meant to reflect human behaviour, we should be ok)
# the we can look at success rates and compare the groups? 

#### > setting up parameters ####
n_reps <- 10                # how many of each distance
n_subs <- 30                # how many participants
n_iter <- 3                 # how many experiments do we want to simulate?
conds <- unique(df$Driving) # conditions
deltas <- unique(df$Delta)  # Deltas

# in the simulation, we will simulate the trial for both sides being selected
# This will effectively give us an estimate of their success rate and should
# be more indicative of a decent trial or not
# this looks at raw success rate, which is pretty decent at replicating the data we have
# but really want to look at the chance of success and see how this looks...
# This is what we're actually looking to replicate anyway... 

#### > setup data table ####
# get number of rows
n <- n_reps * length(deltas) * n_subs * length(conds) * n_iter

# sort data
df_sim <- data.table(Iter = rep(0,n),
                     Participant = rep(0,n),
                     Trial = rep(0,n),
                     Driving = rep("", n),
                     Delta = rep(0, n),
                     Reach = rep(0, n),
                     xplace = rep(0,n),
                     xpos = rep(0, n),
                     ypos = rep(0, n),
                     success = rep(0, n))

#### > run simulation ####
start <- Sys.time()
count <- 1
for(ii in 1:n_iter){
  # loop through conditions
  for(cond in conds){
    # get subset of data
    ss <- df[df$Driving == cond,]
    # get random list of participants
    subjs <- sample(unique(ss$participant), n_subs)
    
    # loop through participants 
    participant <- 0
    for(sub in subjs){
      ss_sub <- ss[ss$participant == sub,]
      
      # loop through deltas
      participant <- participant + 1 
      for(d in deltas){
        # number of reps 
        ss_delta <- ss_sub[ss_sub$Delta == d,]
        for(trial in 1:n_reps){
          A_pos <- sample(ss_delta$norm_place,1) * (d - 100)
          
          # setup bot params
          Rt = round(rlnorm(1, bot_params$RT_mu, bot_params$RT_sd))
          delay = round(rlnorm(1, bot_params$switch_delay_mu, bot_params$switch_delay_sd))
          down_travel = round(rnorm(1, bot_params$down_stop_mu, bot_params$down_stop_sd))
          
          bot_pars <- list(Rt = Rt, delay = delay, down_travel = down_travel)
          
          # run both sides
          # get results
          result <- bot_drive(A_pos, d - 100, bot_pars, 45, unique(ss_delta$speed))
          
          # add to data 
          df_sim[count, Iter := ii]
          df_sim[count, Trial := trial]
          df_sim[count, Participant := participant]
          df_sim[count, Driving := cond]
          df_sim[count, Delta := d]
          df_sim[count, Reach := 45]
          df_sim[count, xplace := A_pos]
          df_sim[count, xpos := result$final_x]
          df_sim[count, ypos := result$final_y]
          df_sim[count, success := result$success]
          
          # increase count
          count <- count + 1
          
        }
      }
    }
  }
}
print(Sys.time()-start)

# tidy 
rm(A_pos, cond, count, d, delay, down_travel, ii, Rt, participant, sub, subjs, trial)

# quick plots 
df_sim %>% 
  # filter(Side == -1) %>%
  ggplot(aes(Delta, success, colour = Driving)) + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = T) 

df_sim %>% 
  # filter(Side == 1) %>%
  mutate(xplace = xplace/Delta) %>%
  ggplot(aes(xplace, colour = Driving, fill = Driving)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~Delta)

#### Add in chance of success #### 
# Look chance of success for distance 
df_sim <- df_sim %>%
  mutate(distance = Delta - 100 - xplace,
         distance2 = Delta - 100 + xplace)

dists <- c(unique(df_sim$distance), unique(df_sim$distance2))
dists <- unique(dists)
n <- length(dists)
check <- round(n/10)
n_tests <- 200

df_acc <- data.table(distance = rep(0,n),
                     acc = rep(0,n))
# something isn't quite right...
# we have a problem when the vehicle is pretty much above the target

count <- 1
start <- Sys.time()
for(d in dists){
  results <- c()
  for(ii in 1:n_tests){
    # setup bo params
    Rt = round(rlnorm(1, bot_params$RT_mu, bot_params$RT_sd))
    delay = round(rlnorm(1, bot_params$switch_delay_mu, bot_params$switch_delay_sd))
    down_travel = round(rnorm(1, bot_params$down_stop_mu, bot_params$down_stop_sd))
    
    bot_pars <- list(Rt = Rt, delay = delay, down_travel = down_travel)
    
    # get success rate
    results <- c(results, bot_drive(0, d, bot_pars, 45, 3.5)$success)
  }
  accuracy <- mean(results)
  df_acc[count, distance := d]
  df_acc[count, acc := accuracy]
  count <- count + 1
  
  if(count %% check == 0){
    print(paste(count/check*10, "% complete", sep = ""))
  }
}
print(Sys.time()-start)


# ading in chance 
test <- merge(df_sim, df_acc)
df_acc2 <- df_acc %>% 
  mutate(distance2 = distance,
         acc2 = acc) %>% 
  select(-distance, -acc)
test <- merge(test, df_acc2) %>% 
  mutate(chance = (acc + acc2)/2)

test <- test %>% 
  mutate(dist_type = ifelse(Delta < 400, "Close", "Far"),
         abs_pos = abs(xplace)/Delta,
         Participant = ifelse(Driving == "Manual", Participant + 30, Participant))

# now make a plot 
test %>%
  ggplot(aes(chance, colour = Driving, fill = Driving)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~dist_type, scale = "free_y")

test %>% 
  ggplot(aes(abs_pos, colour = Driving, fill = Driving)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~dist_type, scale = "free_y")

test %>% 
  group_by(Participant, Driving, dist_type) %>% 
  summarise(mu_chance = mean(chance)) %>% 
  ggplot(aes(mu_chance,
             colour = Driving,
             fill = Driving)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~dist_type)

#### Setup SPSS file ####
df_spss <- test %>% 
  mutate(Distance = dist_type) %>% 
  group_by(Participant, Driving, Distance) %>% 
  summarise(mu_pos = mean(abs_pos),
            mu_chance = mean(chance))

df_spss %>% 
  ggplot(aes(mu_pos, colour = Driving, fill = Driving)) + 
  geom_density(alpha = .3) + 
  facet_wrap(~Distance)

df_spss %>%
  ggplot(aes(mu_chance, colour = Driving, fill = Driving)) +
  geom_density(alpha = .3) +
  facet_wrap(~Distance)


df_spss_acc <- df_spss %>% 
  select(-mu_pos )%>% 
  mutate(mu_chance = round(mu_chance, digits = 3)) %>% 
  spread(Distance, mu_chance) 
df_spss_pos <- df_spss %>% 
  select(-mu_chance) %>% 
  mutate(mu_pos = round(mu_pos, digits = 3)) %>% 
  spread(Distance, mu_pos)

# save these
# write.table(df_spss_pos, file = "scratch/df_spss_position.txt", row.names = F)
# write.table(df_spss_acc, file = "scratch/df_spss_chance.txt", row.names = F)
df_spss_acc <- read.csv("scratch/df_spss_chance.txt", sep = " ")
df_spss_pos <- read.csv("scratch/df_spss_position.txt", sep = " ")

# make a plot...I guess... 
df_spss_acc %>% 
  gather(Close:Far,
         key = "Distance", 
         value = "chance") %>% 
  ggplot(aes(Distance, chance, 
             colour = Driving, 
             fill = Driving)) + 
  geom_boxplot(alpha = .3) + 
  geom_point(alpha = .3, position = position_jitterdodge(.1)) + 
  theme_bw() + 
  see::scale_color_flat() + 
  see::scale_fill_flat()


df_spss_pos %>% 
  gather(Close:Far,
         key = "Distance", 
         value = "chance") %>% 
  ggplot(aes(Distance, chance, 
             colour = Driving, 
             fill = Driving)) + 
  geom_boxplot(alpha = .3) + 
  geom_point(alpha = .3, position = position_jitterdodge(.1)) + 
  theme_bw() + 
  see::scale_color_flat() + 
  see::scale_fill_flat()

# simple line plots 
line_plt_position <- df_spss_pos %>% 
  gather(Close:Far,
         key = "Distance", 
         value = "Position") %>%
  group_by(Driving, Distance) %>% 
  summarise(mu = mean(Position)) %>%
  ggplot(aes(Distance, mu, 
             colour = Driving)) + 
  geom_point() + 
  geom_line(aes(group = Driving)) + 
  theme_bw() + 
  see::scale_color_flat() + 
  scale_y_continuous("Mean placement")

line_plot_chance <- df_spss_acc %>% 
  gather(Close:Far,
         key = "Distance", 
         value = "Chance") %>%
  group_by(Driving, Distance) %>% 
  summarise(mu = mean(Chance)) %>%
  ggplot(aes(Distance, mu, 
             colour = Driving)) + 
  geom_point() + 
  geom_line(aes(group = Driving)) + 
  theme_bw() + 
  see::scale_color_flat() +
  scale_y_continuous("Mean Chance", labels = scales::percent_format(accuracy = 1))

gridExtra::grid.arrange(line_plot_chance, line_plt_position)



