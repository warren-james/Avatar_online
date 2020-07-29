# we're just getting some estimates for RT and down motion for the bot

#### Library ####
library(tidyverse)
library(gridExtra)

#### Constants #### 

#### functions #### 
# sorting data
get_cols <- function(df, string_to_check){
  output <- c()
  for(check in colnames(df)){
    if(grepl(string_to_check, check)){
      output <- c(output, check)
    }
  }
  return(output)
}

# functions that are helpful
source("../0_functions.R")

#### read in data ####
results_files <- c("data/")

df_trials <- tibble()
df_params <- tibble()

for(f in dir(results_files)) {
  if(grepl(".csv", f)){
    d <- read.csv(paste(results_files, f, sep = ""))
    d$participant <- str_split(f, '[_]')[[1]][1]
  } else {
    next
  }
  if("Disagreed" %in% unique(d$params_consent)){
    next
  }
  d_trial <- d %>% 
    select(get_cols(d, "Trial_"), participant) %>% 
    drop_na()
  
  # for now, remove the RT column 
  if("Trial_RTms" %in% colnames(d_trial)){
    d_trial <- d_trial %>%
      select(-Trial_RTms)
  }
  
  d_param <- d %>% 
    select(get_cols(d, "params_"), participant) %>% 
    drop_na() 
  
  df_trials <- rbind(df_trials, d_trial)
  df_params <- rbind(df_params, d_param)
}

# tidy 
rm(f, results_files, d, d_trial, d_param)

#### split up the data ####
df_params <- df_params %>% 
  mutate(params_boxHeight = 50,
         params_boxWidth = 50,
         params_ReachX = params_Reach*params_widthScale,
         params_ReachY = params_Reach*params_heightScale)

df_trials <- df_trials %>% 
  merge(df_params) %>%
  # rescale everything to be back to normal?
  # that is if we actually keep this part?
  mutate(rescaled_delta = Trial_delta/params_widthScale,
         rescaled_reachY = params_ReachY/params_heightScale,
         rescaled_reachX = params_ReachX/params_widthScale,
         rescaled_targetXpos = Trial_targetXPos/params_widthScale,
         rescaled_targetYpos = 0,
         rescaled_AvatarFinalX = Trial_avatarFinalX/params_widthScale,
         rescaled_AvatarFinalY = Trial_avatarFinalY/params_heightScale)

#### process data #### 
# For "RT" use something like sum(travel_time)/length(travel_time)? 
# something like below... but we need to figure it out
# maybe use this as a sort of measure of how much time they need to spend travelling in order to reach the target
# then we can use this to see how close people get to the target on each trial and see if they're responding at a reasonable rate?
# need to account for success and also distance to see how much of the alloted time they need to reach the target?
# so maybe use a "proportion of available time window spent travelling" and we can use that? 
# still working on this... 
temp <- df_trials %>%
  rowwise() %>%
  mutate(move_time = sum(str_to_num(Trial_mouseclick)),
         # movement = as_tibble_col(str_to_list(Trial_movement)),
         right_move = sum(str_to_list(Trial_movement) %in% c("right", "\"right\"")),
         left_move = sum(str_to_list(Trial_movement) %in% c("left", "\"left\"")),
         down_move = sum(str_to_list(Trial_movement) %in% c("down", "\"down\"")),
         up_move = sum(str_to_list(Trial_movement) %in% c("up", "\"right\"")),
         prop_right = right_move/params_TravelTime,
         prop_left = left_move/params_TravelTime,
         prop_up = up_move/params_TravelTime,
         prop_down = down_move/params_TravelTime,
         prop = move_time/params_TravelTime,
         distance = ellipse_dist(rescaled_AvatarFinalX, rescaled_AvatarFinalY, rescaled_targetXpos, 0, rescaled_reachX, rescaled_reachY),
         rough_RTframe = order(str_to_list(Trial_mouseclick))[!duplicated(sort(str_to_list(Trial_mouseclick)))][2],
         rough_RT = rough_RTframe * (1/params_Hz)) %>%
  drop_na() %>%
  rowwise() %>%
  mutate(switch_delay = sum(str_to_list(Trial_mouseclick)[rough_RTframe:length(str_to_list(Trial_mouseclick))] %in% "0"),
         remaining_frames = length(str_to_num(Trial_mouseclick)[rough_RTframe:length(str_to_num(Trial_mouseclick))]),
         Travel_time_afterRT = sum(str_to_num(Trial_mouseclick)[rough_RTframe:length(str_to_num(Trial_mouseclick))]),
         prop_afterRT = Travel_time_afterRT/remaining_frames) %>% 
  select(-c(Trial_mouseclick, Trial_movement))

#### Extract parameters ####
# This is to setup some conditions for our bot so we can simulate performance in the future
# setup parameters 
# how far down should we drive
down_stop <- MASS::fitdistr(temp$down_move*3.5, "normal")
down_stop_mu <- down_stop$estimate[1]
down_stop_sd <- down_stop$estimate[2]/1.5 # should make things a bit nicer?

# response time 
RT <- MASS::fitdistr(temp$rough_RTframe[temp$rough_RTframe <= 100 & temp$rough_RTframe > 10], "lognormal") # remove trials where there appears to be a lapse of attention 
# 80 * 0.0167 = 1.33seconds... so a very long reaction time
RT_mu <- RT$estimate[1]
RT_sd <- RT$estimate[2]/1.5 # just to make things a bit more similar to what we observe otherwise it's far too wide

# delay in switching 
switch_delay <- MASS::fitdistr(temp$switch_delay[temp$rough_RTframe <= 80 & temp$switch_delay > 0], "lognormal")
switch_delay_mu <- switch_delay$estimate[1]
switch_delay_sd <- switch_delay$estimate[2]/1.25

# setup a data frame of this to save 
bot_params <- tibble(RT_mu = RT_mu,
                     RT_sd = RT_sd,
                     down_stop_mu = down_stop_mu,
                     down_stop_sd = down_stop_sd,
                     switch_delay_mu = switch_delay_mu,
                     switch_delay_sd = switch_delay_sd)
save(bot_params, file = "../scratch/bot_params")


# Experiment settings
deltas <- seq(200, 500, 100)
speeds <- seq(2,5,.5)
travel_time <- 120
reach <- 45
vertical_offset <- 100
trial_length <- 120
n_iter <- 25
n_subs <- 20

# set position
init_x <- 0
init_y <- vertical_offset

# setup dataframe 
n <- length(deltas) * length(speeds) * n_iter * n_subs

df_sample <- data.table::data.table(subj = rep(0, n),
                                    iter = rep(0, n),
                                    down_stop_pos = rep(0, n),
                                    delta = rep(0, n),
                                    speed = rep(0, n),
                                    respF = rep(0, n),
                                    switch_del = rep(0, n),
                                    success = rep(0, n),
                                    origin_x = rep(0, n),
                                    origin_y = rep(init_y, n),
                                    target_x = rep(0, n),
                                    target_y = rep(0, n),
                                    final_x = rep(0, n),
                                    final_y = rep(0, n))

# new attempt
# no delay when moving switching to move sideways
start = Sys.time()
count <- 0 
for(sub in 1:n_subs){
  for(d in deltas){
    for(sp in speeds){
      # sim participant 
      for(ii in 1:n_iter){
        # now do a trial
        # setup coordinates
        x_pos = init_x
        y_pos = vertical_offset
        x_targ = d - vertical_offset
        y_targ = 0
        
        # setup delays 
        Rt = round(rlnorm(1, RT_mu, RT_sd))
        delay = round(rlnorm(1, switch_delay_mu, switch_delay_sd))
        
        # setup stopping point 
        down_travel = round(rnorm(1, down_stop_mu, down_stop_sd))
        
        # hit detection
        hit = NA
        suc = 0
        
        # motion 
        cur_mot = ""
        prev_mot = ""
        
        # things to check 
        driving = F
        down_reached = c(F, 0)
        delay_count = 0
        overshoot = F # do we need this?
        switching = F
        for(t in 1:travel_time){
          # Are we driving
          if(t >= Rt & switching == F){
            driving = T
          }
          
          # if we switch, we need to wait 
          
          
          # have we finished waiting 
          if(delay_count == delay){
            switching = F
            driving = T
            delay_count = 0
          }
          
          # check if we've gone too far 
          if(x_pos > x_targ + reach*.5 + sp & down_reached[1] == T){
            overshoot = T
            down_reached[2] = 1
          }
          
          if(y_pos <= 100 - down_travel + sp & down_reached[2] == 0){
            down_reached[1] = T
            down_reached[2] = 1
            switching = T
          }
          
          if(switching){
            delay_count = delay_count + 1
            driving = F
            prev_mot = ""
            # cur_motion = ""
          }
          
          # how are we moving this frame 
          if(driving){
            # move down if we haven't got low enough
            # or 
            # we've gone too far to the side
            if(down_reached[1] == F | overshoot == T){
              y_pos = y_pos - sp
              cur_mot = "down"
              # otherwise, got side ways
            } else {
              x_pos = x_pos + sp
              cur_mot = "side"
            }
          }
          
          # check, have we switched motion 
          if(cur_mot != prev_mot & length(prev_mot) > 1){
            switching = T
          } 
          
          prev_mot = cur_mot
          
          # check if we're in range... if so, quit
          hit = ellipse_dist(x_pos, y_pos, x_targ, y_targ, reach, reach)
          if(hit <= 1){
            suc = 1
            break
          }
        }
        count <- count + 1
        df_sample[count, subj := sub]
        df_sample[count, iter := ii]
        df_sample[count, down_stop_pos := down_travel]
        df_sample[count, delta := d]
        df_sample[count, speed := sp]
        df_sample[count, respF := Rt]
        df_sample[count, switch_del := delay]
        df_sample[count, success := suc]
        df_sample[count, target_x := x_targ]
        df_sample[count, target_y := y_targ]
        df_sample[count, final_x := x_pos]
        df_sample[count, final_y := y_pos]
      }
    }
  }
}
print(Sys.time()-start)

# quick descriptives check 
temp %>% 
  group_by(rescaled_delta) %>% 
  summarise(mu = mean(Trial_success))
df_sample %>% 
  filter(speed == 3.5) %>% 
  group_by(delta) %>% 
  summarise(mu = mean(success))

# plot success rate for all speeds 
x11()
df_sample %>%
  filter(speed == 3.5) %>%
  mutate(speed = as.factor(speed)) %>%
  ggplot(aes(delta, success,
             colour = speed,
             fill = speed)) + 
  geom_smooth(method = glm,
              method.args = list(family = "binomial"),
              se = F) +
  facet_wrap(~subj)

# plot end points for all speeds 
plt_sim_all <- df_sample %>%
  mutate(success = as.factor(success)) %>%
  ggplot(aes(target_x, target_y)) +
  geom_point(aes(x = 0, y = 100),
             shape = 4) +
  geom_jitter(aes(x = final_x, y = final_y,
                  colour = success,
  ), alpha = .3) +
  ggforce::geom_ellipse(aes(x0 = target_x,
                            y0 = 0,
                            a = reach,
                            b = reach, angle = 0),
                        alpha = .01) + 
  facet_grid(speed~delta) + 
  scale_x_continuous("X") + 
  # scale_y_continuous("Y", limits = c(-50, 100)) +
  theme_bw() +  
  theme(legend.position = "none")


# make a plot of this 
plt_sim <- df_sample %>% 
  filter(speed == 3.5) %>%
  mutate(success = as.factor(success)) %>%
  ggplot(aes(target_x, target_y)) +
  geom_point(aes(x = 0, y = 100),
             shape = 4) +
  geom_point(aes(x = final_x, y = final_y,
                 colour = success,
  ), alpha = .3) +
  ggforce::geom_ellipse(aes(x0 = target_x,
                            y0 = 0,
                            a = reach,
                            b = reach, angle = 0),
                        alpha = .01) + 
  facet_wrap(~delta, ncol = 1) + 
  scale_x_continuous("X") + 
  scale_y_continuous("Y", limits = c(-50, 100)) +
  theme_bw() +  
  theme(legend.position = "none",
        strip.text = element_blank())

# show human version of the above 
plt_human <- temp %>% 
  filter(!participant %in% "Pilot3") %>%
  mutate(delta = as.factor(abs(rescaled_delta)),
         Trial_targetXPos = abs(Trial_targetXPos),
         Trial_avatarFinalX = abs(Trial_avatarFinalX),
         rescaled_AvatarFinalX = abs(rescaled_AvatarFinalX),
         rescaled_targetXpos = abs(rescaled_targetXpos),
         success = as.factor(Trial_success),
         inside = ifelse(distance <= 1, 1, 0),
         inside = as.factor(inside)) %>% 
  ggplot(aes(rescaled_targetXpos, rescaled_targetYpos)) + 
  geom_point(aes(x = 0, y = 100),
             shape = 4) +
  geom_jitter(aes(rescaled_AvatarFinalX,rescaled_AvatarFinalY,
                  colour = inside,
  ), alpha = .3) +
  ggforce::geom_ellipse(aes(x0 = rescaled_targetXpos,
                            y0 = 0,
                            a = rescaled_reachY,
                            b = rescaled_reachX, angle = 0),
                        alpha = .01) +
  facet_wrap(~delta, ncol = 1) + 
  scale_x_continuous("X") + 
  scale_y_continuous("Y", limits = c(-50, 100)) +
  theme_bw() +  
  theme(legend.position = "none",
        strip.text = element_blank())

both <- arrangeGrob(plt_sim, plt_human)


# curves with the bot's performance 
plt_botCurve <- temp %>% 
  filter(!participant %in% c("Pilot1.3", "Pilot1.4")) %>%
  ggplot(aes(rescaled_delta, Trial_success)) + 
  geom_smooth(aes(colour = participant,
                  fill = participant),
              method = glm,
              method.args = list(family = "binomial"),
              se = F) + 
  geom_smooth(data = df_sample[df_sample$speed == 3.5,],
              aes(x = delta,
                  y =success),
              method = glm,
              method.args = list(family = "binomial"),
              se = T) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  theme_bw() +
  theme(legend.position = "none",
        axis.title.y = element_blank())
x11()
gridExtra::grid.arrange(both, plt_botCurve, ncol = 2)


# checking the bot behaved like a person 
check_bot <- df_sample %>% 
  filter(speed == 3.5) %>%
  mutate(rough_RTframe = respF,
         switch_delay = switch_del,
         type = "bot") %>% 
  select(type, rough_RTframe, down_stop_pos, switch_delay)
check_hum <- temp %>% 
  mutate(down_stop_pos = down_move * 3.5,
         type = "hum") %>% 
  select(type, rough_RTframe, down_stop_pos, switch_delay)
check_things <- rbind(check_bot, check_hum)

comp_RT <- check_things %>% 
  ggplot(aes(rough_RTframe, colour = type, fill = type)) + 
  geom_density(alpha = .3) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 alpha = .3,
                 position = "dodge")
comp_stop <- check_things %>% 
  ggplot(aes(down_stop_pos, colour = type, fill = type)) + 
  geom_density(alpha = .3) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 3.5,
                 alpha = .3,
                 position = "dodge")
comp_delay <- check_things %>% 
  ggplot(aes(switch_delay, colour = type, fill = type)) + 
  geom_density(alpha = .3) + 
  geom_histogram(aes(y = ..density..),
                 binwidth = 1,
                 alpha = .3,
                 position = "dodge")
x11()
grid.arrange(comp_RT, comp_delay, comp_stop)

#### Generate distances for each participant ####
df_acc <- tibble(participant = character(),
                 distance = numeric(),
                 accuracy = numeric())

df_dists <- tibble(participant = character(),
                   dist_type = character(),
                   distance = numeric(),
                   accuracy = numeric())

dists <- seq(150, 550, 1)

for(subj in unique(df_trials$participant)){
  ss <- df_trials %>% 
    filter(participant == subj)
  
  m <- glm(Trial_success ~ rescaled_delta,
           data = ss,
           family = "binomial")
  
  p <- predict(m, data.frame(rescaled_delta = dists), type = "response")
  df_acc <- rbind(df_acc, tibble(participant = subj,
                                 distance = dists,
                                 accuracy = p))
  
  t <- which(abs(p - .1) == min(abs(p - .1)))
  tf <- which(abs(p - .25) == min(abs(p - .25)))
  sf <- which(abs(p - .75) == min(abs(p - .75)))
  nf <- which(abs(p - .95) == min(abs(p - .95)))
  
  dist <- c(dists[t], dists[tf], dists[sf], dists[nf])
  acc <- c(p[t], p[tf], p[sf], p[nf])
  df_dists <- rbind(df_dists, tibble(participant = subj,
                                     dist_type = c("ten", "tfive", "sfive", "ninety"),
                                     distance = dist,
                                     accuracy = acc))
}

# make a plot of this 
# plot each participant separately with a point to show:
# - their avg acc
# - the line that was fit
# - and the lines for the points to equalise chance of success


df_trials %>% 
  # filter(participant != "Pilot1.3") %>%
  mutate(distance = rescaled_delta) %>%
  group_by(participant, distance) %>% 
  summarise(mu = mean(Trial_success)) %>% 
  ggplot(aes(distance, mu)) + 
  geom_point() + 
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              fullrange = T,
              se = F) + 
  scale_x_continuous(expression(paste("Normalised ", Delta)), limits = c(150, 550)) +
  scale_y_continuous("Success rate", labels = scales::percent_format(accuracy = 1)) +
  geom_segment(data = df_dists, aes(x = distance, y = 0, xend = distance, yend = accuracy),
               linetype = "dashed") + # distance
  geom_segment(data = df_dists, aes(x = 150, y = accuracy, xend = distance, yend = accuracy),
               linetype = "dashed") + # accuracy 
  facet_wrap(~participant) + 
  theme_bw()






