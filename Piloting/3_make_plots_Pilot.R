#### Making plots ####
# this uses the pilot data for now, but it should be fine 

#### library ####
library(tidyverse)

#### Source ####
source("2_process_data.R")

#### Plots ####
#### > Demo plots ####
# we want to plot accuracy over distance 
# also, maybe some plots to look at how close people got to the target? 
plt_acc <- df_demo %>% 
  group_by(participant, Demo_delta) %>% 
  mutate(acc = mean(Demo_Success)) %>% 
  ggplot(aes(Demo_delta, Demo_Success)) +
  # ggplot(aes(abs(Demo_xposTarget), Demo_Success)) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = F) + 
  geom_point(aes(Demo_delta, acc)) +
  # geom_point(aes(abs(Demo_xposTarget), acc)) + 
  facet_wrap(~participant)
# x11()
# plt_acc


plt_end <- df_demo %>% 
  mutate(acc = as.factor(Demo_Success),
         abs_finalx = abs(Demo_finalX),
         success = as.factor(Demo_Success),
         abs_target = abs(Demo_xposTarget),
         side = ifelse(Demo_xposTarget < 0, "left", "right")) %>%
  ggplot(aes(abs_finalx, Demo_finalY)) + 
  geom_point(aes(x = 0, y = 100),
             shape = 4) +
  geom_jitter(aes(x = abs_finalx, y =  Demo_finalY,
                  colour = success,
  ), alpha = .3) +
  ggforce::geom_ellipse(aes(x0 = abs_target,
                            y0 = 0,
                            a = Params_Reach,
                            b = Params_Reach,
                            angle = 0),
                        alpha = .01) + 
  facet_grid(participant~abs_target)
# x11()
# plt_end

# arrange all together 
x11()
gridExtra::grid.arrange(plt_end, plt_acc, widths = c(4, 3), ncol = 2)

#### > Decision plots ####
#### > > Placement ####
# plot where they placed the truck 
test <- df_avatar %>% 
  mutate(abs_placedX = abs(Decision_placement),
         abs_targetX = abs(Decision_xposTarget),
         abs_normplace = abs_placedX/abs_targetX,
         dist_type = ifelse(Decision_Delta < 400, "Close", "Far"))

test %>% 
  ggplot(aes(abs_normplace,
             colour = dist_type,
             fill = dist_type)) +
  geom_density(alpha = .3) + 
  facet_wrap(~participant)

# box plot of placement 
test %>% 
  mutate(delta = as.factor(Decision_Delta)) %>%
  ggplot(aes(delta, abs_normplace)) + 
  geom_boxplot(alpha = .3) + 
  geom_point(alpha = .3) + 
  facet_wrap(~participant)

#### > > Chance ####
df_avatar %>% 
  ggplot(aes(chance)) + 
  # geom_density(fill = "black",
  #              alpha = .3) +
  geom_histogram(aes(y = ..density..),
                 binwidth = .2) +
  scale_x_continuous("Chance of success") +
  # facet_wrap(~Decision_Delta)
  facet_grid(Decision_Delta ~ participant, scales = "free")

# make a box plot? 
df_avatar %>% 
  ggplot(aes(Decision_Delta, chance)) + 
  geom_boxplot(aes(group = Decision_Delta),
               alpha = .3) + 
  geom_point(alpha = .3) + 
  facet_wrap(~participant)

# need to do something to account for their difference in accuracy... 










