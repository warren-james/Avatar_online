#### Making plots ####
# This script makes the plots for the real data

#### library ####
library(tidyverse)

#### Source ####
source("ProcessData/ProcessData.R")

df_avatar%>% 
  filter(exclude == FALSE) %>%
  group_by(participant, Condition) %>% 
  summarise(chance = mean(chance)) %>% 
  ungroup() %>% 
  group_by(Condition) %>% 
  summarise(n = n())

df_avatar <- df_avatar %>%
  filter(exclude == FALSE)

df_demo <- df_demo %>% 
  filter(!participant %in% Manual_exclude)

#### Plots ####
#### > Demo plots ####
# we want to plot accuracy over distance 
# also, maybe some plots to look at how close people got to the target? 
plt_acc <- df_demo %>% 
  group_by(participant, Demo_delta) %>% 
  mutate(acc = mean(Demo_Success)) %>% 
  ggplot(aes(Demo_delta, Demo_Success,
             colour = Condition)) +
  # ggplot(aes(abs(Demo_xposTarget), Demo_Success)) +
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              se = F) + 
  geom_point(aes(Demo_delta, acc)) +
  # geom_point(aes(abs(Demo_xposTarget), acc)) + 
  facet_wrap(~participant, ncol = 6) + 
  see::scale_color_flat() +
  theme_bw() + 
  theme(strip.text.x = element_blank())
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
                            angle = 0,
                            colour = Condition),
                        alpha = .01) + 
  facet_grid(participant~abs_target) + 
  see::scale_color_flat() +
  theme_bw() + 
  theme(strip.text.x = element_blank())
# x11()
# plt_end

# arrange all together 
# x11()
# gridExtra::grid.arrange(plt_end, plt_acc, widths = c(4, 3), ncol = 2)

#### > Decision plots ####
# trim outliers?
df_avatar <- df_avatar %>% 
  filter(abs_norm_place <= 1.1)

#### > > Placement ####
#### > > > Overall ####
# plot where they placed the truck 
plt_pos_overall <- df_avatar %>%
  # filter(Decision_Delta %in% c(200, 500)) %>%
  ggplot(aes(abs_norm_place, 
             colour = Condition,
             fill = Condition)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  scale_x_continuous(expression(paste("Normalised ", Delta, sep = ""))) +
  facet_wrap(~dist_type) +
  theme_bw()

#### > > > By participant ####
# need to fix this...
# plt_pos_pp <- df_avatar %>%
#   filter(Decision_Delta %in% c(200, 500)) %>%
#   ggplot(aes(abs_norm_place,
#              colour = dist_type,
#              fill = dist_type)) +
#   geom_density(alpha = .3) +
#   see::scale_color_flat() +
#   see::scale_fill_flat() +
#   scale_x_continuous(expression(paste("Normalised ", Delta, sep = ""))) +
#   theme_bw() +
#   theme(strip.text.x = element_blank()) +
#   facet_wrap(~participant + Condition, ncol = 5)
#   facet_grid(~Condition + participant)
 
#### > > Chance ####
#### > > > Overall ####
plt_chance_overall <- df_avatar %>% 
  ggplot(aes(chance, 
             colour = Condition,
             fill = Condition)) + 
  geom_density(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  scale_x_continuous("Chance", labels = scales::percent_format(accuracy = 1)) +
  facet_wrap(~dist_type) + 
  theme_bw()

#### > > > By participant ####
# need to think how best to show this... 
# plt_chance_pp <- df_avatar %>% 
#   filter(Decision_Delta %in% c(200, 500)) %>%
#   ggplot(aes(chance, 
#              colour = dist_type,
#              fill = dist_type)) + 
#   # geom_density(alpha = .3) + 
#   geom_histogram(alpha = .3,
#                  position = "dodge",
#                  binwidth = .1) +
#   see::scale_color_flat() + 
#   see::scale_fill_flat() +
#   scale_x_continuous("Chance", labels = scales::percent_format(accuracy = 1)) +
#   theme_bw() +
#   theme(strip.text.x = element_blank()) +
#   facet_wrap(~participant, ncol = 5) 


#### > Scaled by participant accuracy ####
plt_scaled <- df_avatar %>% 
  mutate(
    dist = Decision_xposTarget
    # dist = Decision_Delta
    ) %>% 
  merge(df_acc) %>% 
  ggplot(aes(acc, abs_norm_place, 
             colour = Condition)) +
  geom_jitter(alpha = .3,
              position = position_dodge(.1)) +
  see::scale_colour_flat() + 
  scale_x_continuous("Chance of success from the centre",
                     labels = scales::percent_format(accuracy = 1),
                     breaks = c(.25, .75)) +
  scale_y_continuous(expression(paste("Normalised ", Delta, sep = ""))) + 
  facet_wrap(~participant, ncol = 6) + 
  theme_bw() + 
  theme(strip.text.x = element_blank())

x11() 
plt_scaled






