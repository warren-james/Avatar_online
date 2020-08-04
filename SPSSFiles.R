#### Setup SPSS files ####
# this script will produce some SPSS files for analysis... 

#### library ####
library(tidyverse)

#### load data ####
source("ProcessData/ProcessData.R")

#### Functions ####
source("Functions/0_getLegend.R")

#### Make SPSS friendly version ####
#### > Make wide data ####
# filter out "bad" participants 
df_avatar <- df_avatar %>%
  filter(exclude == FALSE)

df_SPSS_pos <- df_avatar %>%
  group_by(participant, dist_type, Condition) %>% 
  summarise(pos = mean(abs_norm_place)) %>% 
  mutate(pos = round(pos, digits = 3)) %>% 
  spread(dist_type, pos)
  
df_SPSS_acc <- df_avatar %>%
  group_by(participant, dist_type, Condition) %>% 
  summarise(acc = mean(chance)) %>% 
  mutate(acc = round(acc, digits = 3)) %>% 
  spread(dist_type, acc)

#### TEMPORARY #### 
# this bit is just until we have a suitable set of Manual response participants 
# we'll take the 20 that fulfill a looser criteria for now from the Manual response
# and 20 at random from the Automatic 
# set seed so we should always get the same participants
# set.seed(1234)
# # get participants to keep 
# manual_participants <- unique(Manual_keep$participant[Manual_keep$exclude == "keep"])
# auto_participants <- sample(unique(df_trials$participant[df_trials$Condition == "Automatic"]), 20, replace = F)
# participants <- c(as.character(manual_participants), as.character(auto_participants))
# 
# df_SPSS_acc <- df_SPSS_acc %>% 
#   filter(participant %in% participants)
# df_SPSS_pos <- df_SPSS_pos %>%  
#   filter(participant %in% participants)

#### > write these to .txt files ####
write.table(df_SPSS_acc, file = "scratch/SPSS_acc.txt", row.names = F)
write.table(df_SPSS_pos, file = "scratch/SPSS_pos.txt", row.names = F)

#### Demographics ####
participants <- unique(df_SPSS_acc$participant)
SPSS_demographics <- df_demographics %>% 
  filter(participant %in% participants) %>%
  mutate(Gender = tolower(Gender)) 

# save 
write.table(SPSS_demographics, file = "scratch/demographics.txt", row.names = F)

#### make quick plots ####
plt_chance <- df_SPSS_acc %>% 
  gather(Close:Far, 
         key = "dist",
         value = "chance") %>%
  ggplot(aes(chance, colour = dist, fill = dist)) + 
  geom_density(alpha = .3) + 
  guides(fill = guide_legend("Distance"),
         colour = guide_legend("Distance")) + 
  theme_bw() + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  facet_wrap(~Condition) +
  theme(legend.position = "bottom")

plt_pos <- df_SPSS_pos %>% 
  gather(Close:Far, 
         key = "dist",
         value = "pos") %>%
  ggplot(aes(pos, colour = dist, fill = dist)) + 
  geom_density(alpha = .3) + 
  scale_x_continuous("Normalised Position") + 
  guides(fill = guide_legend("Distance"),
         colour = guide_legend("Distance")) + 
  theme_bw()+ 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  facet_wrap(~Condition) +
  theme(legend.position = "none")

grid.arrange(plt_pos, plt_chance, ncol = 2)

#### line plots ####
line_acc <- df_SPSS_acc %>%
  gather(Close:Far,
         key = "Distance",
         value = "Chance") %>% 
  group_by(Distance, Condition) %>% 
  summarise(mu = mean(Chance),
            sdev = sd(Chance)) %>%
  ggplot(aes(Distance, mu,
             colour = Condition)) + 
  geom_point() +
  # geom_errorbar(aes(ymin = mu-sdev, ymax = mu +sdev)) +
  geom_line(aes(group = Condition)) +
  scale_y_continuous("Mean Chance",
                     labels = scales::percent_format(accuracy = 1)) + 
  # scale_colour_manual(values = c("#e74c3c")) +
  see::scale_color_flat() +
  theme_bw() +
  theme(legend.position = "bottom")
  
line_pos <- df_SPSS_pos %>%
  gather(Close:Far,
         key = "Distance",
         value = "Pos") %>% 
  group_by(Distance, Condition) %>% 
  summarise(mu = mean(Pos),
            sdev = sd(Pos)) %>%
  ggplot(aes(Distance, mu,
             colour = Condition)) + 
  geom_point() +
  # geom_errorbar(aes(ymin = mu-sdev, ymax = mu +sdev)) +
  geom_line(aes(group = Condition)) +
  scale_y_continuous("Mean Position") + 
  # scale_colour_manual(values = c("#e74c3c")) 
  see::scale_colour_flat() +
  theme_bw() +
  theme(legend.position = "none")

# setup plot together
line_legend <- g_legend(line_acc)
x11()
grid.arrange(arrangeGrob(line_acc + theme(legend.position = "none"), line_pos, ncol = 2),
             line_legend, heights = c(10,1))






