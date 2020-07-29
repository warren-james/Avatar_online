#### Setup SPSS files ####
# this script will produce some SPSS files for analysis... 
# ugh

#### library ####
library(tidyverse)

#### load data ####
source("2_process_data.R")

#### Make SPSS friendly version ####
#### > process a bit ####
df_avatar <- df_avatar %>%
  mutate(abs_placedX = abs(Decision_placement),
         abs_targetX = abs(Decision_xposTarget),
         abs_normplace = abs_placedX/abs_targetX,
         dist_type = ifelse(Decision_Delta < 400, "Close", "Far"))


#### > Make wide data ####
df_SPSS_pos <- df_avatar %>%
  group_by(participant, dist_type) %>% 
  summarise(pos = mean(abs_normplace)) %>% 
  mutate(pos = round(pos, digits = 3)) %>% 
  spread(dist_type, pos)
  
df_SPSS_acc <- df_avatar %>%
  group_by(participant, dist_type) %>% 
  summarise(acc = mean(chance)) %>% 
  mutate(acc = round(acc, digits = 3)) %>% 
  spread(dist_type, acc)

#### > write these to .txt files ####
write.table(df_SPSS_acc, file = "scratch/SPSS_acc.txt", row.names = F)
write.table(df_SPSS_pos, file = "scratch/SPSS_pos.txt", row.names = F)

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
  see::scale_fill_flat()

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
  see::scale_fill_flat()

grid.arrange(plt_pos, plt_chance, ncol = 2)

#### bad plots ####
line_acc <- df_SPSS_acc %>%
  gather(Close:Far,
         key = "Distance",
         value = "Chance") %>% 
  mutate(Driving = "Manual") %>%
  group_by(Distance, Driving) %>% 
  summarise(mu = mean(Chance),
            sdev = sd(Chance)) %>%
  ggplot(aes(Distance, mu,
             colour = Driving)) + 
  geom_point() +
  # geom_errorbar(aes(ymin = mu-sdev, ymax = mu +sdev)) +
  geom_line(aes(group = Driving)) +
  scale_y_continuous("Mean Chance",
                     labels = scales::percent_format(accuracy = 1)) + 
  scale_colour_manual(values = c("#e74c3c")) +
  theme_bw()
  
line_pos <- df_SPSS_pos %>%
  gather(Close:Far,
         key = "Distance",
         value = "Pos") %>% 
  mutate(Driving = "Manual") %>%
  group_by(Distance, Driving) %>% 
  summarise(mu = mean(Pos),
            sdev = sd(Pos)) %>%
  ggplot(aes(Distance, mu,
             colour = Driving)) + 
  geom_point() +
  # geom_errorbar(aes(ymin = mu-sdev, ymax = mu +sdev)) +
  geom_line(aes(group = Driving)) +
  scale_y_continuous("Mean Position") + 
  scale_colour_manual(values = c("#e74c3c")) +
  theme_bw()

gridExtra::grid.arrange(line_acc, line_pos)
