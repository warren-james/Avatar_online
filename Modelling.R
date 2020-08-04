#### Modelling ####
# Modelling the Avatar online data 

#### library ####
library(tidyverse)
library(brms)
library(tidybayes)

#### Source ####
# gets data files and functions
source("ProcessData/ProcessData.R")

#### Functions ####
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
}

#### Processing ####
# filter out "bad" participants 
df_avatar <- df_avatar %>%
  filter(exclude == FALSE)

pre_remove <- nrow(df_avatar)

model_data <- df_avatar %>% 
  filter(abs_norm_place <= 1.1) %>% 
  mutate(abs_norm_place = ifelse(abs_norm_place > 1, 1, abs_norm_place),
         abs_norm_place = squash(abs_norm_place, 1, 0, 1e-4),
         norm_delta = Decision_Delta/max(Decision_Delta),
         Condition = as.factor(Condition))
  
post_remove <- nrow(model_data)
data_loss <- 100 - ((post_remove/pre_remove)*100)
print(paste("data lost = ", round(data_loss, digits = 2), "%", sep =""))

# tidy 
rm(post_remove, pre_remove)

#### Modelling #### 
# parameters for all models 
m_iter <- 2000
m_control <- list(adapt_delta = .9)

#### > Rand effects: 2 categorical predictors ####
# check if this exists before running 
if("m1" %in% dir("ModelOutput/")){
  print("m1 found. Loading model")
  load("ModelOutput/m1")
} else {
  m1 <- brm(abs_norm_place ~ Condition * dist_type + (Condition * dist_type|participant),
            data = model_data,
            family = "beta",
            chains = 1,
            iter = m_iter,
            warmup = m_iter/2,
            control = m_control)
  beep()
  # save 
  save(m1, file = "ModelOutput/m1")
}

#### Plotting ####
# plotting the outputs of the modelling
x11()
model_data %>%
  # filter(Decision_Delta %in% c(200, 500)) %>%
  ggplot(aes(dist_type, abs_norm_place,
             colour = Condition,
             fill = Condition)) +
  geom_boxplot(alpha = .3) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  # facet_wrap(~participant, ncol = 6) + 
  theme_bw() + 
  theme(strip.text.x = element_blank())

model_data %>% 
  add_predicted_draws(m1) %>% 
  group_by(.draw, Condition, dist_type, participant) %>% 
  summarise(mu = mean(.prediction)) %>% 
  ggplot(aes(mu, 
             colour = Condition,
             fill = Condition)) + 
  geom_density(alpha = .3,
               bw = .05) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  theme_bw() +
  facet_wrap(~dist_type)
  
  




