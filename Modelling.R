#### Modelling ####
# Modelling the Avatar online data 

#### library ####
library(tidyverse)
library(brms)

#### Source ####
# gets data files and functions
source("ProcessData/ProcessData.R")

#### Functions ####
squash <- function(y, max, min, squash){
  y <- y * ((max-squash) - (min + squash)) + (min + squash)
}

#### Processing ####
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
m1 <- brm(abs_norm_place ~ Condition * norm_delta + (Condition * norm_delta|participant),
          data = model_data,
          family = "beta",
          chains = 1,
          iter = 1000,
          warmup = 500)
