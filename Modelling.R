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

source("Functions/0_getLegend.R")

#### Processing ####
# filter out "bad" participants 
df_avatar <- df_avatar %>%
  filter(exclude == FALSE)

pre_remove <- nrow(df_avatar)

model_data <- df_avatar %>% 
  filter(abs_norm_place <= 1.1) %>% 
  mutate(abs_norm_place = ifelse(abs_norm_place > 1, 1, abs_norm_place),
         abs_norm_place = squash(abs_norm_place, 1, 0, 1e-4),
         norm_delta = Decision_Delta/max(Decision_Delta))

post_remove <- nrow(model_data)
data_loss <- 100 - ((post_remove/pre_remove)*100)
print(paste("data lost = ", round(data_loss, digits = 2), "%", sep =""))

# save
save(model_data, file = "scratch/model_data")

# tidy 
rm(post_remove, pre_remove)

#### Modelling #### 
# parameters for all models 
m_iter <- 2000
m_control <- list(adapt_delta = .95)

#### > Rand effects: 2 categorical predictors standard priors ####
m1 <- brm(abs_norm_place ~ Condition * dist_type + (Condition * dist_type|participant),
          data = model_data,
          family = "beta",
          chains = 1,
          iter = m_iter,
          warmup = m_iter/2,
          control = m_control)
beep()
# save 
save(m1, file = "ModelOutput/m_noPriors")


#### > Rand effects: 2 categorical predictors new priors ####
m_priors <- c(set_prior("student_t(3, -1.41, 1)",
                        class = "Intercept"),
              set_prior("student_t(3, 0, 1)",
                        class = "b",
                        coef = "ConditionAutomatic"),
              set_prior("student_t(3, 0, 1)",
                        class = "b",
                        coef = "ConditionAutomatic:dist_typeFar"),
              set_prior("student_t(3, 1.55, 1)",
                        class = "b",
                        coef = "dist_typeFar"))

# model 
m2 <- brm(abs_norm_place ~ Condition * dist_type + (Condition * dist_type|participant),
          prior = m_priors,
          data = model_data,
          family = "beta",
          chains = 1,
          iter = m_iter,
          warmup = m_iter/2,
          control = m_control)
beep()
# save
save(m2, file = "ModelOutput/m_withPriors")

#### > > Run priors only version ####
m3 <- brm(abs_norm_place ~ Condition * dist_type + (Condition * dist_type|participant),
          prior = m_priors,
          data = model_data,
          family = "beta",
          chains = 1,
          iter = m_iter,
          warmup = m_iter/2,
          sample_prior = "only",
          control = m_control)
beep()

#save 
save(m3, file = "ModelOutput/m_priorsonly")

#### Plotting ####
# plotting the outputs of the modelling

plt_raw <- model_data %>%
  # filter(Decision_Delta %in% c(200, 500)) %>%
  ggplot(aes(dist_type, abs_norm_place,
             colour = Condition,
             fill = Condition)) +
  geom_boxplot(alpha = .3) +
  # geom_density(alpha = .3, bw = .05) +
  see::scale_color_flat() + 
  see::scale_fill_flat() +
  facet_wrap(~participant, ncol = 6) +
  theme_bw() + 
  theme(strip.text.x = element_blank(),
        legend.position = "bottom")

plt_model <- model_data %>% 
  add_predicted_draws(m2) %>% 
  group_by(.draw, Condition, dist_type, participant) %>% 
  summarise(mu = mean(.prediction)) %>% 
  ggplot(aes(mu, 
             colour = Condition,
             fill = Condition)) + 
  geom_density(alpha = .3,
               bw = .05) + 
  see::scale_color_flat() + 
  see::scale_fill_flat() + 
  scale_x_continuous("Normalised Placement",
                     breaks = seq(0,1,.5)) +
  theme_bw() +
  theme(legend.position = "bottom") + 
  facet_wrap(~dist_type)

legend <- g_legend(plt_raw)
x11()
grid.arrange(arrangeGrob(plt_raw + theme(legend.position = "none"), plt_model + theme(legend.position = "none"), ncol = 2),
             legend, heights = c(10,1))




