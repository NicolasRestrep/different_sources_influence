library(panelr)
library(tidyverse)
library(brms)
library(rethinking)
options(mc.cores = parallel::detectCores())

# Import the data 
df_wide <- read_csv('Data/drug_wide.csv')
# Turn into a long panel dataset 
df_long <- long_panel(df_wide, 
                      id = "id", 
                      prefix = "_w", 
                      begin = 1, 
                      end = 4, 
                      label_location = "end")

# Rename, change, and clean 
dfl <- df_long %>% 
  mutate_at(.vars = c("friends", 
                      "popular", 
                      "actdrug"), 
            as.numeric) %>% 
  filter(!is.na(friends) & 
           !is.nan(friends) & 
           !is.na(popular) & 
           !is.nan(popular)) 

dfl_scaled <- dfl
dfl_scaled$friends_z <- (dfl_scaled$friends-mean(dfl_scaled$friends)/sd(dfl_scaled$friends))
dfl_scaled$popular_z <- (dfl_scaled$popular-mean(dfl_scaled$popular)/sd(dfl_scaled$popular))  

dfl_scaled <- dfl_scaled %>% 
  panel_data(., id = id, wave = wave)


# Run the model 

b1 <- brm(actdrug ~ 1 + friends_z + wave + popular_z + (1 +  wave | id), 
          family = bernoulli, 
          data = dfl_scaled, 
          prior = c(prior(normal(0, 1.5), class = b),
                    prior(student_t(3, 0, 2.5), class = Intercept), 
                    prior(student_t(3, 0, 2.5), class = sd), 
                    prior(lkj(1), class = cor)), 
          cores = 6, 
          chains = 3, 
          iter = 4000, 
          warmup = 1000)

# Save model 
saveRDS(b1, "Analyses/drug_model.rds")
