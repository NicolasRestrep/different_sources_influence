### Alcohol figure ####
set.seed(33)
library(panelr)
library(tidyverse)
library(brms)
library(rethinking)
options(mc.cores = parallel::detectCores())

# Import the data 
df_wide <- read_csv('Data/alcohol_wide.csv')
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
                      "actalcoh"), 
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

median_popular <- median(dfl_scaled$popular_z)
median_friend <- median(dfl_scaled$friends_z)
one_sd_popular <- median_popular+sd(dfl_scaled$popular)
one_sd_friend<- median_friend+sd(dfl_scaled$friends_z)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(alcmodel, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

alc_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Probability as scores increase",
       subtitle = "Alcohol",
       fill = "Type of Increase") + 
  theme(legend.position = 'top') +
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))


saveRDS(alc_p, "Figures/alcohol_pp.R")
  
#### Drug Figure ####

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

median_popular <- median(dfl_scaled$popular_z)
median_friend <- median(dfl_scaled$friends_z)
one_sd_popular <- median_popular+sd(dfl_scaled$popular)
one_sd_friend<- median_friend+sd(dfl_scaled$friends_z)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(drugmodel, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

drug_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Soft Drugs",
       fill = "") + 
  theme(legend.position = 'none') + 
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))
saveRDS(drug_p, "Figures/drug_pp.R")


### Smoke Figure #### 

# Import the data 
df_wide <- read_csv('Data/smoke_wide.csv')
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
                      "actsmok"), 
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

median_popular <- median(dfl_scaled$popular_z)
median_friend <- median(dfl_scaled$friends_z)
one_sd_popular <- median_popular+sd(dfl_scaled$popular)
one_sd_friend<- median_friend+sd(dfl_scaled$friends_z)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(smokemodel, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

smoke_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Smoking",
       fill = "") + 
  theme(legend.position = 'none') + 
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))
saveRDS(smoke_p, "Figures/smoke_pp.R")



