library(tidyverse)
library(brms)
library(panelr)
#### Bike predicted probabilities #### 
# Import the data 
dfl_scaled <- read_csv('Data/bike_std.csv')
bike_model <- readRDS('Analyses/bike_model.rds')

median_popular <- median(dfl_scaled$popular_z)
median_friend <- median(dfl_scaled$friends_z)
one_sd_popular <- median_popular+sd(dfl_scaled$popular)
one_sd_friend<- median_friend+sd(dfl_scaled$friends_z)
set.seed(11)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(bike_model, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

bike_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Probability as scores increase",
       subtitle = "Running a red light",
       fill = "Type of Increase") + 
  theme(legend.position = 'top') +
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))

saveRDS(bike_p, "Figures/bike_pp.R")

#### Fare predicted probabilites ####

dfl_scaled <- read_csv('Data/fare_std.csv')
fare_model <- readRDS('Analyses/fare_model.rds')

median_popular <- median(dfl_scaled$popular_z)
median_friend <- median(dfl_scaled$friends_z)
one_sd_popular <- median_popular+sd(dfl_scaled$popular)
one_sd_friend<- median_friend+sd(dfl_scaled$friends_z)
set.seed(33)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(fare_model, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

fare_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Skipping traffic fare",
       fill = "") + 
  theme(legend.position = 'none') +
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))

saveRDS(fare_p, "Figures/fare_pp.R")

#### Copy CD ####
dfl_scaled <- read_csv('Data/copy_std.csv')
copy_model <- readRDS('Analyses/copy_model.rds')

median_popular <- median(dfl_scaled$popular_z)
median_friend <- median(dfl_scaled$friends_z)
one_sd_popular <- median_popular+sd(dfl_scaled$popular)
one_sd_friend<- median_friend+sd(dfl_scaled$friends_z)
set.seed(33)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(copy_model, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

copy_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Copy illegal CD",
       fill = "") + 
  theme(legend.position = 'none') +
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))

saveRDS(copy_p, "Figures/copy_pp.R")

#### Alcohol ####
alcohol_model <- readRDS('Analyses/alcohol_model.rds')
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
set.seed(3)
person <- sample(dfl_scaled$id, 1)
newdata <- data.frame(id = rep(person, 200), 
                      friends_z = rep(c(median_friend, one_sd_friend), each = 100), 
                      popular_z = rep(c(one_sd_popular, median_popular), each = 100), 
                      wave = rep(3, 200),
                      type = rep(c("global score", "local score"), each = 100)) 
preds <- predict(alcohol_model, 
                 newdata = newdata)

newdata$predicted_probs <- preds[,1]

alco_p <- newdata %>% 
  ggplot(aes(x = predicted_probs)) + 
  geom_density(aes(fill = type), alpha = 0.5) + 
  theme_light() + 
  labs(x = "Predicted Probability", 
       y = "Density", 
       title = "Alcohol",
       fill = "") + 
  theme(legend.position = 'none') +
  scale_fill_manual(values = c("global score"="red",
                               "local score" = 'lightblue'))

saveRDS(alco_p, "Figures/alcohol_pp.R")

