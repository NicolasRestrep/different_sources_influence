####################### 
#  Drugs ############
#######################

# Packages
library(tidyverse)
library(naniar)
library(haven)
library(igraph)
library(lme4)
library(brms)

# Data 
wave1 <- read_sav("Data/PupilsWaveV.sav")
wave2 <- read_sav("Data/PupilsWaveW_geanonimiseerd.sav")
wave3 <- read_sav("Data/PupilsWaveX.sav")
wave4 <- read_sav("Data/PupilsWaveY.sav")

# Function for getting the scores 
get_scores <- function(df, school,dv) {
  # Begin by taking the friendship columns 
  netdf <- df %>% 
    filter(schoolnr == school) %>% 
    select(namenr, schoolnr, contains('emosu'), contains('perso'), 
           contains('frien'))
  # Build the network 
  net_mat <- matrix(0, max(netdf$namenr), max(netdf$namenr))
  for (j in 1:max(netdf$namenr)) {
    vect <- netdf[netdf$namenr==j,-c(1,2)] %>% as.numeric(.)
    vect[which(vect > max(netdf$namenr))] <- NA
    matches <- unique(vect[which(vect>0)])
    
    if (is_empty(matches)) { 
      net_mat[j,] <- 0
    } else {
      for (i in 1:length(matches)) {
        net_mat[j,matches[[i]]] <- 1
      }
    }
  }
  network <- graph_from_adjacency_matrix(net_mat, mode = 'directed')
  # Function to go through all the kids in a school 
  calculate_school <- function(x){
    friends <- which(net_mat[x,]==1)
    cent_scores <- eigen_centrality(network)$vector
    popular <- which(cent_scores >= quantile(cent_scores, 0.85))
    
    avg_friends <- df %>% 
      filter(schoolnr == school, 
             namenr %in% friends) %>% 
      select(dv) %>% 
      rename(value = dv) %>% 
      summarise(avg = mean(value))
    
    pop_score <- df %>% 
      filter(schoolnr == school, 
             namenr %in% popular) %>% 
      select(dv) %>% 
      rename(value = dv) %>% 
      summarise(pop_avg = mean(value))
    
    return(c(namenr=x, 
             schoolnr = school, 
             popular_score = pop_score$pop_avg, 
             friends_score = avg_friends$avg))
  }
  pupil_names <- unique(netdf$namenr)
  scores_data <- map_df(pupil_names, calculate_school)
  
  return(scores_data)
}

wave1 <-  wave1 %>% 
  mutate(actdrug = case_when(actdrug == 1 ~ 0, 
                              actdrug == 2 ~ 1, 
                              actdrug == 3 ~ 1, 
                              actdrug == 4 ~ 1, 
                              actdrug == 5 ~ 1)) 


wave2 <-  wave2 %>% 
  mutate(actdrugb = case_when(actdrugb == 1 ~ 0, 
                              actdrugb == 2 ~ 1, 
                              actdrugb == 3 ~ 1, 
                              actdrugb == 4 ~ 1, 
                              actdrugb == 5 ~ 1)) 


wave3 <-  wave3 %>% 
  mutate(actdrugc = case_when(actdrugc == 1 ~ 0, 
                              actdrugc == 2 ~ 1, 
                              actdrugc == 3 ~ 1, 
                              actdrugc == 4 ~ 1, 
                              actdrugc == 5 ~ 1))


wave4 <-  wave4 %>% 
  mutate(actdrugd = case_when(actdrugd == 1 ~ 0, 
                              actdrugd == 2 ~ 1, 
                              actdrugd == 3 ~ 1, 
                              actdrugd == 4 ~ 1, 
                              actdrugd == 5 ~ 1)) 


# Get scores for first wave 
list_schools <- unique(wave1$schoolnr)
alc_scores_w1 <- map_df(list_schools, get_scores, df = wave1, dv = 'actdrug')

# Get scores for second wave 
# Get list of schools for wave2 
list_schools_w2 <- unique(wave2$schoolnr)
# Annoyingly they are not the same
# And there is one value missing 
list_schools_w2 <- list_schools_w2[-c(1)]
alc_scores_w2 <- map_df(list_schools_w2, get_scores, df = wave2, dv = 'actdrugb')

# Get scores for wave 3 and 4 
list_schools_w3 <- unique(wave3$schoolnr)
list_schools_w4 <- unique(wave4$schoolnr)
alc_scores_w3 <- map_df(list_schools_w3, get_scores, df = wave3, dv = 'actdrugc')
alc_scores_w4 <- map_df(list_schools_w4, get_scores, df = wave4, dv = 'actdrugd')

# Join Wave 1 
df1 <- alc_scores_w1 %>% 
  mutate(namenr = as.double(namenr)) %>% 
  right_join(., wave1, by = c('namenr', 'schoolnr'))

# Join Wave 2 
df2 <- alc_scores_w2 %>% 
  mutate(namenr = as.double(namenr)) %>% 
  right_join(., wave2, by = c('namenr', 'schoolnr'))

# Join Wave 3 
df3 <- alc_scores_w3 %>% 
  mutate(namenr = as.double(namenr)) %>% 
  right_join(., wave3, by = c('namenr', 'schoolnr'))

# Join Wave 4 
df4 <- alc_scores_w4 %>% 
  mutate(namenr = as.double(namenr)) %>% 
  right_join(., wave4, by = c('namenr', 'schoolnr'))

# Create unique IDs
df1 <- df1 %>% mutate(id = paste0(schoolnr,namenr))
df2 <- df2 %>% filter(schoolnr != " ") %>% 
  mutate(schoolnr = str_trim(tolower(schoolnr))) %>% 
  mutate(id =paste0("0",schoolnr, namenr))
df3 <- df3 %>% mutate(id = paste0(schoolnr, namenr))
df4 <- df4 %>% mutate(id = paste0(schoolnr, namenr))

# Now let's find the intersections 
ids_all_waves <- Reduce(intersect, list(df1$id, df2$id, df3$id, df4$id))

drug_w1 <- df1 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actdrug) %>% 
  rename(friends_w1 = friends_score, 
         popular_w1 = popular_score, 
         actdrug_w1 = actdrug)

drug_w2 <- df2 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actdrugb) %>% 
  rename(friends_w2 = friends_score, 
         popular_w2 = popular_score, 
         actdrug_w2 = actdrugb)

drug_w3 <- df3 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actdrugc) %>% 
  rename(friends_w3 = friends_score, 
         popular_w3 = popular_score, 
         actdrug_w3 = actdrugc)

drug_w4 <- df4 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actdrugd) %>% 
  rename(friends_w4 = friends_score, 
         popular_w4 = popular_score, 
         actdrug_w4 = actdrugd)

# Join the datasets 
df_wide <- left_join(drug_w1, drug_w2, by = "id") %>% 
  left_join(., drug_w3, by = "id") %>% 
  left_join(., drug_w4, by = "id")

write_csv(df_wide, 
          "~/Documents/global_local_influence/Data/drug_wide.csv", col_names = T)

# Now create the long dataframe 
friends_long <- df_wide %>% 
  pivot_longer(cols = contains('friends'), names_to = "wave", values_to = "friends") %>% 
  select(id, friends) 

popular_long <- df_wide %>% 
  pivot_longer(cols = contains('popular'), names_to = "wave", values_to = "popular") %>% 
  select(popular)

drug_long <- df_wide %>% 
  pivot_longer(cols = contains('actdrug'), names_to = "wave", values_to = "actdrug") %>% 
  select(actdrug)

df_long <- cbind(friends_long, popular_long, drug_long)

df_long <- df_long %>% 
  mutate(popular = as.numeric(popular), 
         friends = as.numeric(friends), 
         id = as.factor(id))

dfl_clean <- drop_na(df_long) 
actdrug <- zap_labels(dfl_clean$actdrug)
dfl_clean <- dfl_clean %>% 
  mutate(actdrug=zap_labels(actdrug))

# Fist model 
b1 <- glmer(actdrug ~ 1 + popular + friends + popular*friends + (1 | id), 
           data = dfl_clean, 
           family = 'binomial')

summary(b1)

write_csv(dfl_clean, 
          "~/Documents/global_local_influence/Data/drug_binary.csv", 
          col_names = TRUE)