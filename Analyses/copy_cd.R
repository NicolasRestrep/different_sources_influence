#### Late - Preprocessing #### 

# Libraries
library(tidyverse)
library(naniar)
library(haven)
library(igraph)
library(lme4)
library(brms)
library(panelr)

# Read in the data 
wave1 <- read_sav("~/Documents/global_local_influence/Data/PupilsWaveV.sav")
wave2 <- read_sav("~/Documents/global_local_influence/Data/PupilsWaveW_geanonimiseerd.sav")
wave3 <- read_sav("~/Documents/global_local_influence/Data/PupilsWaveX.sav")
wave4 <- read_sav("~/Documents/global_local_influence/Data/PupilsWaveY.sav")

# Function for calculating global and local scores
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

# Transfrom into dummy variable for all waves 
wave1 <-  wave1 %>% 
  mutate(actcopy = case_when(actcopy == 1 ~ 0, 
                             actcopy == 2 ~ 1, 
                             actcopy == 3 ~ 1, 
                             actcopy == 4 ~ 1, 
                             actcopy == 5 ~ 1)) 


wave2 <-  wave2 %>% 
  mutate(actcopyb = case_when(actcopyb == 1 ~ 0, 
                              actcopyb == 2 ~ 1, 
                              actcopyb == 3 ~ 1, 
                              actcopyb == 4 ~ 1, 
                              actcopyb == 5 ~ 1)) 


wave3 <-  wave3 %>% 
  mutate(actcopyc = case_when(actcopyc == 1 ~ 0, 
                              actcopyc == 2 ~ 1, 
                              actcopyc == 3 ~ 1, 
                              actcopyc == 4 ~ 1, 
                              actcopyc == 5 ~ 1))


wave4 <-  wave4 %>% 
  mutate(actcopyd = case_when(actcopyd == 1 ~ 0, 
                              actcopyd == 2 ~ 1, 
                              actcopyd == 3 ~ 1, 
                              actcopyd == 4 ~ 1, 
                              actcopyd == 5 ~ 1)) 

# Now, I am going to calculate the scores for Wave 1 
# First get all schools in wave 1 
list_schools <- unique(wave1$schoolnr)
alc_scores_w1 <- map_df(list_schools, 
                        get_scores, 
                        df = wave1, 
                        dv = 'actcopy')

# Get list of schools for wave2 
list_schools_w2 <- unique(wave2$schoolnr)
# Annoyingly they are not the same
# And there is one value missing 
list_schools_w2 <- list_schools_w2[-c(1)]
alc_scores_w2 <- map_df(list_schools_w2, 
                        get_scores, 
                        df = wave2, 
                        dv = 'actcopyb')

# Now, let's do waves 3 and 4
list_schools_w3 <- unique(wave3$schoolnr)
list_schools_w4 <- unique(wave4$schoolnr)
alc_scores_w3 <- map_df(list_schools_w3, 
                        get_scores, 
                        df = wave3, 
                        dv = 'actcopyc')
alc_scores_w4 <- map_df(list_schools_w4, 
                        get_scores, 
                        df = wave4, 
                        dv = 'actcopyd')

# Join the newly created scores with the dataframes
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

# Now, I am going to start creating the wide dataframe
# Create unique IDs
df1 <- df1 %>% mutate(id = paste0(schoolnr,namenr))
df2 <- df2 %>% filter(schoolnr != " ") %>% 
  mutate(schoolnr = str_trim(tolower(schoolnr))) %>% 
  mutate(id =paste0("0",schoolnr, namenr))
df3 <- df3 %>% mutate(id = paste0(schoolnr, namenr))
df4 <- df4 %>% mutate(id = paste0(schoolnr, namenr))

# Now let's find the intersections 
ids_all_waves <- Reduce(intersect, 
                        list(df1$id, df2$id, df3$id, df4$id))

# Keep the participants in that show up in all waves
alc_w1 <- df1 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actcopy) %>% 
  rename(friends_w1 = friends_score, 
         popular_w1 = popular_score, 
         actcopy_w1 = actcopy)

alc_w2 <- df2 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actcopyb) %>% 
  rename(friends_w2 = friends_score, 
         popular_w2 = popular_score, 
         actcopy_w2 = actcopyb)

alc_w3 <- df3 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actcopyc) %>% 
  rename(friends_w3 = friends_score, 
         popular_w3 = popular_score, 
         actcopy_w3 = actcopyc)

alc_w4 <- df4 %>% 
  filter(id %in% ids_all_waves) %>% 
  select(id,friends_score, popular_score, actcopyd) %>% 
  rename(friends_w4 = friends_score, 
         popular_w4 = popular_score, 
         actcopy_w4 = actcopyd)

df_wide <- left_join(alc_w1, alc_w2, by = "id") %>% 
  left_join(., alc_w3, by = "id") %>% 
  left_join(., alc_w4, by = "id")

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
                      "actcopy"), 
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
write.csv(dfl_scaled,"Data/copy_std.csv")

b1 <- brm(actcopy ~ 1 + friends_z + wave + popular_z + (1 +  wave | id), 
          family = bernoulli, 
          data = dfl_scaled, 
          prior = c(prior(normal(0, 1.5), class = b),
                    prior(student_t(3, 0, 2.5), class = Intercept), 
                    prior(student_t(3, 0, 2.5), class = sd), 
                    prior(lkj(1), class = cor)), 
          cores = 6, 
          chains = 3, 
          iter = 8000, 
          warmup = 1000)
saveRDS(b1, 
        "Analyses/copy_model.rds")
