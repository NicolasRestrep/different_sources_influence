########################
###### Simple ABM ###### 
########################

# Packages 
library(tidyverse)
library(igraph)
library(rethinking)
library(haven)

set.seed(33)
# Produce initial network visualization
# Create small world network 
network <- sample_smallworld(dim = 1, size = 100, nei = 4, p = 0.1, 10, loops = F)


diff_inf_sim <- function(turns, init_prob, n_agents, socinf, g, rounds) {
# Extract network matrix
net_mat <- as.matrix(network[])
# Create an empty matrix to record the behaviors individuals adopt
# Find centrality scores 
cent_scores <- eigen_centrality(network)$vector
# Find popular nodes 
popular <- which(cent_scores > quantile(cent_scores, 0.85))
runs_list <- list()
for (r in 1:rounds) {
beh_matrix <- matrix(0, n_agents, turns)
# Initial values 
beh_matrix[,1] <- rbinom(n = n_agents, size = 1, prob = init_prob)
for (j in 1:(turns-1)) {
for (i in 1:n_agents) { 
  ties <- which(net_mat[i,] == 1) 
  popular_score <- mean(beh_matrix[popular, j])
  friends_score <- mean(beh_matrix[ties, j])
  copy <- runif(n = 1)
  ind_times <- sum(beh_matrix[i,])
  ind_weight <- softmax(ind_times, j-ind_times)
  prSmoke <- (1 - socinf)*ind_weight[1] + socinf*((1-g)*popular_score + g*friends_score)
  flip <- rbinom(1, 1, prob = prSmoke)
  if(flip ==1 ) {
    beh_matrix[i,j+1] <- 1
  } else {
    beh_matrix[i,j+1] <- 0
  }
}
}
results_round <- apply(beh_matrix,2,mean)
runs_list[[r]] <- results_round
}
last_turn <- map_dbl(c(1:rounds), ~ runs_list[[.x]][turns])
return(list(last_turn, runs_list))
}

d <- diff_inf_sim(turns = 100, 
             init_prob = 0.25, 
             n_agents = 100, 
             socinf = 0.6, 
             g = 0.7, 
             rounds = 5)


