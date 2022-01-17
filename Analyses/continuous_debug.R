# Packages 
library(tidyverse)
library(igraph)
library(haven)


# Softmax function 
softmax <- function (...) 
{
  X <- list(...)
  K <- length(X)
  if (K == 1) {
    X <- X[[1]]
    if (is.null(dim(X))) {
      f <- exp(X)
      denom <- sum(f)
      p <- as.numeric(f/denom)
      return(p)
    }
    else {
      N <- nrow(X)
      f <- lapply(1:N, function(i) exp(X[i, ]))
      denom <- sapply(1:N, function(i) sum(f[[i]]))
      p <- sapply(1:N, function(i) unlist(f[[i]])/denom[i])
      p <- t(as.matrix(p))
      return(p)
    }
  }
  else {
    X <- as.data.frame(X)
    N <- nrow(X)
    if (N == 1) {
      f <- exp(X[1, ])
      denom <- sum(f)
      p <- as.numeric(f/denom)
      return(p)
    }
    else {
      f <- lapply(1:N, function(i) exp(X[i, ]))
      denom <- sapply(1:N, function(i) sum(f[[i]]))
      p <- sapply(1:N, function(i) unlist(f[[i]])/denom[i])
      p <- t(as.matrix(p))
      colnames(p) <- NULL
      return(p)
    }
  }
}

set.seed(33)
# Produce initial network visualization
# Create small world network 
network <- sample_smallworld(dim = 1, size = 100, nei = 4, p = 0.1, 10, loops = F)


# Set up 
turns <- 100
init_prob <- 0.25
n_agents <- 100
socinf <- 0.5
g <- 0.5
rounds <- 1

# Extract network matrix
net_mat <- as.matrix(network[])
# Create an empty matrix to record the behaviors individuals adopt
# Find centrality scores 
cent_scores <- eigen_centrality(network)$vector
# Normalize centrality scores 
normalized_scores <- cent_scores/sum(cent_scores)

runs_list <- list()

for (r in 1:rounds) {
  beh_matrix <- matrix(0, n_agents, turns)
  print(paste0("socinf= ", as.character(socinf), "g= ", as.character(g)))
  # Initial values 
  beh_matrix[,1] <- rbinom(n = n_agents, size = 1, prob = init_prob)
  for (j in 1:(turns-1)) {
    for (i in 1:n_agents) { 
      ties <- which(net_mat[i,] == 1) 
      popular_score <- sum(beh_matrix[,j]*normalized_scores)
      friends_score <- mean(beh_matrix[ties, j])
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
