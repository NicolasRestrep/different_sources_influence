############################################
###### Max-mean Clustering  ###############
############################################
# Packages
library(tidyverse)
library(igraph)

#### Functions ####
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
diff_inf_sim <- function(turns, init_prob, n_agents, socinf, g, rounds, plot_net = F) {
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
    for (j in 2:turns) {
      for (i in 1:n_agents) {
        ties <- which(net_mat[i,] == 1)
        popular_score <- sum(beh_matrix[,j-1]*normalized_scores)
        friends_score <- mean(beh_matrix[ties, j-1])
        ind_times <- sum(beh_matrix[i,])
        ind_weight <- softmax(ind_times, (j-1)-ind_times)
        if (is.nan(ind_weight[1])) {
          ind_weight[1] <- 1
        }
        prSmoke <- (1 - socinf)*ind_weight[1] + socinf*((1-g)*popular_score + g*friends_score)
        beh_matrix[i,j] <- ifelse(rbernoulli(n = 1, p = prSmoke)==TRUE, 1, 0)
      }
    }
    runs_list[[r]] <- beh_matrix
  }
  return(runs_list)
}


#### Max-max Clustering ####
load("max-mean-clustering_100_10.Rdata")
el <- data.frame(
  from = network[,1],
  to = network[,2]
)
network <- graph_from_data_frame(el)
# Run for 0.5
vector_si <- rep(seq(0, 1, by = 0.1), 11)
vector_g <- rep(seq(0, 1, by = 0.1), each = 11)
start <- Sys.time()
d5 <- map2(.x = vector_si, .y = vector_g, diff_inf_sim, turns = 250, init_prob = 0.5,
           n_agents = 100, rounds = 25)
end <- Sys.time()
end - start
saveRDS(d5, "max_mean_clust_prob05.rds")