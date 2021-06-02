########################
###### Simple ABM ###### 
########################

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
    print(paste0("socinf= ", as.character(socinf), "g= ", as.character(g)))
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
  return(last_turn)
}

vector_si <- rep(seq(0, 1, by = 0.1), 11)
vector_g <- rep(seq(0, 1, by = 0.1), each = 11)
start <- Sys.time()
d2 <- map2(.x = vector_si, .y = vector_g, diff_inf_sim, turns = 100, init_prob = 0.25, 
          n_agents = 100, rounds = 25)
end <- Sys.time()
end - start

meds <- map_dbl(d2, median)
mens <- map_dbl(d2, mean)
sds <- map_dbl(d2, sd)
neither <- map_dbl(.x = c(1:121), ~n_distinct(which(d2[[.x]] != 0 & d2[[.x]] != 1)))

df <- tibble(soc_inf = vector_si, 
             imp_friends = vector_g, 
             med_last_run = meds, 
             mean_last_run = mens, 
             neither = neither/25)

(neither_plot <- df %>% 
  ggplot(aes(x = soc_inf, y = imp_friends)) + 
  scale_fill_distiller(palette = "YlGnBu", direction = 1) +
  geom_tile(aes(fill=neither)) + 
  theme_light() +
  labs(title = "Proportion of runs without total convergence", 
       subtitle = "Initial probability = 0.25", 
       fill = "", 
       x = "Weight of social cues", 
       y = "Weight of local influence") + 
  scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
  scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))) + 
  theme(axis.text.x = element_text(angle = 90))
  
(median_plot <- df %>% 
    ggplot(aes(x = soc_inf, y = imp_friends)) + 
    scale_fill_distiller(palette = "YlGnBu", direction = 1) +
    geom_tile(aes(fill=med_last_run)) + 
    theme_light() +
    labs(title = "Median proportion at the end of each run", 
         subtitle = "Initial probability = 0.25", 
         fill = "", 
         x = "Weight of social cues", 
         y = "Weight of local influence") + 
    scale_x_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) + 
    scale_y_continuous(breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1))) + 
  theme(axis.text.x = element_text(angle = 90))

ggsave(plot = median_plot, "prob0.25_median.png")
ggsave(plot = neither_plot, "prob0.35_neither.png")


