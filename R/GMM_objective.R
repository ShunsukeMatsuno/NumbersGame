GMM_objective <- function(theta, df_observed, S = 1000){
  # This function returns the GMM objective function ('criterion function') to optimize theta.
  
  # recover P
  P <- compute_P(theta, S = 1000)
  
  # observed distribution
  df_pi <- compute_pi(df_observed)
  
  pi <- df_pi %>% pull(count_R)
  
  # latent distribution and adjacent bins
  df_x <- compute_x(P, df_pi) 
  
  x <- df_x %>% pull(count_e)
  
  smoothness <- df_x %>% 
    mutate(lag_e = lag(count_e)) %>% 
    mutate(smoothness = count_e - lag_e) %>% 
    drop_na() %>%      # for the first row the lagged variable (left bin) does not exist
    pull(smoothness)
  
  # vector
  obj <- c(pi - x, smoothness)
  
  # GMM objective 
  # For simplicity, I use the identity matrix as a weighting matrix.
  return(sum(obj^2))
}