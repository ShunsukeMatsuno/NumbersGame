CMD_obj <- function(theta, df_observed, df_simulated, S){
  
  ###### observed MLE ######
  df_reported <- count_all_bins(df_observed) %>% 
    mutate(p_obs_MLE = count / sum(count))
  p_obs_MLE <- df_reported %>% pull(p_obs_MLE)
  
  
  ###### simulated MLE ######
  # optimal behavior
  df_simulated_manipulation <- compute_optimal_manipulation_simulated(df_simulated, theta, S) %>%
    select(firm, R) %>% 
    count_all_bins() %>% 
    mutate(p_simulated_MLE = count / sum(count)) 
  p_simulated_MLE <- df_simulated_manipulation %>% pull(p_simulated_MLE)
  
  ##### objective function #####
  obj <- sum( (100*p_obs_MLE - 100*p_simulated_MLE)^2 )
  
  return(obj)
}

