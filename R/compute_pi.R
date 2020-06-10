compute_pi <- function(df_observed){
  # This function returns \pi, the empirical earnings surprise distrbution.
  
  # observed bins
  df_pi_observed <- df_observed %>% 
    group_by(R) %>% 
    summarise(count = n()) %>% 
    rename(bin = R)
  
  bins_observed <- df_pi_observed %>% pull(bin) 
  
  # unobserved bins
  bins_unobserved <- setdiff(-20:20, bins_observed) 
  
  df_pi_unobserved <- tibble(
    bin = bins_unobserved,
    count = 0
  )
  
  # merge them
  df_pi = bind_rows(df_pi_unobserved, df_pi_observed) %>% 
    arrange(bin) %>% 
    rename(count_R = count)    # pi is observed (reported) surprise
  
  return(df_pi)
}