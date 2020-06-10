criterion_normal <- function(theta, df_simulated, df_observed, weight = FALSE, S = 1000){
  # This function returns the GMM objective function ('criterion function') to optimize theta.
  
  
  ######## Inverted data #########
  # recover P
  P <- compute_P(theta, S)
  
  # observed distribution
  df_pi <- compute_pi(df_observed)
  pi <- df_pi %>% pull(count_R)
  
  # latent distribution
  df_x <- compute_x(P, df_pi) 
  x <- df_x %>% pull(count_e)
  
  
  ###### Simulated data   ######
  # observed bins
  df_e_observed <- df_simulated %>% 
    group_by(e) %>% 
    summarise(count = n())
  
  e_observed <- df_e_observed %>% pull(e) 
  
  # unobserved bins
  e_unobserved <- setdiff(-20:20, e_observed) 
  
  df_e_unobserved <- tibble(
    e = e_unobserved,
    count = 0
  )
  
  # merge them
  df_e = bind_rows(df_e_unobserved, df_e_observed) %>% 
    arrange(e) 
  e_normal <- df_e %>% pull(count)
  
  
  ###### squared difference ####
  obj_plain <- sum((x - e_normal)^2)
  
  
  ###### weighted errors
  # I put more weights around 0
  # To do so, I multiply (abs(bin))^-1.
  obj_weighted <- sum( (x-e_normal)^2 / c(20:1,1,1:20)^2 )
  
  if(weight == TRUE){
    return(obj_weighted)
  }else{
    return(obj_plain)
  }
  
  
}