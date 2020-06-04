compute_P <- function(theta, S = 1000){
  e_vec <- -20:20    # each state (bin)
  result <- array(dim = c(S, length(e_vec), 2))   # S x 20 x 2 array
  
  # compute m_opt for each e (b)
  for(i in seq_along(e_vec)){
    result_temp <- compute_optimal_manipulation_for_e(e_vec[i], theta, S)
    result[,i,1] <- result_temp$m_opt    # the 1st elt of 3rd dim is m_opt
    result[,i,2] <- result_temp$eps      # the 2nd elt of 3rd dim is epsilon
  }
  
  # transform the result matrix to data frame.
  colnames_e <- as.character(-20:20)    # column names are states (bins)
  df_result_m <- data.frame(result[,,1])
  df_result_eps <- data.frame(result[,,2])
  colnames(df_result_m) <- colnames_e
  colnames(df_result_eps) <- colnames_e
  
  # transform to long data
  df_result_m_long <- df_result_m %>% 
    pivot_longer(cols = everything(), names_to = 'e', values_to = 'm') %>% 
    mutate(e = as.integer(e))
  df_result_eps_long <- df_result_eps %>% 
    pivot_longer(cols = everything(), names_to = 'e', values_to = 'eps') %>% 
    mutate(e = as.integer(e))
  
  # bind both data for m and eps
  # Then, we get the optimal value of manipulation for each e, where there exists S firms in each bin
  df_result_long <- bind_cols(df_result_m_long, df_result_eps_long) %>% 
    select(e, m, eps)
  
  # compute P
  P <- array(dim = c(41, 41))
  
  for(jj in 1:NROW(P)){
    # computes each row of P
    p_b_j <- df_result_long %>% 
      mutate(flag_j = pmap_int(.l = ., .f = compute_transition, j = jj)) %>% 
      group_by(e) %>% 
      summarise(p_b_j = sum(flag_j) / S) %>% 
      pull(p_b_j)  
    P[jj,] <- p_b_j
  }
  
  return(P)
  
}