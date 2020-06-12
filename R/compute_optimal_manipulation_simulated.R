compute_optimal_manipulation_simulated <- function(df_firm, theta, S){
  # This function is used to compute the optimal behavior with the simulated data.
  # Since the input is simulated data, we know the latent earnings, e.
  
  
  # draw of beta
  df_firm <- df_firm %>% 
    mutate(beta = runif(S, 0, 2 * theta[1]))
  
  
  # the amount of manipulation ranging from 0 to 40 - e for each firm
  df_manipulation <- tibble(
    e = rep(-20:20, each = 41),
    m = rep(0:40, times = 41)
  ) %>% 
    filter(m + e <= 20) %>%      # the maximum amount of manipulation is 20 - b.
    select(e, m)
  
  # join firm-level data and manipulation data
  df_firm_utility <- left_join(df_firm, df_manipulation, by = 'e')
  
  # compute utility 
  utility <- df_firm_utility %>% 
    select(e, m, beta) %>% 
    mutate(utility = pmap_dbl(.l = ., .f = compute_utility, theta = theta)) %>% 
    pull(utility)
  
  # compute optimal behavior for negative firms
  df_firm_utility <- df_firm_utility %>% 
    mutate(utility = utility) %>%     # computed utility
    group_by(firm) %>% 
    mutate(max_utility = if_else(utility == max(utility), 1, 0)) %>%     # indicating max utility
    ungroup() %>% 
    mutate(m_opt = m * max_utility) %>%     # optimal amount of manipulation
    filter(max_utility == 1) %>% 
    mutate(eps_after = map2_dbl(m_opt, e, generate_shock, theta = theta)) %>%    # uncertainty of manipulation is drawn according to the amount of manipulation 
    mutate(R = e + m_opt + eps_after) %>%     # report surprise
    select(firm, e, beta, m_opt, eps_after, R)
  
  return(df_firm_utility)
}
