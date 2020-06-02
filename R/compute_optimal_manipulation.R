compute_optimal_manipulation_for_e <- function(e, benefit, theta){
  # This function is used to compute the optimal bihavior with the 'observed' data
  # Since we don't know the state variable (latent earnigns) e, we make them.
  # 
  
  
  # draw of beta
  df_firm <- tibble(
    firm = 1:S,
    e = e
  )
  df_firm <- df_firm %>% 
    mutate(beta = runif(N, 0, 2 * theta[1]))
  
  # If `df_firm` does not include the state variable e (bin), make them.
  # In the process of simulating data, we know e. But, in estimation we don't know e, so that we have to make them.
  if(!('e' %in% colnames(df_firm))){
    df_state <- tibble(
      firm = rep(1:N, each = 41),
      e = rep(-20:20, times = N))
    
    df_firm <- left_join(df_firm, df_state, by = 'firm')
  }
  
  
  # the amout of manipulation ranging from 0 to 20 for each firm
  df_manipulation <- tibble(
    e = rep(-20:20, each = 41),
    m = rep(0:40, times = 41)
  ) %>% 
    mutate(m_normalized = m - e) %>% 
    filter(m_normalized <= 20) %>%      # the maximum amount of manipulation is 20 - b.
    select(e, m)
  
  # join firm-level data
  df_firm_utility <- left_join(df_firm, df_manipulation, by = 'e')
  
  # compute utility 
  utility <- df_firm_utility %>% 
    select(e, m, beta) %>% 
    mutate(utility = pmap_dbl(.l = ., .f = compute_utility, benefit = benefit, theta = theta)) %>% 
    pull(utility)
  
  # 
  df_firm_utility <- df_firm_utility %>% 
    mutate(utility = utility) %>%     # computed utility
    group_by(firm) %>% 
    mutate(max_utility = if_else(utility == max(utility), 1, 0)) %>%     # indicating max utility
    ungroup() %>% 
    mutate(m_opt = m * max_utility) %>%     # optimal amout of manipulation
    filter(max_utility == 1) %>% 
    mutate(eps_after = case_when(     # uncertanty of manipulation is drawn accordiing to the amount of manipulation
      m_opt == 0 ~ 0,
      m_opt > 0 ~ extraDistr::rdnorm(N, mean = 0, sd = sqrt((1 + theta[4]*(m_opt - 1)) * theta[3])))
    ) %>% 
    mutate(R = e + m_opt + eps_after)    # reporte surprise
  
  return(df_firm_utility)
}