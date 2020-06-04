compute_optimal_manipulation_for_e <- function(e, theta, S = 1000){
  # This function is used to simulate the optimal behavior for each bin over [-20, 20]
  # This function computes optimal behavior for given e.
  # Output: optimal amount of manipulation and epsilon. [m_{b,s=1:S}, eps_{b,s=1:S}]  
  
  # draw of beta
  df_firm <- tibble(
    firm = 1:S,
    e = e,
    beta = runif(S, 0, 2 * theta[1])
  )
  
  # the amount of manipulation ranging from 0 to 20 - e for each firm
  df_manipulation <- tibble(
    e = rep(-20:20, each = 41),
    m = rep(0:40, times = 41)
  ) %>% 
    filter(m + e <= 20) %>%      # the maximum amount of manipulation is 20 - b.
    select(e, m)
  
  # join firm-level data
  df_firm_utility <- left_join(df_firm, df_manipulation, by = 'e')
  
  # compute utility 
  utility <- df_firm_utility %>% 
    select(e, m, beta) %>% 
    mutate(utility = pmap_dbl(.l = ., .f = compute_utility, theta = theta)) %>% 
    pull(utility)
  
  # compute optimal behavior 
  df_firm_utility <- df_firm_utility %>% 
    mutate(utility = utility) %>%     # computed utility
    group_by(firm) %>% 
    mutate(max_utility = if_else(utility == max(utility), 1, 0)) %>%     # indicating max utility
    ungroup() %>% 
    mutate(m_opt = m * max_utility) %>%     # optimal amount of manipulation
    filter(max_utility == 1) %>% 
    mutate(eps_after = map_dbl(m_opt, generate_shock)) %>%     # uncertainty of manipulation is drawn accordiing to the amount of manipulation 
    mutate(R = e + m_opt + eps_after)      # reported surprise
      
  m_opt <- df_firm_utility %>% pull(m_opt)
  eps <- df_firm_utility %>% pull(eps_after)
  
  return(list(m_opt = m_opt, eps = eps))
}