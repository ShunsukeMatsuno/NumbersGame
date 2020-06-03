compute_optimal_manipulation_simulated <- function(df_firm, theta){
  # This function is used to compute the optimal behavior with the simulated data.
  # Since the input is simulated data, we know the latent earnings, e.
  
  
  # draw of beta
  df_firm <- df_firm %>% 
    mutate(beta = runif(N, 0, 2 * theta[1]))
  
  # Only firms with negative e considers manipulation
  df_firm_neg_e <- df_firm %>% 
    filter(e < 0)
  df_firm_pos_e <- df_firm %>% 
    filter(e >= 0)
  
  # for the positive firms, the optimal manipulation is zero
  df_firm_pos_e <- df_firm_pos_e %>% 
    mutate(m_opt = 0,
           R = e)
  
  # negative firms
  # the amout of manipulation ranging from 0 to 20 for each firm
  df_manipulation <- tibble(
    e = rep(-20:-1, each = 41),
    m = rep(0:40, times = 20)
  ) %>% 
    mutate(m_normalized = m - e) %>% 
    filter(m_normalized <= 20) %>%      # the maximum amount of manipulation is 20 - b.
    select(e, m)
  
  # join firm-level data
  df_firm_utility <- left_join(df_firm_neg_e, df_manipulation, by = 'e')
  
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
    mutate(m_opt = m * max_utility) %>%     # optimal amout of manipulation
    filter(max_utility == 1) %>% 
    mutate(eps_after = map_dbl(m_opt, generate_shock)) %>%    # uncertanty of manipulation is drawn accordiing to the amount of manipulation 
    mutate(R = e + m_opt + eps_after) %>%     # reporte surprise
    select(firm, e, beta, m_opt, eps_after, R)
  
  # merge the positive and negative firms
  df_firm_utility <- bind_rows(df_firm_utility, df_firm_pos_e) %>% 
    arrange(firm)
  return(df_firm_utility)
}
