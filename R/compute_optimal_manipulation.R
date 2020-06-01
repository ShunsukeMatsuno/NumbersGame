compute_optimal_manipulation <- function(df_firm, benefit, theta){
  # draw of beta
  df_firm <- df_firm %>% 
    mutate(beta = runif(N, 0, 2 * theta[1]))
  
  # the amout of manipulation ranging from 0 to 20 for each firm
  df_manipulation <- tibble(
    firm = rep(1:N, each = 21),
    m = rep(0:20, times = N)
  )
  
  # join firm-level data
  df_firm_utility <- left_join(df_firm, df_manipulation)
  
  # compute utility 
  utility <- df_firm_utility %>% 
    select(bin_e, m, beta) %>% 
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
    mutate(R = bin_e + m_opt + eps_after)    # reporte surprise
  
  return(df_firm_utility)
}