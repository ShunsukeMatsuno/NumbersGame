compute_CMD_BLPish <- function(theta, df_observed,  df_manipulation, S){
  
  gamma <- theta[1]
  
  
  
  # actual data
  obs_share <- count_all_bins(df_observed) %>% 
    mutate(share = count / sum(count)) %>% 
    pull(share)
  
  
  # simulated data
  df_simulated <- simulate_firm(theta[2], S)

  
  df_firm_em <- left_join(df_simulated, df_manipulation, by = 'e') %>%
    mutate(R = m + e) %>% 
    mutate(benefit = compute_benefit(m + e),
           cost = gamma * m^2) %>% 
    group_by(firm) %>% 
    mutate(exp_util = exp(benefit - cost)) %>% 
    mutate(choice_prob = exp_util / sum(exp_util)) %>% 
    mutate(m_opt = sample(0:max(m), size = 1, prob = choice_prob)) %>% 
    ungroup() %>% 
    filter(m_opt == m) %>% 
    select(-m_opt)
  
  sim_share <- df_firm_em %>% 
    select(firm, R) %>% 
    count_all_bins() %>% 
    mutate(share = count / sum(count)) %>% 
    pull(share)
  
  return( sum((obs_share - sim_share)^2) )
}

