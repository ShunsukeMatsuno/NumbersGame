compute_CMD_BLPish_nonpara <- function(theta_full, df_observed,  df_manipulation, S){
  
  gamma <- theta_full[1]
  sigma <- theta_full[2]
  theta_benefit <- theta_full[-(1:2)]
  
  
  
  # actual data
  obs_share <- count_all_bins(df_observed) %>% 
    mutate(share = count / sum(count)) %>% 
    pull(share)
  
  
  # simulated data
  df_simulated <- simulate_firm(sigma, S)
  
  
  df_firm_em <- left_join(df_simulated, df_manipulation, by = 'e') %>%
    mutate(R = m + e) %>% 
    mutate(benefit = compute_benefit_nonpara(theta_benefit, m + e),
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

