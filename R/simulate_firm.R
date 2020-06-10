simulate_firm <- function(sigma, S){
  # This function simulates firms latent earning surprise distribution
  # The distirubiton is assumed to be multinomial distribution with normal density probablitities.
  # sigma: the sd of normal dist
  
  df_firm <- data.frame(t(rmultinom(S, 1, dnorm(-20:20, 0, sigma))))   # multinomial dist
  colnames(df_firm) <- as.character(-20:20)
  e <- apply(df_firm, 1, function(x) colnames(df_firm)[which(x == 1)])
  df_firm <- tibble(df_firm) %>% 
    mutate(firm = 1:S, e = as.numeric(e)) %>% 
    select(firm, e)
  
  return(df_firm)
  
}