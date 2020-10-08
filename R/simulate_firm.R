simulate_firm <- function(sigma, S){
  # This function simulates firms latent earning surprise distribution
  # The distirubiton is assumed to be multinomial distribution with normal density probablitities.
  # sigma: the sd of normal dist

  
  df_firm <- tibble(
    firm = 1:S,
    e = sample(x = -20:20,
               size = S,
               replace = TRUE,
               prob = dnorm(-20:20, 0, sigma)))
  
  return(df_firm)
  
}