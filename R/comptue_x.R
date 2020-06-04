compute_x <- function(P, df_pi){
  # This function invert the observed distribution using P to get the latent distribution.
  
  df_x <- tibble(
    bin = -20:20,
    count_e = solve(P) %*% df_pi$count_R    # x is latent earnings
  )
  
  return(df_x)
  
  
}