compute_benefit <- function(R){
  # This function computes the capital market benefit of earnings surprise.
  # Since the estimates of coefficients on polynomial terms are not reported in the origina paper,
  #  I roughly approximate the functional form of Fig. 1. 
  # You may want to change the funtional form of this function.
  
  benefit <- (R < 0) * (0.1 * R -0.5) + (R >= 0 & R < 10) * (0.15 * R + 0.5) # + (R >= 10) * (0.2 * R + 3)
  
  return(benefit)
  
}