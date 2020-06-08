compute_benefit <- function(R){
  # This function computes the capital market benefit of earnings surprise.
  # Since the estimates of coefficients on polynomial terms are not reported in the origina paper,
  #  I roughly approximate the functional form of Fig. 1. 
  # You may want to change the functional form of this function.
  
  # # return is in terms of real number (not percent)
  # benefit <- (R < 0) * (0.01 * R - 1/100) + (R >= 0 ) * (0.01 * R + 0.5/100) 
  
  # return is in terms of percent
  benefit <- (R < 0) * (0.01 * R - 1) + (R >= 0 ) * (0.01 * R + 0.5) 
  
  return(benefit)
  
}