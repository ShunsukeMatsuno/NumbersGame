compute_benefit_parametrized <- function(R, theta_benefit){
  # second order polynomial with jumps
  
  benefit <- (R < 0) *  (theta_benefit[1] + theta_benefit[2] * R + theta_benefit[3] * R^2) 
             (R >= 0 ) * (theta_benefit[4] + theta_benefit[5] * R + theta_benefit[6] * R^2) 
  
  return(benefit)
}