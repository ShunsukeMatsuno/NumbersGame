compute_benefit_nonpara <- function(theta_benefit, R){
  # theta_benefit is coefficient for each -20:20 bins
  (R < 0) * (theta_benefit[1] * R) + (R >= 0) * (theta_benefit[2] * R + theta_benefit[3])
}