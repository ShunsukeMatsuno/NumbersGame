compute_utility <- function(e, m, beta, benefit, theta){
  if(m == 0){
    utility = (e >= 0) * benefit + (e < 0) * 0
  }else{
    eps <- -20:20
    prob_eps <- extraDistr::ddnorm(eps, mean = 0, sd = sqrt( (1 + theta[4]*(m - 1)) * theta[3]))
    report <- e + m + eps
    E_benefit <- sum(prob_eps * ((report >= 0) * benefit + (report < 0) * 0))
    cost <- beta * m^theta[2]
    utility <- E_benefit - cost
  }
  return(utility)
}