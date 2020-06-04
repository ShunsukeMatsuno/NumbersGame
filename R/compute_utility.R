compute_utility <- function(e, m, beta, theta){
  # This function computes the utility of manipulation for each state (e).
  if(m == 0){
    # If the firm decides not to engage in the manipulation, then the reported surprise is e.
    utility = compute_benefit(e)
  }else{
    eps <- -20:20
    prob_eps <- extraDistr::ddnorm(eps, mean = 0, sd = sqrt( (1 + theta[4]*(m - 1)) * theta[3]))
    report <- e + m + eps
    E_benefit <- sum(prob_eps * compute_benefit(report))
    cost <- beta * m^theta[2]
    utility <- E_benefit - cost
  }
  return(utility)
}