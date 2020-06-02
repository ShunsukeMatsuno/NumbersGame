generate_shock <- function(m_opt){
  # This function generates uncertanty according to the chosen amount of manipulation.
  if(m_opt == 0){
    eps = 0
  }else{
    eps = extraDistr::rdnorm(1, mean = 0, sd = sqrt((1 + theta[4]*(m_opt - 1)) * theta[3]))
  }
  
  return(eps)
}