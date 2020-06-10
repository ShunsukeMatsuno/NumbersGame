generate_shock <- function(m_opt, e){
  # This function generates uncertainty according to the chosen amount of manipulation.
  if(m_opt == 0){
    # If the firm decides no to manipulate, there is no uncertainty 
    eps = 0
  }else{
    # Since e + m + eps should be in [-20, 20], we use (discretized) truncated normal distribution
    eps = floor(truncnorm::rtruncnorm(n = 1,
                                      a = - 20 - (m_opt + e), 
                                      b = 21 - (m_opt + e),   # since we are taking floor, the upper bd of continuous value is 20.999
                                      mean = 0,
                                      sd = sqrt( (1 + theta[4]*(m_opt - 1)) * theta[3] ) 
                                      )
                )
  }
  return(eps)
}