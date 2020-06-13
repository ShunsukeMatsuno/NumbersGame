compute_P <- function(theta, S = 1000){
  e_vec <- -20:20    # each state (bin)
  result <- array(dim = c(S, length(e_vec)))   # S x 41  array

  # sequential computation
  for(i in seq_along(e_vec)){
    result_temp <- compute_optimal_manipulation_for_e(e_vec[i], theta, S)
    result[,i] <- result_temp$R        
  }

  # compute P
  P <- array(dim = c(41, 41))
  

  for(b in 1:NROW(P)){
    for(j in 1:NCOL(P)){
      P[j, b] <- sum(result[,b] == j - 21) / S
    }
  }
  
  
  return(P)
  
}