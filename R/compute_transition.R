compute_transition <- function(e, m, eps, j){
  # This function returns \sum_{k=0}^{20-j} 1(m = j+k-b) * 1(m = -k), i.e.,
  #  the probability of moving from e to j given m and eps.
  # first: 1(m = j+k-b) 
  # second: 1(m = -k)
  
  j_normalized <- j - 21    # j \in {1,...,41} but m, eps \in {-20,...,20}
  first <- ( m + e == j_normalized + 0:(41-j_normalized) )
  second <- ( eps == -(0:(41-j_normalized)) )
  result <- sum(first * second)
  return(result)
  
}