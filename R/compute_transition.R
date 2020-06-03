compute_transition <- function(e, m, eps, j){
  # This function returns \sum_{k=0}^{20-j} 1(m = j+k-b) * 1(m = -k)
  # first: 1(m = j+k-b) 
  # second: 1(m = -k)
  first <- ( m == j + 0:(20-j) - e )
  second <- ( eps == -(0:(20-j)) )
  result <- sum(first * second)
  return(result)
}