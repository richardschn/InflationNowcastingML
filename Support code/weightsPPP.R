weightsPPP <- function(X, threshold) {
  
  ################################## Description ###############################
  # Given a vector of lenght N and a point 1<=T<=N 
  # returns a set of N weights w_n computed as
  #  w_n = -(log(1-n/T))(T-1) if 1<= n <= T-1
  #         (log T)/(T-1)  if n=> T
  #
  # INPUTS            X             matrix of size N x p
  #                   threshold     integer
  #
  # OUTPUTS           weights       vector of size N
  #
  ##############################################################################
  
  if (threshold == 1){threshold <- threshold + 1}
  
  n       <- length(X[,1])
  weights <- as.vector(seq(1, n))
  
  for (i in 1:n){
    if (i <= threshold-1){
      weights[i] <- -(log(1-i/threshold))/(threshold-1)
    } else {
      weights[i] <- (log(threshold))/(threshold-1)
    }
  }
  
  # Note that we obtain one set of weights only
  weights <- weights/sum(weights)
  
  return(weights)
  
}