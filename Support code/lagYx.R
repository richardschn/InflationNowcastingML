lagYx <- function(x,lags){
  
  ################################## Description ###############################
  # Generate a lagged matrix of regressors
  #
  # INPUTS            x                        vector of data
  #                   lags                     number of lags 
  #
  # OUTPUTS           xlagged                  data.frame (nrow(x)-lags x lags)
  #
  ##############################################################################
  
  xlagged = data.frame(matrix(nrow = length(x)-lags, ncol = lags))
  for (p in 1:lags){
    xlagged[,p] <- x[(1+lags-p):(length(x)-p)]
    colnames(xlagged)[p] <- paste0("Y",p)
  }
  return(xlagged)
}

