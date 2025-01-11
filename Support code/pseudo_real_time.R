pseudo_real_time <- function(x,xDate,mday){
  
  ################################## Description ###############################
  # Generate data frame of "pseudo real-time" high-frequency indicators
  #
  # INPUTS            x                        data.frame
  #                   xDate                    vector of high-frequency dates
  #                   mday                     data.frame of reference dates
  #
  # OUTPUTS           xfull                    data.frame (nrow(mday) x ncol(x)*ncol(mday))
  #
  ##############################################################################
  xnames  <- colnames(x)
  ndays   <- length(mday)
  mdayc   <- rep(c(1:ndays),ncol(x))
  xcol    <- ncol(x)*ncol(mday)
  mdayrep <- do.call(cbind, replicate(ncol(x), mday, simplify=FALSE))
  xfull   <- data.frame(matrix(nrow = nrow(mday), ncol = xcol))
  k <- 1
  for (j in 1:xcol){
    K <- ceiling(k/ndays)
    for (i in 1:nrow(mday)){
      xfull[i,j] <- x[which.max(xDate[xDate<=mdayrep[i,j]]),K]
      }
    colnames(xfull)[j] <- paste0(xnames[K],mdayc[j])
    k <- k+1
  }
  return(xfull)
}