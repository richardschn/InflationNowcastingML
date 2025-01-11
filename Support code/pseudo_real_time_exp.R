pseudo_real_time_exp <- function(x,xDate,mday,yPubl){
  
  ################################## Description ###############################
  # Generate data frame of "pseudo real-time" high-frequency inflation expectations
  #
  # INPUTS            x                        data.frame
  #                   xDate                    vector of high-frequency dates
  #                   mday                     data.frame of reference dates
  #                   yPubl                    vector of IPCA release dates
  #
  # OUTPUTS           xfull                    data.frame (nrow(mday) x ncol(x)*ncol(mday))
  #
  ##############################################################################
  
  # Align/balance time series
  t0exp <- which.max(xDate>=floor_date(mday[1,1],"month"))                                       # Set initial in-sample period for series of expectations
  t1exp <- which.max(xDate[xDate<=ceiling_date(tail(mday[,length(mday)],1),"month") - days(1)])  # Set final in-sample period for series of expectations
  x <- x[(t0exp:t1exp),]
  xDate <- xDate[(t0exp:t1exp)]
  yPubl <- yPubl[yPubl>=floor_date(mday[1,1],"month")]

  xnames  <- colnames(x)
  ndays   <- length(mday)
  mdayc   <- rep(c(1:ndays),ncol(x)-1)
  xcol    <- (ncol(x)-1)*ncol(mday)
  mdayrep <- do.call(cbind, replicate(ncol(x), mday, simplify=FALSE))
  xfull   <- data.frame(matrix(nrow = nrow(mday), ncol = xcol))
  k <- 1
  for (j in 1:xcol){
    K <- ceiling(k/ndays)
    for (i in 1:nrow(mday)){
      if (yPubl[i]>mdayrep[i,j]){                                     # This if-else structure recovers the correct expectations for the month of reference in case of "early month" nowcasts...
        xfull[i,j] <- x[which.max(xDate[xDate<=mdayrep[i,j]]),K+1]
        } else {
          xfull[i,j] <- x[which.max(xDate[xDate<=mdayrep[i,j]]),K]
        }
    }
    colnames(xfull)[j] <- paste0(xnames[K],mdayc[j])
    k <- k+1
  }
  return(xfull)
}