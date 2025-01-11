MoMexp2YoY <- function(xexpt,yt,ndays){
  
  ################################## Description ###############################
  # Transforms a series of IPCA expectations (given in MoM rates) into YoY rates (% change)
  #
  # INPUTS            xexpt                    T x (ndays*nhorizons)
  # IPCA MoM rates:   yt                       (T+11) x 1
  #                   ndays                    number of sub-monthly reference dates
  #
  # OUTPUTS           xexp                     T x (ndays*nhorizons) 
  #
  ##############################################################################
  # Note: in case nhorizons>3 (e.g., t+3, t+4, etc.), then this code must be expanded...
  #source('./Support code/exp2YoY.r') # Load function that transforms a single MoM IPCA expectation into an YoY rate
  
xexp  <- xexpt
k     <- 1
for (j in 1:ncol(xexpt)){
  K <- ceiling(k/ndays)
  for (i in 1:nrow(xexpt)){
    if (K == 1){
      xexp[i,j] <- exp2YoY(c(yt[(i):(i+10)],xexpt[i,j]))
    } else if (K == 2){
      xexp[i,j] <- exp2YoY(c(yt[(i):(i+9)],xexpt[i,j-ndays],xexpt[i,j]))
    } else if (K == 3){
      xexp[i,j] <- exp2YoY(c(yt[(i):(i+8)],xexpt[i,j-2*ndays],xexpt[i,j-ndays],xexpt[i,j]))
    }
  }
  k <- k+1
}

return(xexp)

}
