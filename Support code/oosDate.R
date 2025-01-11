oosDate <- function(N0, N1, N, ndays, woosDate, mday, xw, xdt, xd, xfocus, focusFlag, sdfocusFlag, h, moosDate, poosDate){

  ################################## Description ###############################
  # Combine out-of-sample release dates for regressors X
  #
  # INPUTS            
  #
  # OUTPUTS           
  #                                   
  ##############################################################################
  
    # Recover the proper out-of-sample release dates of energy prices
    wmday <- mday[N0:N1,]                                                  # Extract out-of-sample period in mday
    for (i in 1:N){                                                        # Loop over out-of-sample periods                      
      for (j in 1:ndays){                                                  # Loop over nowcasting days
        wind <- which.max(woosDate>mday[N0+i-1,j])                         # Find corresponding release date index
        tw <- woosDate[wind]                                               # Find corresponding release date
        wdiff <- as.numeric(difftime(tw,mday[N0+i-1,j], units = "days"))   # Compute interval between nowcasting day and release date
        if (wdiff>1){                                                      # If interval equals 1, then nowcasting day is on a Sunday
          wmday[i,j] <- tw                                                 # Corresponding release date in case nowcasting day does not fall in a Sunday
        } else {
          wmday[i,j] <- woosDate[wind+1]                                   # Corresponding release date in case nowcasting day falls in a Sunday
        }
      }
    }
    
    # Replicate release dates according to the number of high-frequency energy price indicators
    woosDate <- do.call(cbind, replicate(ndays, wmday, simplify=FALSE))    
    colnames(woosDate) <- names(xw)
    
    # Replicate release dates according to the number of high-frequency financial indicators + IPCA expectations
    if (focusFlag == 1 & sdfocusFlag == 1){
      doosDate <- do.call(cbind, replicate(length(xdt)+2, mday[N0:N1,], simplify=FALSE))
      colnames(doosDate) <- names(cbind(xd,xfocus))
    } else if(focusFlag == 1 & sdfocusFlag == 0){
      doosDate <- do.call(cbind, replicate(length(xdt)+1, mday[N0:N1,], simplify=FALSE))
      colnames(doosDate) <- names(cbind(xd,xfocus))
    } else{
      doosDate <- do.call(cbind, replicate(length(xdt), mday[N0:N1,], simplify=FALSE))
      colnames(doosDate) <- names(xd)
    }
    
    # Combine out-of-sample release dates for all regressors
    XoosDate <- data.frame(moosDate,poosDate,woosDate,doosDate)    
    return(XoosDate)
}