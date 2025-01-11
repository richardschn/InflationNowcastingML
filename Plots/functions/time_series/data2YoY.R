data2YoY <- function(x, lag, transf){
  
  ################################## Description ###############################
  # Applies the following transformations:
  #     1 - MoM2YoY:   MoM rates into YoY rates (% changes) 
  #     2 - perc2YoY:  first-differences (on a yearly basis)
  #     3 - price2YoY: log first-differences (on a yearly basis)
  #
  # INPUTS            x                        (nobs x 1)
  #                   lag                      (12, 52 or 252)
  #                   trasnf                   (1, 2 or 3)
  #
  # OUTPUTS           YoY                      (nobs-lag x 1)
  #
  ##############################################################################
  if(transf==1 && lag!=12) stop('MoM2YoY transformation can only be applied to monthly data')
  
  p = c()
  YoY = c()
  
  if(transf == 1){
  p[1] = 100
  for(i in 2:length(x)){
    p[i] = p[i-1]*(x[i]/100+1)
  }
  for(i in 1:length(x)-lag){
    YoY[i] = (p[i+lag]/p[i]-1)*100
  }
  } else if(transf == 2){
    YoY = diff(x, lag=lag)
  } else if(transf == 3){
    YoY = diff(log(x), lag=lag)*100
  }
  return(YoY)
}