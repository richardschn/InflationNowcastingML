exp2YoY <- function(x){
  
  ################################## Description ###############################
  # Transforms a single MoM IPCA expectation into an YoY rate (% change)
  #
  # INPUTS            x                        (12 x 1)
  #
  # OUTPUTS           YoY                      scalar
  #
  ##############################################################################
  if(length(x)!=12) stop('The number of elements must be 12 because exp2YoY can only compute one YoY rate at a time!')
  
  p = c()
  YoY = c()
  
  p[1] = 100
  for(i in 1:length(x)){
    p[i+1] = p[i]*(x[i]/100+1)
  }
  YoY = (p[13]/p[1]-1)*100
  return(YoY)
}