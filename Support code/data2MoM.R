data2MoM <- function(x, lag, transf){
  
  ################################## Description ###############################
  # Applies the following transformations:
  #     1 - perc2MoM:  first-differences (on a monthly basis)
  #     2 - price2MoM: log first-differences (on a monthly basis)
  #
  # INPUTS            x                        (nobs x 1)
  #                   lag                      (4 or 21) => weekly or daily
  #                   trasnf                   (1 or 2)
  #
  # OUTPUTS           MoM                      (nobs-lag x 1)
  #
  ##############################################################################
  
  MoM = c()
  
  if(transf == 1){
    MoM = diff(x, lag=lag)
  } else if(transf == 2){
    MoM = diff(log(x), lag=lag)*100
  } 
  return(MoM)
}