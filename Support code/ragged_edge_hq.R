ragged_edge_hq <- function(Xx,x,ndays,XoosDate,N){

################################## Description ###############################
# Construct high-frequency vectors of data and release dates
#
# INPUTS            Xx                      complete data frame of regressors
#                   x                       data frame of low-frequency regressors
#                   ndays                   number of nowcasting days
#                   XoosDate                out-of-sample release dates of regressors
#                   N                       number of monthly forecasting windows
#
# OUTPUTS           hqout                   a list of multiple objects
#                                             - Xhq: high-frequency matrix of data
#                                             - XhqoosDate: high-frequency matrix of release dates
#                                             - XxRegr: vector of names (without high-frequency complement)
#                                             - XhqRegr: vector of unique names of high-frequency regressors
#                                   
##############################################################################

xRegr   <- colnames(Xx)                                          # Names of regressors
nx      <- ncol(x)                                               # Number of low-frequency regressors
hqnames <- substr(xRegr[-(1:nx)],1,nchar(xRegr[-(1:nx)])-1)      # Recover "root names" of HF regressors
XhqRegr <- unique(hqnames)                                       # Recover unique names of HF regressors
nhq     <- length(XhqRegr)                                       # Number of HF regressors
XxRegr  <- c(colnames(x),hqnames)                                # Vector of names without complement
colnames(Xx)  <- XxRegr                                          # Re-define colnames with unique names
Xhq     <- data.frame(matrix(NA, nrow=ndays*nrow(Xx), ncol=nhq)) # Pre-allocate HF matrix
XhqoosDate <- data.frame(matrix(NA, nrow=ndays*nrow(Xx), ncol=nhq)) # Pre-allocate HF release dates
colnames(Xhq) <- XhqRegr                                         # Set colnames for HF matrix
colnames(XhqoosDate) <- XhqRegr                                  # Set colnames for HF release dates
for (j in 1:nhq){                                                # Loop over HF regressors
  XjRegr  <- XhqRegr[j] == colnames(Xx)                          # Get columnns associated with the HF regressor j
  Xhq[,j] <- c(t(Xx[,XjRegr]))                                   # Construct HF vector j and save into Xhq
  Ndiff   <- N0-1-(N1-N0+1)                                      # Difference in time dimension between in-sample and out-of-sample periods
  # From now on, construction of HF vectors depend on the relative size between in- and out-of-sample periods:
  if (Ndiff > 0){
    Xoosmatch <- matrix(NA,nrow=N0-1-(N1-N0+1),ncol=ndays)       # Create matrix of NAs to combine different dimensions
    colnames(Xoosmatch) <- colnames(XoosDate[,XjRegr])           # Match colnames
    XhqoosDatej <- c(cbind(c(t(mday[(1:N0-1),])),c(t(rbind(XoosDate[,XjRegr],Xoosmatch))))) # Construct HF vector of dates and save into XhqoosDate
    XhqoosDate[,j] <- XhqoosDatej[1:(ndays*(2*(N0-1)-Ndiff))]
  } else if (Ndiff < 0){
    Xoosmatch <- matrix(NA,nrow=-(N0-1-(N1-N0+1)),ncol=ndays)    # Create matrix of NAs to combine different dimensions
    colnames(Xoosmatch) <- colnames(XoosDate[,XjRegr])           # Match colnames
    mday2 <- mday
    colnames(mday2) <- colnames(XoosDate[,XjRegr])
    XhqoosDatej <- c(cbind(c(t(rbind(mday2[(1:N0-1),],Xoosmatch))),c(t(rbind(XoosDate[,XjRegr]))))) # Construct HF vector of dates and save into XhqoosDate
    XhqoosDate[,j] <- XhqoosDatej[!is.na(XhqoosDatej)]
  } else{
    XhqoosDate[,j] <- c(cbind(c(t(mday[(1:N0-1),])),c(t(XoosDate[,XjRegr])))) # Construct HF vector of dates and save into XhqoosDate
  }
}
hqout <- list("Xhq"=Xhq,"XhqoosDate"=XhqoosDate,"XxRegr"=XxRegr,"XhqRegr"=XhqRegr)
return(hqout)
}