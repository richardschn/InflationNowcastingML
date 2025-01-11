###############################################################################################################
# Rolling-window sensibility analysis: what's the optimal time span size?
###############################################################################################################

#|=== Redefine inital dates for a rolling-window sensibility analysis
if (MoMFlag == 1 & rollwdwFlag == 1){fixedDateBegin <- "2003-02-08"} if(MoMFlag != 1 & rollwdwFlag == 1){fixedDateBegin <- "2004-01-08"} # RW size of 119 for MoM / 108 for YoY
# {fixedDateBegin <- "2004-02-08"} / {fixedDateBegin <- "2005-01-08"} # RW size of 107 for MoM / 96 for YoY
# {fixedDateBegin <- "2005-02-08"} / {fixedDateBegin <- "2006-01-08"} # RW size of 95 for MoM / 84 for YoY

#|=== Redefine relevant variables
mday  <- mday[mday[,1]>=fixedDateBegin,]     # Redefine mday
N0    <- which.max(mday[mday[,1]<=T0,1]) + 1 # Counter for T0 in the monthly frequency
N1    <- which.max(mday[mday[,1]<=T1,1])     # Counter for T1 in the monthly frequency
Xx    <- tail(Xx,n=N1)                       # Extract corresponding observations of regressors
y     <- tail(y,n=N1)                        # Extract corresponding observations of the target
xexp  <- tail(xexp,n=N1)                     # Extract corresponding observations of Focus expectations
mDate <- tail(mDate,n=N1)                    # Extract corresponding observations of mDate