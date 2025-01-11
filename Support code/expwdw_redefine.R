###############################################################################################################
# Redefine key variables for the expanding-window scheme choice: 
# set initial date to "2004-07-08" for MoM or "2005-06-08" for YoY
###############################################################################################################

#|=== Redefine initial date for the expanding-window scheme depending on imputFlag and MoMFlag choices
if (imputFlag==1 & MoMFlag==1){fixedDateBegin <- "2003-02-08"} 
if (imputFlag==1 & MoMFlag!=1){fixedDateBegin <- "2004-01-08"} 
if (imputFlag!=1 & MoMFlag==1){fixedDateBegin <- "2004-07-08"} 
if (imputFlag!=1 & MoMFlag!=1){fixedDateBegin <- "2005-06-08"} 

#|=== Redefine relevant variables
mday  <- mday[mday[,1]>=fixedDateBegin,]     # Redefine mday
N0    <- which.max(mday[mday[,1]<=T0,1]) + 1 # Counter for T0 in the monthly frequency
N1    <- which.max(mday[mday[,1]<=T1,1])     # Counter for T1 in the monthly frequency
Xx    <- tail(Xx,n=N1)                       # Extract corresponding observations of regressors
y     <- tail(y,n=N1)                        # Extract corresponding observations of the target
xexp  <- tail(xexp,n=N1)                     # Extract corresponding observations of Focus expectations
mDate <- tail(mDate,n=N1)                    # Extract corresponding observations of mDate