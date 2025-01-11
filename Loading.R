########################################### Mixed-frequency setup #############################################
# Load data and organize the mixed-frequency settings for the nowcasting exercise
###############################################################################################################

###################################### Sourcing of auxiliary functions ########################################

setwd(paste0(path, "/Support code/"))    # Working directory for the auxiliary functions
source('data2MoM.r')                     # Load function that applies the necessary MoM transformations
source('data2YoY.r')                     # Load function that applies the necessary YoY transformations
source('pseudo_real_time.r')             # Load function that generates "pseudo real-time" nowcasting matrices
source('pseudo_real_time_exp.r')         # Load function that generates "pseudo real-time" nowcasting matrices for inflation expectations
source('MoMexp2YoY.r')                   # Load function that transforms a series of MoM IPCA survey-based expectations into YoY rates
source('exp2YoY.r')                      # Load function that transforms a single MoM IPCA survey-based expectation into YoY rate
source('oosDate.r')                      # Load script that combines out-of-sample release dates for regressors X
source('ragged_edge_hq.r')               # Load function that constructs high-frequency vectors of data and release dates for ragged-edge with RW
source('lagYx.r')                        # Load function that produces a lagged matrix of regressors
source('weightsPPP.r')                   # Weight the sample for smoothing out the past
setwd(path)                              # Set main working directory

############################################ Custom-build choices #############################################

#|=== Set in-sample and out-of-sample periods
if(MoMFlag == 1){fixedDateBegin <- "2003-02-08"} else{fixedDateBegin <- "2004-01-08"}  # Define initial dates based on data source availability and MoMFlag choice (Min Date: "2003-02-08" for MoM / "2004-01-08" for YoY) 
oosMinStartPoint <- "2012-12-31"                                                       # Set lower bound for starting the out-of-sample period
fixedDateEnd     <- "2024-10-08"                                                       # Define last period of the nowcast (fix at the 1st nowcasting day of the chosen month)                                     
T0               <- as.Date("2013-01-01")                                              # First month to be nowcasted
T1               <- ceiling_date(as.Date(fixedDateEnd),"month") - days(1)              # Last month to be nowcasted

#|=== Warning messages
if(oosMinStartPoint != "2012-12-31") stop('The out-of-sample period cannot start before Jan 2013 due to non-availability of real-time publishing dates.') 
if(as.integer(format(as.Date(fixedDateEnd),"%d")) != mdays[1]) stop('The day in fixedDateEnd must match the 1st relevant day of the nowcast in mdays.') 
if(p<1) stop('The chosen autoregressive lag must be at least equal to one.')

#|=== Choose low- (LF) and high-frequency (HF) regressors based on ordering of variables in "Meta-sheet" of the data file
posLowFreq  <- 3  # Position of the first LF price indicator
nLowFreq    <- 8  # Number of LF price indicators
posHFPrices <- 11 # Position of the first HF price indicator 
nHFPrices   <- 8  # Number of HF price indicators x frequency ratio m (length of mdays)
posHFEnergy <- 31 # Position of the first HF energy price indicator
nHFEnergy   <- 4  # Number of HF energy price indicators
posHFFinanc <- 35 # Position of the first HF financial indicator
nHFFinanc   <- 8  # Number of HF financial indicators
posHFExpec  <- 43 # Position of the first HF market expectations indicator (Focus-Market Readout)
nHFExpec    <- 4  # Number of HF market expectations indicators (number of relevant horizons)

################################################## Load data ##################################################

#|=== Loading data
setwd(paste0(path, "/Data/"))             # Working directory for the auxiliary functions
myFile <- "Brazil_nowcasting_data.xlsx"   # Data file

#|=== Variable names / mnemonics
mnemo <- openxlsx::read.xlsx(myFile, sheet = "Meta", cols = 2, skipEmptyRows = TRUE)

#|=== Low-frequency data
mData <- openxlsx::read.xlsx(myFile, sheet = "Monthly", na.strings = "NaN", detectDates = TRUE)        # Import monthly data
mDate <- mData$Reference                                                                               # Set LF reference time periods
mPubl <- openxlsx::read.xlsx(myFile, sheet = "M-Releases", na.strings = "NaN", detectDates = TRUE)     # Import monthly publishing/release dates
yPubl <- openxlsx::read.xlsx(myFile, sheet = "IPCA-Releases", na.strings = "NaN", detectDates = TRUE)  # Import release dates of target variable
mRegr <- as.character(mnemo[(posLowFreq:(posLowFreq+nLowFreq-1)),])                       # Select LF regressors
yt    <- mData$IPCA                                                                       # Set LF target variable
xt    <- mData[,mRegr]                                                                    # Set LF regressors
moosDate <- mPubl[,mRegr]                                                                 # Out-of-sample release dates

#|=== High-frequency data
# Price indicators
pRegr <- as.character(mnemo[(posHFPrices:(posHFPrices+nHFPrices-1)),])                    # Select HF price indicators
xmt   <- mData[,pRegr]                                                                    # Set HF price indicators
poosDate <- mPubl[,pRegr]                                                                 # Out-of-sample release dates
# Energy prices
wData <- openxlsx::read.xlsx(myFile, sheet = "Weekly", na.strings = "NaN", detectDates = TRUE) # Import weekly data on energy prices
wRegr <- as.character(mnemo[(posHFEnergy:(posHFEnergy+nHFEnergy-1)),])                    # Select HF energy prices
wDate <- wData$Reference                                                                  # Set weekly reference time periods
xwt   <- wData[,-1]                                                                       # Set HF energy prices
woosDate <- wDate[wDate > oosMinStartPoint] + days(8)                                     # Out-of-sample release dates - Note: average delay of two days after closing of a given week on Saturdays
# Financial markets
dData <- openxlsx::read.xlsx(myFile, sheet = "Daily", na.strings = "NaN", detectDates = TRUE)  # Import daily data on financial variables
dRegr <- as.character(mnemo[(posHFFinanc:(posHFFinanc+nHFFinanc-1)),])                    # Select HF financial indicators
if (rollwdwFlag != 1){dRegr <- setdiff(dRegr,"CDS")}                                      # Exclude "CDS" as regressor for a balanced panel when choosing the expanding-window scheme
xdt   <- dData[,dRegr]                                                                    # Set HF financial indicators
dDate <- dData$Reference                                                                  # Set daily reference time periods
# IPCA survey-based expectations
Top5Flag <- 0                                                                             # Top5 expectations == 1, Median expectations ==0
if (Top5Flag == 1){expData <- openxlsx::read.xlsx(myFile, sheet = "Expectations_Top5", na.strings = "NaN", detectDates = TRUE) # Import daily data on Top5 inflation expectations
} else {expData <- openxlsx::read.xlsx(myFile, sheet = "Expectations_Median", na.strings = "NaN", detectDates = TRUE)}         # Import daily data on Median inflation expectations
expstdData <- openxlsx::read.xlsx(myFile, sheet = "Expectations_StdDev", na.strings = "NaN", detectDates = TRUE)               # Import daily data on Std Dev inflation expectations
expRegr    <- as.character(mnemo[(posHFExpec:(posHFExpec+nHFExpec-1)),])                  # Select HF expectations
xexpt      <- expData[,expRegr]                                                           # Set HF expectations
xexpstdt   <- expstdData[,expRegr]                                                        # Set HF expectations (Std Dev)
expDate    <- expData$Reference                                                           # Set daily reference time periods

################################################ Transform data ###############################################

if (MoMFlag == 1){ 
  #|=== Transform data into month-on-month (MoM) rates (% changes)
  y  <- yt[-1]    # Note that IPCA data is already given in MoM format
  x  <- xt[-1,]   # Note that LF price indicators are already given in MoM format
  xm <- xmt[-1,]  # Note that HF price indicators are already given in MoM format
  xw <- sapply(xwt, data2MoM, lag = 4, transf = 2)                        # Apply price2MoM transformation to HF energy prices (weekly freq)
  #|===  Select transformations to be applied to HF financial indicators (daily freq)
  xdt.transf <- c(1,2,2,2,1,1,2,2)                                        # ==1 (first-difference); ==2 (log first-difference)
  xd <- data.frame(matrix(nrow = nrow(xdt)-21, ncol = ncol(xdt)))         # Pre-allocate dataframe of transformed HF financial indicators
  colnames(xd) <- dRegr                                                   # Set column names to xd
  for (i in 1:ncol(xdt)){                                                 # Loop over HF financial indicators
    xd[,i] <- data2MoM(xdt[,dRegr[i]], lag = 21, transf = xdt.transf[i])  # Apply transformations to each daily HF financial indicator
  }
  #|=== Adjust corresponding reference dates to match MoM transformations
  mDate  <- mDate[-1]             # Drop initial month of monthly dates  
  wDate  <- wDate[-(1:4)]         # Drop initial 4 weeks of weekly dates
  dDate  <- dDate[-(1:21)]        # Drop initial 21 days of daily dates
} else {
  #|=== Transform data into year-on-year (YoY) rates (% changes)
  y            <- data2YoY(yt, lag = 12, transf = 1)                          # Apply MoM2YoY transformation to target variable (monthly freq)
  x            <- sapply(xt, data2YoY, lag = 12, transf = 1)                  # Apply MoM2YoY transformation to LF regressors (monthly freq)
  xm           <- sapply(xmt, data2YoY, lag = 12, transf = 1)                 # Apply MoM2YoY transformation to HF price indicators (monthly freq behavior)
  xw           <- sapply(xwt, data2YoY, lag = 52, transf = 3)                 # Apply price2YoY transformation to HF energy prices (weekly freq)
  #|=== Select transformations to be applied to HF financial indicators (daily freq)
  xdt.transf   <- c(2,3,3,3,2,2,3,3)                                          # ==1 (% changes); ==2 (first-difference); ==3 (log first-difference)
  xd           <- data.frame(matrix(nrow = nrow(xdt)-252, ncol = ncol(xdt)))  # Pre-allocate dataframe of transformed HF financial indicators
  colnames(xd) <- dRegr                                                       # Set column names to xd
  for (i in 1:ncol(xdt)){                                                     # Loop over HF financial indicators
    xd[,i] <- data2YoY(xdt[,dRegr[i]], lag = 252, transf = xdt.transf[i])     # Apply transformations to each HF financial indicator
  }
  #|=== Adjust corresponding reference dates to match YoY transformations
  mDate  <- mDate[-(1:12)]        # Drop initial 12 months of monthly dates         
  wDate  <- wDate[-(1:52)]        # Drop initial 52 weeks of weekly dates
  dDate  <- dDate[-(1:252)]       # Drop initial 252 days of daily dates
}

########################################### Solve frequency mismatch ##########################################

#|=== Fix four sub-monthly dates to solve the mismatch between monthly and weekly frequencies
ndays <- length(mdays)                                                     # Number of nowcasting days
mday1 <- seq(as.Date(fixedDateBegin), as.Date(fixedDateEnd), by = "month") # 1st sequence of fixed days
mday2 <- mday1 + days(mdays[2]-mdays[1])                                   # 2nd sequence of fixed days
mday3 <- mday1 + days(mdays[3]-mdays[1])                                   # 3rd sequence of fixed days
mday4 <- ceiling_date(mday1,"month") - days(1)                             # End of month dates
mday  <- data.frame(mday1,mday2,mday3,mday4)                               # Full dates at which nowcasts/forecasts are made

#|=== Generate "pseudo real-time" indicators
xw      <- pseudo_real_time(xw, wDate, mday)                               # In-sample nowcasting matrix of energy prices
xd      <- pseudo_real_time(xd, dDate, mday)                               # In-sample nowcasting matrix of financial indicators
xexpt   <- pseudo_real_time_exp(xexpt, expDate, mday, yPubl$IPCA)          # In-sample nowcasting matrix of inflation expectations - Note: "survey-based nowcasts" up to IPCA's release date are given in columns EXPt0(1:ndays)
xexpstd <- pseudo_real_time_exp(xexpstdt, expDate, mday, yPubl$IPCA)       # In-sample nowcasting matrix of inflation expectations - Note: "survey-based std dev" up to IPCA's release date are given in columns EXPt0(1:ndays)

######################################## Set relevant expectations data ########################################

#|=== Transform IPCA survey-based expectations into YoY rates (% changes) if MoMFlag != 1
if (MoMFlag == 1){xexp <- xexpt} else{xexp <- MoMexp2YoY(xexpt,yt[2:length(yt)],ndays)} # In-sample nowcasting matrix of YoY inflation expectations

#|=== Select relevant columns in expectations dataset
exph_idx  <- c((h*ndays+1):((h+1)*ndays))                       # Column idx of forecast horizon h in expectations dataset
xexp      <- xexp[,exph_idx]                                    # Relevant expectations data for horizon h and mdays
focusRegr <- colnames(xexp)                                     # Final names of inflation expectations variables
xexpstd   <- xexpstd[,exph_idx]                                 # Relevant expectations std dev data for horizon h and mdays
colnames(xexpstd) <- sub("EXP","STD",colnames(xexpstd))         # Replace column names of expectations std dev
if (sdfocusFlag == 1){xfocus <- data.frame(xexp,xexpstd)        # Combine median/top5 expectations data with std dev of Focus survey
} else{xfocus <- xexp}                                          # Keep only median/top5 expectations data

#################################### Set out-of-sample spec and ingredients ####################################

#|=== Set counters and number of windows for the nowcasting/forecasting experiment
N0 <- which.max(mDate[mDate<=T0]) # Counter for T0 in the monthly frequency
N1 <- which.max(mDate[mDate<=T1]) # Counter for T1 in the monthly frequency
N  <- N1-N0+1                     # Number of monthly forecasting windows

#|=== Combine low- and high-frequency regressors
if (focusFlag == 1){Xx <- data.frame(x,xm,xw,xd,xfocus)  # Construct matrix of predictors with expectations data
} else{Xx <- data.frame(x,xm,xw,xd)}                     # Construct matrix of predictors without expectations data

#|=== Replace in-sample missing values in Xx using PCA analysis 
imputFlag <- 0                                           # Dummy for replacing missing values using PCA: Yes == 1, otherwise No
if (rollwdwFlag!=1 & imputFlag==1){
  # ncomp         <- estim_ncpPCA(Xx[1:(N0-1),], ncp.min = 1, ncp.max = 20, method.cv = "Kfold", verbose = FALSE) # Cross-validate to find the optimal number of components
  Xx_PCA        <- imputePCA(Xx[1:(N0-1),], ncp = 15)    # Impute in-sample missing values using the optimal number of components from previous step
  Xx[1:(N0-1),] <- Xx_PCA$completeObs                    # Replace matrix of predictors with imputed values
}

#|=== Redefine key variables for the expanding-window scheme choice:
if (rollwdwFlag != 1){
  setwd(paste0(path, "/Support code/")) 
  source("expwdw_redefine.R") 
  setwd(path)
}

#|=== Set estimation parameters and prepare key ingredients for the out-of-sample experiment
nX         <- ncol(Xx)                                               # Number of regressors
xRegr      <- colnames(Xx)                                           # Names of regressors
hqRegr     <- setdiff(xRegr,mRegr)                                   # Names of HF regressors
YoosDate   <- yPubl$IPCA[yPubl$Reference>=T0 & yPubl$Reference<=T1]  # Out-of-sample IPCA release dates
XoosDate   <- oosDate(N0, N1, N, ndays, woosDate, mday, xw, xdt, xd, xfocus, focusFlag, sdfocusFlag, h, moosDate, poosDate) # Combine out-of-sample release dates for X
hqout      <- ragged_edge_hq(Xx,tail(x,n=N1),ndays,XoosDate,N)       # Construct HF vectors of data and release dates 
Xhq        <- hqout$Xhq                                              # Save HF matrix of data
XhqoosDate <- hqout$XhqoosDate                                       # Save HF matrix of release dates
XxRegr     <- hqout$XxRegr                                           # Save vector of names (without HF complement)
XhqRegr    <- hqout$XhqRegr                                          # Save vector of unique names of HF regressors

#|=== Specify monthly seasonal dummies
if (MoMFlag==1 && seasFlag==1){
  seasdum = data.frame(matrix(0, nrow = nrow(x)+1+h, ncol = 12))     # Matrix of seasonal dummies
  colnames(seasdum) <- c(paste0("M",1:12))                           # Name seasonal dummies from M1 to M12
  for (i in 1:12){                                                   # Loop over months
    seas_idx <- seq(i,nrow(seasdum),by=12)                           # Create sequence of monthly dummies
    seasdum[seas_idx,i] = 1                                          # Set deterministic seasonal dummies
  }
  seasdum <- tail(seasdum,n=N1+h)                                    # Drop initial observations if fixedDateBegin>"2003M2"
  seasdum <- seasdum[,1:11]                                          # Keep only 11 dummies due to the model intercept
}