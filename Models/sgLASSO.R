################################################## sg-LASSO ###################################################
# sg-LASSO-MIDAS estimates for inflation nowcasts/forecasts
###############################################################################################################

# Sourcing auxiliary objects
setwd(paste0(path, "/Support code/"))    # Working directory for the auxiliary functions
source('specTable.r')                    # Table with the model specification
setwd(path)


# Set sg-LASSO parameter choices
degree  <- 1                                                        # Degree of the Legendre polynomial (L)
nfolds  <- 10                                                       # Number of folds in hold-out cross-validation (size of each fold = oos months/nfolds)
sglstdFlag <- 0                                                     # Dummy for standardizing variables in sg-LASSO: Yes == 1, otherwise No

# Sourcing auxiliary functions
setwd(paste0(path, "/Support code/"))    # Working directory for the auxiliary functions
source('midas_functions.r')              # Load MIDAS functions
source('midasml_functions.r')            # Load sg-LASSO-MIDAS functions
setwd(path)

# Create empty objects for storing estimated coefficients, forecasts and realized values
coefnames   <- c("Const",paste0("Y",1:p),mRegr)                               # Name low-frequency coefficients of coef_sgl
for(j in 1:length(XhqRegr)){                                         
  coefnames <- c(coefnames,paste0(XhqRegr[j],"pol",0:degree))                 # Name high-frequency coefficients of coef_sgl associated with the Legendre polynomials
}
coef_matrix        <- rep(list(data.frame(matrix(NA,                          # Initialize data frame to store estimated coefficients for each forecasting window and each nday
                                                 nrow = N, ncol = length(coefnames)))), ndays)  
for (i in 1:ndays){colnames(coef_matrix[[i]]) <- coefnames}                   # Name coefficients associated with each regressor
gamma_sgl          <- NULL                                                    # Initialize vector to store optimized gammas
Yresults           <- data.frame(matrix(NA,nrow=(N-1),ncol=(1+2*ndays)))      # Initialize matrix to store observed y, the nowcasts at each ndays and corresponding market expectations
colnames(Yresults) <- c("Yobs",paste0("Yhat",1:ndays),paste0("Exp",1:ndays))  # Name Yresults

# Start computation timer
start_time = Sys.time()

# Start forecast iterations
for (n in 1:N){                                                       # Loop over out-of-sample periods
  
  # Extract data from the relevant estimation window
  if (rollwdwFlag == 1){idx <- n:(N0+n-2)} else{idx <- 1:(N0+n-2)}    # Estimation rolling-window
  Y   <- y[idx[-(1:p)]]                                               # Relevant estimation window of target variable Y
  Yx  <- lagYx(y[idx],p)                                              # Relevant estimation window of lagged variables Y
  if (sglstdFlag == 1){Yx <- scale(Yx, center = TRUE, scale = TRUE)}  # (standardized version)
  Ydate <- ceiling_date(mDate[idx], "month") - days(1)                # Full in-sample reference dates at the low-frequency
  
  for (i in 1:ndays){                                                 # Loop over nowcasting days
    t     <- mday[N0+n-1,i]                                           # Date of the nowcast/forecast

    # Construct high-frequency environment for the relevant sg-LASSO-MIDAS estimation window
    if (rollwdwFlag == 1){idxhq <- ((n-1)*ndays+1):(ndays*(N0+n-2))   # Estimation rolling-window at the high-frequency time index 
    } else{idxhq <- 1:(ndays*(N0+n-2))}                               # Estimation expanding-window at the high-frequency time index
    Xdate <- as.vector(t(mday[idx[-(1:p)],]))                         # In-sample reference dates at the high-frequency
    Xh0   <- Xhq[idxhq[-(1:ndays*p)],]                                # Extract relevant estimation window of HF regressors
    Xhna  <- !is.na(Xh0[1,])                                          # Check for data availability of each predictor
    Xh    <- Xh0[,Xhna]                                               # Obtain a balanced panel with available data for all periods and stardardize regressors
    if (sglstdFlag == 1){Xh <- scale(Xh, center = TRUE, scale = TRUE)}# (standardized version)
    Xhnames <- colnames(Xh)                                           # Store names of available regressors
    legendre_degree <- c(rep(degree, times = ncol(Xh)))               # Replicate Legendre degree for each regressor
    x.lag <- rep(ndays, times = ncol(Xh))                             # Frequency ratio of each regressor
    x_str <- NULL                                                     # Initialize matrix to store group transformed data
    group_index <- 0                                                  # Initialize vector to store group indices/domain
    for (j in 1:ncol(Xh)){                                            # Loop over available high-frequency regressors
      tmp <- mixed_freq_data_single(data.refdate = Ydate, data.x = Xh[,j], # Get MIDAS structure
                                    data.xdate = Xdate, x.lag[j], horizon = 0, Xdate[1], Xdate[length(Xdate)], disp.flag = FALSE)
      leg_w <- lb(legendre_degree[j],a=0,b=1,jmax=x.lag[j])           # Get Legendre weights
      tmp_w <- tmp$est.x%*%leg_w                                      # Apply Legendre polynomials to the group
      colnames(tmp_w) <- paste0(Xhnames[j],"pol",0:degree)            # Rename group transformed data
      x_str <- cbind(x_str, tmp_w)                                    # Aggregate in-sample structured group data
      group_index <- c(group_index, rep(max(group_index)+1, times = legendre_degree[j]+1)) # Get group indices/domain
    }
    group_index <- group_index[-1]
  
    # Combine low- and high-frequency regressors to estimate sg-LASSO-MIDAS at the relevant estimation window
    mRegr_ok <- mRegr[(which(as.matrix(XoosDate[n,mRegr])<=t))]           # Obtain LF regressors that enter the model specification at nowcast day t
    if (lowfrFlag != 1){mRegr_ok <- character(0)}                         # Set condition to remove LF indicators from X if lowfrFlag!=1
    if(identical(mRegr_ok, character(0))){                                # Check whether no LF regressor is available yet
      nLow <- ncol(Yx)                                                    # Number of LF regressors at iteration n (including autoregressive components)
      X    <- data.frame(Yx,x_str)                                        # Construct full matrix of regressors with structured group data
    } else {
      Xmt  <- Xx[idx[-(1:p)],mRegr_ok]                                    # Extract relevant estimation window of available LF regressors
      if(is.vector(Xmt) && is.na(Xmt[1])){                                # Check whether only one LF regressor has been selected and whether data are not fully available
        nLow <- ncol(Yx)                                                  # Number of LF regressors at iteration n (including autoregressive components)                                   
        X    <- data.frame(Yx,x_str)                                      # Construct full matrix of regressors with structured group data
      } else if(is.vector(Xmt) && !is.na(Xmt[1])){                        # Check whether only one LF regressor has been selected and all data is available   
        if (sglstdFlag == 1){Xmt <- scale(Xmt, center = TRUE, scale = TRUE)} # (standardized version)
        nLow <- ncol(cbind(Yx,Xmt))                                       # Number of LF regressors at iteration n (including autoregressive components)
        X    <- data.frame(Yx,Xmt,x_str)                                  # Construct full matrix of regressors with structured group data
        colnames(X) <- c(paste0("Y",1:p),mRegr_ok,colnames(x_str))        # Define column names
      } else {
        Xna <- !is.na(Xmt[1,])                                            # Check for data availability of the LF regressors
        Xmt  <- Xmt[,Xna]                                                 # Obtain a balanced panel with available data for all periods
        if (sglstdFlag == 1){Xmt <- scale(Xmt, center = TRUE, scale = TRUE)} # (standardized version)
        nLow <- ncol(cbind(Yx,Xmt))                                       # Number of LF regressors at iteration n (including autoregressive components)
        X    <- data.frame(Yx,Xmt,x_str)                                  # Construct full matrix of regressors with structured group data
      }
    }
    gindex_str <- c(seq(1,nLow),group_index+nLow)                         # Update group indices/domain
    xRegr_ok   <- colnames(X)                                             # Get names of regressors that are available
    if (seasFlag==1){
      seasadj <- seasdum[idx[-(1:p)],]                                    # Extract relevant seasonal dummies
      X <- cbind(X,seasadj)                                               # Add seasonal dummies if seasFlag=1
      gindex_str <- c(gindex_str,seq(nLow+ncol(Xh)+1,nLow+ncol(Xh)+11))   # Update group indices/domain
    }
  
    # Time series cross-validation and model estimation
    gsg    <- seq(0, 1, length = ndays)                                 # Distributed lag space
    gammas <- seq(1, 0, length = 21)                                    # Grid of gammas
    cvms   <- NULL                                                      # Initialize vector of optimized lambda (smallest average of CV error curve) for each gamma
    for (i_gamma in gammas) {                                                                # Loop over gammas (interpolates between LASSO when =1 and group LASSO when =0)
      tmp  <- cv.sglfit(x = X, y = Y, gamma = i_gamma, gindex = gindex_str, nfolds = nfolds) # sg-LASSO estimates for i_gamma using 100 default values for lambda 
      cvms <- c(cvms, min(tmp$cvm))                                                          # Store smallest average of CV error curve over the nfolds (optimized lambda for i_gamma)
    }
    gamma <- gammas[which(min(cvms) == cvms)]                                                # Obtain optimized gamma
    fit <- cv.sglfit(x = X, y = Y, gamma = gamma, gindex = gindex_str, nfolds = nfolds)      # sg-LASSO estimates for optimized gamma
  
    # Save coefficients
    gamma_sgl    <- c(gamma_sgl,gamma)                                                       # Save optimized gamma for oos period n
    lambda_opt   <- which(min(fit$cvm) == fit$cvm)                                           # Find optimized lambda from optimized gamma
    b0 <- as.matrix(fit$sgl.fit$b0[lambda_opt])                                              # Obtain intercept/constant of optimized sg-LASSO
    rownames(b0) <- "Const"                                                                  # Name b0
    coef_sgl  <- rbind(b0,as.matrix(fit$sgl.fit$beta[,lambda_opt]))                          # Store coefficients of optimized sg-LASSO
    coef_matrix[[i]][n,rownames(coef_sgl)] <- coef_sgl                                       # Save coefficients for oos period n and relevant nowcasting day i
  
    # Solve ragged-edge problem of HF regressors
    Xh_ok <- unlist(lapply(Xhnames,function(x) paste0(x,1:ndays)))             # Select relevant HF regressors at nowcast day t
    if(identical(mRegr_ok,character(0))){Xt_ok<-Xh_ok} else{Xt_ok<-c(mRegr_ok,Xh_ok)} # Select relevant Xx regressors at nowcast day t
    Xt    <- Xx[idx[-(1:p)],Xt_ok]                                             # Extract relevant estimation window of regressors X
    Xpred_idx <- nrow(Xt)+1                                                    # Row idx of the nowcasting vector in Xt
    Xt[Xpred_idx,] <- Xx[N0+n-1,Xt_ok]                                         # Vector of pseudo real-time regressors X at t
    if (ragedgeFlag == 1){                                                     # Use RW forecasts to complete the ragged edge
      for (j in 1:ncol(Xt)){                                                   # Loop over regressors
        jRegr   <- colnames(Xt[j])                                             # Get regressor name
        jRegrhq <- substr(jRegr,1,nchar(jRegr)-1)                              # Get regressor name without HF complement
        if (jRegrhq%in%XhqRegr){                                               # Check whether regressor j is a HF indicator
          Xt[Xpred_idx,j] <- Xhq[max(which(XhqoosDate[,jRegrhq]<=t)),jRegrhq]  # Save RW prediction for HF regressor j
        }
      }
    } else{                                                                    # Overcome the ragged edge problem by using the latest ndays available observations
      for (j in 1:length(Xhnames)){                                            # Loop over unique regressors
        jRegrhq <- Xhnames[j]                                                  # Get regressor name without HF complement
        jMaxidx <- max(which(XhqoosDate[,jRegrhq]<=t))                         # HF idx of latest available data
        Xt_jidx <- c(rep(F,length(mRegr_ok)),substr(Xh_ok,1,nchar(Xh_ok)-1)==jRegrhq) # Obtain all idx of jRegrhq in Xt
        Xt[Xpred_idx,Xt_jidx] <- Xhq[(jMaxidx-ndays+1):jMaxidx,jRegrhq]        # Save latest ndays available observations
      }
    }
    
    # Prepare conditional information available (already published) at day t under the sg-LASSO-MIDAS environment using ragged-edge solved data
    Ydateout  <- ceiling_date(mDate[idx+1], "month") - days(1)                # Full in-sample reference dates at the low-frequency
    Xdateout  <- as.vector(t(mday[idx[-(1:p)]+1,]))                           # In-sample reference dates at the high-frequency
    x_str_out <- NULL                                                         # Initialize matrix to store group transformed data
    for (j in 1:ncol(Xh)){
    Xdataout <- rbind(as.matrix(Xh0[-(1:ndays),Xhnames[j]]),as.matrix(t(Xt[nrow(Xt),paste0(Xhnames[j],1:ndays)]))) # Combine high-frequency observations with ragged-edge
    if (sglstdFlag == 1){Xdataout <- scale(Xdataout, center = TRUE, scale = TRUE)} # (standardized version)
    tmp <- mixed_freq_data_single(data.refdate = Ydateout, data.x = Xdataout, # Get MIDAS structure
                                  data.xdate = Xdateout, x.lag[j], horizon = 0, Xdateout[1], Xdateout[length(Xdateout)-ndays], disp.flag = FALSE)
    leg_w <- lb(legendre_degree[j],a=0,b=1,jmax=x.lag[j])                     # Get Legendre weights
    tmp_w <- tmp$out.x%*%leg_w                                                # Apply Legendre polynomials to the group
    colnames(tmp_w) <- paste0(Xhnames[j],"pol",0:degree)                      # Rename group transformed data
    x_str_out <- cbind(x_str_out, tmp_w)                                      # Aggregate out-of-sample structured group data
    }
    
    # Prepare remaining conditional information available (already published) at day t
    if (sglstdFlag == 0){
      Yxpred <- lagYx(y[(N0+n-1-p):(N0+n-1)],p)                               # Save lagged variables Y for nowcasting/forecasting
      if(!identical(mRegr_ok, character(0))){                                 # Check whether any LF regressor is available
        Xmpred <- tail(Xt[,mRegr_ok], n=1)                                    # Save low-frequency predictors for nowcasting/forecasting
        Xpred  <- data.frame(Yxpred,Xmpred,x_str_out)                         # Save vector of all conditional information at the time of the nowcast/forecast
      } else {
        Xpred  <- data.frame(Yxpred,x_str_out)                                # Save vector of all conditional information at the time of the nowcast/forecast
      }
    } 
    if (sglstdFlag == 1){
      Yxpred <- tail(scale(lagYx(y[n:(N0+n-1)],p), center=TRUE, scale=TRUE), n=1) # (standardized version)
      if(!identical(mRegr_ok, character(0))){                                 # Check whether any LF regressor is available
        Xmpred <- tail(scale(Xt[,mRegr_ok], center=TRUE, scale=TRUE), n=1)    # (standardized version)
        Xpred  <- data.frame(Yxpred,Xmpred,x_str_out)                         # Save vector of all conditional information at the time of the nowcast/forecast
      } else{
        Xpred  <- data.frame(Yxpred,x_str_out)                                # Save vector of all conditional information at the time of the nowcast/forecast
      }
    }
    if (seasFlag == 1){Xpred <- cbind(Xpred,seasdum[N0+n-1,])}                # Add seasonal dummies if seasFlag=1    

    # Compute nowcast/forecast at day t using available data
    Yhat  <- predict(fit, newx = data.matrix(Xpred))                          # Nowcast/forecast for Y_t+h        
    
    # Survey-based expectations
    Yfocus <- xexp[(N0+n-1),focusRegr[i]]                               # Nowcast of market expectations (median or Top5)
    
    # Convert implied MoM predicted rates into YoY rates and store nowcast/forecast
    if (MoMFlag==1){                                 
      Yhat   <- exp2YoY(c(y[(N0+n-12):(N0+n-2)],Yhat))                  # Forecast of the YoY inflation rate via implied MoM
      Yfocus <- exp2YoY(c(y[(N0+n-12):(N0+n-2)],Yfocus))                # Market expectations for the YoY inflation rate via implied MoM
    }
    Yresults[n,(1+i)]       <- Yhat                                     # Store nowcast/forecast in Yresults
    Yresults[n,(1+ndays+i)] <- Yfocus                                   # Store market expectations in Yresults
    
  } # End loop over nowcasting days

  # Out-of-sample realized target variable Y_t+h
  if (MoMFlag==1){Yout <- exp2YoY(y[(N0+n-12):(N0+n-1)])}               # Observed YoY inflation rate if MoMFlag==1
  if (MoMFlag!=1){Yout <- y[N0+n-1]}                                    # Observed YoY inflation rate if MoMFlag==0
  Yresults[n,1] <- Yout                                                 # Store observed YoY inflation rate in Yresults
  
  # Print timer on screen
  if (n %% 10 == 0){
    print(paste0("Now at iteration ", n, " of ", N, "."))
    print(round(Sys.time () - start_time,2))
  }
  
} # End loop over out-of-sample periods

# Computes overall time
print(paste0("End of forecasts using ", modelName))
print(round(Sys.time () - start_time,2))

# Evaluate forecast errors: computing RMSE and MAE metrics
RMSE <- rep(NA, 2*ndays)                        # Initialize vector to store the RMSE value for each nowcasting day i (model vs market expectations)
MAE  <- rep(NA, 2*ndays)                        # Initialize vector to store the MAE value for each nowcasting day i (model vs market expectations)
for (i in 1:(2*ndays)) {                        # Loop over ndays and competing nowcasts
  RMSE[i] <- rmse(Yresults[,1], Yresults[,1+i]) # Compute RMSEs
  MAE[i]  <- mae(Yresults[,1], Yresults[,1+i])  # Compute MAEs
}
RMSE <- data.frame(matrix(as.vector(round(RMSE,4)), ncol = 2*ndays))                           # Save RMSEs in data frame format
MAE  <- data.frame(matrix(as.vector(round(MAE,4)), ncol = 2*ndays))                            # Save MAEs in data frame format
colnames(RMSE) <- c(paste0("RMSE_",modelName,"_nday",1:ndays),paste0("RMSE_MarketExp_nday",1:ndays)) # Name RMSE columns
colnames(MAE)  <- c(paste0("MAE_",modelName,"_nday",1:ndays),paste0("MAE_MarketExp_nday",1:ndays))   # Name MAE columns
FcstError <- cbind(RMSE, MAE)                                                                  # Combine RMSE and MAE in a single data frame

if (saveFlag == 1) {
  today <- paste0(substring(Sys.Date(), 9, 10), substring(Sys.Date(), 6, 7), substring(Sys.Date(), 3, 4))
  
  modelSpecs       <- paste0(modelName, today)
  
  fileName_results <- paste0("results_", modelSpecs, ".xlsx")                                  
  fileName_RMSE    <- paste0("RMSE_", modelSpecs, ".xlsx")
  fileName_coef    <- paste0("coef_", modelSpecs, ".xlsx")
  fileName_data    <- paste0("Env_", modelSpecs, ".RData")
  
  setwd(paste0(path, "/Results/", modelName))
  save.image(file=fileName_data)
  
  xlsx::write.xlsx(data.frame(Yresults), file=fileName_results, sheetName="Yresults", 
                   row.names = FALSE, append=FALSE)
  xlsx::write.xlsx(dummyTab, file=fileName_results, sheetName="ModelSpec", 
                   row.names = FALSE, append=TRUE)
  
  xlsx::write.xlsx(data.frame(FcstError), file=fileName_RMSE, sheetName="RMSE", 
                   row.names = FALSE, append=FALSE)
  xlsx::write.xlsx(dummyTab, file=fileName_RMSE, sheetName="ModelSpec", 
                   row.names = FALSE, append=TRUE)

  xlsx::write.xlsx(data.frame(coef_matrix), file=fileName_coef, sheetName="Coefficients", 
                   row.names = FALSE, append=FALSE)
  xlsx::write.xlsx(dummyTab, file=fileName_coef, sheetName="ModelSpec", 
                   row.names = FALSE, append=TRUE)
  
  
}