#################################################### LASSO RF ####################################################
# Random Forests-MIDAS estimates after a LASSO variable selection for inflation nowcasts/forecasts
###############################################################################################################

# Sourcing auxiliary objects
setwd(paste0(path, "/Support code/"))    # Working directory for the auxiliary functions
source('specTable.r')                    # Table with the model specification
setwd(path)

# Create empty objects for storing estimated coefficients, forecasts and realized values
Yresults            <- data.frame(matrix(NA,nrow=N,ncol=(1+2*ndays)))         # Initialize matrix to store observed y, the nowcasts at each nday and corresponding market expectations
colnames(Yresults)  <- c("Yobs",paste0("Yhat",1:ndays),paste0("Exp",1:ndays)) # Name Yresults

# Start computation timer
start_time = Sys.time()

# Start forecast iterations
for (n in 1:N){                                                     # Loop over out-of-sample periods
  
  # Extract data from the relevant estimation window
  if (rollwdwFlag == 1){idx <- n:(N0+n-2)} else{idx <- 1:(N0+n-2)}  # Estimation rolling-window
  Y   <- y[idx[-(1:p)]]                                             # Relevant estimation window of target variable Y
  Yx  <- lagYx(y[idx],p)                                            # Relevant estimation window of lagged variables Y
  
  for (i in 1:ndays){                                               # Loop over nowcasting days
    t        <- mday[N0+n-1,i]                                      # Date of the nowcast/forecast
    xRegr_ok <- xRegr[-(which(as.matrix(XoosDate[n,mRegr])>t))]     # Obtain regressors that enter the model specification at nowcast day t
    
    # Relevant estimation window of available regressors X for the U-MIDAS without ragged-edge:
    if ((ragedgeFlag==0) & i<ndays){
      hqRegr_not <- hqRegr[substr(hqRegr,nchar(hqRegr),nchar(hqRegr))>i]           # HF regressors not available at nowcast day t
      hqXt  <- Xx[idx,hqRegr_not]                                                  # Vertical shift/alignment of HF regressors not available at nowcast day t
      Xt_ok <- data.frame(Xx[c(idx[-1],N0+n-1),setdiff(xRegr_ok,hqRegr_not)],hqXt) # Combine all xRegr_ok regressors for the relevant estimation window
      Xt_ok <- Xt_ok[,xRegr_ok]                                                    # Reorder columns of Xt
      pRegr_i    <- pRegr[substr(pRegr,nchar(pRegr),nchar(pRegr))<=i]              # HF price indicators (IPCS and FIPE) potentially available at nowcast day t
      for (j in 1:length(pRegr_i)){ if (XoosDate[n,pRegr_i[j]]>t){                 # Check availability of HF price indicators (IPCS and FIPE) at nowcast day t
        Xt_idx   <- nrow(Xt_ok)                                                    # Obtain time dimension of Xt_ok
        if (i==1){
          Xt_ok[Xt_idx,pRegr_i[j]] <- Xx[N0+n-2,paste0(substr(pRegr_i[j],1,nchar(pRegr_i[j])-1),ndays)] # Substitute by latest available data if i=1
        } else{
          Xt_ok[Xt_idx,pRegr_i[j]] <- Xx[N0+n-1,paste0(substr(pRegr_i[j],1,nchar(pRegr_i[j])-1),i-1)] # Substitute by latest available data if i>1
        }                                                  
      }                                                   
      }
      if (p>1){Xt <- head(Xt_ok[-(1:(p-1)),],-1)} else{Xt <- head(Xt_ok,-1)}       # Remove initial observations if p>1
    } else{Xt <- Xx[idx[-(1:p)],xRegr_ok]} # Relevant estimation window of available regressors X for the U-MIDAS with ragged-edge 
    # Keep only the lastly available HF regressors:
    if (onehqFlag == 1){ 
      hqRegr_last  <- hqRegr[substr(hqRegr,nchar(hqRegr),nchar(hqRegr))==i]        # HF regressors lastly available at nowcast day t
      onehqRegr_ok <- union(intersect(xRegr_ok,mRegr),hqRegr_last)                 # Only keep lastly available HF and LF regressors
      Xt <- Xt[,onehqRegr_ok]                                                      # Redefine Xt by keeping only onehqRegr_ok
    }
    X <- data.frame(Yx,Xt)                                          # Combine lagged variables Y with regressors X
    if (lowfrFlag != 1){X[1,mRegr] <- NA}                           # Set condition to remove LF indicators from X if lowfrFlag!=1
    Xna <- !is.na(X[1,])                                            # Check for data availability of each predictor
    X   <- X[,Xna]                                                  # Obtain a balanced panel with available data for all periods
    if (weightFlag == 1){                                           # Set a weighting scheme for the covariates if weightFlag==1
      vWeights <- weightsPPP(X, nrow(X)-weightWindow)               # Obtain weights
      X        <-  sweep(X, MARGIN=1, vWeights, `*`)                # Multiply the weights and the covariate Matrix
    }  
    if (seasFlag==1){X <- cbind(X,seasdum[idx[-(1:p)],])}           # Add seasonal dummies if seasFlag=1
    
    # LASSO variable selection prior to Random Forest: time series cross-validation and model estimation
    myTimeControl <- trainControl(method = "timeslice", initialWindow = foldinitsize,            # Set parameters for hold-out cross-validation
                                  horizon = foldsize, fixedWindow = FALSE, allowParallel = TRUE)
    grid <- 10 ^ seq(1, -5, length= 100)                                                        # Specify grid for lambda
    cv_model <- suppressWarnings(train(y = Y, x = as.matrix(X, ncol = ncol(X)), method = "glmnet", # Train model using cross-validation specifications
                                       tuneGrid = expand.grid(alpha = 1, lambda = grid), trControl = myTimeControl, metric='RMSE'))
    best_lambda <- as.numeric(cv_model$bestTune[2])                                              # Obtain optimized lambda
    best_model  <- glmnet(X, Y, alpha = 1, lambda = best_lambda)                                 # Re-estimate best model
    
    sel_coef <- rownames(coef(best_model, s = 'lambda.min'))[coef(best_model, s = 'lambda.min')[,1]!= 0] # Return non-zero coefficients
    
    # Time series cross-validation and model estimation
    if (CVtreeFlag == 1){
      myTimeControl <- trainControl(method = "timeslice", initialWindow = foldinitsize,            # Set parameters for hold-out cross-validation
                                    horizon = foldsize, fixedWindow = FALSE, allowParallel = TRUE)
      mtry     <- sqrt(ncol(X[, sel_coef[-1]]))
      tunegrid <- expand.grid(.mtry = mtry)
      cv_model <- suppressWarnings(train(y = Y, x = as.matrix(X[, sel_coef[-1]], ncol = ncol(X[, sel_coef[-1]])), method = "rf", # Train model using cross-validation specifications
                                         tuneGrid = tunegrid, trControl = myTimeControl, metric='RMSE'))
      
      best_mtry   <- as.numeric(cv_model$bestTune) # Obtain optimized mtry
      best_model  <- randomForest(y = Y, x= as.matrix(X[, sel_coef[-1]], ncol = ncol(X[, sel_coef[-1]])), mtry = best_mtry, ntree = 500) # Re-estimate best model
    } else {
      best_model  <- randomForest(y = Y, as.matrix(X[, sel_coef[-1]], ncol = ncol(X[, sel_coef[-1]])), ntree = 500) # Estimate best model with default mtry: max(floor(ncol(x)/3), 1)
    }

    # Prepare the nowcasting vector of released data up to the nowcast day t for the U-MIDAS without ragged-edge:
    Xpred_idx <- nrow(Xt)+1                                               # Row idx of the nowcasting vector in Xt
    if ((ragedgeFlag==0) & i<ndays){
      if (onehqFlag==1){Xt[Xpred_idx,] <- tail(Xt_ok[,onehqRegr_ok],n=1)  # Vector of pseudo real-time regressors X at t if onehqFlag==1
      } else{Xt[Xpred_idx,] <- tail(Xt_ok,n=1)}                           # Vector of pseudo real-time regressors X at t
    } else{ # Solve ragged-edge problem of HF regressors using random walk forecasts (via output of "ragged_edge_hq"):
      if (onehqFlag==1){Xt[Xpred_idx,] <- Xx[N0+n-1,onehqRegr_ok]         # Vector of pseudo real-time regressors X at t if onehqFlag==1
      } else{Xt[Xpred_idx,] <- Xx[N0+n-1,xRegr_ok]}                       # Vector of pseudo real-time regressors X at t
      for (j in 1:ncol(Xt)){                                              # Loop over regressors
        jRegr   <- colnames(Xt[j])                                        # Get regressor name
        jRegrhq <- substr(jRegr,1,nchar(jRegr)-1)                         # Get regressor name without HF complement
        if (jRegrhq%in%XhqRegr && XoosDate[n,jRegr]>t){                   # Check whether ragged-edge is a problem for HF regressor j at nowcast day t
          Xt[Xpred_idx,j] <- Xhq[max(which(XhqoosDate[,jRegrhq]<=t)),jRegrhq] # Save RW prediction for HF regressor j
        }
      }
    }
    
    # Prepare conditional information available (already published) at day t
    Yxpred <- lagYx(y[(N0+n-1-p):(N0+n-1)],p)                           # Save lagged variables Y for nowcasting/forecasting
    Xpred  <- data.frame(Yxpred,tail(Xt,n=1))                           # Save vector of all conditional information at the time of the nowcast/forecast
    Xpred  <- Xpred[,Xna]                                               # Obtain balanced conditional information at the time of the nowcast/forecast
    if (weightFlag == 1){Xpred <- Xpred*vWeights[length(vWeights)]}     # Multiply by the last weight to avoid bias
    if (seasFlag==1){Xpred <- cbind(Xpred,seasdum[N0+n-1,])}            # Add seasonal dummies if seasFlag=1
    
    # Compute nowcast/forecast at day t using available data
    Yhat  <- predict(best_model, newdata = as.matrix(Xpred[,sel_coef[-1]], ncol = ncol(Xpred[,sel_coef[-1]]))) # Nowcast/forecast for Y_t+h
    
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
  fileName_data    <- paste0("Env_", modelSpecs, ".RData")
  
  setwd(paste0(path, "/Results/", modelName))
  save.image(file=fileName_data)
  
  xlsx::write.xlsx(data.frame(Yresults), file=fileName_results, sheetName="Yresults", 
                   row.names = FALSE, append=FALSE)
  xlsx::write.xlsx(dummyTab, file=fileName_results, sheetName="ModelSpec", 
                   row.names = FALSE, append=TRUE)
  
  xlsx::write.xlsx(data.frame(FcstError), file=fileName_RMSE, sheetName="RSME", 
                   row.names = FALSE, append=FALSE)
  xlsx::write.xlsx(dummyTab, file=fileName_RMSE, sheetName="ModelSpec", 
                   row.names = FALSE, append=TRUE)
}