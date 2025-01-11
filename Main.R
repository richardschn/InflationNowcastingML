######################## Harnessing Machine Learning for Real-Time Inflation Nowcasting #######################
#
#                           R. Schnorrenberger, A. Schmidt and G. V. Moura (2024)
#                                         Code update: December 2024
#
###############################################################################################################

#|===  Directory setup
rm(list=ls())                                             # Clean working environment 
path = dirname(rstudioapi::getSourceEditorContext()$path) # Path is directory of this file
setwd(path)                                               # Set working directory

#|===  Installing and loading packages
chooseCRANmirror(graphics = FALSE, ind = 10)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(openxlsx, lubridate, glmnet, dplyr, Rcpp,
               Metrics, xlsx, glmnetUtils, Rfast, caret,
               randomForest, midasml, quantregForest, grf,
               bartMachine, rbart, tictoc, qcc, pls, FactoMineR, 
               missMDA, xgboost)

#|=== Specify model specs
h           <- 0   # Forecast horizon of interest (nowcast == 0)
p           <- 1   # Number of autoregressive lags (choose at least 1 lag)
focusFlag   <- 1   # Dummy for keeping Focus survey-based expectations as regressors (Median or Top5): Keep == 1, otherwise Remove
sdfocusFlag <- 0   # Dummy for keeping Focus standard deviations as regressors: Keep == 1, otherwise Remove
lowfrFlag   <- 1   # Dummy for keeping low-frequency price indicators as regressors: Keep == 1, otherwise Remove
MoMFlag     <- 1   # Dummy for working with MoM growth rates of the variables: MoM == 1, otherwise YoY
seasFlag    <- 1   # Dummy for including deterministic monthly seasonal dummies: Yes == 1, otherwise No
ragedgeFlag <- 0   # Dummy for including a ragged-edge structure in the U-MIDAS: Yes == 1, otherwise No
onehqFlag   <- 1   # Dummy for keeping only the lastly available HF regressor: Yes == 1, otherwise No  
rollwdwFlag <- 0   # Dummy for a rolling-window scheme ==1, otherwise expanding-window
CVtreeFlag  <- 0   # Dummy for performing a cross-validation of tree-based methods: Yes == 1, otherwise No  
weightFlag  <- 0   # Dummy for a weighting scheme applied to the regressors: Yes == 1, otherwise No
if (weightFlag == 1){weightWindow <- 48 # Change here the balance between exponential and uniform weights
} else {weightWindow <- 0}

#|=== Set relevant days of the nowcast. Note: nowcasts are made during the month of reference (the month before official releases)
mdays <- c(8,15,22,99)  # Nowcasts at "end of month" == 99

#|=== Results
saveFlag  <- 1          # Dummy for saving the results

#|=== Select model for estimation       
models   <- c("LASSO","Ridge","ElasticNet","sgLASSO","RF","LLF","BART","LASSOLLF", "AR", "RW")
runModel <- 1:length(models)

#|=== Set cross-validation parameter choices
foldsize     <- 12      # Size of each adjacent fold in hold-out time series cross-validation (e.g., 12 for a one-year fold size)
foldinitsize <- 36      # Size of initial fold in hold-out time series cross-validation (e.g., 36 for a three-year fold size)

#|=== Run selected models  
for (k in 1:length(runModel)){
  setwd(path)
  # Set global seed
  seeds <- 6969
  set.seed(seeds)
  # Loading and pre-processing data for nowcasting/forecasting exercise
  source("Loading.R")
  # Run out-of-sample nowcasting/forecasting exercise for the k-th model
  setwd(paste0(path, "/Models/"))    # Working directory for the auxiliary functions
  modelNum  <- runModel[k]
  modelName <- models[modelNum]     
  source(paste0(modelName, ".r"))  
}