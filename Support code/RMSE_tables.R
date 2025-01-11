######################## Harnessing Machine Learning for Real-Time Inflation Nowcasting #######################
#
#                           R. Schnorrenberger, A. Schmidt and G. V. Moura (2024)
#                                         Code update: December 2024
#
###############################################################################################################

# This script generates the RMSE tables.

# Installing and loading packages:
rm(list=ls())                                           
chooseCRANmirror(graphics = FALSE, ind = 10)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(xlsx,Metrics,forecast)

path = dirname(rstudioapi::getSourceEditorContext()$path) # Path is directory of this file
setwd(path) 
setwd("..")
setwd("./Plots/data/results") # Directory with final predictions

file_results <- list.files(getwd())
name_file <- paste0(file_results[1])

LASSOLLF   <- xlsx::read.xlsx(file = "results_LASSOLLF.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
BART       <- xlsx::read.xlsx(file = "results_BART.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
sgLASSO    <- xlsx::read.xlsx(file = "results_sgLASSO.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
LLF        <- xlsx::read.xlsx(file = "results_LLF.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
ElasticNet <- xlsx::read.xlsx(file = "results_ElasticNet.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
RF         <- xlsx::read.xlsx(file = "results_RF.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
LASSO      <- xlsx::read.xlsx(file = "results_LASSO.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
Ridge      <- xlsx::read.xlsx(file = "results_Ridge.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
MedianSPF  <- xlsx::read.xlsx(file = "results_BART.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,6,7,8,9))
Top5       <- xlsx::read.xlsx(file = "results_Top5.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,6,7,8,9))
AR         <- xlsx::read.xlsx(file = "results_AR.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))
RW         <- xlsx::read.xlsx(file = "results_RW.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = c(1,2,3,4,5))

LASSOLLF   <- LASSOLLF[, -ncol(LASSOLLF)]
BART       <- BART[, -ncol(BART)]
sgLASSO    <- sgLASSO[, -ncol(sgLASSO)]
LLF        <- LLF[, -ncol(LLF)]
ElasticNet <- ElasticNet[, -ncol(ElasticNet)]
RF         <- RF[, -ncol(RF)]
LASSO      <- LASSO[, -ncol(LASSO)]
Ridge      <- Ridge[, -ncol(Ridge)]
MedianSPF  <- MedianSPF[, -ncol(MedianSPF)]
Top5       <- Top5[, -ncol(Top5)]
AR         <- AR[, -ncol(AR)]
RW         <- RW[, -ncol(RW)]

models_names <- c("LASSO", "Ridge", "ElasticNet", "sgLASSO", 
                  "RF", "LLF", "BART", "LASSOLLF", "AR", "RW")

RMSE_LASSOLLF <- c(rmse(LASSOLLF[,1], LASSOLLF[,2]),
                   rmse(LASSOLLF[,1], LASSOLLF[,3]),
                   rmse(LASSOLLF[,1], LASSOLLF[,4]),
                   rmse(LASSOLLF[,1], LASSOLLF[,5]))

RMSE_BART <- c(rmse(BART[,1], BART[,2]),
                   rmse(BART[,1], BART[,3]),
                   rmse(BART[,1], BART[,4]),
                   rmse(BART[,1], BART[,5]))

RMSE_sgLASSO <- c(rmse(sgLASSO[,1], sgLASSO[,2]),
                   rmse(sgLASSO[,1], sgLASSO[,3]),
                   rmse(sgLASSO[,1], sgLASSO[,4]),
                   rmse(sgLASSO[,1], sgLASSO[,5]))

RMSE_LLF <- c(rmse(LLF[,1], LLF[,2]),
                   rmse(LLF[,1], LLF[,3]),
                   rmse(LLF[,1], LLF[,4]),
                   rmse(LLF[,1], LLF[,5]))

RMSE_RF <- c(rmse(RF[,1], RF[,2]),
                   rmse(RF[,1], RF[,3]),
                   rmse(RF[,1], RF[,4]),
                   rmse(RF[,1], RF[,5]))

RMSE_LASSO <- c(rmse(LASSO[,1], LASSO[,2]),
                   rmse(LASSO[,1], LASSO[,3]),
                   rmse(LASSO[,1], LASSO[,4]),
                   rmse(LASSO[,1], LASSO[,5]))

RMSE_Ridge <- c(rmse(Ridge[,1], Ridge[,2]),
                   rmse(Ridge[,1], Ridge[,3]),
                   rmse(Ridge[,1], Ridge[,4]),
                   rmse(Ridge[,1], Ridge[,5]))

RMSE_ElasticNet <- c(rmse(ElasticNet[,1], ElasticNet[,2]),
                rmse(ElasticNet[,1], ElasticNet[,3]),
                rmse(ElasticNet[,1], ElasticNet[,4]),
                rmse(ElasticNet[,1], ElasticNet[,5]))

RMSE_MedianSPF <- c(rmse(MedianSPF[,1], MedianSPF[,2]),
                   rmse(MedianSPF[,1], MedianSPF[,3]),
                   rmse(MedianSPF[,1], MedianSPF[,4]),
                   rmse(MedianSPF[,1], MedianSPF[,5]))

RMSE_Top5 <- c(rmse(Top5[,1], Top5[,2]),
                   rmse(Top5[,1], Top5[,3]),
                   rmse(Top5[,1], Top5[,4]),
                   rmse(Top5[,1], Top5[,5]))

RMSE_AR <- c(rmse(AR[,1], AR[,2]),
               rmse(AR[,1], AR[,3]),
               rmse(AR[,1], AR[,4]),
               rmse(AR[,1], AR[,5]))

RMSE_RW <- c(rmse(RW[,1], RW[,2]),
               rmse(RW[,1], RW[,3]),
               rmse(RW[,1], RW[,4]),
               rmse(RW[,1], RW[,5]))

nmodels      <- 10

# Make an empty table
tab_rows  <- 4          # 4 weeks x (RMSE)
tab_cols  <- 3+nmodels  # header + SPF + top5 + nmodels

tab <- data.frame(matrix(NA, ncol = tab_cols, nrow = tab_rows))
tab[,1]       <- c("day 8", "day 15", "day 22", "end-of-month")
colnames(tab) <- c("Horizon", "Median", "Top 5", models_names)

# Populate the table
tab[,2]  <- RMSE_MedianSPF
tab[,3]  <- RMSE_Top5
tab[,4]  <- RMSE_LASSO
tab[,5]  <- RMSE_Ridge
tab[,6]  <- RMSE_ElasticNet
tab[,7]  <- RMSE_sgLASSO
tab[,8]  <- RMSE_RF
tab[,9]  <- RMSE_LLF
tab[,10] <- RMSE_BART
tab[,11] <- RMSE_LASSOLLF
tab[,12] <- RMSE_AR
tab[,13] <- RMSE_RW

# Table with relative values
rel_tab_median   <- format(round(tab[1:tab_rows,2:tab_cols]/tab[1:tab_rows,2],3), nsmall = 3)
rel_tab_top5     <- format(round(tab[1:tab_rows,2:tab_cols]/tab[1:tab_rows,3],3), nsmall = 3)
rel_tab_median   <- data.frame(cbind(tab[,1], rel_tab_median))
rel_tab_top5     <- data.frame(cbind(tab[,1], rel_tab_top5))
colnames(rel_tab_median) <- c("Horizon", "Median", "Top 5", models_names)
colnames(rel_tab_top5)   <- c("Horizon", "Median", "Top 5", models_names)

# Run the DM test against the median
# Compute the forecast errors of all models across weeks
ew1_LASSOLLF <- LASSOLLF[,2] - LASSOLLF[,1] 
ew2_LASSOLLF <- LASSOLLF[,3] - LASSOLLF[,1]
ew3_LASSOLLF <- LASSOLLF[,4] - LASSOLLF[,1] 
ew4_LASSOLLF <- LASSOLLF[,5] - LASSOLLF[,1] 

ew1_BART <- BART[,2] - BART[,1] 
ew2_BART <- BART[,3] - BART[,1]
ew3_BART <- BART[,4] - BART[,1] 
ew4_BART <- BART[,5] - BART[,1] 

ew1_sgLASSO <- sgLASSO[,2] - sgLASSO[,1] 
ew2_sgLASSO <- sgLASSO[,3] - sgLASSO[,1]
ew3_sgLASSO <- sgLASSO[,4] - sgLASSO[,1] 
ew4_sgLASSO <- sgLASSO[,5] - sgLASSO[,1] 

ew1_LLF <- LLF[,2] - LLF[,1] 
ew2_LLF <- LLF[,3] - LLF[,1]
ew3_LLF <- LLF[,4] - LLF[,1] 
ew4_LLF <- LLF[,5] - LLF[,1]

ew1_RF <- RF[,2] - RF[,1] 
ew2_RF <- RF[,3] - RF[,1]
ew3_RF <- RF[,4] - RF[,1] 
ew4_RF <- RF[,5] - RF[,1]

ew1_LASSO <- LASSO[,2] - LASSO[,1] 
ew2_LASSO <- LASSO[,3] - LASSO[,1]
ew3_LASSO <- LASSO[,4] - LASSO[,1] 
ew4_LASSO <- LASSO[,5] - LASSO[,1]

ew1_Ridge <- Ridge[,2] - Ridge[,1] 
ew2_Ridge <- Ridge[,3] - Ridge[,1]
ew3_Ridge <- Ridge[,4] - Ridge[,1] 
ew4_Ridge <- Ridge[,5] - Ridge[,1]

ew1_ElasticNet <- ElasticNet[,2] - ElasticNet[,1] 
ew2_ElasticNet <- ElasticNet[,3] - ElasticNet[,1]
ew3_ElasticNet <- ElasticNet[,4] - ElasticNet[,1] 
ew4_ElasticNet <- ElasticNet[,5] - ElasticNet[,1]

ew1_MedianSPF <- MedianSPF[,2] - MedianSPF[,1] 
ew2_MedianSPF <- MedianSPF[,3] - MedianSPF[,1]
ew3_MedianSPF <- MedianSPF[,4] - MedianSPF[,1] 
ew4_MedianSPF <- MedianSPF[,5] - MedianSPF[,1]

ew1_Top5 <- Top5[,2] - Top5[,1] 
ew2_Top5 <- Top5[,3] - Top5[,1]
ew3_Top5 <- Top5[,4] - Top5[,1] 
ew4_Top5 <- Top5[,5] - Top5[,1]

ew1_AR <- AR[,2] - AR[,1] 
ew2_AR <- AR[,3] - AR[,1]
ew3_AR <- AR[,4] - AR[,1] 
ew4_AR <- AR[,5] - AR[,1]

ew1_RW <- RW[,2] - RW[,1] 
ew2_RW <- RW[,3] - RW[,1]
ew3_RW <- RW[,4] - RW[,1] 
ew4_RW <- RW[,5] - RW[,1]

# Compare each model to the MedianSPF using the DM test
# For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1
dmw1_LASSOLLF <- dm.test(ew1_MedianSPF, ew1_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_LASSOLLF <- dm.test(ew2_MedianSPF, ew2_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_LASSOLLF <- dm.test(ew3_MedianSPF, ew3_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_LASSOLLF <- dm.test(ew4_MedianSPF, ew4_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_LASSOLLF <- c(ifelse(dmw1_LASSOLLF < 0.01, "***", ifelse(dmw1_LASSOLLF < 0.05, "**", ifelse(dmw1_LASSOLLF < 0.1, "*", ""))),
                 ifelse(dmw2_LASSOLLF < 0.01, "***", ifelse(dmw2_LASSOLLF < 0.05, "**", ifelse(dmw2_LASSOLLF < 0.1, "*", ""))),
                 ifelse(dmw3_LASSOLLF < 0.01, "***", ifelse(dmw3_LASSOLLF < 0.05, "**", ifelse(dmw3_LASSOLLF < 0.1, "*", ""))),
                 ifelse(dmw4_LASSOLLF < 0.01, "***", ifelse(dmw4_LASSOLLF < 0.05, "**", ifelse(dmw4_LASSOLLF < 0.1, "*", ""))))


dmw1_BART <- dm.test(ew1_MedianSPF, ew1_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_BART <- dm.test(ew2_MedianSPF, ew2_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_BART <- dm.test(ew3_MedianSPF, ew3_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_BART <- dm.test(ew4_MedianSPF, ew4_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_BART   <- c(ifelse(dmw1_BART < 0.01, "***", ifelse(dmw1_BART < 0.05, "**", ifelse(dmw1_BART < 0.1, "*", ""))),
                 ifelse(dmw2_BART < 0.01, "***", ifelse(dmw2_BART < 0.05, "**", ifelse(dmw2_BART < 0.1, "*", ""))),
                 ifelse(dmw3_BART < 0.01, "***", ifelse(dmw3_BART < 0.05, "**", ifelse(dmw3_BART < 0.1, "*", ""))),
                 ifelse(dmw4_BART < 0.01, "***", ifelse(dmw4_BART < 0.05, "**", ifelse(dmw4_BART < 0.1, "*", ""))))

dmw1_sgLASSO <- dm.test(ew1_MedianSPF, ew1_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_sgLASSO <- dm.test(ew2_MedianSPF, ew2_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_sgLASSO <- dm.test(ew3_MedianSPF, ew3_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_sgLASSO <- dm.test(ew4_MedianSPF, ew4_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_sgLASSO   <- c(ifelse(dmw1_sgLASSO < 0.01, "***", ifelse(dmw1_sgLASSO < 0.05, "**", ifelse(dmw1_sgLASSO < 0.1, "*", ""))),
               ifelse(dmw2_sgLASSO < 0.01, "***", ifelse(dmw2_sgLASSO < 0.05, "**", ifelse(dmw2_sgLASSO < 0.1, "*", ""))),
               ifelse(dmw3_sgLASSO < 0.01, "***", ifelse(dmw3_sgLASSO < 0.05, "**", ifelse(dmw3_sgLASSO < 0.1, "*", ""))),
               ifelse(dmw4_sgLASSO < 0.01, "***", ifelse(dmw4_sgLASSO < 0.05, "**", ifelse(dmw4_sgLASSO < 0.1, "*", ""))))

dmw1_LLF <- dm.test(ew1_MedianSPF, ew1_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_LLF <- dm.test(ew2_MedianSPF, ew2_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_LLF <- dm.test(ew3_MedianSPF, ew3_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_LLF <- dm.test(ew4_MedianSPF, ew4_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_LLF   <- c(ifelse(dmw1_LLF < 0.01, "***", ifelse(dmw1_LLF < 0.05, "**", ifelse(dmw1_LLF < 0.1, "*", ""))),
           ifelse(dmw2_LLF < 0.01, "***", ifelse(dmw2_LLF < 0.05, "**", ifelse(dmw2_LLF < 0.1, "*", ""))),
           ifelse(dmw3_LLF < 0.01, "***", ifelse(dmw3_LLF < 0.05, "**", ifelse(dmw3_LLF < 0.1, "*", ""))),
           ifelse(dmw4_LLF < 0.01, "***", ifelse(dmw4_LLF < 0.05, "**", ifelse(dmw4_LLF < 0.1, "*", ""))))

dmw1_RF <- dm.test(ew1_MedianSPF, ew1_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_RF <- dm.test(ew2_MedianSPF, ew2_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_RF <- dm.test(ew3_MedianSPF, ew3_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_RF <- dm.test(ew4_MedianSPF, ew4_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_RF   <- c(ifelse(dmw1_RF < 0.01, "***", ifelse(dmw1_RF < 0.05, "**", ifelse(dmw1_RF < 0.1, "*", ""))),
           ifelse(dmw2_RF < 0.01, "***", ifelse(dmw2_RF < 0.05, "**", ifelse(dmw2_RF < 0.1, "*", ""))),
           ifelse(dmw3_RF < 0.01, "***", ifelse(dmw3_RF < 0.05, "**", ifelse(dmw3_RF < 0.1, "*", ""))),
           ifelse(dmw4_RF < 0.01, "***", ifelse(dmw4_RF < 0.05, "**", ifelse(dmw4_RF < 0.1, "*", ""))))

dmw1_LASSO <- dm.test(ew1_MedianSPF, ew1_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_LASSO <- dm.test(ew2_MedianSPF, ew2_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_LASSO <- dm.test(ew3_MedianSPF, ew3_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_LASSO <- dm.test(ew4_MedianSPF, ew4_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_LASSO   <- c(ifelse(dmw1_LASSO < 0.01, "***", ifelse(dmw1_LASSO < 0.05, "**", ifelse(dmw1_LASSO < 0.1, "*", ""))),
           ifelse(dmw2_LASSO < 0.01, "***", ifelse(dmw2_LASSO < 0.05, "**", ifelse(dmw2_LASSO < 0.1, "*", ""))),
           ifelse(dmw3_LASSO < 0.01, "***", ifelse(dmw3_LASSO < 0.05, "**", ifelse(dmw3_LASSO < 0.1, "*", ""))),
           ifelse(dmw4_LASSO < 0.01, "***", ifelse(dmw4_LASSO < 0.05, "**", ifelse(dmw4_LASSO < 0.1, "*", ""))))

dmw1_Ridge <- dm.test(ew1_MedianSPF, ew1_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_Ridge <- dm.test(ew2_MedianSPF, ew2_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_Ridge <- dm.test(ew3_MedianSPF, ew3_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_Ridge <- dm.test(ew4_MedianSPF, ew4_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_Ridge   <- c(ifelse(dmw1_Ridge < 0.01, "***", ifelse(dmw1_Ridge < 0.05, "**", ifelse(dmw1_Ridge < 0.1, "*", ""))),
           ifelse(dmw2_Ridge < 0.01, "***", ifelse(dmw2_Ridge < 0.05, "**", ifelse(dmw2_Ridge < 0.1, "*", ""))),
           ifelse(dmw3_Ridge < 0.01, "***", ifelse(dmw3_Ridge < 0.05, "**", ifelse(dmw3_Ridge < 0.1, "*", ""))),
           ifelse(dmw4_Ridge < 0.01, "***", ifelse(dmw4_Ridge < 0.05, "**", ifelse(dmw4_Ridge < 0.1, "*", ""))))

dmw1_ElasticNet <- dm.test(ew1_MedianSPF, ew1_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_ElasticNet <- dm.test(ew2_MedianSPF, ew2_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_ElasticNet <- dm.test(ew3_MedianSPF, ew3_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_ElasticNet <- dm.test(ew4_MedianSPF, ew4_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_ElasticNet   <- c(ifelse(dmw1_ElasticNet < 0.01, "***", ifelse(dmw1_ElasticNet < 0.05, "**", ifelse(dmw1_ElasticNet < 0.1, "*", ""))),
           ifelse(dmw2_ElasticNet < 0.01, "***", ifelse(dmw2_ElasticNet < 0.05, "**", ifelse(dmw2_ElasticNet < 0.1, "*", ""))),
           ifelse(dmw3_ElasticNet < 0.01, "***", ifelse(dmw3_ElasticNet < 0.05, "**", ifelse(dmw3_ElasticNet < 0.1, "*", ""))),
           ifelse(dmw4_ElasticNet < 0.01, "***", ifelse(dmw4_ElasticNet < 0.05, "**", ifelse(dmw4_ElasticNet < 0.1, "*", ""))))

dmw1_Top5 <- dm.test(ew1_MedianSPF, ew1_Top5, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_Top5 <- dm.test(ew2_MedianSPF, ew2_Top5, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_Top5 <- dm.test(ew3_MedianSPF, ew3_Top5, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_Top5 <- dm.test(ew4_MedianSPF, ew4_Top5, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_Top5   <- c(ifelse(dmw1_Top5 < 0.01, "***", ifelse(dmw1_Top5 < 0.05, "**", ifelse(dmw1_Top5 < 0.1, "*", ""))),
           ifelse(dmw2_Top5 < 0.01, "***", ifelse(dmw2_Top5 < 0.05, "**", ifelse(dmw2_Top5 < 0.1, "*", ""))),
           ifelse(dmw3_Top5 < 0.01, "***", ifelse(dmw3_Top5 < 0.05, "**", ifelse(dmw3_Top5 < 0.1, "*", ""))),
           ifelse(dmw4_Top5 < 0.01, "***", ifelse(dmw4_Top5 < 0.05, "**", ifelse(dmw4_Top5 < 0.1, "*", ""))))

dmw1_AR <- dm.test(ew1_MedianSPF, ew1_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_AR <- dm.test(ew2_MedianSPF, ew2_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_AR <- dm.test(ew3_MedianSPF, ew3_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_AR <- dm.test(ew4_MedianSPF, ew4_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_AR   <- c(ifelse(dmw1_AR < 0.01, "***", ifelse(dmw1_AR < 0.05, "**", ifelse(dmw1_AR < 0.1, "*", ""))),
               ifelse(dmw2_AR < 0.01, "***", ifelse(dmw2_AR < 0.05, "**", ifelse(dmw2_AR < 0.1, "*", ""))),
               ifelse(dmw3_AR < 0.01, "***", ifelse(dmw3_AR < 0.05, "**", ifelse(dmw3_AR < 0.1, "*", ""))),
               ifelse(dmw4_AR < 0.01, "***", ifelse(dmw4_AR < 0.05, "**", ifelse(dmw4_AR < 0.1, "*", ""))))

dmw1_RW <- dm.test(ew1_MedianSPF, ew1_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_RW <- dm.test(ew2_MedianSPF, ew2_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_RW <- dm.test(ew3_MedianSPF, ew3_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_RW <- dm.test(ew4_MedianSPF, ew4_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_RW   <- c(ifelse(dmw1_RW < 0.01, "***", ifelse(dmw1_RW < 0.05, "**", ifelse(dmw1_RW < 0.1, "*", ""))),
           ifelse(dmw2_RW < 0.01, "***", ifelse(dmw2_RW < 0.05, "**", ifelse(dmw2_RW < 0.1, "*", ""))),
           ifelse(dmw3_RW < 0.01, "***", ifelse(dmw3_RW < 0.05, "**", ifelse(dmw3_RW < 0.1, "*", ""))),
           ifelse(dmw4_RW < 0.01, "***", ifelse(dmw4_RW < 0.05, "**", ifelse(dmw4_RW < 0.1, "*", ""))))

# Merge results in the table
rel_tab_median[, 3]  <- paste0(rel_tab_median[, 3], dm_Top5)
rel_tab_median[, 4]  <- paste0(rel_tab_median[, 4], dm_LASSO)
rel_tab_median[, 5]  <- paste0(rel_tab_median[, 5], dm_Ridge)
rel_tab_median[, 6]  <- paste0(rel_tab_median[, 6], dm_ElasticNet)
rel_tab_median[, 7]  <- paste0(rel_tab_median[, 7], dm_sgLASSO)
rel_tab_median[, 8]  <- paste0(rel_tab_median[, 8], dm_RF)
rel_tab_median[, 9]  <- paste0(rel_tab_median[, 9], dm_LLF)
rel_tab_median[, 10] <- paste0(rel_tab_median[, 10], dm_BART)
rel_tab_median[, 11] <- paste0(rel_tab_median[, 11], dm_LASSOLLF)
rel_tab_median[, 12] <- paste0(rel_tab_median[, 12], dm_AR)
rel_tab_median[, 13] <- paste0(rel_tab_median[, 13], dm_RW)

# Compare each model to the TOP5 using the DM test
# For alternative="greater", the alternative hypothesis is that method 2 is more accurate than method 1
dmw1_LASSOLLF <- dm.test(ew1_Top5, ew1_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_LASSOLLF <- dm.test(ew2_Top5, ew2_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_LASSOLLF <- dm.test(ew3_Top5, ew3_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_LASSOLLF <- dm.test(ew4_Top5, ew4_LASSOLLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_LASSOLLF <- c(ifelse(dmw1_LASSOLLF < 0.01, "***", ifelse(dmw1_LASSOLLF < 0.05, "**", ifelse(dmw1_LASSOLLF < 0.1, "*", ""))),
                 ifelse(dmw2_LASSOLLF < 0.01, "***", ifelse(dmw2_LASSOLLF < 0.05, "**", ifelse(dmw2_LASSOLLF < 0.1, "*", ""))),
                 ifelse(dmw3_LASSOLLF < 0.01, "***", ifelse(dmw3_LASSOLLF < 0.05, "**", ifelse(dmw3_LASSOLLF < 0.1, "*", ""))),
                 ifelse(dmw4_LASSOLLF < 0.01, "***", ifelse(dmw4_LASSOLLF < 0.05, "**", ifelse(dmw4_LASSOLLF < 0.1, "*", ""))))



dmw1_BART <- dm.test(ew1_Top5, ew1_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_BART <- dm.test(ew2_Top5, ew2_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_BART <- dm.test(ew3_Top5, ew3_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_BART <- dm.test(ew4_Top5, ew4_BART, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_BART   <- c(ifelse(dmw1_BART < 0.01, "***", ifelse(dmw1_BART < 0.05, "**", ifelse(dmw1_BART < 0.1, "*", ""))),
               ifelse(dmw2_BART < 0.01, "***", ifelse(dmw2_BART < 0.05, "**", ifelse(dmw2_BART < 0.1, "*", ""))),
               ifelse(dmw3_BART < 0.01, "***", ifelse(dmw3_BART < 0.05, "**", ifelse(dmw3_BART < 0.1, "*", ""))),
               ifelse(dmw4_BART < 0.01, "***", ifelse(dmw4_BART < 0.05, "**", ifelse(dmw4_BART < 0.1, "*", ""))))


dmw1_sgLASSO <- dm.test(ew1_Top5, ew1_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_sgLASSO <- dm.test(ew2_Top5, ew2_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_sgLASSO <- dm.test(ew3_Top5, ew3_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_sgLASSO <- dm.test(ew4_Top5, ew4_sgLASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_sgLASSO   <- c(ifelse(dmw1_sgLASSO < 0.01, "***", ifelse(dmw1_sgLASSO < 0.05, "**", ifelse(dmw1_sgLASSO < 0.1, "*", ""))),
                  ifelse(dmw2_sgLASSO < 0.01, "***", ifelse(dmw2_sgLASSO < 0.05, "**", ifelse(dmw2_sgLASSO < 0.1, "*", ""))),
                  ifelse(dmw3_sgLASSO < 0.01, "***", ifelse(dmw3_sgLASSO < 0.05, "**", ifelse(dmw3_sgLASSO < 0.1, "*", ""))),
                  ifelse(dmw4_sgLASSO < 0.01, "***", ifelse(dmw4_sgLASSO < 0.05, "**", ifelse(dmw4_sgLASSO < 0.1, "*", ""))))


dmw1_LLF <- dm.test(ew1_Top5, ew1_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_LLF <- dm.test(ew2_Top5, ew2_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_LLF <- dm.test(ew3_Top5, ew3_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_LLF <- dm.test(ew4_Top5, ew4_LLF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_LLF   <- c(ifelse(dmw1_LLF < 0.01, "***", ifelse(dmw1_LLF < 0.05, "**", ifelse(dmw1_LLF < 0.1, "*", ""))),
              ifelse(dmw2_LLF < 0.01, "***", ifelse(dmw2_LLF < 0.05, "**", ifelse(dmw2_LLF < 0.1, "*", ""))),
              ifelse(dmw3_LLF < 0.01, "***", ifelse(dmw3_LLF < 0.05, "**", ifelse(dmw3_LLF < 0.1, "*", ""))),
              ifelse(dmw4_LLF < 0.01, "***", ifelse(dmw4_LLF < 0.05, "**", ifelse(dmw4_LLF < 0.1, "*", ""))))

dmw1_RF <- dm.test(ew1_Top5, ew1_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_RF <- dm.test(ew2_Top5, ew2_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_RF <- dm.test(ew3_Top5, ew3_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_RF <- dm.test(ew4_Top5, ew4_RF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_RF   <- c(ifelse(dmw1_RF < 0.01, "***", ifelse(dmw1_RF < 0.05, "**", ifelse(dmw1_RF < 0.1, "*", ""))),
             ifelse(dmw2_RF < 0.01, "***", ifelse(dmw2_RF < 0.05, "**", ifelse(dmw2_RF < 0.1, "*", ""))),
             ifelse(dmw3_RF < 0.01, "***", ifelse(dmw3_RF < 0.05, "**", ifelse(dmw3_RF < 0.1, "*", ""))),
             ifelse(dmw4_RF < 0.01, "***", ifelse(dmw4_RF < 0.05, "**", ifelse(dmw4_RF < 0.1, "*", ""))))

dmw1_LASSO <- dm.test(ew1_Top5, ew1_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_LASSO <- dm.test(ew2_Top5, ew2_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_LASSO <- dm.test(ew3_Top5, ew3_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_LASSO <- dm.test(ew4_Top5, ew4_LASSO, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_LASSO   <- c(ifelse(dmw1_LASSO < 0.01, "***", ifelse(dmw1_LASSO < 0.05, "**", ifelse(dmw1_LASSO < 0.1, "*", ""))),
                ifelse(dmw2_LASSO < 0.01, "***", ifelse(dmw2_LASSO < 0.05, "**", ifelse(dmw2_LASSO < 0.1, "*", ""))),
                ifelse(dmw3_LASSO < 0.01, "***", ifelse(dmw3_LASSO < 0.05, "**", ifelse(dmw3_LASSO < 0.1, "*", ""))),
                ifelse(dmw4_LASSO < 0.01, "***", ifelse(dmw4_LASSO < 0.05, "**", ifelse(dmw4_LASSO < 0.1, "*", ""))))


dmw1_Ridge <- dm.test(ew1_Top5, ew1_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_Ridge <- dm.test(ew2_Top5, ew2_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_Ridge <- dm.test(ew3_Top5, ew3_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_Ridge <- dm.test(ew4_Top5, ew4_Ridge, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_Ridge   <- c(ifelse(dmw1_Ridge < 0.01, "***", ifelse(dmw1_Ridge < 0.05, "**", ifelse(dmw1_Ridge < 0.1, "*", ""))),
                ifelse(dmw2_Ridge < 0.01, "***", ifelse(dmw2_Ridge < 0.05, "**", ifelse(dmw2_Ridge < 0.1, "*", ""))),
                ifelse(dmw3_Ridge < 0.01, "***", ifelse(dmw3_Ridge < 0.05, "**", ifelse(dmw3_Ridge < 0.1, "*", ""))),
                ifelse(dmw4_Ridge < 0.01, "***", ifelse(dmw4_Ridge < 0.05, "**", ifelse(dmw4_Ridge < 0.1, "*", ""))))


dmw1_ElasticNet <- dm.test(ew1_Top5, ew1_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_ElasticNet <- dm.test(ew2_Top5, ew2_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_ElasticNet <- dm.test(ew3_Top5, ew3_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_ElasticNet <- dm.test(ew4_Top5, ew4_ElasticNet, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_ElasticNet   <- c(ifelse(dmw1_ElasticNet < 0.01, "***", ifelse(dmw1_ElasticNet < 0.05, "**", ifelse(dmw1_ElasticNet < 0.1, "*", ""))),
                     ifelse(dmw2_ElasticNet < 0.01, "***", ifelse(dmw2_ElasticNet < 0.05, "**", ifelse(dmw2_ElasticNet < 0.1, "*", ""))),
                     ifelse(dmw3_ElasticNet < 0.01, "***", ifelse(dmw3_ElasticNet < 0.05, "**", ifelse(dmw3_ElasticNet < 0.1, "*", ""))),
                     ifelse(dmw4_ElasticNet < 0.01, "***", ifelse(dmw4_ElasticNet < 0.05, "**", ifelse(dmw4_ElasticNet < 0.1, "*", ""))))


dmw1_MedianSPF <- dm.test(ew1_Top5, ew1_MedianSPF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_MedianSPF <- dm.test(ew2_Top5, ew2_MedianSPF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_MedianSPF <- dm.test(ew3_Top5, ew3_MedianSPF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_MedianSPF <- dm.test(ew4_Top5, ew4_MedianSPF, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_MedianSPF   <- c(ifelse(dmw1_MedianSPF < 0.01, "***", ifelse(dmw1_MedianSPF < 0.05, "**", ifelse(dmw1_MedianSPF < 0.1, "*", ""))),
               ifelse(dmw2_MedianSPF < 0.01, "***", ifelse(dmw2_MedianSPF < 0.05, "**", ifelse(dmw2_MedianSPF < 0.1, "*", ""))),
               ifelse(dmw3_MedianSPF < 0.01, "***", ifelse(dmw3_MedianSPF < 0.05, "**", ifelse(dmw3_MedianSPF < 0.1, "*", ""))),
               ifelse(dmw4_MedianSPF < 0.01, "***", ifelse(dmw4_MedianSPF < 0.05, "**", ifelse(dmw4_MedianSPF < 0.1, "*", ""))))

dmw1_AR <- dm.test(ew1_Top5, ew1_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_AR <- dm.test(ew2_Top5, ew2_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_AR <- dm.test(ew3_Top5, ew3_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_AR <- dm.test(ew4_Top5, ew4_AR, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_AR   <- c(ifelse(dmw1_AR < 0.01, "***", ifelse(dmw1_AR < 0.05, "**", ifelse(dmw1_AR < 0.1, "*", ""))),
             ifelse(dmw2_AR < 0.01, "***", ifelse(dmw2_AR < 0.05, "**", ifelse(dmw2_AR < 0.1, "*", ""))),
             ifelse(dmw3_AR < 0.01, "***", ifelse(dmw3_AR < 0.05, "**", ifelse(dmw3_AR < 0.1, "*", ""))),
             ifelse(dmw4_AR < 0.01, "***", ifelse(dmw4_AR < 0.05, "**", ifelse(dmw4_AR < 0.1, "*", ""))))

dmw1_RW <- dm.test(ew1_Top5, ew1_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw2_RW <- dm.test(ew2_Top5, ew2_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw3_RW <- dm.test(ew3_Top5, ew3_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dmw4_RW <- dm.test(ew4_Top5, ew4_RW, alternative = "greater", varestimator = "acf", h = 1)$p.value
dm_RW   <- c(ifelse(dmw1_RW < 0.01, "***", ifelse(dmw1_RW < 0.05, "**", ifelse(dmw1_RW < 0.1, "*", ""))),
             ifelse(dmw2_RW < 0.01, "***", ifelse(dmw2_RW < 0.05, "**", ifelse(dmw2_RW < 0.1, "*", ""))),
             ifelse(dmw3_RW < 0.01, "***", ifelse(dmw3_RW < 0.05, "**", ifelse(dmw3_RW < 0.1, "*", ""))),
             ifelse(dmw4_RW < 0.01, "***", ifelse(dmw4_RW < 0.05, "**", ifelse(dmw4_RW < 0.1, "*", ""))))

# Merge results in the table
rel_tab_top5[, 2]  <- paste0(rel_tab_top5[, 2], dm_MedianSPF)
rel_tab_top5[, 4]  <- paste0(rel_tab_top5[, 4], dm_LASSO)
rel_tab_top5[, 5]  <- paste0(rel_tab_top5[, 5], dm_Ridge)
rel_tab_top5[, 6]  <- paste0(rel_tab_top5[, 6], dm_ElasticNet)
rel_tab_top5[, 7]  <- paste0(rel_tab_top5[, 7], dm_sgLASSO)
rel_tab_top5[, 8]  <- paste0(rel_tab_top5[, 8], dm_RF)
rel_tab_top5[, 9]  <- paste0(rel_tab_top5[, 9], dm_LLF)
rel_tab_top5[, 10] <- paste0(rel_tab_top5[, 10], dm_BART)
rel_tab_top5[, 11] <- paste0(rel_tab_top5[, 11], dm_LASSOLLF)
rel_tab_top5[, 12] <- paste0(rel_tab_top5[, 12], dm_AR)
rel_tab_top5[, 13] <- paste0(rel_tab_top5[, 13], dm_RW)


# Creating a file
wb = createWorkbook()

sheet = createSheet(wb, "Sheet 1")

addDataFrame(tab, sheet=sheet, startColumn=1, row.names=FALSE)
addDataFrame(rel_tab_median, sheet=sheet, startColumn=1, startRow = 12, row.names=FALSE)
addDataFrame(rel_tab_top5, sheet=sheet, startColumn=1, startRow = 22, row.names=FALSE)

today <- paste0(substring(Sys.Date(), 9, 10), substring(Sys.Date(), 6, 7), substring(Sys.Date(), 1, 4))

fileName_results <- paste0("RMSE_", today, ".xlsx")  

saveWorkbook(wb, fileName_results)


