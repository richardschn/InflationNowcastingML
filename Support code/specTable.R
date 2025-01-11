################################################## specTable ###################################################
# This file makes a table with the specifications used in the model
###############################################################################################################

# Make an empty table
tabNames <- c('date', "h", "p", "focusFlag", 'sdfocusFlag', "lowfrFlag", 
            "MoMFlag", "seasFlag", "ragedgeFlag", "onehqFlag", 
            "rollwdwFlag", "CVtreeFlag", "weightFlag", "degree",
            "nfolds", "sglstdFlag")

dummyTab        <- data.frame(matrix(NA, nrow = 1, ncol = length(tabNames)))
names(dummyTab) <- tabNames

dummyTab[1,1]  <- paste0(Sys.Date())
dummyTab[1,2]  <- h
dummyTab[1,3]  <- p
dummyTab[1,4]  <- focusFlag
dummyTab[1,5]  <- sdfocusFlag
dummyTab[1,6]  <- lowfrFlag
dummyTab[1,7]  <- MoMFlag
dummyTab[1,8]  <- seasFlag
dummyTab[1,9]  <- ragedgeFlag
dummyTab[1,10] <- onehqFlag
dummyTab[1,11] <- rollwdwFlag
dummyTab[1,12] <- CVtreeFlag
dummyTab[1,13] <- weightFlag

if (modelName == 'sgLASSO'){
  dummyTab[1,14] <- degree
  dummyTab[1,15] <- nfolds
  dummyTab[1,16] <- sglstdFlag
} else {
  dummyTab[1,14] <- 0
  dummyTab[1,15] <- 0
  dummyTab[1,16] <- 0
}





  



