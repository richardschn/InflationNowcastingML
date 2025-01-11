######################## Harnessing Machine Learning for Real-Time Inflation Nowcasting #######################
#
#                           R. Schnorrenberger, A. Schmidt and G. V. Moura (2024)
#                                         Code update: December 2024
#
###############################################################################################################

# testing.R
# In this file we do tests to compare the forecast errors between different models

#|===  Directory setup
rm(list=ls())                                             # Clean working environment 
path = dirname(rstudioapi::getSourceEditorContext()$path) # Path is directory of this file
setwd(path)                                               # Set working directory

#|===  Installing and loading packages
chooseCRANmirror(graphics = FALSE, ind = 10)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(openxlsx, lubridate, dplyr,
               Metrics, xlsx, murphydiagram, remotes,
               ggplot2, directlabels, gridExtra,MultiHorizonSPA)

remotes::install_github("lucabarbaglia/MultiHorizonSPA") # If you have it not installed yet
library(MultiHorizonSPA) # It is outside the cran, thus needs to be loaded separately


#|=== Set relevant days of the nowcast. Note: nowcasts are made during the month of reference (the month before official releases)
mdays <- c(8,15,22,99)  # Nowcasts at "end of month" == 99

#|=== Select model for estimation       
models   <- c("LASSO", "Ridge", "ElasticNet", "sgLASSO", 
              "RF", "LLF", "BART", "LASSOLLF")

#source("Loading.R")
#path = dirname(rstudioapi::getSourceEditorContext()$path) # Path is directory of this file
#setwd(path)

# put here the economic events and crisis for vertical lines
crisis <- c(#"2001-09-01",  # Crise do apagão: https://pt.wikipedia.org/wiki/Crise_do_apag%C3%A3o
           # "2003-01-01",  # Posse do Lula: https://pt.wikipedia.org/wiki/Luiz_In%C3%A1cio_Lula_da_Silva
            #"2005-06-01",  # Crise do mensalão: https://pt.wikipedia.org/wiki/Esc%C3%A2ndalo_do_mensal%C3%A3o
            #"2008-09-01",  # GFC
            "2013-06-01",  # Popular protests against the government  
            "2014-07-01",  # Economic crisis
            "2020-04-01"#,  # Corona crisis
            #"2022-03-01"
            )  # Ukraine invasion

crisis.mon <- data.frame(as.Date(crisis))
names(crisis.mon) <- "Date"

# Creates empty objects to store results imported from xlsx files
# dfErrors is a dataframe with observed y, the 4 forecasts and the 4 expectations
listErrors <- list()
dfErrors   <- data.frame(matrix(NA, nrow = 500, ncol = (1+length(mdays)+4)))
names(dfErrors) <- c("Yobs", "Yhat1", "Yhat2", "Yhat3", "Yhat4",
                     "Exp1", "Exp2", "Exp3", "Exp4")

# List of errors starts with dfErrors in each position
for (i in 1:(length(models))){
  listErrors[[i]] <- dfErrors
}
names(listErrors) <- models

setwd("..")
setwd("./Plots/data/results") 

# LASSO
listErrors[[1]] <-  xlsx::read.xlsx("results_LASSO.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9)
listErrors[[1]] <- listErrors[[1]][, -ncol(listErrors[[1]])]

# Ridge  
listErrors[[2]] <-  xlsx::read.xlsx("results_Ridge.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9) 
listErrors[[2]] <- listErrors[[2]][, -ncol(listErrors[[2]])]
  
# ElasticNet
listErrors[[3]] <-  xlsx::read.xlsx("results_ElasticNet.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9)  
listErrors[[3]] <- listErrors[[3]][, -ncol(listErrors[[3]])]

# sgLASSO
listErrors[[4]] <-  xlsx::read.xlsx("results_sgLASSO.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9) 
listErrors[[4]] <- listErrors[[4]][, -ncol(listErrors[[4]])]

# RF 
listErrors[[5]] <-  xlsx::read.xlsx("results_RF.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9) 
listErrors[[5]] <- listErrors[[5]][, -ncol(listErrors[[5]])]

# LLF
listErrors[[6]] <-  xlsx::read.xlsx("results_LLF.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9) 
listErrors[[6]] <- listErrors[[6]][, -ncol(listErrors[[6]])]

# BART
listErrors[[7]] <-  xlsx::read.xlsx("results_BART.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9) 
listErrors[[7]] <- listErrors[[7]][, -ncol(listErrors[[7]])]

# LASSOLLF 
listErrors[[8]] <-  xlsx::read.xlsx("results_LASSOLLF.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9)  
listErrors[[8]] <- listErrors[[8]][, -ncol(listErrors[[8]])]

# AR
#listErrors[[9]] <-  xlsx::read.xlsx("results_AR.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9)  
#listErrors[[9]] <- listErrors[[9]][, -ncol(listErrors[[9]])]

# RW
#listErrors[[10]] <-  xlsx::read.xlsx("results_RW.xlsx", sheetIndex = 1, skipEmptyRows = TRUE, colIndex = 1:9)  
#listErrors[[10]] <- listErrors[[10]][, -ncol(listErrors[[10]])]

#|=== Organize the losses from different models, per week

N <- nrow(listErrors[[1]])
nModels  <- length(models)+1
dfLoss   <- data.frame(matrix(NA, ncol = nModels, nrow = N))
names(dfLoss) <- c("Focus", models)
listLoss <- list()

for(i in 1:length(mdays)){
  listLoss[[i]] <- dfLoss
  for (j in 2:nModels){
   listLoss[[i]][,j] <- (listErrors[[(j-1)]][,(1+i)]-listErrors[[(j-1)]][,1])^2
  }
  listLoss[[i]][,1] <- (listErrors[[1]][,(5+i)]-listErrors[[1]][,1])^2
}

#|=== Compute the fluctuation test
#| https://search.r-project.org/CRAN/refmans/murphydiagram/html/fluctuation.html

#p <- 120 # Out of sample size
#m <- 118 # Rolling window size
#mu <- floor(m/p*10)/10 # Parameter for the Fluctuation test - 0.9 is shitty
mu <- 0.1 # Forcibly
nDates <- nrow(listLoss[[1]]) # To add actual dates to the labels, you need to inform the number of periods here

# Use this to see how the fluctuation test works for comparing focus to RW
# fluct_test_results <- fluctuation_test(listLoss[[1]][,1], listLoss[[1]][,2], 
#                                       mu = 0.1, lag_truncate = 5, conf_level = 0.1)

# For each model and for each week, compute the fluctuation test against FOCUS
dfFlucTest <- data.frame(matrix(NA, ncol = 4, nrow = 1))
dfFlucTestModel <- data.frame(matrix(NA, ncol = 4, nrow = nDates-13))
dfFlucTestModel[,1] <- 1:(nDates-13)
names(dfFlucTest) <- c("time", "dmstat", "day", "model")
names(dfFlucTestModel) <- c("time", "dmstat", "day", "model")
namesModels <- c("Focus", models)
fluct_test <- NA

for (i in 1:length(mdays)){
  for (j in 2:nModels){
    dfFlucTestModel[,2] <- fluctuation_test(listLoss[[i]][,1], listLoss[[i]][,j], 
                                        mu = mu, lag_truncate = 5, conf_level = 0.1)$df[,2] # Acho que o intervalo de confiança está muito amplo no gráfico
    dfFlucTestModel[,3] <- mdays[i]
    dfFlucTestModel[,4] <- namesModels[j]
    dfFlucTest <- rbind(dfFlucTest, dfFlucTestModel)
  }
  fluct_test <- fluctuation_test(listLoss[[i]][,1], listLoss[[i]][,j], 
                                 mu = mu, conf_level = 0.1, lag_truncate = 5)
}

dfFlucTest <- dfFlucTest[-1,]
# OOS forecast exercise starts in Jan 2013
dfFlucTest$Date <- NA
dfFlucTest$Date <- rep(seq(as.Date("2014-2-1"), length.out = nDates-13, by = "months"), 32)

# Labeller for graphs
DayLabellernames <- as_labeller(c(`8` = "Day 8", 
                                      `15` = "Day 15",`22` = "Day 22", 
                                      `99` = "End of the month"))

# Make individual plots per day
p0 <- ggplot(dfFlucTest[which(dfFlucTest$day == 8),], 
             aes(x=Date, y = dmstat, group = model))+
  geom_line(size=0.5, aes(color=model, linetype=model))+
  geom_hline(yintercept=fluct_test$CV[1],linetype=2)+
  geom_hline(yintercept=fluct_test$CV[2],linetype=2)+
  scale_y_continuous(limits = c(-4.5, 4.5))+
  scale_x_date(date_breaks = '12 months', date_labels =  "%Y")+
  labs(x=element_blank(), y = "Relative performance", title = 'Day 8')+ 
  geom_dl(aes(label = model, color = model),
          method = list(dl.combine("last.polygons"), cex = 0.5, vjust=0.5), cex = 1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=25, hjust = 1, size = 8),
        plot.margin = margin(0.5,0.5,0.5,0.5, "cm"))+
  geom_vline(xintercept=crisis.mon[1,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[2,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[3,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[4,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[5,1], linetype=3, lwd = 0.5)+
  annotate(geom = "text",
           label = c("(a)", # Lula's first mandate
                     "(b)", # Great financial crisis  
                     "(c)", # "2013 protests in Brazil
                     "(d)", # "2014 Brazilian economic crisis",
                     "(d)"), # "Corona crisis",
           x = c(crisis.mon[1,1],crisis.mon[2,1], crisis.mon[3,1],
                 crisis.mon[4,1],crisis.mon[5,1]),
           y = rep(2.5,5), # Ajuste da posição vertical das legendas, se remover uma data, mude o número 4
           angle = 90, 
           vjust = 1, 
           size = 3) + # Tamanho da fonte da anotação
  guides(colour="none", linetype = "none")


# Make individual plots per day
p1 <- ggplot(dfFlucTest[which(dfFlucTest$day == 15),], 
             aes(x=Date, y = dmstat, group = model))+
  geom_line(size=0.5, aes(color=model, linetype=model))+
  geom_hline(yintercept=fluct_test$CV[1],linetype=2)+
  geom_hline(yintercept=fluct_test$CV[2],linetype=2)+
  scale_y_continuous(limits = c(-4.5, 4.5))+
  scale_x_date(date_breaks = '12 months', date_labels =  "%Y")+
  #scale_colour_grey()+
  labs(x=element_blank(), y = "Relative performance", title = 'Day 15')+ 
  geom_dl(aes(label = model, color = model),
          method = list(dl.combine("right.polygons"), cex = 0.5, vjust=0.5), cex = 1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=25, hjust = 1, size = 8))+
  geom_vline(xintercept=crisis.mon[1,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[2,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[3,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[4,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[5,1], linetype=3, lwd = 0.5)+
  annotate(geom = "text",
           label = c("(a)", # Lula's first mandate
                     "(b)", # Great financial crisis  
                     "(c)", # "2013 protests in Brazil
                     "(d)", # "2014 Brazilian economic crisis",
                     "(d)"), # "Corona crisis",
           x = c(crisis.mon[1,1],crisis.mon[2,1], crisis.mon[3,1],
                 crisis.mon[4,1],crisis.mon[5,1]),
           y = rep(2.5,5), # Ajuste da posição vertical das legendas, se remover uma data, mude o número 4
           angle = 90, 
           vjust = 1, 
           size = 3) + # Tamanho da fonte da anotação
  guides(colour="none", linetype = "none")

# Make individual plots per day
p2 <- ggplot(dfFlucTest[which(dfFlucTest$day == 22),], 
             aes(x=Date, y = dmstat, group = model))+
  geom_line(size=0.5, aes(color=model, linetype=model))+
  geom_hline(yintercept=fluct_test$CV[1],linetype=2)+
  geom_hline(yintercept=fluct_test$CV[2],linetype=2)+
  scale_y_continuous(limits = c(-4.5, 4.5))+
  scale_x_date(date_breaks = '12 months', date_labels =  "%Y")+
  #scale_colour_grey()+
  labs(x=element_blank(), y = "Relative performance", title = 'Day 22')+ 
  geom_dl(aes(label = model, color = model),
          method = list(dl.combine("right.polygons"), cex = 0.5, vjust=0.5), cex = 1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=25, hjust = 1, size = 8))+
  geom_vline(xintercept=crisis.mon[1,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[2,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[3,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[4,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[5,1], linetype=3, lwd = 0.5)+
  annotate(geom = "text",
           label = c("(a)", # Lula's first mandate
                     "(b)", # Great financial crisis  
                     "(c)", # "2013 protests in Brazil
                     "(d)", # "2014 Brazilian economic crisis",
                     "(d)"), # "Corona crisis",
           x = c(crisis.mon[1,1],crisis.mon[2,1], crisis.mon[3,1],
                 crisis.mon[4,1],crisis.mon[5,1]),
           y = rep(2.5,5), # Ajuste da posição vertical das legendas, se remover uma data, mude o número 4
           angle = 90, 
           vjust = 1, 
           size = 3) + # Tamanho da fonte da anotação
  guides(colour="none", linetype = "none")


# Make individual plots per day
p3 <- ggplot(dfFlucTest[which(dfFlucTest$day == 99),], 
             aes(x=Date, y = dmstat, group = model))+
  geom_line(size=0.5, aes(color=model, linetype=model))+
  geom_hline(yintercept=fluct_test$CV[1],linetype=2)+
  geom_hline(yintercept=fluct_test$CV[2],linetype=2)+
  scale_y_continuous(limits = c(-4.5, 4.5))+
  scale_x_date(date_breaks = '12 months', date_labels =  "%Y")+
  #scale_colour_grey()+
  labs(x=element_blank(), y = "Relative performance", title = 'End of the Month')+ 
  geom_dl(aes(label = model, color = model),
          method = list(dl.combine("right.polygons"), cex = 0.5, vjust=0.5), cex = 1)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle=25, hjust = 1, size = 8))+
  geom_vline(xintercept=crisis.mon[1,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[2,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[3,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[4,1], linetype=3, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[5,1], linetype=3, lwd = 0.5)+
  annotate(geom = "text",
           label = c("(a)", # Lula's first mandate
                     "(b)", # Great financial crisis  
                     "(c)", # "2013 protests in Brazil
                     "(d)", # "2014 Brazilian economic crisis",
                     "(d)"), # "Corona crisis",
           x = c(crisis.mon[1,1],crisis.mon[2,1], crisis.mon[3,1],
                 crisis.mon[4,1],crisis.mon[5,1]),
           y = rep(2.5,5), # Ajuste da posição vertical das legendas, se remover uma data, mude o número 4
           angle = 90, 
           vjust = 1, 
           size = 3) + # Tamanho da fonte da anotação
  guides(colour="none", linetype = "none")

setwd("..")
setwd("..")
setwd("./figures") 

pdf(file = paste0("FluctuationTest.pdf"),   # Location and filename
    width = 12, # The width of the plot in inches
    height = 8) # The height of the plot in inches
grid.arrange(p0, p1, p2, p3, ncol=1)
dev.off()

pdf(file = paste0("FluctuationTest0.pdf"),   # Location and filename
    width = 11, # The width of the plot in inches
    height = 4) # The height of the plot in inches
  p0
dev.off()

pdf(file = paste0("FluctuationTest1.pdf"),   # Location and filename
    width = 11, # The width of the plot in inches
    height = 4) # The height of the plot in inches
p1
dev.off()

pdf(file = paste0("FluctuationTest2.pdf"),   # Location and filename
    width = 11, # The width of the plot in inches
    height = 4) # The height of the plot in inches
p2
dev.off()

pdf(file = paste0("FluctuationTest3.pdf"),   # Location and filename
    width = 11, # The width of the plot in inches
    height = 4) # The height of the plot in inches
p3
dev.off()

