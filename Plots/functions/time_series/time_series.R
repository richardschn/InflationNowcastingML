#|===  Directory setup
rm(list=ls())                             # Clean working environment 
path = dirname(rstudioapi::getSourceEditorContext()$path) # Path is directory of this file
setwd(path)                              # Set working directory
source('data2YoY.r')                     # Load function that applies the necessary MoM transformations

#|===  Installing and loading packages
chooseCRANmirror(graphics = FALSE, ind = 0)
if(!require("pacman")) install.packages("pacman")
pacman::p_load(openxlsx, lubridate, dplyr,
               xlsx, ggplot2, zoo, reshape2, anytime, extrafont)

# put here the economic events and crisis for vertical lines
crisis <- c(#"2001-09-01",  # Crise do apagão: https://pt.wikipedia.org/wiki/Crise_do_apag%C3%A3o
  "2003-01-01",  # Posse do Lula: https://pt.wikipedia.org/wiki/Luiz_In%C3%A1cio_Lula_da_Silva
  #"2005-06-01",  # Crise do mensalão: https://pt.wikipedia.org/wiki/Esc%C3%A2ndalo_do_mensal%C3%A3o
  "2008-09-01",  # GFC
  "2013-06-01",  # Popular protests against the government  
  "2014-07-01",  # Economic crisis
  "2020-04-01"#,  # Corona crisis
  #"2022-03-01"
)  # Ukraine invasion

crisis.mon <- data.frame(as.Date(crisis))
names(crisis.mon) <- "Date"


##############################################
#
# Graph for all indexes together
#
##############################################

# Load all the inflation indexes
# Source: https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries

dfData01 <- data.frame(read.csv(paste0(path,"/Inflation_various_indexes.csv"), skip =1 , sep = ";", dec = "."))
names(dfData01) <- c("DATA", "IGP-M", "IPC-S", "FIPE", "IPCA", "IGP-10", "IPA-M",
                     "INCC-M", "IPCA-15")
dfData01 <- dfData01[-nrow(dfData01),]
dfData01[,1] <- parse_date_time(dfData01[,1], "%y-%m-%d")

# Do you want YoY, then flag should be == 1, otherwise MoM
yoyFlag <- 1

# Converts %MoM to %YoY
if (yoyFlag == 1){
  for (i in 2:ncol(dfData01)){
    dfData01[-(1:12),i] <- round(data2YoY(as.numeric(dfData01[,i]), 12, 1),4)  
    
  }
  # Removes first 12 observations
  dfData01 <- dfData01[-(1:12),]
}

# Organize format for the plot
dfData02 <- melt(dfData01, id = "DATA")
names(dfData02) <- c("DATE", "Variable", "Value")


p1 <- ggplot(dfData02) +
  geom_line(aes(x = as.Date(DATE, format = "%Y"), y = as.numeric(Value), color = Variable), 
            data = dfData02[-which(dfData02$Variable == "IPCA"),], alpha = 1, lwd = 0.9) + # IPCA goes separately for color
  geom_line(aes(x = as.Date(DATE, format = "%Y"), y = as.numeric(Value)), 
            data = dfData02[which(dfData02$Variable == "IPCA"),], alpha = 1, lwd = 1.2, color = 'black')+
  scale_x_date(date_labels = "%Y", date_breaks = "24 months", expand = c(0, 0))+ # You can try to adjust the data format here 
  ylim(-12, 51)+
  labs(y = "Year-on-Year (%)", x= "", color = "") + # Series is the title of legend
  theme(axis.text.x = element_text(angle=0, hjust = 1, size = 10),
        axis.text.y = element_text(size = 10),
        legend.position = "bottom", legend.box = "vertical",
        legend.justification = "center", legend.box.just = "right",
        legend.margin = ggplot2::margin(t = -0.5, r = 0, b = 0, l = 0, unit = "cm"),
        legend.key.size = unit(0.8, "cm"), legend.spacing.x = unit(0.2, "cm"),
        legend.text = element_text(size = 10),
        text = element_text(family="LM Roman 10", size=10)) + # Controls x axis size and positioning
  geom_vline(xintercept=crisis.mon[1,1], linetype=2, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[2,1], linetype=2, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[4,1], linetype=2, lwd = 0.5)+
  geom_vline(xintercept=crisis.mon[5,1], linetype=2, lwd = 0.5)+
  annotate(geom = "text",
           label = c("Lula's First Mandate",
                     "Great Financial Crisis", 
                     "Economic Domestic Crisis", 
                     "COVID-19 Crisis"),
           x = c(crisis.mon[1,1]-180,crisis.mon[2,1]-180,
                 crisis.mon[4,1]-180,crisis.mon[5,1]-180),
           y = c(rep(35,4)), # Ajuste da posição vertical das legendas, se remover uma data, mude o número 8
           angle = 90, 
           vjust = 1, 
           size = 3.5) +
  guides(color=guide_legend(nrow=1,byrow=TRUE))

plot(p1)