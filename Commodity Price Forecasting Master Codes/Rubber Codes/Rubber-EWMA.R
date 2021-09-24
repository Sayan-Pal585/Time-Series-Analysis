############################################################################
####################------DAIMLER------#####################################
############################################################################
# CODE: Exponential Weighted Moving Average
# COMMODITY: Rubber
# AUTHOR: Sayan Pal
############################################################################
############################################################################

############################### used libraries #############################
if(!require(pracma)){install.packages("pracma")}
if(!require(forecast)){install.packages("forecast")}
library(pracma)      
library(forecast)
##################################################################################################
###################                  R3.Rubber Data                         ######################
##################################################################################################
rubber <- read.csv(file.choose(),header = TRUE)
View(rubber)

date <- as.Date(rubber$Month, format = "%d-%m-%Y")

rubber1 <- data.frame(date, rubber$Rubber.price)
View(rubber1)
str(rubber1)

rubber.ts <- ts(rubber1$rubber.Rubber.price, start = c(2007,01),frequency = 12)
str(rubber.ts)

ru.ema <- movavg(rubber.ts,n=24,type = "e")
ru <- forecast(ru.ema,h=24)

########################################################
##                     CODE ENDS HERE                 ## 
########################################################