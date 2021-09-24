############################################################################
####################------DAIMLER------#####################################
############################################################################
# CODE: Non Linear TSA
# COMMODITY: Rubber
# AUTHOR: Sayan Pal
############################################################################
############################################################################

############################### used libraries #############################
if(!require(tsDyn)){install.packages("tsDyn")}
library(tsDyn)      #non linear time series models with regime switching


##################################################################################################
###################                  R3.Rubber Data                         ######################
##################################################################################################
rubber <- read.csv(file.choose(),header = TRUE)
View(rubber)

#--------convert the date column-----#
month <- as.Date(rubber$Month, format = "%d-%m-%Y")

#--------Create data frame with formatted date------#
rubber1 <- data.frame(month,rubber$Rubber.price )
View(rubber1)
str(rubber1)

#--------Convert dataset into time series data------#
ruts <- ts(rubber1$rubber.Rubber.price, start = c(2007,01), frequency = 12)
plot.ts(ruts, col = "blue", main = "Rubber")

##model selection
selectSETAR(ruts, m=3, mL=1:3, mH=1:3, thSteps = 5, thDelay=0:2)

par(mfrow=c(2,1), mar=c(2,4,0,0))
acf(ruts)
pacf(ruts)

##model selection

mod1 <- list()
mod1[["linear"]] <- linear(ruts, m=1,d=3,type = "level")
mod1[["setar"]] <- setar(ruts, m=3, mL=1, mH=1, thDelay=2)
mod1[["aar"]] <- aar(ruts, m=2, d=3)

sapply(mod1, AIC)
sapply(mod1, MAPE)

##24 months forecast
ru.new <- predict(mod1[["setar"]], n.ahead=24)
ru1.new <- predict(mod1[["linear"]], n.ahead=24)
ru2.new <- predict(mod1[["aar"]], n.ahead=24)

########################################################
##                     CODE ENDS HERE                 ## 
########################################################