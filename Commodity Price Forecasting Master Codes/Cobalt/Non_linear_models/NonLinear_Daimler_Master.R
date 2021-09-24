############################################################################
####################------DAIMLER------#####################################
############################################################################
# CODE: Non Linear TSA
# COMMODITY: Kobalt 
# AUTHOR: Sayan Pal
############################################################################
############################################################################

############################### used libraries #############################
if(!require(tsDyn)){install.packages("tsDyn")}
library(tsDyn)      #non linear time series models with regime switching

##################################################################################################
###################                  R3.Kobalt Data                         ######################
##################################################################################################
kobalt <- read.csv(file.choose(),header = TRUE)
View(kobalt)

#--------convert the date column-----#
date <- as.Date(kobalt$Date, format = "%d-%m-%Y")

#--------Create data frame with formatted date------#
kobalt1 <- data.frame(date, kobalt$Cobalt.Price.USD.lb.)
View(kobalt1)
str(kobalt1)

#--------Convert dataset into time series data------#
kots <- ts(kobalt1$kobalt.Cobalt.Price.USD.lb., start = c(2010,05), frequency = 12)
plot.ts(kots, col = "blue", main = "Kobalt")

##model selection
selectSETAR(kots, m=3, mL=1:3, mH=1:3, thSteps = 5, thDelay=0:2)

par(mfrow=c(2,1), mar=c(2,4,0,0))
acf(kots)
pacf(kots)

mod <- list()
mod[["linear"]] <- linear(kots, m=2,d=3)
mod[["setar"]] <- setar(kots, m=2, mL=1, mH=1, thDelay=0)
mod[["lstar"]] <- lstar(kots, m=2, mL=1, mH=1, thDelay=0)  ##R3.Recommended Model

sapply(mod, AIC)
sapply(mod, MAPE)

##24 months forecast
ko.new <- predict(mod[["setar"]], n.ahead=24)
ko1.new <- predict(mod[["linear"]], n.ahead=24)
ko2.new <- predict(mod[["lstar"]], n.ahead=24)

########################################################
##                     CODE ENDS HERE                 ## 
########################################################