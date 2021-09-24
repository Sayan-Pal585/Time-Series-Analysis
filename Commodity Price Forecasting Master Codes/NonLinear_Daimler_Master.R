############################################################################
####################------DAIMLER------#####################################
############################################################################
# CODE: Non Linear TSA
# COMMODITY: Kobalt & Rubber
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

##################################################################################################
###################                  Lithium Data                            #####################
##################################################################################################
lithium <- read.csv(file.choose(),header = TRUE)
View(lithium)

#--------convert the date column-----#
month <- as.Date(lithium$Date, format = "%d-%m-%Y")

#--------Create data frame with formatted date------#
lithium1 <- data.frame(month,lithium$Price )
View(lithium1)
str(lithium1)

#--------Convert dataset into time series data------#
lits <- ts(lithium1$lithium.Price, start = c(2011,07), frequency = 12)
plot.ts(lits, col = "blue", main = "Lithium")

library(tsDyn) #non linear time series models with regime switching

##model selection
selectSETAR(lits, m=3, mL=1:3, mH=1:3, thSteps = 5, thDelay=0:2)

par(mfrow=c(2,1), mar=c(2,4,0,0))
acf(ruts)
pacf(ruts)

library(tseriesChaos)
mutual(ruts)

recurr(ruts, m=3, d=1, levels=c(0,0.2,1))

lag.plot(ruts, lags=3, layout=c(1,3))

delta.test(ruts)

delta.lin.test(ruts)

##model selection

mod2.ar <- linear(lits, m=1,d=1)
mod2.ar
summary(mod2.ar)

mod2.setar <- setar(lits, m=3, mL=1, mH=1, thDelay=2)
mod2.setar
summary(mod2.setar)
plot(mod2.setar)

modl.aar <- aar(lits,m=2,d=1)
modl <- list()
modl[["linear"]] <- linear(lits, m=1,d=3,type = "level")
modl[["setar"]] <- setar(lits, m=3, mL=1, mH=1, thDelay=2)
modl[["lstar"]] <- lstar(lits, m=3, mL=1, mH=1, thDelay=2)
modl[["nnetTs"]] <- nnetTs(lits, m=2, size=3)
modl[["aar"]] <- aar(lits, m=2,d=3)

sapply(modl, AIC)
sapply(modl, MAPE)

summary(modl[["setar"]])

plot(modl[["setar"]])
li.new <- predict(modl[["setar"]], n.ahead=24)
li1.new <- predict(modl[["linear"]], n.ahead=24)
li2.new <- predict(modl[["lstar"]], n.ahead=24)
li3.new <- predict(modl[["nnetTs"]], n.ahead=24)
li4.new <- predict(modl[["aar"]], n.ahead=24)


########################################################
##                     CODE ENDS HERE                 ## 
########################################################