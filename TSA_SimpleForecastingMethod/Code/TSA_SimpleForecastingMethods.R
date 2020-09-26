############################################################################
####################-Simple forecasting methods-############################
############################################################################
# AUTHOR: Sayan Pal
############################################################################
############################################################################

############################### used libraries #############################
if(!require(reshape2)){install.packages("reshape2")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tibble)){install.packages("tibble")}
if(!require(changepoint)){install.packages("changepoint")}
if(!require(MASS)){install.packages("MASS")}
if(!require(forecast)){install.packages("forecast")}
if(!require(tseries)){install.packages("tseries")}
if(!require(Metrics)){install.packages("Metrics")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(rucm)){install.packages("rucm")}

#--------Setup Work Directory---------#
setwd("File Directory") ##Set file directory
getwd()

#--------Loading data file----------#
lithium <- read.csv(file.choose(),header=TRUE)        ##Lithium Hydroxide data input
lithiumc <- read.csv(file.choose(),header = TRUE)     ##Lithium Carbonate data input
lithiumh <- read.csv(file.choose(),header = TRUE)     ##Lithium Hybrid data input

View(lithium)
str(lithium)
summary(lithium$Price)

#--------Data Preparation(lithium)--------#
##Follow same suit for Lithiumc & Lithiumh##
#--------convert the date column-----#
date <- as.Date(lithium$Date, format = "%d-%m-%Y")

#--------Create data frame with formatted date------#
lithium1 <- data.frame(date, lithium$Price)
View(lithium1)
str(lithium1)

#--------Convert dataset into time series data------#
lits <- ts(lithium1$lithium.Price, start = c(2015,10), frequency = 12)
plot.ts(lits, col = "blue", main = "Lithium Hydroxide Monohydrate 56.5%min Delivered China RMB/mt")

#######################################################
# Code Univariate TSA Train & Test Data Set Creation  #        
#######################################################

#--------Train Data & test data(horizon=11M)-----------#
lithiumtrn11 <- lithium1[-c(40:50),]
View(lithiumtrn11)
str(lithiumtrn11)
lithiumtrn11ts <- ts(lithiumtrn11$lithium.Price, start = c(2015,10), frequency = 12)
lithiumtrn11ts

lithiumtst11 <- lithium1[-c(1:39),]
View(lithiumtst11)
str(lithiumtst11)
lithiumtst11ts <- ts(lithiumtst11$lithium.Price, start = c(2019,01), frequency = 12)
lithiumtst11ts

#--------Train Data & test data(horizon7M)-----------#
lithiumtrn7 <- lithium1[-c(44:50),]
View(lithiumtrn7)
str(lithiumtrn7)
lithiumtrn7ts <- ts(lithiumtrn7$lithium.Price, start = c(2015,10), frequency = 12)
lithiumtrn7ts

lithiumtst7 <- lithium1[-c(1:43),]
View(lithiumtst7)
str(lithiumtst7)
lithiumtst7ts <- ts(lithiumtst7$lithium.Price, start = c(2019,05), frequency = 12)
lithiumtst7ts

#--------Train Data & test data(horizon=4M)-----------#
lithiumtrn4 <- lithium1[-c(47:50),]
View(lithiumtrn4)
str(lithiumtrn4)
lithiumtrn4ts <- ts(lithiumtrn4$lithium.Price, start = c(2015,10), frequency = 12)
lithiumtrn4ts

lithiumtst4 <- lithium1[-c(1:46),]
View(lithiumtst4)
str(lithiumtst4)
lithiumtst4ts <- ts(lithiumtst4$lithium.Price, start = c(2019,08), frequency = 12)
lithiumtst4ts

###-------------------Univariate Time Series Analysis------------------------###
##Loading Required Library
library(forecast)
library(tseries)
library(rucm)
library(Metrics)
library(ggplot2)
#######################################################
##--------------Simple forecasting methods-----------##
#######################################################
#Average method(mean forecast)
m0 <- meanf(lithiumtrn11ts, h=20)
plot(m0)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

summary(m0)

#Naive method
m1 <- naive(lithiumtrn11ts,h=12)
plot(m1)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

summary(m1)

#Seasonal Naive method
m2 <- snaive(lithiumtrn11ts,h=12)
plot(m2)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

summary(m2)

#Drift method
m3 <- rwf(lithiumtrn11ts,h=12, drift = TRUE)
plot(m3)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

summary(m3)

# Plot some forecasts
autoplot(lithiumtrn11ts) +
  autolayer(meanf(lithiumtrn11ts, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(lithiumtrn11ts, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(lithiumtrn11ts, h=11),
            series="Seasonal naïve", PI=FALSE) +
  autolayer(lithiumtst11ts,
            series="Test Period") +
  ggtitle("Forecasts for monthly Lithium price(Test : 11M)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

########################################################
##                     CODE ENDS HERE                 ## 
########################################################