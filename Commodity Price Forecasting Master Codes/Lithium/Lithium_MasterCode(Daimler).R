############################################################################
####################------DAIMLER------#####################################
############################################################################
# CODE: Lithium Master R-Code
# COMMODITY: LITHIUM HYDROXIDE MONOHYDRATE 56.5%min Delivered CHina RMB/mt
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
#str(lithium)
#summary(lithium$Price)

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

#############################################################################
##                     CODE for Change Point Analysis HERE                 ## 
#############################################################################
#--------Exploratory Data Analysis------#
#--------Change Point Analysis----------#
library(changepoint)
li_cpt <- cpt.meanvar(lits, penalty = "AIC", method = "PELT",shape=4,minseglen=5,Q=5)
cpts.ts(li_cpt)
#plot.ts(lits, col = "blue", main = "Lithium Hydroxide Monohydrate 56.5%min Delivered China RMB/mt", ylab= "Lithium Price")
plot(li_cpt,col="blue", main="Change Point Analysis \nLithium Hydroxide Monohydrate 56.5%min delivered China RMB/mt",lwd=1.8,ylab= "Lithium Price")
legend("topright", lty=1, col = c("blue",2),legend = c("Actual","ChangePoint"))

########################################################
##                     CODE ENDS HERE                 ## 
########################################################

######################################################################
##                     CODE for Univariate TSA HERE                 ## 
######################################################################

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
############################################################################################################################
##--------------Simple forecasting methods-----------##
##for Horizon=11M (Similarly for Horizon=7M & 3M change the dataset in each algorithm)
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

##########################################################################################################################
##ARIMA Time Series Analysis:-

#--------ARIMA for train & test(horizon=11M)--------#
plot.ts(lithiumtrn11ts, main = "Lithium Hydroxide Monohydrate 56.5%min Delivered China RMB/mt")

#Augmented Dickey fuller test
adf.test(lithiumtrn11ts)
#Augmented Dickey-Fuller Test
#hypothesis: H0: the series is non-stationary vs H1:the series is stationary.
#critical value considered is 0.05
#data:  lits
#Dickey-Fuller = -2.7989, Lag order = 3, p-value = 0.2605
#alternative hypothesis: stationary
#here p-value= 0.2605 > 0.05. Hence, we reject H1.
#hence we can conlclude that the lithium time series is stationary series. we need no differencing/transformation to proceed with 
#classical time seris model ARIMA

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(lithiumtrn11ts, null = "Level", lshort = TRUE)
#KPSS Test for Level Stationarity
#hypothesis: H0: the series is stationary vs H1:the series is non-stationary.
#data:  lits
#KPSS Level = 0.14372, Truncation lag parameter = 3, p-value = 0.1
#here p-value= 0.1 > 0.05. Hence, we reject H1.
#hence we can conlclude that the lithium time series is stationary series. we need no differencing/transformation to proceed with 
#classical time seris model ARIMA

#---------time series decomposition--------#
decomp <- decompose(lithiumtrn11ts)
plot(decomp)
stl(lithiumtrn11ts,s.window = "periodic")
plot(stl(lithiumtrn11ts,s.window = "periodic"))

#-----auto-correlation of original Time series------#
acf(lithiumtrn11ts)
pacf(lithiumtrn11ts)

ndiffs(lithiumtrn11ts,test = "adf", type = "level") 
d1 <- diff(lithiumtrn11ts,1)
plot(d1)
adf.test(d1)

nsdiffs(lithiumtrn11ts)

#------Arima modeling--------#
li11mod <- auto.arima(lithiumtrn11ts, stationary = TRUE, stepwise = FALSE, trace = TRUE, ic= "aic", method = "CSS-ML", seasonal.test = "ocsb", allowdrift = TRUE, allowmean = TRUE) 
summary(li11mod)

a11 <- arima(lithiumtrn11ts,order = c(1,0,2))
summary(a11)

#------Forecast(ARIMA)-----#
f11 <- forecast(a11, h=24, level = c(80,85,90,95,99))
plot(f11)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#------Forecast(Auto.ARIMA)-----#
fauto11 <- forecast(li11mod, h=24, level = c(80,85,90,95,99))
plot(fauto11)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#---------test data mape--------#

#forecasted11 <- c(144644.1998,153516.0422,160122.3897,163269.0689,162595.1364,158576.179,152351.067,145424.0769,139316.0312,135242.4114)
#testmape11 <- mape(lithiumtst11$lithium.Price, forecasted11)

#------Residual test----------#
Box.test(li11mod$residuals, type = "Ljung-Box")
Box.test(a11$residuals, type = "Ljung-Box")
#Box-Ljung test
#H0: The data are independently distributed (i.e. the correlations in the population from which the sample is taken are 0, so that any observed correlations in the data result from randomness of the sampling process).
#Ha: The data are not independently distributed; they exhibit serial correlation.
#data:  limod$residuals
#X-squared = 0.28718, df = 1, p-value = 0.592
#here p-value= 0.592 > 0.05. Hence, we reject Ha.

##Graphical check for autocorrelation in residuals
acf(li11mod$residuals) 
acf(a11$residuals)

#--------ARIMA for train & test(horizon=7M)--------#
plot.ts(lithiumtrn7ts, main = "Lithium Hydroxide Monohydrate 56.5%min Delivered China RMB/mt")

#Augmented Dickey fuller test
adf.test(lithiumtrn7ts)
#Augmented Dickey-Fuller Test
#hypothesis: H0: the series is non-stationary vs H1:the series is stationary.
#critical value considered is 0.05
#data:  lits
#Dickey-Fuller = -2.865, Lag order = 3, p-value = 0.232
#alternative hypothesis: stationary
#here p-value= 0.232 > 0.05. Hence, we reject H1.
#hence we can conlclude that the lithium time series is non-stationary series. we need differencing/transformation to proceed with 
#classical time seris model ARIMA

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(lithiumtrn7ts, null = "Level", lshort = TRUE)
#KPSS Test for Level Stationarity
#hypothesis: H0: the series is stationary vs H1:the series is non-stationary.
#data:  lits
#KPSS Level = 0.25923, Truncation lag parameter = 3, p-value = 0.1
#here p-value= 0.1 > 0.05. Hence, we reject H1.
#hence we can conlclude that the lithium time series is stationary series. we need no differencing/transformation to proceed with 
#classical time seris model ARIMA

#---------time series decomposition--------#
decomp <- decompose(lithiumtrn7ts)
plot(decomp)
stl(lithiumtrn7ts,s.window = "periodic")
plot(stl(lithiumtrn7ts,s.window = "periodic"))

#-----auto-correlation of original Time series------#
acf(lithiumtrn7ts)
pacf(lithiumtrn7ts)

ndiffs(lithiumtrn7ts,test = "adf", type = "level") 
d1 <- diff(lithiumtrn7ts,1)
plot(d1)
adf.test(d1)

acf(d1)
pacf(d1)

nsdiffs(lithiumtrn7ts)

#------Arima modeling--------#
li7mod <- auto.arima(lithiumtrn7ts, stepwise = FALSE, trace = TRUE, ic= "aic", method = "CSS-ML", seasonal.test = "ocsb",stationary = FALSE, allowdrift = TRUE, allowmean = TRUE) 
summary(li7mod)

a7 <- arima(lithiumtrn7ts,order = c(0,1,2), method = "CSS-ML")
summary(a7)

#------Forecast(ARIMA)-----#
f7 <- forecast(a7, h=12, level = c(99,95,90,85,80))
plot(f7)
lines(lithiumtst7ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#------Forecast(Auto.ARIMA)-----#
fauto7 <- forecast(li7mod, h=12, level = c(99,95,90,85,80))
plot(fauto7)
lines(lithiumtst7ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#---------test data mape--------#
#forecasted7 <- c(97026.79751,98676.33158,98676.33158,98676.33158,98676.33158,98676.33158)
#testmape7 <- mape(lithiumtst7$lithium.Price, forecasted7)

#------Residual test----------#
Box.test(li7mod$residuals, type = "Ljung-Box")
Box.test(a7$residuals, type = "Ljung-Box")
#Box-Ljung test
#H0: The data are independently distributed (i.e. the correlations in the population from which the sample is taken are 0, so that any observed correlations in the data result from randomness of the sampling process).
#Ha: The data are not independently distributed; they exhibit serial correlation.
#data:  limod$residuals
#X-squared = 0.072322, df = 1, p-value = 0.788
#here p-value= 0.788 > 0.05. Hence, we reject Ha.

##Graphical check for autocorrelation in residuals
acf(li7mod$residuals)
acf(a7$residuals)

#--------ARIMA for train & test(horizon=4M)--------#
plot.ts(lithiumtrn4ts, main = "Lithium Hydroxide Monohydrate 56.5%min Delivered China RMB/mt")

#Augmented Dickey fuller test
adf.test(lithiumtrn4ts)
#Augmented Dickey-Fuller Test
#hypothesis: H0: the series is non-stationary vs H1:the series is stationary.
#critical value considered is 0.05
#data:  lits
#Dickey-Fuller = -2.4057, Lag order = 3, p-value = 0.4126
#alternative hypothesis: stationary
#here p-value= 0.4126 > 0.05. Hence, we reject H1.
#hence we can conlclude that the lithium time series is non-stationary series. we need differencing/transformation to proceed with 
#classical time seris model ARIMA

#Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test
kpss.test(lithiumtrn4ts, null = "Level", lshort = TRUE)
#KPSS Test for Level Stationarity
#hypothesis: H0: the series is stationary vs H1:the series is non-stationary.
#data:  lits
#KPSS Level = 0.39995, Truncation lag parameter = 3, p-value = 0.07718
#here p-value= 0.07718 > 0.05. Hence, we reject H1.
#hence we can conlclude that the lithium time series is stationary series. we need no differencing/transformation to proceed with 
#classical time seris model ARIMA

#---------time series decomposition--------#
decomp <- decompose(lithiumtrn4ts)
plot(decomp)
stl(lithiumtrn4ts,s.window = "periodic")
plot(stl(lithiumtrn4ts,s.window = "periodic"))

#-----auto-correlation of original Time series------#
acf(lithiumtrn4ts)
pacf(lithiumtrn4ts)

ndiffs(lithiumtrn4ts)          ##detects p,d,q for ARIMA modeling
d <- diff(lithiumtrn4ts,1)
plot(d)
adf.test(d)

acf(d)
pacf(d)

nsdiffs(lithiumtrn4ts)

#------Arima modeling--------#
li4mod <- auto.arima(lithiumtrn4ts, stepwise = FALSE, trace = TRUE, ic= "aic", method = "CSS-ML", seasonal.test = "ocsb",stationary = FALSE, allowdrift = TRUE, allowmean = TRUE) 
summary(li4mod)

a4 <- arima(lithiumtrn4ts,order = c(2,1,0), method = "CSS-ML")
summary(a4)

#af3 <- arfima(litrn3ts, drange = c(0,0.5), estim = "mle")
#summary(af3)

#------Forecast(Auto.ARIMA)-----#
fauto4 <- forecast(li4mod, h=12, level = c(80,85,90,95,99))
plot(fauto4)
lines(lithiumtst4ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#------Forecast-----#
f4 <- forecast(a4, h=12, level = c(80,85,90,95,99))
plot(f4)
lines(lithiumtst4ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#---------test data mape--------#
#forecasted4 <- c(75489.27627, 73107.95558,72234.19899)

#testmape4 <- mape(lithiumtst4$lithium.Price, forecasted4)

#------Residual test----------#
Box.test(li4mod$residuals, type = "Ljung-Box")
Box.test(a4$residuals, type = "Ljung-Box")
#Box-Ljung test
#H0: The data are independently distributed (i.e. the correlations in the population from which the sample is taken are 0, so that any observed correlations in the data result from randomness of the sampling process).
#Ha: The data are not independently distributed; they exhibit serial correlation.
#data:  limod$residuals
#X-squared = 0.43718, df = 1, p-value = 0.5085
#here p-value= 0.5085 > 0.05. Hence, we reject Ha.

##Graphical check for autocorrelation in residuals
acf(li3mod$residuals)
acf(a4$residuals)

#----------LOG transformation forcast horizon=3----------#
#log(litrn3ts)
#plot(log(litrn3ts))
#acf(log(litrn3ts))
#pacf(log(litrn3ts))
#acf(diff(log(litrn3ts),1))
#pacf(diff(log(litrn3ts),1))

#------Arima modeling--------#
#li3modlog <- auto.arima(log(litrn3ts), stepwise = FALSE, trace = TRUE, ic= "aic", method = "CSS-ML", seasonal.test = "ocsb",stationary = FALSE, allowdrift = TRUE, allowmean = TRUE) 
#summary(li3modlog)

#a3log <- arima(log(litrn3ts),order = c(0,2,2), method = "CSS-ML")
#summary(a3log)

#------Forecast-----#
#f3 <- forecast(li3modlog, h=12, level = 99)
#plot(f3)
#lines(log(litest3ts), col="red", lwd= 2)
#legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

#f3 <- forecast(a3log, h=12, level = 99)
#plot(f3)
#lines(log(litest3ts), col="red", lwd= 2)
#legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))

##-----------Single Exponential Smoothing-----------##
fcast11 <- ses(lithiumtrn11ts, h=24, initial = "optimal")
plot(fcast11)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fcast11)

#forecasted11 <- c(108072.2,108072.2,108072.2,108072.2,108072.2,108072.2,108072.2,108072.2,108072.2,108072.2)
#testmape11 <- mape(lithiumtst11$lithium.Price, forecasted11) ##0.2906326


fcast7 <- ses(lithiumtrn7ts, h=12, initial = "optimal")
plot(fcast7)
lines(lithiumtst7ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fcast7)

#forecasted7 <- c(94500.12,94500.12,94500.12,94500.12,94500.12,94500.12)
#testmape7 <- mape(lithiumtst7$lithium.Price, forecasted6) ##0.245479

fcast4 <- ses(lithiumtrn4ts, h=12, initial = "optimal")
plot(fcast4)
lines(lithiumtst4ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fcast4)

#forecasted4 <- c(80109.65,80109.65,80109.65)
#testmape4 <- mape(lithiumtst4$lithium.Price, forecasted3) ##0.1763504

##Write the results in csv format in excel
write.csv(fcast11,'fcast11.csv')
write.csv(fcast7,'fcast7.csv')
write.csv(fcast4,'fcast4.csv')

# Plot some forecasts horizon=11M
autoplot(lithiumtrn11ts) +
  autolayer(forecast(a11, h=24),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn11ts, h=24, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(lithiumtst11ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 11Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts horizon=7M
autoplot(lithiumtrn7ts) +
  autolayer(forecast(a7, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn7ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(lithiumtst7ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 7Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts horizon=4M
autoplot(lithiumtrn4ts) +
  autolayer(forecast(a4, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn7ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(lithiumtst4ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 4Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

##----------Holt's Method (Second order Exponential Smoothing)----------##
fholt11 <- holt(lithiumtrn11ts, h=24, initial = "optimal", PI=FALSE)
plot(fholt11)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fholt11)

#forecasted11 <- c(96143.3767,84214.9265,72286.4764,60358.0263,48429.5761,36501.1260,24572.6759,12644.2258, 715.7756,-11212.6745 )
#testmape11 <- mape(lithiumtst11$lithium.Price, forecasted11) ##0.5597188

fholt7 <- holt(lithiumtrn7ts, h=24, initial = "optimal", PI=FALSE)
plot(fholt7)
lines(lithiumtst7ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fholt7)

#forecasted7 <- c(93308.27,92117.12,90925.96,89734.81,88543.65,87352.50)
#testmape7 <- mape(lithiumtst7$lithium.Price, forecasted7) ##0.1871378

fholt4 <- holt(lithiumtrn4ts, h=24, initial = "optimal",PI=FALSE)
plot(fholt4)
lines(lithiumtst4ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fholt4)

#forecasted4 <- c(73639.332,67169.456,60699.581)
#testmape4 <- mape(lithiumtst4$lithium.Price, forecasted4) ##0.02240142

##Write the results in csv format in excel
write.csv(fholt11,'fholt11.csv')
write.csv(fholt7,'fholt7.csv')
write.csv(fholt4,'fholt4.csv')

# Plot some forecasts horizon=11
autoplot(lithiumtrn11ts) +
  autolayer(forecast(a11, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn11ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(holt(lithiumtrn11ts, h=12, initial = "optimal"),
            series="Holt's Method", PI=FALSE) +
  autolayer(lithiumtst11ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 11Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts horizon=7
autoplot(lithiumtrn7ts) +
  autolayer(forecast(a7, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn7ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(holt(lithiumtrn7ts, h=12, initial = "optimal"),
            series="Holt's Method", PI=FALSE) +
  autolayer(lithiumtst7ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 7Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts horizon=3
autoplot(lithiumtrn4ts) +
  autolayer(forecast(a4, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn4ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(holt(lithiumtrn4ts, h=12, initial = "optimal"),
            series="Holt's Method", PI=FALSE) +
  autolayer(lithiumtst4ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 4Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

##------------Holt Winter Method (3rd order exponential smoothing)------------##
fhw11 <- hw(lithiumtrn11ts, h=24, seasonal = "additive", damped = TRUE,initial = "optimal")
plot(fhw11)
lines(lithiumtst11ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fhw11)

#forecasted11 <- c(101975.38,103260.38,110554.78,119006.33,114775.90,110336.82,105527.22,103017.54,102660.95,94118.12)
#testmape11 <- mape(lithiumtst11$lithium.Price, forecasted11) ##0.2691603

fhw7 <- hw(lithiumtrn7ts, h=12, seasonal = "additive", damped = TRUE,initial = "optimal")
plot(fhw7)
lines(lithiumtst7ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fhw7)

forecasted7 <- c(96501.79,88873.66,83827.71,77069.06,77949.05,70725.86)
testmape7 <- mape(lithiumtst7$lithium.Price, forecasted7) ##0.07369822


fhw4 <- hw(lithiumtrn4ts, h=12, seasonal = "additive", damped = TRUE,initial = "optimal")
plot(fhw4)
lines(lithiumtst4ts, col="red", lwd= 2)
legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
summary(fhw4)

#forecasted4 <- c(74106.98,70281.88,68938.63)
#testmape4 <- mape(lithiumtst4$lithium.Price, forecasted4) ##0.04244025

##Write the results in csv format in excel
write.csv(fhw11,'fhw11.csv')
write.csv(fhw7,'fhw7.csv')
write.csv(fhw4,'fhw4.csv')

# Plot some forecasts horizon=11M
autoplot(lithiumtrn11ts) +
  autolayer(forecast(a11, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn11ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(holt(lithiumtrn11ts, h=12, initial = "optimal"),
            series="Holt's Method", PI=FALSE) +
  autolayer(hw(lithiumtrn11ts, h=24, seasonal = "additive", damped = TRUE),
            series ="Holt Winter's Method", PI=FALSE)+
  autolayer(lithiumtst11ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 11Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts horizon=7M
autoplot(lithiumtrn7ts) +
  autolayer(forecast(a7, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn7ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(holt(lithiumtrn7ts, h=12, initial = "optimal"),
            series="Holt's Method", PI=FALSE) +
  autolayer(hw(lithiumtrn7ts, h=24, seasonal = "additive", damped = TRUE),
            series ="Holt Winter's Method", PI=FALSE)+
  autolayer(lithiumtst7ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 7Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

# Plot some forecasts horizon=4M
autoplot(lithiumtrn4ts) +
  autolayer(forecast(a4, h=12),
            series="ARIMA", PI=FALSE) +
  autolayer(ses(lithiumtrn4ts, h=12, initial = "optimal"),
            series="SES", PI=FALSE) +
  autolayer(holt(lithiumtrn4ts, h=12, initial = "optimal"),
            series="Holt's Method", PI=FALSE) +
  autolayer(hw(lithiumtrn4ts, h=24, seasonal = "additive", damped = TRUE),
            series ="Holt Winter's Method", PI=FALSE)+
  autolayer(lithiumtst4ts,
            series="Test Period")+
  ggtitle("Forecasts for monthly Lithium price(Test: 4Month)") +
  xlab("Years") + ylab("Lithium Price") +
  guides(colour=guide_legend(title="Forecast"))

##------------Univariate Unobserved Component Model(UCM)------------##
#--------Loading data file----------#
lithiumlg <- read.csv(file.choose(),header=TRUE) ##Enter log data for lithium hydroxide
View(lithiumlg)
str(lithiumlg)

#--------convert the date column-----#
date <- as.Date(lithiumlg$Date, format = "%d-%m-%Y")
#Price <- lithium$Price/100

#--------Create data frame with formatted date------#
lithiumlg1 <- data.frame(date, lithiumlg$Price)
View(lithiumlg1)
str(lithiumlg1)

#--------Train & Test data Creation 11M------# 
lilgtrn <-lithiumlg1[-c(40:50),] ##Can vary the month count for test period
lilgtst <-lithiumlg1[-c(1:39),]    

#--------Convert dataset into time series data------#
lilgtrnts <- ts(lilgtrn$lithiumlg.Price, start = c(2015,10), frequency = 12)
lilgtstts <- ts(lilgtst$lithiumlg.Price, start = c(2019,01), frequency = 12)

#--------Model Build---------#
fitucm <- ucm(lilgtrnts~0,data = lilgtrnts,irregular = T,slope = T
              ,level=T
              ,season =F,season.length = 12, irregular.var = 0.00004, level.var = 0.00005,slope.var = 0.000002, season.var = 0.000001)
##, irregular.var = 0.00004, level.var = 0.00005,slope.var = 0.000002, season.var = 0.000001

Fitted_ucm<-fitted.values(fitucm$model) 
Fitted_ucm   ##fitted value
fore <- predict(fitucm$model,n.ahead = 24)
#fu9 <- fore*100 ##forecasted value
fu9 <- 10^fore
fu9
##Write the results in csv format in excel
write.csv(fu9,'fu9.csv')

##------------Univariate Unobserved Component Model(UCM)------------##
#--------Loading data file----------#
lithiumhlg <- read.csv(file.choose(),header=TRUE) ##Enter log data for lithium hybrid
View(lithiumhlg)
str(lithiumhlg)

#--------convert the date column-----#
date <- as.Date(lithiumhlg$Date, format = "%d-%m-%Y")
#Price <- lithium$Price/100

#--------Create data frame with formatted date------#
lithiumhlg1 <- data.frame(date, lithiumhlg$Price)
View(lithiumhlg1)
str(lithiumhlg1)

#--------Train & Test data Creation 11M------# 
lihlgtrn <-lithiumhlg1[-c(91:101),] ##Can vary the month count for test period
lihlgtst <-lithiumhlg1[-c(1:90),]    

#--------Convert dataset into time series data------#
lihlgtrnts <- ts(lihlgtrn$Price, start = c(2011,07), frequency = 12)
lihlgtstts <- ts(lihlgtst$Price, start = c(2019,01), frequency = 12)

#--------Model Build---------#
fitucm2 <- ucm(lihlgtrnts~0,data = lihlgtrnts,irregular = T,slope = T
               ,level=T
               ,season =F,season.length = 12)

Fitted_ucm<-fitted.values(fitucm2$model) 
Fitted_ucm   ##fitted value
fore2 <- predict(fitucm2$model,n.ahead = 24)
#fu9 <- fore*100 ##forecasted value
fuh9 <- 10^fore2
fuh9
##Write the results in csv format in excel
write.csv(fuh9,'fuh9.csv')
########################################################
##              Top 3 Selected Models                 ## 
########################################################
###TOP 3 MODELS
##R1.Lithium Carbonate UCM
##R2.Lithium Carbonate HW
##R3.Lithium Hybrid HW

##------------R2.Univariate Unobserved Component Model(UCM)------------##
#--------Loading data file----------#
lithiumclg <- read.csv(file.choose(),header=TRUE) ##Enter log data for lithium carbonate
View(lithiumclg)
str(lithiumclg)

#--------convert the date column-----#
date <- as.Date(lithiumclg$Date, format = "%d-%m-%Y")
#Price <- lithium$Price/100

#--------Create data frame with formatted date------#
lithiumclg1 <- data.frame(date, lithiumclg$Price)
View(lithiumclg1)
str(lithiumclg1)

#--------Train & Test data Creation 11M------# 
liclgtrn <-lithiumclg1[-c(91:101),] ##Can vary the month count for test period
liclgtst <-lithiumclg1[-c(1:90),]    

#--------Convert dataset into time series data------#
liclgtrnts <- ts(liclgtrn$lithiumclg.Price, start = c(2011,07), frequency = 12)
liclgtstts <- ts(liclgtst$lithiumclg.Price, start = c(2019,01), frequency = 12)

#--------Model Build---------#
fitucm1 <- ucm(liclgtrnts~0,data = liclgtrnts,irregular = T,slope = T
              ,level=T
              ,season =F,season.length = 12)
##, irregular.var = 0.00004, level.var = 0.00005,slope.var = 0.000002, season.var = 0.000001

Fitted_ucm<-fitted.values(fitucm1$model) 
Fitted_ucm   ##fitted value
fore1 <- predict(fitucm1$model,n.ahead = 24)
#fu9 <- fore*100 ##forecasted value
fuc9 <- 10^fore1
fuc9
##Write the results in csv format in excel
write.csv(fuc9,'fuc9.csv')

#Forecast for 24M from Dec19

liclgtrn <-lithiumclg1
#--------Convert dataset into time series data------#
liclgtrnts <- ts(liclgtrn$lithiumclg.Price, start = c(2011,07), frequency = 12)

#--------Model Build---------#
fitucmff <- ucm(liclgtrnts~0,data = liclgtrnts,irregular = T,slope = T
               ,level=T
               ,season =F,season.length = 12)
##, irregular.var = 0.00004, level.var = 0.00005,slope.var = 0.000002, season.var = 0.000001

Fitted_ucm<-fitted.values(fitucmff$model) 
Fitted_ucm   ##fitted value
foreff <- predict(fitucmff$model,n.ahead = 24)
#fu9 <- fore*100 ##forecasted value
fuc24 <- 10^foreff
fuc24
##Write the results in csv format in excel
write.csv(fuc24,'fuc24.csv')

##------------R3.Holt Winter Method (3rd order exponential smoothing) for Lithium hybrid data------------##
#--------convert the date column-----#
date <- as.Date(lithiumh$Date, format = "%d-%m-%Y")

#--------Create data frame with formatted date------#
lithiumh1 <- data.frame(date, lithiumh$Price)
View(lithiumh1)
str(lithiumh1)

#--------Train & Test data Creation 11M------# 
litrn11hw <-lithiumh1[-c(91:101),]
litst11hw <-lithiumh1[-c(1:90),]
#--------Convert dataset into time series data------#
litrn11hwts <- ts(litrn11hw$lithiumh.Price, start = c(2011,07), frequency = 12)
litst11hwts <- ts(litst11hw$lithiumh.Price, start = c(2019,01), frequency = 12)

fhwh11 <- hw(litrn11hwts, h=24, seasonal = "additive", damped = TRUE,initial = "optimal")
#plot(fhwh11)
#lines(litst11hwts, col="red", lwd= 2)
#legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
#summary(fhwh11)
#write.csv(fhwh11,'fhwh11.csv')
fhwh11

#Forecast for 24M from Dec19

litrnfhw <- lithiumh1
#--------Convert dataset into time series data------#
litrnfhwts <- ts(litrnfhw$lithiumh.Price, start = c(2011,07), frequency = 12)

fhwhf <- hw(litrnfhwts, h=24, seasonal = "additive", damped = TRUE,initial = "optimal")
#plot(fhwhf)
#lines(litst11hwts, col="red", lwd= 2)
#legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
#summary(fhwhf)
#write.csv(fhwhf,'fhwhf.csv')
fhwhf

##------------R1.Holt Winter Method (3rd order exponential smoothing) for Lithium carbonate data------------##
#--------convert the date column-----#
date <- as.Date(lithiumc$ï..Date, format = "%d-%m-%Y")

#--------Create data frame with formatted date------#
lithiumc1 <- data.frame(date, lithiumc$Price)
View(lithiumc1)
str(lithiumc1)

#--------Train & Test data Creation 11M------# 
lictrn11hw <-lithiumc1[-c(91:101),]
lictst11hw <-lithiumc1[-c(1:90),]
#--------Convert dataset into time series data------#
lictrn11hwts <- ts(lictrn11hw$lithiumc.Price, start = c(2011,07), frequency = 12)
lictst11hwts <- ts(lictst11hw$lithiumc.Price, start = c(2019,01), frequency = 12)

fhwc11 <- hw(lictrn11hwts, h=24, seasonal = "additive", damped = TRUE,initial = "optimal")
#plot(fhwc11)
#lines(lictst11hwts, col="red", lwd= 2)
#legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
#summary(fhwc11)
#write.csv(fhwc11,'fhwc11.csv')
fhwc11

#Forecast for 24M from Dec19

litrnfhwc <- lithiumc1
#--------Convert dataset into time series data------#
litrnfhwcts <- ts(litrnfhwc$lithiumc.Price, start = c(2011,07), frequency = 12)

fhwcf <- hw(litrnfhwcts, h=24, seasonal = "additive", damped = TRUE,initial = "optimal")
#plot(fhwcf)
#lines(litst11hwts, col="red", lwd= 2)
#legend("topright", lty=1, col = c(2,4),legend = c("Actual","Predicted"))
#summary(fhwhf)
#write.csv(fhwhf,'fhwhf.csv')
fhwcf
########################################################
##                     CODE ENDS HERE                 ## 
########################################################