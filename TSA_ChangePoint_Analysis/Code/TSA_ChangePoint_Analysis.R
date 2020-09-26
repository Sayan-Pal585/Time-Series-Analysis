############################################################################
####################-Change Point Analysis-#################################
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