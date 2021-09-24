
##############################################################################################################
# Code: VAR/VECM Model
# Author: Bishu Giri
# Date: 04/08/2017
# Last Updated: NA

############################################### VAR model #####################################################

#VAR model is used when there some variables that are interdependent in each other. (for eg: beer_volume, beer_price, cider_price)
#In that case, variable is estimated using
# the lagged values of itself & lagged values of other variables. 

#Endogeneous variable: variables that are interdependent or having bi-directional relationship. 
#In VAR model all the endogeneous
#variables will have separate equation & estimated within the model.

#Exogeneous variable: variables that affect the endogeneous variables but not vice versa. 
#This variables won't be estimated within the model.


#Note: to forecast for the 2017-2019, train the model till last data points. Only exogeneous variables should have 
#independent forecast till 2019. All the endogeneous variables will be forecasted in this model.

###################### A.Install Required Package ##############################

if(!require(vars)){install.packages("vars")};library(vars)
if(!require(urca)){install.packages("urca")};library(urca)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(fpp)){install.packages("fpp")};library(fpp)
if(!require(tsDyn)){install.packages("tsDyn")};library(tsDyn)
if(!require(tcltk)){install.packages("tcltk")};library(tcltk)
if(!require(svDialogs)){install.packages("svDialogs")};library(svDialogs)
#if(!require(MSBVAR)){install.packages('MSBVAR')}:library(MSBVAR) #for bayesian VAR

#################### B. Importing the data ######################################

raw_data <- read.csv(file.choose(),header=T)
#raw_data <- read.delim('clipboard')

################### C. User Input #############################################
start_yr <- 2008      #start year of the data
start_mon <- 1          #start month of the data
end_yr <- 2016          #ending year of the data
end_mon <- 12           #ending month of the data 
train_end_yr <- 2015 #year till which data model needs to be trained
var_list<-colnames(raw_data)
#### NOTE: endogemnous and exogeneous variables should be different ###########
var_endo<-dlgList(var_list,multiple=T,title="Select Endogeneous variables")$res
var_exog<-dlgList(var_list,multiple=T,title="Select Exogeneous variables")$res

################### E. Data Preparation ##############################################################################

#Note: Date column should be named as Month

######################Run code from here:###############################################################################

#changing Month as Date variable
if(!class(raw_data$Month)=="Date"){
  raw_data$Month=as.Date(raw_data$Month, "%m/%d/%Y")}

raw_data$Year <- year(raw_data$Month)

################### D. Calculation for YOY ############################################################################

st <- end_yr-1

d1<- subset(raw_data, raw_data$Year==st)
ac1 <- sum(d1$Beer_Volume)

d2 <- subset(raw_data, raw_data$Year==end_yr)
ac2 <- sum(d2$Beer_Volume)

actual_YoY <- (ac2-ac1)*100/ac1 

######################################################################################################################
#subsetting the data for required period

#raw_data <- subset(raw_data, raw_data$Year < end_yr & raw_data$Year >= start_yr)

#Creating different dataset for endogeneous & exogeneous variable

#For Endogeneous variables
raw_data_endo <- raw_data[, var_endo]

#For Exogeneous variables
raw_data_exog<- raw_data[, var_exog]

#Cretaing time series data of variable list

#For Endogeneous variables
raw_data_endo <-ts(raw_data_endo,start=c(start_yr,start_mon),end=c(end_yr,end_mon),frequency = 12)

#For Exogeneous variables
raw_data_exog <-ts(raw_data_exog, start=c(start_yr,start_mon), end=c(end_yr,end_mon),frequency=12)

# Preparing training & test data

#For Endogeneous varibales
endo_train <- window(raw_data_endo, end=c(train_end_yr,end_mon))
endo_valid <- window(raw_data_endo, start=c(end_yr,start_mon),end=c(end_yr,end_mon))

#For Exogeneous varibales
exog_train <- window(raw_data_exog, end=c(train_end_yr,end_mon))
exog_valid <- window(raw_data_exog, start=c(end_yr,start_mon),end=c(end_yr,end_mon))

####################Data preparation code ends here ################################################################


#################### Vector Auto Regressive Model ##################################################################

# For VAR model all the variables has to be stationary. So first step would be to check for the stationarity of the variables.

#Run Augmented Dickey-Fuller for stationarity check   

#endogeneous variables

for (i in 1:ncol(endo_train)){
  assign(paste0("endo_adf_",i,sep="_"), ur.df(endo_train[, i],type=c('trend'),lags=2)  )
}

summary(endo_adf_1_) #similarly check for all the endogeneous variables.

#exogeneous variables

for (i in 1:ncol(exog_train)){
  assign(paste0("exog_adf_",i,sep="_"), ur.df(exog_train[, i],type=c('trend'),lags=2)  )
}

summary(exog_adf_1_)

#check the tau test statistics & compare with critical values to understand the  stationarity of the series.

#if all the series are stationary, move to estimation of VAR model. Else if all the series are
#I(1) then difference the series and use those series for estimation. But before differencing the series, we 
#have to check for cointegration among the variables. IF we found the co-integrated relationship among the 
#variables then use vecm model for estimation. So, move to VECM in this case.


#Note: Series is co-integrated means they have a long run equilibrum even though in short run they don't move together.


##############
#Lag optimisation
opt_var<-VARselect(endo_train, lag.max=10, type="const",season=12)$selection
opt_var

#lag optimisation step will give optimal number of lag (k), we use AIC criteria.

#estimation of var model

var.fit <- VAR(endo_train, p = opt_var[[1]], type = "const", exogen = exog_train)
summary(var.fit) #check estimates for all the equation.

#forecast using var model
pred_var <- predict(var.fit, n.ahead =12, dumvar = as.matrix(exog_valid))


#checking yearly accuracy
f1 <- sum(pred_var$fcst[[1]][,"fcst"])
error_var<-abs((ac2-f1))*100/ac2
error_var

#YOY

forecasted_YoY_var <- (f1-ac1)*100/ac1
forecasted_YoY_var

################## Vector Error Correction Method #########################################################

#Lag optimisation
opt_var<-VARselect(endo_train, lag.max=10, type="const",season=12)$selection
opt_var


#Checking the data for cointegration and if it is co-integrated then skip below steps & go for VECM
jo_eigen <- ca.jo(data.frame(endo_train), type = "eigen", ecdet = "none", K = opt_var[[1]], dumvar= data.frame(exog_train))
jo_trace <- ca.jo(data.frame(endo_train), type = "trace", ecdet = "none", K = opt_var[[1]], dumvar= data.frame(exog_train))
summary(jo_eigen)
summary(jo_trace)

#select the number for r where the null hypothesis is not rejected.

#test 10pct  5pct  1pct
#r <= 2 |  2.06  6.50  8.18 11.65
#r <= 1 | 14.25 15.66 17.95 23.52
#r = 0  | 38.49 28.71 31.52 37.22

#In the above case, select r =1, where the test statistic is less than critical values.


#VECM with lags set according to results of lag optimisation & using eigen.
vecm <- cajorls(jo_eigen,r=1)
vecm
summary(vecm$rlm)
vecm$rlm$coefficients[1,1]
var.form <- vec2var(jo_eigen) 
summary(var.form)
pred_vecm <- predict(var.form,n.ahead = 12, dumvar=as.matrix(exog_valid))

#ect: is the error correction term, used to see how fast the series come back to equilibrum
#if it diverts from the equilibrum condition.
# in equation ect has to be significant.

#checking yearly accuracy
f2 <- sum(pred_vecm$fcst[[1]][,"fcst"])
error_vecm<-abs((ac2-f2))*100/ac2
error_vecm 

#YOY

forecasted_YoY_vecm <- (f2-ac1)*100/ac1
forecasted_YoY_vecm

#Final output

#1. accuracy & direction
output_var <- cbind(ac2,actual_YoY,f1,forecasted_YoY_var,error_var)
output_vecm <- cbind(ac2, actual_YoY,f2, forecasted_YoY_vecm, error_vecm)

output<- rbind(output_var,output_vecm)
View(output)
class(output)

colnames(output) <- c("Actual_Beer_Volume", "Actual YoY", "Forecasted_Beer_Volume", "Forecasted_YoY","Error")
row.names(output) <- c("VAR","VECM")
View(output)

#2. coefficients

coeff_var<- summary(var.fit, equation="Beer_Volume")[[2]]
var_coeff <- data.frame(coeff_var[[1]]$coefficients)
var_coeff$var<-rownames(var_coeff)
rownames(var_coeff)<-NULL
class(var_coeff)
colnames(var_coeff)

coeff_vecm <- summary(vecm$rlm)[[1]] 
vecm_coeff<-data.frame(coeff_vecm$coefficients)
vecm_coeff$var<-rownames(vecm_coeff)
rownames(vecm_coeff)<-NULL
class(vecm_coeff)
colnames(vecm_coeff)


write.csv(output,"accuracy.csv")
write.csv(var_coeff,"model_results_VAR.csv")
write.csv(vecm_coeff,"model_results_VECM.csv")
