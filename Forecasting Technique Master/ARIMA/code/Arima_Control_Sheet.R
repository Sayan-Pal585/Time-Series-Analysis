##-----------------------------HW CONTROL SHEET-------------------------------##
##   Author : Ali Umar Qureshi                                                ##  
##   Date : 11-01-2018                                                        ##
##   Purpose: The Code is used to build Logistic Regression Models            ##    
##   All Rights Reserved by TheMathCompany Pvt Ltd.                           ##
##                                                                            ##
##   Change Log:                                                              ##
##                                                                            ##
##                                                                            ##          
##                                                                            ##
##                                                                            ##
##----------------------------------------------------------------------------##

##---------------------------Initializations----------------------------------##
#Setting the time environmet to GMT standards
Sys.setenv(TZ='GMT')

## Clearing up the workspace
rm(list = ls())


##---------------------------End of Initializations---------------------------##

##---------------------------Setting up the workspace-------------------------##
#Set the working directory-This is the path where the dataset is placed

home_path  <<- "/Users/aliqureshi/Desktop/TheMathCompany/KM/Arima_Module"
setwd(home_path)

# #Path for the codes-Path where the codes are present
# codes_path<-"/Users/aliqureshi/Desktop/TheMathCompany/KM/ARIMA_Module/Code"


#Path for the final output to be placed
# out_path<-"/Users/aliqureshi/Desktop/TheMathCompany/KM/ARIMA_Module/Output"
out_path <<- paste(home_path, "Output", sep = "/")

##---------------------------End of Setting up the workspace------------------##

##---------------------------Function Initialization--------------------------##
#Setting the working directory to be codes path
# setwd(codes_path)
# 
# Intialize all functions


#Intialize all functions
source(paste(home_path, "Code/Arima_Functions.R", sep = "/"))

## Function to install all required packages at one shot
load.function()

##---------------------------End of Function Initialization-------------------##

##---------------------------Reading the dataset------------------------------##

#Choose the file you want to read
#this should be the forecasting dataset

data <- read.csv(paste(home_path, "Input/data.csv", sep = "/"), 
                 stringsAsFactors = T)

# data <- read.csv("/Users/aliqureshi/Desktop/TheMathCompany/KM/ARIMA_Module/Input/data.csv")

#Check if all the columns and rows are imported correctly
#Dimensions
dim(data)

#Structure
str(data)
##---------------------------End of Reading the dataset-----------------------##


# -----------------------Standardization---------------------------------##

# Enter the date column
date_column <- 'DATE_SK'
# Enter the forcast variable column
forecast_variable_column <- 'VOLUME..KHL.'
# Function for standardizing column names
colnames(data)[colnames(data) == date_column] <- 'Date'
colnames(data)[colnames(data) == forecast_variable_column] <- 'forecast_variable'

# -----------------------End of Standardization--------------------------##


##---------------------------Data Checks -----------------------##

# Fucntion that checks for any missing values in the data
data_check(data)

# Arranage the data by date and group
data <- data %>% arrange(Date)

## Function for creating time series 
data_ts <- create_timeseries(data)

#Function for Decomposing to look at the seasonal, trend and irregular components and storing it as PDF in output folder
Decomposing_the_timeseries(data_ts)

##---------------------------End of Data Checks -----------------------##

##------------------------------ Train & Test Creation--------------------------##
myvals <- inputs()
# Create_train_and_test(data_ts,myvals,freq)

# Define Frequency below based on monthly or quarterly runs
freq <- 12


return_data <- Create_train_and_test(data_ts,myvals,freq)

# training_data <-  ts(data_ts[1:trn.period], frequency = freq)
# test_data <- ts(data_ts[(trn.period + 1):(trn.period + tst.period)], frequency = freq)

training_data <- return_data[[1]]
test_data <- return_data[[2]]

##------------------------- End of Train & Test Creation-----------------------##

##------------------------ Auto ARIMA model building-----------------##



forecast.duration <- 12
in.err.skip.dur <- 1
error_threshold <- 0.2
trn.period <- myvals[1]
tst.period <- myvals[2]


arima.model.results (training_data, 
                  test_data, 
                  trn.period, 
                  tst.period, 
                  freq, 
                  forecast.duration, 
                  start_date,
                  error_threshold,
                  in.err.skip.dur)



##------------------------ End of Auto ARIMA model building-----------------##


##------------------------ ARIMA model building to get all P,D,Q Possibilities-----------------##
# Post deciding the p,d,q, P,D,Q based on the ACF and PACF plots. Make adjustments in the code below. 
# Multiple iterations can be run 
# Set limits below
p_limit <- 2
q_limit <- 2
d_limit <- 1

arima_results <- arima_function(training_data,test_data)

# Arima function
##--------------------- End of ARIMA model building-----------------##


##--------------------- Model Diagnostics for any ARIMA Model-----------------##
# Use the decided p,d,q and P,D,Q and analyze model diagnostics
arimafit <- arima(data_ts, order = c(1,0,0),seasonal = c(1,0,0), xreg = NULL)
# Forecasting for the next months of 2016
forecast <- predict(arimafit, n.ahead = 6)$pred

summary(arimafit)
ts.diag(arimafit)


# Concatenating the values fitted and predicted by the model
fittedval <- ts(c(fitted(arimafit),forecast), frequency = 12)
plot(as.numeric(data_ts), col = "blue", type="l", xaxt="n")
# axis(1, c(0,12,24,36,48,60,72), 2011:2017)
lines(as.numeric(fittedval), col = "red")
##------------------ End of Model Diagnostics for ARIMA Model-------------##

# Saving workspace
save.image (paste0(out_path,"/arima.RData"))
