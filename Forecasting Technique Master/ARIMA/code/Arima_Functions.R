##-------------------------------ARIMA FUNCTIONS------------------------------##
##   Author : Ali Umar Qureshi                                                ##  
##   Date : 15-01-2018                                                        ##
##   Purpose: The code is used to define all the functions used in the        ##
##            STL module                                                      ##    
##   All Rights Reserved by TheMathCompany Pvt Ltd.                           ##
##                                                                            ##
##   Change Log:                                                              ##
##                                                                            ##
##                                                                            ##          
##                                                                            ##
##                                                                            ##
##----------------------------------------------------------------------------##

##------------------------1.INSTALLING REQUIRED PACKAGES----------------------##
##               Installing packages from cran and devtools                   ##
##                                                                            ##
##                                                                            ##
##----------------------------------------------------------------------------##

## Function to install all required packages at one shot
load.function <- function(){
  Sys.setenv(TZ='GMT')
  ## List all packages##
  packagelist <- c("lubridate","plyr","dplyr","forecast","reshape","reshape2","scales","rucm","zoo",
                   "stringi","e1071","tseries", "tsoutliers", "ArgumentCheck", "forecastHybrid", "car", "fmsb", 
                   "MASS", "QuantPsyc", "tibble", "opera", "aTSA", "plotly", "Hmisc", "dplyr", "knitr", "tm", "svDialogs", "tcltk","tcltk2")
  
  ## ---------- List all packages that need to be installed and will install them ---------- ##
  newpackages <- packagelist[!(packagelist %in% installed.packages()[,"Package"])]
  if(length(newpackages)) install.packages(newpackages, dependencies = T, repos = "http://cran.us.r-project.org")
  
  if ("forecastxgb" %in% installed.packages()[,"Package"] == F) {
    if ("devtools" %in% installed.packages()[,"Package"] == F)
      install.packages("devtools")
    
    library(devtools)
    devtools::install_github("ellisp/forecastxgb-r-package/pkg")
  }
  
  packagelist <- c(packagelist, "forecastxgb")
  
  ## ---------- List checklist of loaded packages ---------- ##
  check.lib <- data.frame(lapply(packagelist, require, character.only = T))
  colnames(check.lib) <- packagelist
  return(check.lib)
}


##----------------------------------------------------------------------------##
##---------------------------- Standardize Column ----------------------------##
##               Function to calculate VIF                                    ##
##               Inputs:                                                      ##
##                 1.df- Forcasting Dataset                                   ##
##                                                                            ##
##               Outputs:                                                     ##
##                 1.  Returns forecasting dataset with standardized          ##
##                      column names                                          ## 
##                                                                            ##
##----------------------------------------------------------------------------##
# standardize_col <- function(data) {
#   colnames(data)[colnames(data) == date_column] <- 'Date'
#   colnames(data)[colnames(data) == forecast_variable_column] <- 'forecast_variable'
#   return(data)
# }


##----------------------------------------------------------------------------##
##----------------------------2.MISSING VALUE CHECK---------------------------##
##               Function to calculate VIF                                    ##
##               Inputs:                                                      ##
##                 1.df- Forcasting Dataset                                   ##
##                                                                            ##
##               Outputs:                                                     ##
##                 1.  Returns error                                          ##
##                                                                            ## 
##                                                                            ##
##----------------------------------------------------------------------------##

# Check for the input data having any missing values
data_check <- function(data)
{
  #* Establish a new 'ArgCheck' object
  Check <- ArgumentCheck::newArgCheck()
  
  ## Checking for missing iput variables
  if (missing(data))
    ArgumentCheck::addError(
      msg = "Invalid input.",
      argcheck = Check
    )
  
  # Return errors and warnings (if any)
  ArgumentCheck::finishArgCheck(Check)
}

##----------------------------------------------------------------------------##
##------------------------3.CREATING TIME SERIES------------------------------##
##               Function to create timeseries                                ##
##               Inputs:                                                      ##
##                 1.df- Forecasting Dataset                                  ##
##                 2. Change the name of the date & forecast column           ##
##                                                                            ##
##                                                                            ##
##               Outputs:                                                     ##
##                  1.Timeseries data, volume ts                              ## 
##----------------------------------------------------------------------------##

## Function for creating time series 
create_timeseries <- function(data) {
  
  data$Date <- dmy(as.character(data$Date))
  data <- data %>% arrange(Date)
  # data$forecast_variable <- data$VOLUME..KHL.
  # data$Date <- data$Date
  
  ## timeseries analysis
  start_date <- min(data$Date)
  start_year <- year(start_date)
  start_month <- month(start_date)
  
  end_date <- max(data$Date)
  end_year <- year(end_date)
  end_month <- month(end_date)
  
  #Converting it into a time series
  ts_sample <- ts(data$forecast_variable,start = c(start_year, start_month),end = c(end_year, end_month),frequency = 12)
}

##------------------------4.Decomposition-------------------------------------##
##               Function to create training and test datasets                ##
##               Inputs:                                                      ##
##                 1.Timeseries data                                          ##
##                                                                            ##
##               Outputs:                                                     ##
##                  1.Decomposition plots                                     ##
##                                                                            ##
##----------------------------------------------------------------------------##
#Decomposing to look at the seasonal, trend and irregular components

Decomposing_the_timeseries <- function(data_ts){
  decomp_stl <- stl(data_ts, s.window = "periodic")
  decomp_add <- decompose(data_ts, type = "additive")
  decomp_mult <- decompose(data_ts, type = "multiplicative")
  pdf(paste(out_path,"decompostion.pdf",sep="/"))
  plot(decomp_stl)
  plot(decomp_add)
  plot(decomp_mult)
  dev.off()
}

# Decomposing_the_timeseries(data_ts)

##------------------------6.TRAIN AND TEST DATASET----------------------------##
##               Function to create training and test datasets                ##
##               Inputs:                                                      ##
##                 1.Training and test period to be enterted in the pop up    ##
##                                                                            ##
##               Outputs:                                                     ##
##                  1.train-Training dataset returned to global environment   ##
##                  2.test-Testing dataset returned to global environment     ## 
##----------------------------------------------------------------------------##

inputs <- function(){
  
  library(tcltk2)
  library(tcltk)
  xvar <- tclVar("")
  yvar <- tclVar("")
  
  tt <- tktoplevel()
  tkwm.title(tt,"Input Numbers")
  x.entry <- tkentry(tt, textvariable=xvar)
  y.entry <- tkentry(tt, textvariable=yvar)
  
  reset <- function()
  {
    tclvalue(xvar)<-""
    tclvalue(yvar)<-""
  }
  
  reset.but <- tkbutton(tt, text="Reset", command=reset)
  
  submit <- function() {
    x <- as.numeric(tclvalue(xvar))
    y <- as.numeric(tclvalue(yvar))
    e <- parent.env(environment())
    e$x <- x
    e$y <- y
    tkdestroy(tt)
  }
  submit.but <- tkbutton(tt, text="submit", command=submit)
  
  tkgrid(tklabel(tt,text="Enter Two Inputs"),columnspan=2)
  tkgrid(tklabel(tt,text="Training Period"), x.entry, pady = 10, padx =10)
  tkgrid(tklabel(tt,text="Test Period"), y.entry, pady = 10, padx =10)
  tkgrid(submit.but, reset.but)
  
  tkwait.window(tt)
  return(c(x,y))
}


Create_train_and_test <- function(data_ts,myvals,freq){
  # print (min(data$month))
  # print (max(data$month))
  return_data <- list()
  trn.period <- myvals[1]
  tst.period <- myvals[2]
  return_data$training_data <-  ts(data_ts[1:trn.period], frequency = freq)
  return_data$test_data <- ts(data_ts[(trn.period + 1):(trn.period + tst.period)], frequency = freq)
  return(return_data)
}

# a <- ts(data_ts[1:trn.period], frequency = freq)
##-----------------------------------Auto ARIMA ------------------------------##
##               Function to create training and test datasets                ##
##               Inputs:                                                      ##
##                 1.    Time series data                                     ##                           
##                                                                            ##
##               Outputs:                                                     ##
##                  1.Fitted series                                           ##
##                  2.Forecasted series                                       ##
##                  3.In Sample MAPE                                          ##
##                  4.Out Sample MAPE                                         ##
##----------------------------------------------------------------------------##

arima.model.results <- function(training_data, 
                                test_data, 
                                trn.period, 
                                tst.period, 
                                freq, 
                                forecast.duration, 
                                start_date, 
                                error_threshold,
                                in.err.skip.dur){
  ###############
  #             #
  #    ARIMA    #
  #             #
  ###############
  
  # Build arima Model
  arima.model <- auto.arima(training_data, lambda=0)
  
  # Forecast Values
  arima.fitted.pred <- arima.model$fitted
  arima.pred <- predict(arima.model, h = (forecast.duration + tst.period))
  arima.forecast.series <- c(arima.fitted.pred, arima.pred$mean)
  
  # Build the series (insample, outsampl, forecast)
  if (tst.period != 0) {
    arima.series <- data.frame(cbind(arima.forecast.series,
                                     c(training_data[1:trn.period],
                                       test_data,
                                       rep(NA, forecast.duration))))
  } else {
    arima.series <- data.frame(cbind(arima.forecast.series,
                                     c(training_data[1:trn.period],
                                       rep(NA, forecast.duration))))
  }
  
  colnames(arima.series) <- c("PREDICTED","ACTUAL")
  
  arima.series$ERROR <- arima.series$PREDICTED - arima.series$ACTUAL
  arima.series$APE <- abs(arima.series$ERROR)/arima.series$ACTUAL
  
  arima.insample <- mean(arima.series[((freq * in.err.skip.dur) + 1):trn.period, ]$APE, na.rm = T)
  
  if (tst.period != 0) {
    arima.outsample <- mean(arima.series[trn.period + 1: (trn.period + tst.period), ]$APE, na.rm = T)
  } else {
    arima.outsample <- 0
  }
  
  arima.overall <- mean(arima.series[1:(trn.period + tst.period), ]$APE, na.rm = T)
  
  arima.MAPE <- c(rep(NA, 3))
  if (abs(arima.insample - arima.outsample) < error_threshold)
    arima.MAPE <- c(arima.insample,
                    arima.outsample,
                    arima.overall)
  arima.MAPE <- as.data.frame(arima.MAPE)
  row.names(arima.MAPE) <- c("InSample","OutSample","OverallMape")
  
  
  print("Freq, Skip duration, trn.period, tst.period, forecast duration")
  print(freq)
  print(in.err.skip.dur)
  print(trn.period)
  print(tst.period)
  print(forecast.duration)
  
  write.csv(arima.MAPE, paste(out_path, "ARIMA MAPE.csv",sep="/"))
  write.csv(arima.series, paste(out_path, "ARIMA Series.csv",sep="/"))
  
  pdf(paste(out_path, "- ARIMA Actual vs Predicted.pdf",sep="/"))
  plot(arima.series$PREDICTED, col = "red", main = "ARIMA - Actual (blue) vs Predicted (red)", type = "l", xaxt = "n")
  lines(arima.series$ACTUAL, col = "blue", type = "l")
  dev.off()
  
  return(list(arima.series, arima.MAPE))
}

##-----------------------All possible scenarios of ARIMA----------------------##
##               Function to create training and test datasets                ##
##               Inputs:                                                      ##
##                 1.    ##
##                                                                            ##
##               Outputs:                                                     ##
##                  1.train-Training dataset returned to global environment   ##
##                  2.test-Testing dataset returned to global environment     ## 
##----------------------------------------------------------------------------##

arima_function <- function(training_data,test_data){
  j=0
  p <- 0
  dif <- 0
  q <- 0
  for (p in 0:p_limit){
    for(q in 0:q_limit){
      for(dif in 0:d_limit){
        
        fit <- arima(training_data, order = c(p,dif,q), xreg = NULL)
        
        # forecast_fit<-fitted(arimafit)
        # MAPE_In <- mean(abs((forecast_fit - training_data)/training_data))*100
        
        d_i <- (training_data) - fit$residuals
        res <- training_data - (d_i)
        d <- sum(abs(res/training_data))
        MAPE_In <- (100 * d)/25
        
        
        if(j==0)
        {
          pred<-predict(fit,n.ahead=length(test_data))
          accu<-accuracy(ts((pred$pred)),ts(test_data))
          out<-rbind(list=c(arimaorder(fit),MAPE_In,accu[5],fit$aic))
          arima_output<-data.frame(out)
          
          j=j+1
        }
        else
        {
          pred<-predict(fit,n.ahead=length(test_data))
          accu<-accuracy(ts((pred$pred)),ts(test_data))
          out<-rbind(list=c(arimaorder(fit),MAPE_In,accu[5],fit$aic))
          arima_output<-rbind(arima_output,data.frame(out))
          
        }
      }
    }
  }
  colnames(arima_output)<-c("p","d","q","Insample MAPE","Outsample MAPE","AIC")
  write.csv(arima_output, paste(out_path,"arima_output1.csv",sep="/"))
  return(arima_output)
}


arima_function(training_data,test_data)
