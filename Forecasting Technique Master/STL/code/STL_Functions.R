##------------------------------ STL FUNCTIONS--------------------------------##
##   Author : Ali Umar Qureshi                                                ##  
##   Date : 11-01-2018                                                        ##
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

Decomposing_the_timeseries(data_ts)

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
##-----------------------------------STL--------------------------------------##
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

stl.model.results <- function(training_data, 
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
  #    STL      #
  #             #
  ###############
  
  # Adjusting for number of periods less than 2
  if (length(training_data) < ((2 * freq)+1)) {
    if ((trn.period + tst.period) > ((2 * freq)+1)) {
      stl.trn.period <- (2 * freq)+1
      stl.train.data <- ts(c(training_data, 
                             test_data[1:((2 * freq) + 1 - length(training_data))]), 
                           start = start(training_data), 
                           frequency = frequency(training_data))
      stl.tst.period <- length(test_data) - ((2 * freq) + 1 - length(training_data))
      
      if (stl.tst.period>0)
        stl.test.data <- ts(test_data[((2 * freq) - length(training_data) + 2): length(test_data)],
                            end = end(test_data),
                            frequency = frequency(test_data))
    } else {
      stop("Cycle length less than 2 periods.")
    }
  } else {
    stl.trn.period <- trn.period
    stl.train.data <- training_data
    stl.tst.period <- tst.period
    
    if (stl.tst.period>0)
      stl.test.data <- test_data
  }
  
  # training_data <- ts(data_ts[1:trn.period], frequency = freq)
  
  stl.model <-  stlf(training_data,
                     h = forecast.duration + stl.tst.period,
                     s.window = freq,
                     robust = TRUE,
                     etsmodel = "MAN")
  
  stl.fitted.pred <- stl.model$fitted
  stl.prediction <- data.frame(stl.model)
  stl.pred <- ts(stl.prediction$Point.Forecast)
  stl.forecast.series <- c(stl.fitted.pred[1:stl.trn.period], stl.pred)
  
  # Build the series
  if (stl.tst.period != 0) {
    stl.series <- data.frame(cbind(stl.forecast.series,
                                   c(stl.train.data,
                                     stl.test.data,
                                     rep(NA, forecast.duration))))
  } else {
    stl.series <- data.frame(cbind(stl.forecast.series,
                                   c(stl.train.data,
                                     rep(NA, forecast.duration))))
  }
  
  colnames(stl.series) <- c("PREDICTED","ACTUAL")
  
  stl.series$ERROR <- stl.series$PREDICTED - stl.series$ACTUAL
  stl.series$APE <- abs(stl.series$ERROR)/stl.series$ACTUAL
  
  # Calculate OutSample Error
  stl.insample <- mean(stl.series[((freq * in.err.skip.dur) + 1):stl.trn.period, ]$APE, na.rm = T)
  
  if (stl.tst.period != 0) {
    stl.outsample <- mean(stl.series[(stl.trn.period + 1): (stl.trn.period + stl.tst.period), ]$APE, na.rm = T)
  } else {
    stl.outsample <- 0
  }
  
  stl.overall <- mean(stl.series[((freq * in.err.skip.dur) + 1):(stl.trn.period + stl.tst.period), ]$APE, na.rm = T)
  
  stl.MAPE <- c(rep(NA, 3))
  if (abs(stl.insample - stl.outsample) < error_threshold)
    stl.MAPE <- c(stl.insample,
                  stl.outsample,
                  stl.overall)
  stl.MAPE <- as.data.frame(stl.MAPE)
  row.names(stl.MAPE) <- c("InSample","OutSample","OverallMape")
  
  print("Freq, Skip duration, trn.period, tst.period, forecast duration")
  print(freq)
  print(in.err.skip.dur)
  print(stl.trn.period)
  print(stl.tst.period)
  print(forecast.duration)
  
  write.csv(stl.MAPE, paste(out_path,"STL MAPE.csv",sep="/"))
  write.csv(stl.series, paste(out_path,"STL Series.csv",sep="/"))
  
  pdf(paste(out_path,"- STL Actual vs Predicted.pdf",sep="/"))
  
  # pdf(paste(country, brand, kpi, "- STL Actual vs Predicted.pdf"))
  plot(stl.series$PREDICTED, col = "red", main = "STL - Actual (blue) vs Predicted (red)", type = "l", xaxt = "n")
  lines(stl.series$ACTUAL, col = "blue", type = "l")
  dev.off()
  
  
  return(list(stl.series, stl.MAPE, plot(stl.model)))
}

