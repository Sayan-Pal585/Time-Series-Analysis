##-------------------------------HW  FUNCTIONS--------------------------------##
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
##-----------------------------------HW---------------------------------------##
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

hw.model.results <- function(training_data, 
                             test_data, 
                             trn.period, 
                             tst.period, 
                             freq, 
                             forecast.duration, 
                             start_date, 
                             error_threshold,
                             in.err.skip.dur){
  
  #Number of initial years to exlcude from InSample Error
  
  model_summary <- data.frame(t(c(0,0,0,0,0,0)))
  colnames(model_summary) <- c("alpha", "beta", "gamma", "OutSample_Error","InSample_Error","Mean_MAPE")
  
  #Ranges for alpha, beta, gamma
  alpha_range <- seq(0.05,0.95,0.1)   
  beta_range <- seq(0.05,0.95,0.1)
  gamma_range <- seq(0.05,0.95,0.1)
  
  L_MAPE = 1
  
  #Hyperparameter Tuning for the Best Model
  for (alpha in alpha_range)   {
    for (beta in beta_range)     {
      for (gamma in gamma_range)   {
        
        # Build Model for a given set of Hyperparams
        HW.fit <- HoltWinters(training_data,
                              seasonal = "mult", 
                              alpha    = alpha, 
                              beta     = beta, 
                              gamma    = gamma)
        
        # Forecast Future Values
        HW.predicted <- predict(HW.fit,
                                n.ahead = forecast.duration + tst.period,
                                prediction.interval = T,
                                level = 0.95)
        HW.fitted <- HW.fit$fitted[,1]
        
        
        HW.forecast <- c(rep(NA, freq), HW.fitted, HW.predicted[,1])
        
        if (tst.period != 0) {
          HW.actual <- c(training_data[1:trn.period],
                         test_data,
                         rep(NA, forecast.duration))
        } else {
          HW.actual <- c(training_data[1:trn.period],
                         rep(NA, forecast.duration))
        }
        
        
        # Build the series (insample, outsample, forecast)
        HW.series.initial <- data.frame(cbind(HW.forecast,HW.actual))
        colnames(HW.series.initial) <- c("PREDICTED","ACTUAL")
        
        HW.series.initial$ERROR <- HW.series.initial$PREDICTED - HW.series.initial$ACTUAL
        HW.series.initial$APE <- abs(HW.series.initial$ERROR)/HW.series.initial$ACTUAL
        
        # Calculate Error
        HW.insample.initial <- mean(HW.series.initial[((freq * in.err.skip.dur) + 1):trn.period, ]$APE, na.rm = T)
        HW.outsample.initial <- mean(HW.series.initial[(trn.period + 1):(trn.period + tst.period), ]$APE, na.rm = T)
        HW.overall.initial <- mean(HW.series.initial[((freq * in.err.skip.dur) + 1):(trn.period + tst.period), ]$APE, na.rm = T)
        
        HW.MAPE <- c(HW.outsample.initial,
                     HW.insample.initial,
                     HW.overall.initial)
        
        # Store the results of Best Models
        Latest_OutSample_Error <- HW.MAPE[1]
        Latest_InSample_Error <- HW.MAPE[2]
        Latest_Overall_Error <- HW.MAPE[3]
        
        new_summary  <- data.frame(t(c(alpha,
                                       beta,
                                       gamma,
                                       Latest_OutSample_Error,
                                       Latest_InSample_Error,
                                       Latest_Overall_Error)))
        colnames(new_summary) <- c("alpha","beta","gamma","OutSample_Error","InSample_Error","Mean_MAPE")
        
        model_summary <- rbind(model_summary, new_summary)
        
        error_diff = abs(as.numeric(Latest_InSample_Error) - as.numeric(Latest_OutSample_Error))
        best_mape <- Latest_OutSample_Error
        
        # Choose best model by MAPE
        if (error_diff < error_threshold) {
          if (best_mape < L_MAPE)    {
            L_MAPE = best_mape
            best_alpha = alpha
            best_beta = beta
            best_gamma = gamma
            best_hyperparameters = c(alpha = best_alpha,
                                     beta  = best_beta,
                                     gamma = best_gamma)
            
          }
        }
      }
    }
  }
  
  # View(model_summary)
  print(best_hyperparameters)
  
  # Build Model for a given set of Hyperparams
  hw.model <- HoltWinters(training_data,
                          seasonal = "mult", 
                          alpha    = best_alpha, 
                          beta     = best_beta, 
                          gamma    = best_gamma)
  
  # Forecast Future Values
  hw.predicted <- predict(hw.model,
                          n.ahead = (forecast.duration + tst.period),
                          prediction.interval = T,
                          level = 0.95)
  hw.fitted.pred <- hw.model$fitted[,1]
  
  
  hw.forecast.series <- c(rep(NA, freq), hw.fitted.pred, hw.predicted[,1])
  
  if (tst.period != 0) {
    hw.actual.series <- c(training_data[1:trn.period],
                          test_data,
                          rep(NA, forecast.duration))
  } else {
    hw.actual.series <- c(training_data[1:trn.period],
                          rep(NA, forecast.duration))
  }
  
  # Build the series (insample, outsample, forecast)
  hw.series <- data.frame(cbind(hw.forecast.series,hw.actual.series))
  colnames(hw.series) <- c("PREDICTED","ACTUAL")
  
  hw.series$ERROR <- hw.series$PREDICTED - hw.series$ACTUAL
  hw.series$APE <- abs(hw.series$ERROR)/hw.series$ACTUAL
  
  # Calculate OutSample Error
  hw.insample <- mean(hw.series[((freq * in.err.skip.dur) + 1):trn.period, ]$APE, na.rm = T)
  hw.outsample <- mean(hw.series[(trn.period + 1): (trn.period + tst.period), ]$APE, na.rm = T)
  hw.overall <- mean(hw.series[((freq * in.err.skip.dur) + 1):(trn.period + tst.period), ]$APE, na.rm = T)
  
  hw.MAPE <- c(hw.insample,
               hw.outsample,
               hw.overall)
  hw.MAPE <- as.data.frame(hw.MAPE)
  row.names(hw.MAPE) <- c("InSample","OutSample","OverallMape")
  
  print("Freq, Skip duration, trn.period, tst.period, forecast duration")
  print(freq)
  print(in.err.skip.dur)
  print(trn.period)
  print(tst.period)
  print(forecast.duration)
  

  write.csv(best_hyperparameters, paste(out_path, "HW best parameters.csv",sep="/"))
  write.csv(model_summary, paste(out_path, "HW all combination summary.csv",sep="/"))
  write.csv(hw.MAPE, paste(out_path, "HW MAPE.csv",sep="/"))
  write.csv(hw.series, paste(out_path, "HW Series.csv",sep="/"))
  
  pdf(paste(out_path, "- HW Actual vs Predicted.pdf",sep="/"))
  plot(hw.series$PREDICTED, col = "red", main = "HW - Actual (blue) vs Predicted (red)", type = "l", xaxt = "n")
  lines(hw.series$ACTUAL, col = "blue", type = "l")
  dev.off()
  
  #Return Model & Series for Holt Winters
  return(list(hw.series, hw.MAPE))
  
}




