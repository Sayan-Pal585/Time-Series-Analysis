############################################################################
############################################################################
# CODE: INDEPENDENT FORECASTING
# AUTHOR: DIPANKAR DOLEY - GAC BANGALORE
# DATE: 16TH JUN 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
#INSTRUCTIONS#
#
#
#
############################################################################

rm(list=ls())

#############################################################################################
if(!require(forecast)){install.packages("forecast")};library(forecast)
if(!require(fpp)){install.packages("fpp")};library(fpp)
if(!require(rucm)){install.packages("rucm")};library(rucm)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(svDialogs)){install.packages("svDialogs")};library(svDialogs)
if(!require(dplyr)){install.packages("dplyr")};library(dplyr)
if(!require(tcltk)){install.packages("tcltk")};library(tcltk)
if(!require(truncnorm)){install.packages("truncnorm")};library(truncnorm)
#############################SET DIRECTORY##################################

setwd("O:\\Dipankar\\GFF\\Independent_Forecast_02-08\\OP")

########################################################################################

start_year = 2001
start_month = 1       #[Eg. 3 for March]
end_year = 2016
end_month = 12        #[Eg. 3 for March]
forecast_horizon = 3  #[in Years]
validation_period = 3 #[In Years]

end_year-start_year+1


############################## IMPORT Dataset ########################################

raw_data <-read.csv(file.choose(),header=T, na.strings = c("","NA"))



################################# DATA PREPARATION #################################

raw_data[,1] <- as.Date(raw_data[,1],"%m/%d/%Y")
names(raw_data[,1]) <- "Month"
raw_data$Year <- year(raw_data$Month)
raw_data$Quarter <- quarter(raw_data$Month)


col_idx <- grep("Year", names(raw_data))
col_idx_2 <- grep("Quarter", names(raw_data))
raw_data  <- raw_data[, c(col_idx,col_idx_2, (1:ncol(raw_data))[-c(col_idx,col_idx_2)])] 
names(raw_data)



#Temporary subsetting till 2016
raw_data<-subset(raw_data,year(raw_data$Month)<2017)
##################### PREPARE MONTHLY, QUATER AND YEARLY TEMPLATE ############################

#need error handling
#MOnth Template
m_dates_a <- seq.Date(as.Date(paste(start_year,start_month,1,sep="/"),"%Y/%m/%d"),as.Date(paste(end_year+forecast_horizon,end_month,1,sep="/"),"%Y/%m/%d"),by="1 month")
m_temp <- data.frame(Year=year(m_dates_a),Quarter=quarter(m_dates_a),Month=m_dates_a)

# Quarterly Template
q_temp <- unique(m_temp[,-3])


# Yearly Template
(y_temp <- data.frame(Year=unique(m_temp[,-c(2:3)])))


############################## SELECT VARIABLES ###########################
var_list <- colnames(raw_data)
Pre_select <- "Month"
res1 <- dlgList(var_list,multiple=T,title="Select Monthly Variables")$res
res2 <- dlgList(var_list,multiple=T,title="Select Quarterly Variables")$res
res3 <- dlgList(var_list,multiple=T,title="Select Yearly Variables")$res
res_all <- c(res1,res2,res3)

############################## CHECK FOR DATA ENTRY ERROR #############################

if(any(as.character(res_all)=='Month')==TRUE){
  tkmessageBox(title="Attention:",message="You Have selected 'Month/Dates' as variable. Please De-Select")
}else{
  
  ############################# DATASETS FOR ANALYSIS ####################################
  m_dt=NULL;q_dt=NULL;y_dt=NULL
  
  tryCatch({
    #  m_dt <- select(raw_data,Year,Quarter,Month,res1)  
    m_dt <-  data.frame(raw_data[,colnames(raw_data) %in% c('Year','Quarter','Month',res1)])
    colnames(m_dt) <- c('Year','Quarter','Month',res1)
  },error=function(e){cat("ERROR :",conditionMessage(e),"'Monthly Data Not available.'", "\n")})
  
  tryCatch({
    #  q_dt <- select(raw_data,Year,Quarter,get(noquote(res2)))  
    q_dt <- data.frame(raw_data[,colnames(raw_data) %in% c('Year','Quarter',res2)])
    colnames(q_dt) <- c('Year','Quarter',res2)
  },error=function(e){cat("ERROR :",conditionMessage(e),"'Quarterly Data Not Available.'", "\n")})
  
  tryCatch({
    #y_dt <- select(raw_data,Year,get(res3)) 
    y_dt <- data.frame(raw_data[,colnames(raw_data) %in% c('Year',res3)])
    colnames(y_dt) <- c('Year',res3)
  },error=function(e){cat("ERROR :",conditionMessage(e),"'Yearly Data Not Available.'", "\n")})
  
  ########################### TREATMENT FOR SINGLE VARIABLE SELECTION ######################
  ############ PROCESS ONLY FOR MODEL BUILDING (AUTOMATION PURPOSE) #############################
  
  if(length(res1) %in% c(0,1)){  
    m_dt$Dummy1 <- rtruncnorm(n=nrow(m_dt), a=10, b=30, mean=39.4, sd=25.09)
  }
  
  if(length(res2) %in% c(0,1)){  
    q_dt$Dummy2 <- q_dt[,'Quarter']*100
  }
  
  if(length(res3) %in% c(0,1)){  
    y_dt$Dummy3 <- y_dt[,'Year']
  }
  
  ############################# PREPARINF TIMESERIES ########################################################
  m_ts=NULL;q_ts=NULL;y_ts=NULL
  
  #if(ncol(m_dt)>3){    
  tryCatch({
    m_ts <- ts(m_dt[,!colnames(m_dt) %in% c('Month','Year','Quarter')],start = c(start_year,start_month),frequency = 12)
  },error=function(e){cat("ERROR :",conditionMessage(e),"'Monthly Data Not available.'", "\n")})
  #}
  
  #if(ncol(q_dt)>2){    
  tryCatch({
    q_dt1 <- q_dt %>% group_by(Year,Quarter) %>% summarise_all(funs(unique(.)))
    
    if(nrow(q_dt1) != (end_year-start_year+1)*4){
      tkmessageBox(title="Attention:",message="Check the Variables again. It might not be Quarterly")
    }
    
    q_ts <- ts(data.frame(q_dt1[,!colnames(q_dt1) %in% c("Year","Quarter")]),start = c(start_year,start_month),frequency = 4)
  },error=function(e){cat("ERROR :",conditionMessage(e),"'Quarterly Data Not available.'", "\n")})
  #}
  
  #if(ncol(y_dt)>1){    
  tryCatch({
    y_dt1 <- y_dt %>% group_by(Year) %>% summarise_all(funs(unique(.)))
    
    if(nrow(y_dt1) != (end_year-start_year+1)){
      tkmessageBox(title="Attention:",message="Check the Variables again. It might not be Yearly")
    }
    
    y_ts <- ts(data.frame(y_dt1[,!colnames(y_dt1) %in% "Year"]),start = start_year,frequency = 1)
  },error=function(e){cat("ERROR :",conditionMessage(e),"'Yearly Data Not available.'", "\n")})
  #}
  
  
  # SELECTING TRAINING PERIODS BASED ON VALIDATION PERIOD ########
  y1 <- end_year-validation_period 
  y2 <- end_year-(validation_period-1)
  y3 <- end_year-(validation_period-2) 
  
  # PREPARING DATASET FOR FORECAST
  
  
  
  ################################## ANALYSIS BEGINS ##########################################
  ############################ SUBSET DATA FOR MONTHLY ###################################
  m_result_list <- NULL
  
  if(!is.null(m_ts)){
    
    ec13 <- window(m_ts,start=c(start_year,start_month),end=c(y1,end_month))
    ec14 <- window(m_ts,start=c(start_year,start_month),end=c(y2,end_month))
    ec15 <- window(m_ts,start=c(start_year,start_month),end=c(y3,end_month))
    ec16 <- m_ts
    
    names <- colnames(ec13)
    
    forecasted_13<-data.frame(NULL)
    forecasted_14<-data.frame(NULL)
    forecasted_15<-data.frame(NULL)
    forecasted_16<-data.frame(NULL)
    
    forecasted_13.1<-data.frame(NULL)
    forecasted_14.1<-data.frame(NULL)
    forecasted_15.1<-data.frame(NULL)
    forecasted_16.1<-data.frame(NULL)
    
    forecasted_13.2<-data.frame(NULL)
    forecasted_14.2<-data.frame(NULL)
    forecasted_15.2<-data.frame(NULL)
    forecasted_16.2<-data.frame(NULL)
    
    for(i in 1:ncol(ec13)){
      temp13<- ec13[,i]
      temp14<- ec14[,i]
      temp15<- ec15[,i]
      temp16<- ec16[,i]
      
      ## TAKING CARE OF BIG NUMBERS
      cnt=0
      transform_flag <- 0
      
      if(max(temp13) >= 100){
        temp13<-log(temp13)
        temp14<-log(temp14)
        temp15<-log(temp15)
        temp16<-log(temp16)
        transform_flag <- 1
        cnt=cnt+1
        print(cnt)
      }  
      
      # UCM
      
      mod13.2 <- ucm(temp13~0,temp13,season = T,season.length = 12,slope=T)
      mod14.2 <- ucm(temp14~0,temp14,season = T,season.length = 12,slope=T)
      mod15.2 <- ucm(temp15~0,temp15,season = T,season.length = 12,slope=T)
      mod16.2 <- ucm(temp16~0,temp16,season = T,season.length = 12,slope=T)
      
      #  Auto Arima
      
      mod13<-auto.arima(temp13)
      mod14<-auto.arima(temp14)
      mod15<-auto.arima(temp15)
      mod16<-auto.arima(temp16)
      
      #ETS
      
      mod13.1<-ets(temp13)
      mod14.1<-ets(temp14)
      mod15.1<-ets(temp15)
      mod16.1<-ets(temp16)
      
      # Prediction
      
      if(transform_flag==1){
        
        pred13<-exp(forecast(mod13,h = forecast_horizon*12)$mean)
        pred14<-exp(forecast(mod14,h = forecast_horizon*12)$mean)
        pred15<-exp(forecast(mod15,h = forecast_horizon*12)$mean)
        pred16<-exp(forecast(mod16,h = forecast_horizon*12)$mean)
        
        pred13.1<-exp(predict(mod13.1,h = forecast_horizon*12)$mean)
        pred14.1<-exp(predict(mod14.1,h = forecast_horizon*12)$mean)
        pred15.1<-exp(predict(mod15.1,h = forecast_horizon*12)$mean)
        pred16.1<-exp(predict(mod16.1,h = forecast_horizon*12)$mean)
        
        pred13.2 <- exp(predict(mod13.2$model,n.ahead=forecast_horizon*12))
        pred14.2 <- exp(predict(mod14.2$model,n.ahead=forecast_horizon*12))
        pred15.2 <- exp(predict(mod15.2$model,n.ahead=forecast_horizon*12))
        pred16.2 <- exp(predict(mod16.2$model,n.ahead=forecast_horizon*12))
        
      }else{
        
        pred13<-forecast(mod13,h = forecast_horizon*12)$mean
        pred14<-forecast(mod14,h = forecast_horizon*12)$mean
        pred15<-forecast(mod15,h = forecast_horizon*12)$mean
        pred16<-forecast(mod16,h = forecast_horizon*12)$mean
        
        pred13.1<-predict(mod13.1,h = forecast_horizon*12)$mean
        pred14.1<-predict(mod14.1,h = forecast_horizon*12)$mean
        pred15.1<-predict(mod15.1,h = forecast_horizon*12)$mean
        pred16.1<-predict(mod16.1,h = forecast_horizon*12)$mean
        
        pred13.2 <- predict(mod13.2$model,n.ahead=forecast_horizon*12)
        pred14.2 <- predict(mod14.2$model,n.ahead=forecast_horizon*12)
        pred15.2 <- predict(mod15.2$model,n.ahead=forecast_horizon*12)
        pred16.2 <- predict(mod16.2$model,n.ahead=forecast_horizon*12)
      } 
      # Forecated Values
      
      forecasted_13[1:(forecast_horizon*12),i]<-as.vector(pred13)
      forecasted_14[1:(forecast_horizon*12),i]<-as.vector(pred14)
      forecasted_15[1:(forecast_horizon*12),i]<-as.vector(pred15)
      forecasted_16[1:(forecast_horizon*12),i]<-as.vector(pred16)
      
      forecasted_13.1[1:(forecast_horizon*12),i]<-as.vector(pred13.1)
      forecasted_14.1[1:(forecast_horizon*12),i]<-as.vector(pred14.1)
      forecasted_15.1[1:(forecast_horizon*12),i]<-as.vector(pred15.1)
      forecasted_16.1[1:(forecast_horizon*12),i]<-as.vector(pred16.1)
      
      forecasted_13.2[1:(forecast_horizon*12),i]<-as.vector(pred13.2)
      forecasted_14.2[1:(forecast_horizon*12),i]<-as.vector(pred14.2)
      forecasted_15.2[1:(forecast_horizon*12),i]<-as.vector(pred15.2)
      forecasted_16.2[1:(forecast_horizon*12),i]<-as.vector(pred16.2)
      
    }   #END OF MONTHLY
    
    # Changing the column names
    
    colnames(forecasted_13) <- names
    colnames(forecasted_14) <- names
    colnames(forecasted_15) <- names
    colnames(forecasted_16) <- names
    
    # ETS
    
    colnames(forecasted_13.1) <- names
    colnames(forecasted_14.1) <- names
    colnames(forecasted_15.1) <- names
    colnames(forecasted_16.1) <- names
    
    # UCM
    colnames(forecasted_13.2) <- names
    colnames(forecasted_14.2) <- names
    colnames(forecasted_15.2) <- names
    colnames(forecasted_16.2) <- names
    
    # Combining forecasted and actual values
    
    #Arima
    
    ec13.1 <- rbind.data.frame(ec13,forecasted_13)
    ec14.1 <- rbind.data.frame(ec14,forecasted_14)
    ec15.1 <- rbind.data.frame(ec15,forecasted_15)
    ec16.1 <- rbind.data.frame(ec16,forecasted_16)
    
    # ETS
    
    ec2013.1 <- rbind.data.frame(ec13,forecasted_13.1)
    ec2014.1 <- rbind.data.frame(ec14,forecasted_14.1)
    ec2015.1 <- rbind.data.frame(ec15,forecasted_15.1)
    ec2016.1 <- rbind.data.frame(ec16,forecasted_16.1)
    
    
    # UCM
    
    ec2013.ucm <- rbind.data.frame(ec13,forecasted_13.2)
    ec2014.ucm <- rbind.data.frame(ec14,forecasted_14.2)
    ec2015.ucm <- rbind.data.frame(ec15,forecasted_15.2)
    ec2016.ucm <- rbind.data.frame(ec16,forecasted_16.2)
    
    
    # Combining the year and month column
    
    # Arima
    
    ec13.2 <- cbind.data.frame(m_temp[1:nrow(ec13.1),],ec13.1)
    ec14.2 <- cbind.data.frame(m_temp[1:nrow(ec14.1),],ec14.1)
    ec15.2 <- cbind.data.frame(m_temp[1:nrow(ec15.1),],ec15.1)
    ec16.2 <- cbind.data.frame(m_temp[1:nrow(ec16.1),],ec16.1)
    
    # ETS
    
    ec2013.2 <- cbind.data.frame(m_temp[1:nrow(ec2013.1),],ec2013.1)
    ec2014.2 <- cbind.data.frame(m_temp[1:nrow(ec2014.1),],ec2014.1)
    ec2015.2 <- cbind.data.frame(m_temp[1:nrow(ec2015.1),],ec2015.1)
    ec2016.2 <- cbind.data.frame(m_temp[1:nrow(ec2016.1),],ec2016.1)
    
    # UCM
    
    ec2013.ucm.2 <- cbind.data.frame(m_temp[1:nrow(ec2013.ucm),],ec2013.ucm)
    ec2014.ucm.2 <- cbind.data.frame(m_temp[1:nrow(ec2014.ucm),],ec2014.ucm)
    ec2015.ucm.2 <- cbind.data.frame(m_temp[1:nrow(ec2015.ucm),],ec2015.ucm)
    ec2016.ucm.2 <- cbind.data.frame(m_temp[1:nrow(ec2016.ucm),],ec2016.ucm)
    
    
    ## STORING MONTHLY RESULTS
    m_result_list <- list(Arima=list(ec13.2,ec14.2,ec15.2,ec16.2),
                          ETS=list(ec2013.2,ec2014.2,ec2015.2,ec2016.2),
                          UCM=list(ec2013.ucm.2,ec2014.ucm.2,ec2015.ucm.2,ec2016.ucm.2)
    )
  }  
  ################################# MONTHLY ENDS ##############################################  
  
  rm(list = ls()[!ls() %in% c('raw_data','start_year','start_month','end_year','end_month','forecast_horizon','validation_period',
                              'm_temp','q_temp','y_temp','res1','res2','res3','res_all','m_dt','q_dt1','y_dt1',
                              'm_ts','q_ts','y_ts','y1','y2','y3','m_result_list')])
  
  
  ################################# QUARTERLY BEGINS ###################################### 
  
  q_result_list <- NULL
  if(!is.null(q_ts)){   
    ec13 <- window(q_ts,start=c(start_year,1),end=c(y1,4))
    ec14 <- window(q_ts,start=c(start_year,1),end=c(y2,4))
    ec15 <- window(q_ts,start=c(start_year,1),end=c(y3,4))
    ec16 <- q_ts
    
    
    names <- colnames(ec13)
    
    forecasted_13<-data.frame(NULL)
    forecasted_14<-data.frame(NULL)
    forecasted_15<-data.frame(NULL)
    forecasted_16<-data.frame(NULL)
    
    forecasted_13.1<-data.frame(NULL)
    forecasted_14.1<-data.frame(NULL)
    forecasted_15.1<-data.frame(NULL)
    forecasted_16.1<-data.frame(NULL)
    
    forecasted_13.2<-data.frame(NULL)
    forecasted_14.2<-data.frame(NULL)
    forecasted_15.2<-data.frame(NULL)
    forecasted_16.2<-data.frame(NULL)
    
    for(i in 1:ncol(ec13))
    {
      
      
      temp13<- ec13[,i]
      temp14<- ec14[,i]
      temp15<- ec15[,i]
      temp16<- ec16[,i]
      
      cnt=0
      transform_flag <- 0
      
      if(max(temp13) >= 100){
        temp13<-log(temp13)
        temp14<-log(temp14)
        temp15<-log(temp15)
        temp16<-log(temp16)
        transform_flag <- 1
        cnt=cnt+1
        print(cnt)
      }  
      
      # UCM
      
      mod13.2 <- ucm(temp13~0,temp13,season = T,season.length = 4,slope=T)
      mod14.2 <- ucm(temp14~0,temp14,season = T,season.length = 4,slope=T)
      mod15.2 <- ucm(temp15~0,temp15,season = T,season.length = 4,slope=T)
      mod16.2 <- ucm(temp16~0,temp16,season = T,season.length = 4,slope=T)
      
      #  Auto Arima
      
      mod13<-auto.arima(temp13)
      mod14<-auto.arima(temp14)
      mod15<-auto.arima(temp15)
      mod16<-auto.arima(temp16)
      
      #ETS
      
      mod13.1<-ets(temp13)
      mod14.1<-ets(temp14)
      mod15.1<-ets(temp15)
      mod16.1<-ets(temp16)
      
      # Prediction
      
      if(transform_flag==1){
        
        pred13<-exp(forecast(mod13,h = forecast_horizon*4)$mean)
        pred14<-exp(forecast(mod14,h = forecast_horizon*4)$mean)
        pred15<-exp(forecast(mod15,h = forecast_horizon*4)$mean)
        pred16<-exp(forecast(mod16,h = forecast_horizon*4)$mean)
        
        pred13.1<-exp(predict(mod13.1,h = forecast_horizon*4)$mean)
        pred14.1<-exp(predict(mod14.1,h = forecast_horizon*4)$mean)
        pred15.1<-exp(predict(mod15.1,h = forecast_horizon*4)$mean)
        pred16.1<-exp(predict(mod16.1,h = forecast_horizon*4)$mean)
        
        pred13.2 <- exp(predict(mod13.2$model,n.ahead=forecast_horizon*4))
        pred14.2 <- exp(predict(mod14.2$model,n.ahead=forecast_horizon*4))
        pred15.2 <- exp(predict(mod15.2$model,n.ahead=forecast_horizon*4))
        pred16.2 <- exp(predict(mod16.2$model,n.ahead=forecast_horizon*4))
        
      }else{
        
        pred13<-forecast(mod13,h = forecast_horizon*4)$mean
        pred14<-forecast(mod14,h = forecast_horizon*4)$mean
        pred15<-forecast(mod15,h = forecast_horizon*4)$mean
        pred16<-forecast(mod16,h = forecast_horizon*4)$mean
        
        pred13.1<-predict(mod13.1,h = forecast_horizon*4)$mean
        pred14.1<-predict(mod14.1,h = forecast_horizon*4)$mean
        pred15.1<-predict(mod15.1,h = forecast_horizon*4)$mean
        pred16.1<-predict(mod16.1,h = forecast_horizon*4)$mean
        
        pred13.2 <- predict(mod13.2$model,n.ahead=forecast_horizon*4)
        pred14.2 <- predict(mod14.2$model,n.ahead=forecast_horizon*4)
        pred15.2 <- predict(mod15.2$model,n.ahead=forecast_horizon*4)
        pred16.2 <- predict(mod16.2$model,n.ahead=forecast_horizon*4)
      } 
      # Forecated Values
      
      forecasted_13[1:(forecast_horizon*4),i]<-as.vector(pred13)
      forecasted_14[1:(forecast_horizon*4),i]<-as.vector(pred14)
      forecasted_15[1:(forecast_horizon*4),i]<-as.vector(pred15)
      forecasted_16[1:(forecast_horizon*4),i]<-as.vector(pred16)
      
      forecasted_13.1[1:(forecast_horizon*4),i]<-as.vector(pred13.1)
      forecasted_14.1[1:(forecast_horizon*4),i]<-as.vector(pred14.1)
      forecasted_15.1[1:(forecast_horizon*4),i]<-as.vector(pred15.1)
      forecasted_16.1[1:(forecast_horizon*4),i]<-as.vector(pred16.1)
      
      forecasted_13.2[1:(forecast_horizon*4),i]<-as.vector(pred13.2)
      forecasted_14.2[1:(forecast_horizon*4),i]<-as.vector(pred14.2)
      forecasted_15.2[1:(forecast_horizon*4),i]<-as.vector(pred15.2)
      forecasted_16.2[1:(forecast_horizon*4),i]<-as.vector(pred16.2)
      
    }   #END OF QUARTERLY
    
    # Changing the column names
    
    colnames(forecasted_13) <- names
    colnames(forecasted_14) <- names
    colnames(forecasted_15) <- names
    colnames(forecasted_16) <- names
    
    # ETS
    
    colnames(forecasted_13.1) <- names
    colnames(forecasted_14.1) <- names
    colnames(forecasted_15.1) <- names
    colnames(forecasted_16.1) <- names
    
    # UCM
    colnames(forecasted_13.2) <- names
    colnames(forecasted_14.2) <- names
    colnames(forecasted_15.2) <- names
    colnames(forecasted_16.2) <- names
    
    # Combining forecasted and actual values
    
    #Arima
    
    ec13.1 <- rbind.data.frame(ec13,forecasted_13)
    ec14.1 <- rbind.data.frame(ec14,forecasted_14)
    ec15.1 <- rbind.data.frame(ec15,forecasted_15)
    ec16.1 <- rbind.data.frame(ec16,forecasted_16)
    
    # ETS
    
    ec2013.1 <- rbind.data.frame(ec13,forecasted_13.1)
    ec2014.1 <- rbind.data.frame(ec14,forecasted_14.1)
    ec2015.1 <- rbind.data.frame(ec15,forecasted_15.1)
    ec2016.1 <- rbind.data.frame(ec16,forecasted_16.1)
    
    
    # UCM
    
    ec2013.ucm <- rbind.data.frame(ec13,forecasted_13.2)
    ec2014.ucm <- rbind.data.frame(ec14,forecasted_14.2)
    ec2015.ucm <- rbind.data.frame(ec15,forecasted_15.2)
    ec2016.ucm <- rbind.data.frame(ec16,forecasted_16.2)
    
    
    # Combining the year and month column
    
    # Arima
    
    ec13.2 <- cbind.data.frame(q_temp[1:nrow(ec13.1),],ec13.1)
    ec14.2 <- cbind.data.frame(q_temp[1:nrow(ec14.1),],ec14.1)
    ec15.2 <- cbind.data.frame(q_temp[1:nrow(ec15.1),],ec15.1)
    ec16.2 <- cbind.data.frame(q_temp[1:nrow(ec16.1),],ec16.1)
    
    # ETS
    
    ec2013.2 <- cbind.data.frame(q_temp[1:nrow(ec2013.1),],ec2013.1)
    ec2014.2 <- cbind.data.frame(q_temp[1:nrow(ec2014.1),],ec2014.1)
    ec2015.2 <- cbind.data.frame(q_temp[1:nrow(ec2015.1),],ec2015.1)
    ec2016.2 <- cbind.data.frame(q_temp[1:nrow(ec2016.1),],ec2016.1)
    
    # UCM
    
    ec2013.ucm.2 <- cbind.data.frame(q_temp[1:nrow(ec2013.ucm),],ec2013.ucm)
    ec2014.ucm.2 <- cbind.data.frame(q_temp[1:nrow(ec2014.ucm),],ec2014.ucm)
    ec2015.ucm.2 <- cbind.data.frame(q_temp[1:nrow(ec2015.ucm),],ec2015.ucm)
    ec2016.ucm.2 <- cbind.data.frame(q_temp[1:nrow(ec2016.ucm),],ec2016.ucm)
    
    
    ## STORING QUARTERLY RESULTS
    q_result_list <- list(Arima=list(ec13.2,ec14.2,ec15.2,ec16.2),
                          ETS=list(ec2013.2,ec2014.2,ec2015.2,ec2016.2),
                          UCM=list(ec2013.ucm.2,ec2014.ucm.2,ec2015.ucm.2,ec2016.ucm.2)
    )
    
  }  
  ################################### QUARTERLY ENDS ######################################  
  
  rm(list = ls()[!ls() %in% c('raw_data','start_year','start_month','end_year','end_month','forecast_horizon','validation_period',
                              'm_temp','q_temp','y_temp','res1','res2','res3','res_all','m_dt','q_dt1','y_dt1',
                              'm_ts','q_ts','y_ts','y1','y2','y3',
                              'q_result_list','m_result_list')])
  
  
  #################################### YEARLY BEGINS ###################################### 
  y_result_list <- NULL
  if(!is.null(y_ts)){
    ec13 <- window(y_ts,start=start_year,end=y1)
    ec14 <- window(y_ts,start=start_year,end=y2)
    ec15 <- window(y_ts,start=start_year,end=y3)
    ec16 <- y_ts
    
    
    names <- colnames(ec13)
    
    forecasted_13<-data.frame(NULL)
    forecasted_14<-data.frame(NULL)
    forecasted_15<-data.frame(NULL)
    forecasted_16<-data.frame(NULL)
    
    forecasted_13.1<-data.frame(NULL)
    forecasted_14.1<-data.frame(NULL)
    forecasted_15.1<-data.frame(NULL)
    forecasted_16.1<-data.frame(NULL)
    
    forecasted_13.2<-data.frame(NULL)
    forecasted_14.2<-data.frame(NULL)
    forecasted_15.2<-data.frame(NULL)
    forecasted_16.2<-data.frame(NULL)
    
    for(i in 1:ncol(ec13))
    {
      
      
      temp13<- ec13[,i]
      temp14<- ec14[,i]
      temp15<- ec15[,i]
      temp16<- ec16[,i]
      
      cnt=0
      transform_flag <- 0
      
      if(max(temp13) >= 100){
        temp13<-log(temp13)
        temp14<-log(temp14)
        temp15<-log(temp15)
        temp16<-log(temp16)
        transform_flag <- 1
        cnt=cnt+1
        print(cnt)
      }  
      
      # UCM
      
      mod13.2 <- ucm(temp13~0,temp13,slope = T)
      mod14.2 <- ucm(temp14~0,temp14,slope = T)
      mod15.2 <- ucm(temp15~0,temp15,slope = T)
      mod16.2 <- ucm(temp16~0,temp16,slope = T)
      
      #  Auto Arima
      mod13<-auto.arima(temp13)
      mod14<-auto.arima(temp14)
      mod15<-auto.arima(temp15)
      mod16<-auto.arima(temp16)
      
      #ETS
      
      mod13.1<-ets(temp13)
      mod14.1<-ets(temp14)
      mod15.1<-ets(temp15)
      mod16.1<-ets(temp16)
      
      # Prediction
      
      if(transform_flag==1){
        
        pred13<-exp(forecast(mod13,h = forecast_horizon)$mean)
        pred14<-exp(forecast(mod14,h = forecast_horizon)$mean)
        pred15<-exp(forecast(mod15,h = forecast_horizon)$mean)
        pred16<-exp(forecast(mod16,h = forecast_horizon)$mean)
        
        pred13.1<-exp(predict(mod13.1,h = forecast_horizon)$mean)
        pred14.1<-exp(predict(mod14.1,h = forecast_horizon)$mean)
        pred15.1<-exp(predict(mod15.1,h = forecast_horizon)$mean)
        pred16.1<-exp(predict(mod16.1,h = forecast_horizon)$mean)
        
        pred13.2 <- exp(predict(mod13.2$model,n.ahead=forecast_horizon))
        pred14.2 <- exp(predict(mod14.2$model,n.ahead=forecast_horizon))
        pred15.2 <- exp(predict(mod15.2$model,n.ahead=forecast_horizon))
        pred16.2 <- exp(predict(mod16.2$model,n.ahead=forecast_horizon))
        
      }else{
        
        pred13<-forecast(mod13,h = forecast_horizon)$mean
        pred14<-forecast(mod14,h = forecast_horizon)$mean
        pred15<-forecast(mod15,h = forecast_horizon)$mean
        pred16<-forecast(mod16,h = forecast_horizon)$mean
        
        pred13.1<-predict(mod13.1,h = forecast_horizon)$mean
        pred14.1<-predict(mod14.1,h = forecast_horizon)$mean
        pred15.1<-predict(mod15.1,h = forecast_horizon)$mean
        pred16.1<-predict(mod16.1,h = forecast_horizon)$mean
        
        pred13.2 <- predict(mod13.2$model,n.ahead=forecast_horizon)
        pred14.2 <- predict(mod14.2$model,n.ahead=forecast_horizon)
        pred15.2 <- predict(mod15.2$model,n.ahead=forecast_horizon)
        pred16.2 <- predict(mod16.2$model,n.ahead=forecast_horizon)
      } 
      # Forecated Values
      
      forecasted_13[1:(forecast_horizon),i]<-as.vector(pred13)
      forecasted_14[1:(forecast_horizon),i]<-as.vector(pred14)
      forecasted_15[1:(forecast_horizon),i]<-as.vector(pred15)
      forecasted_16[1:(forecast_horizon),i]<-as.vector(pred16)
      
      forecasted_13.1[1:(forecast_horizon),i]<-as.vector(pred13.1)
      forecasted_14.1[1:(forecast_horizon),i]<-as.vector(pred14.1)
      forecasted_15.1[1:(forecast_horizon),i]<-as.vector(pred15.1)
      forecasted_16.1[1:(forecast_horizon),i]<-as.vector(pred16.1)
      
      forecasted_13.2[1:(forecast_horizon),i]<-as.vector(pred13.2)
      forecasted_14.2[1:(forecast_horizon),i]<-as.vector(pred14.2)
      forecasted_15.2[1:(forecast_horizon),i]<-as.vector(pred15.2)
      forecasted_16.2[1:(forecast_horizon),i]<-as.vector(pred16.2)
      
    }   #END OF YEARLY
    
    # Changing the column names
    
    colnames(forecasted_13) <- names
    colnames(forecasted_14) <- names
    colnames(forecasted_15) <- names
    colnames(forecasted_16) <- names
    
    # ETS
    
    colnames(forecasted_13.1) <- names
    colnames(forecasted_14.1) <- names
    colnames(forecasted_15.1) <- names
    colnames(forecasted_16.1) <- names
    
    # UCM
    colnames(forecasted_13.2) <- names
    colnames(forecasted_14.2) <- names
    colnames(forecasted_15.2) <- names
    colnames(forecasted_16.2) <- names
    
    # Combining forecasted and actual values
    
    #Arima
    
    ec13.1 <- rbind.data.frame(ec13,forecasted_13)
    ec14.1 <- rbind.data.frame(ec14,forecasted_14)
    ec15.1 <- rbind.data.frame(ec15,forecasted_15)
    ec16.1 <- rbind.data.frame(ec16,forecasted_16)
    
    # ETS
    
    ec2013.1 <- rbind.data.frame(ec13,forecasted_13.1)
    ec2014.1 <- rbind.data.frame(ec14,forecasted_14.1)
    ec2015.1 <- rbind.data.frame(ec15,forecasted_15.1)
    ec2016.1 <- rbind.data.frame(ec16,forecasted_16.1)
    
    
    # UCM
    
    ec2013.ucm <- rbind.data.frame(ec13,forecasted_13.2)
    ec2014.ucm <- rbind.data.frame(ec14,forecasted_14.2)
    ec2015.ucm <- rbind.data.frame(ec15,forecasted_15.2)
    ec2016.ucm <- rbind.data.frame(ec16,forecasted_16.2)
    
    
    # Combining the year and month column
    
    # Arima
    
    ec13.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec13.1),],ec13.1)
    ec14.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec14.1),],ec14.1)
    ec15.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec15.1),],ec15.1)
    ec16.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec16.1),],ec16.1)
    
    # ETS
    
    ec2013.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2013.1),],ec2013.1)
    ec2014.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2014.1),],ec2014.1)
    ec2015.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2015.1),],ec2015.1)
    ec2016.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2016.1),],ec2016.1)
    
    # UCM
    
    ec2013.ucm.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2013.ucm),],ec2013.ucm)
    ec2014.ucm.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2014.ucm),],ec2014.ucm)
    ec2015.ucm.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2015.ucm),],ec2015.ucm)
    ec2016.ucm.2 <- cbind.data.frame(Year=y_temp[1:nrow(ec2016.ucm),],ec2016.ucm)
    
    
    ## STORING QUARTERLY RESULTS
    y_result_list <- list(Arima=list(ec13.2,ec14.2,ec15.2,ec16.2),
                          ETS=list(ec2013.2,ec2014.2,ec2015.2,ec2016.2),
                          UCM=list(ec2013.ucm.2,ec2014.ucm.2,ec2015.ucm.2,ec2016.ucm.2)
    )
    
  }  
  ################################### QUARTERLY ENDS ######################################  
  rm(list = ls()[!ls() %in% c('raw_data','start_year','start_month','end_year','end_month','forecast_horizon','validation_period',
                              'm_temp','q_temp','y_temp','res1','res2','res3','res_all','m_dt','q_dt1','y_dt1',
                              'm_ts','q_ts','y_ts','y1','y2','y3',
                              'q_result_list','m_result_list','y_result_list')])
  
  ################################### SUMMARY PREPARATION ###############################
  
  ## ARIMA  
  
  
  arima_standing_2013<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$Arima[1]),
                                                                  data.frame(q_result_list$Arima[1]),
                                                                  data.frame(y_result_list$Arima[1])))
  
  arima_standing_2014<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$Arima[2]),
                                                                  data.frame(q_result_list$Arima[2]),
                                                                  data.frame(y_result_list$Arima[2])))
  
  arima_standing_2015<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$Arima[3]),
                                                                  data.frame(q_result_list$Arima[3]),
                                                                  data.frame(y_result_list$Arima[3])))
  
  arima_standing_2016<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$Arima[4]),
                                                                  data.frame(q_result_list$Arima[4]),
                                                                  data.frame(y_result_list$Arima[4])))
  
  
  ## ETS  
  
  ets_standing_2013<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$ETS[1]),
                                                                data.frame(q_result_list$ETS[1]),
                                                                data.frame(y_result_list$ETS[1])))
  
  ets_standing_2014<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$ETS[2]),
                                                                data.frame(q_result_list$ETS[2]),
                                                                data.frame(y_result_list$ETS[2])))
  
  ets_standing_2015<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$ETS[3]),
                                                                data.frame(q_result_list$ETS[3]),
                                                                data.frame(y_result_list$ETS[3])))
  
  ets_standing_2016<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$ETS[4]),
                                                                data.frame(q_result_list$ETS[4]),
                                                                data.frame(y_result_list$ETS[4])))
  
  ## UCM  
  
  ucm_standing_2013<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$UCM[1]),
                                                                data.frame(q_result_list$UCM[1]),
                                                                data.frame(y_result_list$UCM[1])))
  
  ucm_standing_2014<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$UCM[2]),
                                                                data.frame(q_result_list$UCM[2]),
                                                                data.frame(y_result_list$UCM[2])))
  
  ucm_standing_2015<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$UCM[3]),
                                                                data.frame(q_result_list$UCM[3]),
                                                                data.frame(y_result_list$UCM[3])))
  
  ucm_standing_2016<- Reduce(function(x,y)merge(x,y,all=T),list(data.frame(m_result_list$UCM[4]),
                                                                data.frame(q_result_list$UCM[4]),
                                                                data.frame(y_result_list$UCM[4])))
  
  ##################################  OUTSAMPLE ACCURACY CHECKS  #########################################
  #Arima: 
  # 1:  
  
  f = subset(arima_standing_2013,arima_standing_2013$Year > y1 & arima_standing_2013$Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y1,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_arima_standing_2013 <- data.frame(Model='ARIMA',Trained_upto=y1,Month=a$Month,do.call(cbind,temp_1))
  
  # 2:
  f<-NULL;a<-NULL
  f = subset(arima_standing_2014,arima_standing_2014$Year > y2 & arima_standing_2014$Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y2,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_arima_standing_2014 <- data.frame(Model='ARIMA',Trained_upto=y2,Month=a$Month,do.call(cbind,temp_1))
  
  # 3:
  f<-NULL;a<-NULL
  f = subset(arima_standing_2015,arima_standing_2015$Year > y3 & arima_standing_2015$Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y3,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_arima_standing_2015 <- data.frame(Model='ARIMA',Trained_upto=y3,Month=a$Month,do.call(cbind,temp_1))
  
  #ETS
  
  # 1:  
  f = subset(ets_standing_2013,ets_standing_2013$Year > y1 & Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y1,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_ets_standing_2013 <- data.frame(Model='ETS',Trained_upto=y1,Month=a$Month,do.call(cbind,temp_1))
  
  # 2:
  f<-NULL;a<-NULL
  f = subset(ets_standing_2014,ets_standing_2014$Year > y2 & Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y2,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_ets_standing_2014 <- data.frame(Model='ETS',Trained_upto=y2,Month=a$Month,do.call(cbind,temp_1))
  
  # 3:
  f<-NULL;a<-NULL
  f = subset(ets_standing_2015,ets_standing_2015$Year > y3 & Year < end_year+1,c('Month',res_all))
  a = subset(raw_data, raw_data$Year > y3,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_ets_standing_2015 <- data.frame(Model='ETS',Trained_upto=y3,Month=a$Month,do.call(cbind,temp_1))
  
  
  #UCM
  
  # 1:  
  f = subset(ucm_standing_2013,ucm_standing_2013$Year > y1 & ucm_standing_2013$Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y1,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_ucm_standing_2013 <- data.frame(Model='UCM',Trained_upto=y1,Month=a$Month,do.call(cbind,temp_1))
  
  # 2:
  f<-NULL;a<-NULL
  f = subset(ucm_standing_2014,ucm_standing_2014$Year > y2 & ucm_standing_2014$Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y2,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_ucm_standing_2014 <- data.frame(Model='UCM',Trained_upto=y2,Month=a$Month,do.call(cbind,temp_1))
  
  # 3:
  f<-NULL;a<-NULL
  f = subset(ucm_standing_2015,ucm_standing_2015$Year > y3 & ucm_standing_2015$Year < end_year+1,c('Month',res_all)) 
  a = subset(raw_data, raw_data$Year > y3,c('Month',res_all))
  i1=1;temp_1 <- list()
  for(cnt in res_all){
    temp_1[[i1]]  <- setNames(data.frame(100*(1-abs(f[,cnt]-a[,cnt])/a[,cnt])),paste(cnt,'Accuracy',sep = "_"))  
    i1=i1+1
  }
  accuracy_ucm_standing_2015 <- data.frame(Model='UCM',Trained_upto=y3,Month=a$Month,do.call(cbind,temp_1))
  
  ############################## FORECASTING SUMMARY ##########################################
  Independent_forecasts  <- rbind.data.frame(
    data.frame(Trained_upto=y1,arima_standing_2013),
    data.frame(Trained_upto=y2,arima_standing_2014),
    data.frame(Trained_upto=y3,arima_standing_2015),
    data.frame(Trained_upto=y3+1,arima_standing_2016),
    data.frame(Trained_upto=y1,ets_standing_2013),
    data.frame(Trained_upto=y2,ets_standing_2014),
    data.frame(Trained_upto=y3,ets_standing_2015),
    data.frame(Trained_upto=y3+1,ets_standing_2016),
    data.frame(Trained_upto=y1,ucm_standing_2013),
    data.frame(Trained_upto=y2,ucm_standing_2014),
    data.frame(Trained_upto=y3,ucm_standing_2015),
    data.frame(Trained_upto=y3+1,ucm_standing_2016)
  )
  
  Independent_forecasts <-  Independent_forecasts[,!colnames(Independent_forecasts) %in% c('Dummy1','Dummy2','Dummy3')]
  ############################## ACCURACY CHECK SUMMARY ######################################
  
  monthly_accuracy_summary <- rbind.data.frame(
    accuracy_arima_standing_2013,
    accuracy_arima_standing_2014,
    accuracy_arima_standing_2015,
    accuracy_ets_standing_2013,
    accuracy_ets_standing_2014,
    accuracy_ets_standing_2015,
    accuracy_ucm_standing_2013,
    accuracy_ucm_standing_2014,
    accuracy_ucm_standing_2015)
  
  monthly_accuracy_summary$Year <- year(monthly_accuracy_summary$Month)
  
  temporary_data<- monthly_accuracy_summary[,!colnames(monthly_accuracy_summary)=="Month"]
  
  Accuracy_average_yearly<- data.frame(temporary_data %>% group_by(Model,Trained_upto,Year) %>% summarise_all(funs(mean(.))))
  
  ##############################################################################################
  
  rm(list = ls()[!ls() %in% c('raw_data','start_year','start_month','end_year','end_month','forecast_horizon','validation_period',
                              'm_temp','q_temp','y_temp','res1','res2','res3','res_all','m_dt','q_dt1','y_dt1',
                              'm_ts','q_ts','y_ts','y1','y2','y3',
                              'q_result_list','m_result_list','y_result_list',
                              'Accuracy_average_yearly','monthly_accuracy_summary','Independent_forecasts'
  )])
  
  
  ################################  OUTPUT ###########################################  
  
  
  write.csv(Accuracy_average_yearly,paste('Accuracy_average_yearly','.csv',sep = ))
  write.csv(monthly_accuracy_summary,paste('monthly_accuracy_summary','.csv',sep = ))
  write.csv(Independent_forecasts,paste('Independent_forecasts','.csv',sep = ))
  
  ################################### ENDS ########################################
  
  