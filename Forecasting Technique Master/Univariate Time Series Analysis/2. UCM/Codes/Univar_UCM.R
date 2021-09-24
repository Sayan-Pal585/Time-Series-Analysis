############################################################################
############################################################################
# CODE: UCM UNIVARIATE FORECASTING
# AUTHOR: VEDANT PRASAD - GAC BANGALORE
# DATE: 31ST JUL 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
#INSTRUCTIONS#
#MONTH SHOULD BE THE FIRST COLUMN
#DEP VARIABLE SHOULD BE THE SECOND COLUMN
#DATA SHOULD BE UNTIL 2019
############################################################################

rm(list=ls())

############################### used libraries ###################################

if(!require(car)){install.packages("car")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(fmsb)){install.packages("fmsb")}
if(!require(MASS)){install.packages("MASS")}
if(!require(QuantPsyc)){install.packages("QuantPsyc")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tibble)){install.packages("tibble")}
if(!require(combinat)){install.packages("combinat")}
if(!require(rucm)){install.packages("rucm")}
if(!require(forecast)){install.packages("forecast")}
if(!require(tseries)){install.packages("tseries")}
if(!require(ggplot2)){install.packages("ggplot2")}
if(!require(reshape)){install.packages("reshape")}

############################   user inputs required  ##########################

setwd("C:\\Users\\C938474\\Documents\\GFF\\UCM")
getwd()

data<-read.csv(file.choose(),header=T)

############USER INPUTS##############

Start_date<-"2001-01-01" #START DATE OF THE DEP VARIABLE
Train_end_year<-c(2014,2015,2016)
Train_end_month<-12
End_year<-2019
End_month<-12
Start_year<-2001 #START YEAR OF THE DEP VARIABLE
Start_month<-1   #START MONTH OF THE DEP VARIABLE
forecast_horizon<-36 #NO. OF MONTHS TO BE FORECASTED

########################################################
##                  1.MODEL BUILD                     ## 
########################################################

colnames(data)[1:2]<-c("Month","Beer_Volume")
data = cbind(data, format_date = as.Date(as.character(data$Month),"%m/%d/%Y"))
data_backup<-data
### subsetting data based on beer volume availability 
Dataset<-subset(data,!is.na(data$Month))
Dataset$year<-year(Dataset$Month)

#Month backup
actual_backup<-subset(data_backup, format_date >= Start_date)
actual_backup_f<-actual_backup[,1:2]

Dataset<-ts(Dataset[Dataset$format_date>=as.Date(Start_date),],start = c(Start_year,Start_month), end = c(End_year,End_month), frequency = 12)
Dataset_train<-window(Dataset,start = c(Start_year,Start_month), end = c(Train_end_year,Train_end_month))

final_forecasted<-NULL
final_forecasted<- data.frame(Trained_until_year=integer(),
                              Month = character(),
                              Beer_Volume = integer(),
                              forecasted=integer()
)



for(k in 1:length(Train_end_year)){
  
  #Creating the timeseries data
  
  Dataset_train<-window(Dataset,start = c(Start_year,Start_month), end = c(Train_end_year[k],Train_end_month))
  Trained_upto_year<-Train_end_year[k]  
  
  ####Trains####
  
  for(i in 2:ncol(Dataset_train)){
    temp<-Dataset_train[,i]
    assign(paste(colnames(Dataset_train)[i],c("train"),sep = "_"),temp)
  }
  
  #Univariate UCM
  
  #View(Dataset_train)
  
  univar_ucm<-ucm(Beer_Volume_train~0,Beer_Volume_train,data=Dataset
                  ,slope = T
                  ,level=T
                  ,season =T,season.length = 12
                  #,cycle =T,cycle.period = 36
  )
  
  pred_ucm<-predict(univar_ucm$model,n.ahead = forecast_horizon)
  fitted<-fitted.values(univar_ucm$model)
  forecasted<-c(fitted,pred_ucm)
  forecasted<-as.data.frame(forecasted)
  
  n<-nrow(actual_backup_f)-nrow(forecasted)
  tmp<-as.data.frame(rep(NA,n),row.names=NULL)
  colnames(tmp)[1]<-"forecasted"
  
  tmp2<-as.data.frame(rep(Train_end_year[k],nrow(actual_backup_f)),row.names=NULL)
  colnames(tmp2)[1]<-"Trained_until_year"
  
  forecasted_bf<-rbind(forecasted,tmp)
  forecasted_f<-cbind.data.frame(tmp2,actual_backup_f,forecasted_bf)
  
  final_forecasted<-rbind(final_forecasted,forecasted_f)
}

View(final_forecasted)
write.csv(final_forecasted,"univar_ucm_results.csv",row.names = F)

