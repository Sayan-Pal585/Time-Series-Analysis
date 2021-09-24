############################################################################
############################################################################
# CODE: UCM MULTIVARIATE FORECASTING
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

############################### used libraries #############################

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
if(!require(svDialogs)){install.packages("svDialogs")}


############################  Set Working directory  #####################

setwd("C:\\Users\\C938474\\Documents\\NewChina\\ucm")
getwd()

data<-read.csv(file.choose(),header=T)

############USER INPUTS##############

Start_date<-"2001-01-01"     #START DATE OF DEP VARIABLE
Train_end_year<-2015         #END YEAR OF TRAINING DATA
Train_end_month<-12          #END MONTH OF TRAINING DATA
End_year<-2016               #END YEAR OF TEST DATA OR FORECAST DATA
End_month<-12                #END MONTH OF TEST DATA OR FORECAST DATA
Start_year<-2001             #START YEAR OF RAW DATA
Start_month<-1               #START MONTH OF RAW DATA   
#End_date<-"2016-12-01"

########################################################
##                  1.MODEL BUILD                     ## 
########################################################

colnames(data)[1:2]<-c("Month","Beer_Volume")
data = cbind(data, format_date = as.Date(as.character(data$Month),"%m/%d/%Y"))
data_backup<-data
### subsetting data based on beer volume availability 
Dataset<-subset(data,!is.na(data$Beer_Volume))
Dataset$year<-year(Dataset$Month)

#Actual Beer volume backup
actual_backup<-subset(data_backup, format_date >= Start_date)
actual_backup_f<-actual_backup[,1:2]

#Creating the timeseries data
Dataset<-ts(Dataset[Dataset$format_date>=as.Date(Start_date),],start = c(Start_year,Start_month), end = c(End_year,End_month), frequency = 12)

Dataset_train<-window(Dataset,start = c(Start_year,Start_month), end = c(Train_end_year,Train_end_month))

if(Train_end_month<12)
  Dataset_test<-window(Dataset,start = c(Train_end_year,Train_end_month+1), end = c(End_year,End_month),frequency=12)
if(Train_end_month == 12)
  Dataset_test<-window(Dataset,start = c(Train_end_year+1,1), end = c(End_year,End_month),frequency = 12)

####Trains####

for(i in 2:ncol(Dataset_train)){
  temp<-Dataset_train[,i]
  assign(paste(colnames(Dataset_train)[i],c("train"),sep = "_"),temp)
}

library("svDialogs")
selection<-dlgList(colnames(Dataset_train), preselect = NULL, multiple = TRUE, title = "Select your variables"
                   
                   ,gui = .GUI)$res

Indeps <- paste(paste(selection,c("train"),sep = "_"), collapse = "+")
ucm_eqn <- as.formula(paste("Beer_Volume_train", Indeps, sep = "~"))

ucm_mod <- ucm(ucm_eqn ,data=Dataset_train,
               #slope = T,
               level=T,season =T,season.length = 12)

ucm_mod
ucm_summary<-data.frame(ucm_mod$est)

########################################################
##                  2.MODEL SCORING                   ## 
########################################################

#######Tests#####

for(i in 2:ncol(Dataset_test)){
  temp<-Dataset_test[,i]
  assign(paste(colnames(Dataset_test)[i],c("test"),sep = "_"),temp)
}

Indeps_test <- paste(paste(selection,c("test"),sep = "_"), collapse = "+")
temp_eqn<-paste(Indeps_test,"SSMtrend(1, Q = ucm_mod$est.var.level)+SSMseasonal(12, Q = ucm_mod$est.var.season)",sep="+")
temp_eqn

#ImpComb
ucm_score_eqn <- as.formula(paste("rep(NA,nrow(Dataset_test))", temp_eqn, sep = "~"))
ucm_score_eqn

newdata <- SSModel(ucm_score_eqn
                   
                   #######for slope
                   #+SSMtrend(2, list(Q = ucm_mod$est.var.level,Q = ucm_mod$est.var.slope) )
                   ########       
                   ,H = ucm_mod$irr.var
                   ,data=Dataset_test
)
newdata

#Generate forecasts#
ucm.predict <- predict(ucm_mod$model, newdata = newdata)
#exp(ucm.predict)

Fitted_ucm<-fitted.values(ucm_mod$model)
Forecast<-c(Fitted_ucm,ucm.predict)

Actual_Vol<-  c(Beer_Volume_train, rep(NA,length(Forecast)-length(Beer_Volume_train)))
Actual_Predicted<-cbind.data.frame(Actual_Vol,Forecast)

#for(i in 1:nrow(Actual_Predicted))
#  Actual_Predicted$Actual_Vol[i]<- ifelse(is.na(Actual_Predicted$Actual_Vol[i]),"",Actual_Predicted$Actual_Vol[i])

write.csv(Actual_Predicted,"Actual_Predicted_ucm.csv",row.names = F)
cbind(month_backup$Month,Actual_Predicted)
write.csv(ucm_summary,"ucm_estimates.csv")

########################################################
##                     CODE ENDS HERE                 ## 
########################################################
