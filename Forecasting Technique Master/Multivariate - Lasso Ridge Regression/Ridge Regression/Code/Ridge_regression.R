############################################################################
############################################################################
# CODE: RIDGE REGRESSION
# AUTHOR: VEDANT PRASAD - GAC BANGALORE
# DATE: 3RD AUG 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
#Install required packages

rm(list=ls())

if(!require(glmnet)){install.packages("glmnet")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(forecast)){install.packages("forecast")}
if(!require(matrixStats)){install.packages("matrixStats")}

if(!require(car)){install.packages("car")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(fmsb)){install.packages("fmsb")}
if(!require(MASS)){install.packages("MASS")}
if(!require(QuantPsyc)){install.packages("QuantPsyc")}
if(!require(plyr)){install.packages("plyr")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(tibble)){install.packages("tibble")}
if(!require(sqldf)){install.packages("sqldf")}
if(!require(combinat)){install.packages("combinat")}

library(glmnet)
library(lubridate)
library(forecast)
library(matrixStats)

library(car)
library(reshape2)
library(fmsb)
library(MASS)
library(QuantPsyc)
library(plyr)
library(dplyr)
library(tibble)
library(sqldf)
library(combinat)
############################################################################
#INSTRUCTIONS
#TAKE LOG(TO BASE 10) FOR ALL INDEPENDENT VARS BEFORE RUNNING;NOT FOR DEPENDENT
#RUN "VARIABLE SELECTION - RIDGE REGRESSION" MODULE FOR SHORTLISTED
#FIRST COLUMN SHOULD BE "Month" IN %m/%d/%Y FORMAT
#SECOND COLUMN SHOULD BE "beer_volume"
############################################################################
#Set working directory

setwd("O:\\DATA\\Argentina\\Data")

############################################################################
#Read input file

raw_data <-read.csv(file.choose(),header=T)

###############################################################################
#User inputs required

country="Brazil"                ## name of the country
dep_var="beer_volume"   ## name of the dependent variable as in raw data
date_var="Month"                  ## name of the date variable as in raw data
num_min_var=3                    ## number of minimum variables in the model
num_max_var=4                    ## number of maximum variables in the model                              
global_start=2012                ## year from which data is available
global_end=2016                  ## year upto which data is available
cross_val_prd<-2  ## num of max years to go back from global end to train models

iteration<-1      ## number of the LR model iteration , depending on new set of variables or data
###############################################################################
#Start

raw_data[,date_var]<-as.Date(as.character(raw_data[,date_var]),format="%m/%d/%Y")
class(raw_data[,date_var])

################## creating automated names for files to be saved ############
##############################################################################

name<-paste0("Linear regression iter-",iteration)
date<-format(Sys.Date(),"%d%b%Y")

################### importing list of shortlisted variables ###################
################### including mandatory variables #############################

##    NOTE:header of the shortlisted variables 
##         file should be exactly like given format

short_listed_var<-read.csv("shortlist.csv",header=T)
### saving the list of variables selected for this particular iteration#######
write.csv(short_listed_var,paste0("variables considered for ",name," ,country-","run on",date,".csv"))
##### creating a vector of variables for creating various combinations ######
comb_all<-as.vector(short_listed_var$variables)

################### importing list of mandatory variables  ####################
###############################################################################

##    NOTE:header of the mandatory variables 
##         file should be exactly like given format

####default values if  no mandatory variables are selected ####################
mandate_var<-NULL
num_mandate_var<-0

mandate_var<-read.csv("mandatory.csv",header=T)
num_mandate_var<-length(mandate_var$variables)

not_mandate_var<-short_listed_var[-which(short_listed_var$variables %in% mandate_var$variables),"variables"]
comb_mandate<-as.vector(mandate_var$variables)
comb_not_mandate<-as.vector(not_mandate_var)

num_min_nm_var<-num_min_var-num_mandate_var
num_max_nm_var<-num_max_var-num_mandate_var


###############################################################################
#################### creating data for modelling           ####################

model_data<-raw_data[,which(colnames(raw_data) %in% 
                              c(date_var,dep_var,as.vector(short_listed_var$variables)))]


#model_data2<-raw_data[,which(colnames(raw_data) %in% 
#                             c(date_var,dep_var,as.vector(short_listed_var$variables),as.vector(short_listed_var$dir_flag)))]

model_data<-subset(model_data,!is.na(model_data$beer_volume))
model_data$beer_volume<-log10(model_data$beer_volume)

################### creating different combinations of ########################
################### independent variables to run model iterations #############

Masterlist_combo <- data.frame(No_of_pred = double())
## this master list of combination will have list of variables for each iteration
## each row of this file corresponds to one distinct ieration
for (j in num_min_nm_var:num_max_nm_var){
  No_of_pred <- j+num_mandate_var
  da1 <- data.frame(t(data.frame(combn(comb_not_mandate, j))))
  da_c <- cbind.data.frame(No_of_pred,da1)
  
  Masterlist_combo <- rbind.fill(Masterlist_combo, da_c)
}

temp<-ncol(Masterlist_combo)
#######################
da1 <- data.frame(t(data.frame(combn(comb_mandate,num_mandate_var))))
rownames(da1)<-NULL
colnames(da1)<-c(paste0("Y",seq(1:ncol(da1)),by=""))
Masterlist_combo<-cbind(Masterlist_combo,da1)
colnames(Masterlist_combo)<-c("NO_of_pred",paste0("X",seq(1:(ncol(Masterlist_combo)-1)),by=""))
Masterlist_combo_1 <- Masterlist_combo[,-1]
## creating the RHS of the fit modelled based on variables for each iteration
Eq <- data.frame(equation = character())
for (i in 1:nrow(Masterlist_combo_1)){
  comb <- data.frame(t(Masterlist_combo_1[i,]))
  comb <- na.omit(comb)
  comb[,1] <- as.character(comb[,1])
  RHS <- data.frame(paste(comb[,1], collapse  = "+"))
  names(RHS) <- "equation"
  Eq <- rbind.data.frame(Eq,RHS)
}

############### selecting model iterations with mandatory variables #############
#################################################################################

selected_layer<-data.frame(equation=Eq$equation)
selected_layer$equation<-as.character(selected_layer$equation)
class(selected_layer$equation)
selected_layer$Model_iter_num<-c(paste("Model_iter",1:nrow(selected_layer),sep="_")) 

############# creating data structure for data generated ########################
#################################################################################

Model_LR_fitted_actual<- data.frame(Model_iter_num=character(),
                                    Mod_eq = character(),
                                    No_of_indep = integer(),
                                    Trained_upto_year=integer(),
                                    Fitted = double(),
                                    Actual = double(),
                                    Percent_Error = double()
)

Model_LR_Summary <- data.frame(Model_iter_num=character(),
                               Mod_eq = character(),
                               No_of_indep = integer(),
                               Trained_upto_year=integer(),
                               Variables = character(),
                               Estimate = double()
)

Model_Dir_Summary <- data.frame(Model_iter_num=character(),
                                Mod_eq = character(),
                                No_of_indep = integer(),
                                Trained_upto_year=integer(),
                                Variables = character(),
                                mod_dir_flag=integer(),
                                dir_flag=integer()
)


############## fitting models and getting the statistics ########################
#################################################################################

for(i in 1:nrow(selected_layer)){       
  print(paste0("i=",i))
  ##outside loop selects a particular set of variables from selected combinations
  mod_eq<-paste(dep_var,selected_layer$equation[i],sep="~")
  no_of_ind<-(sum(charToRaw(mod_eq)==charToRaw("+"))+1)
  actual<-model_data[which(year(model_data[,date_var])<=global_end),dep_var] 
  for(j in 1:cross_val_prd){
    print(paste0("j=",j))
    ## inside loop fit models for  different training periods and
    ## creating test and train datasets
    
    train<-model_data[which(year(model_data[,date_var])<=(global_end-j)),
                      -which(colnames(model_data) %in% c(date_var))]
    test<-model_data[which(year(model_data[,date_var])>(global_end-j) 
                           & year(model_data[,date_var])<=global_end),
                     -which(colnames(model_data) %in% c(date_var))]
    
    # Construct design matrix
    
    ridge_x <- data.matrix(train[,c(2:ncol(train))])
    ridge_y <- train$beer_volume
    
    # Model selection
    
    set.seed(1)
    
    cv.ridge <- cv.glmnet(x = ridge_x, y = ridge_y, alpha = 0)
    
    # Applying ridge
    
    lamb <- cv.ridge$lambda.1se
    
    fit.ridge <- glmnet(x = ridge_x, y = ridge_y,alpha = 0, lambda = lamb)
    
    
    # Validating the model against validation data
    
    getcoeff <- function(method = "method"){
      #ridge_coeff <- as.data.frame(as.matrix(coef(cv.ridge, s = method)))
      ridge_coeff <- as.data.frame(as.matrix(coef(cv.ridge, s = method)))
      ridge_coeff$variables <- row.names(ridge_coeff)
      row.names(ridge_coeff) <- NULL
      ridge_coeff <- ridge_coeff[,c(2,1)]
      names(ridge_coeff) <- c("Variables", "Coefficient")
      ridge_coeff$abscoeff <- abs(ridge_coeff$Coefficient)
      ridge_coeff <- subset(ridge_coeff, ridge_coeff$abscoeff != 0)
      ridge_coeff <- ridge_coeff[order(-ridge_coeff$abscoeff),]
      return (ridge_coeff)
    }
    
    ridge_coeff1se <- getcoeff(method = "lambda.1se")
    
    #ridge_coeffMSE <- getcoeff(method = "lambda.min")
    #class(ridge_coeff1se)
    #ridge_coeff1se
    #ridge_coeffMSE
    
    ## fitting ridge regression model  
    ridge_train <- data.matrix(train[,c(2:ncol(train))])
    pred_train <- predict(fit.ridge, as.matrix(ridge_train))
    
    ridge_test <- data.matrix(test[,c(2:ncol(test))])
    pred_test <- predict(fit.ridge, as.matrix(ridge_test))
    
    ##creating fitted and actual dataset
    #fit_val<-c(as.vector(predict(fit,as.matrix(train))),as.vector(predict(fit,as.matrix(test))))
    
    fitted_val<-c(as.vector(pred_train),as.vector(pred_test))
    fit_val<-10^fitted_val
    
    #Change 10 power here
    pe<-(fit_val-(10^actual))/(10^actual)
    
    fit_fitted_actual<-data.frame(Model_iter_num=selected_layer$Model_iter_num[i],
                                  Mod_eq = mod_eq,
                                  No_of_indep = no_of_ind,
                                  Trained_upto_year=(global_end-j),
                                  Fitted = fit_val,
                                  Actual = (10^actual),
                                  Percent_Error = pe
    )
    
    fit_fitted_actual[,date_var]<-model_data[which(year(model_data[,date_var])<=global_end),date_var]
    
    if(i==1 & j==1){
      Model_LR_fitted_actual<-rbind(Model_LR_fitted_actual,fit_fitted_actual)
    }else{
      Model_LR_fitted_actual<-rbind.fill(Model_LR_fitted_actual,fit_fitted_actual)
    }
    
    
    ## creating model summary dataset
    ridge_coeff1se
    summary<-data.frame(Variables=ridge_coeff1se[,1])
    summary$Model_iter_num<-selected_layer$Model_iter_num[i]
    summary$Mod_eq=mod_eq
    summary$No_of_indep<-no_of_ind
    summary$Trained_upto_year=(global_end-j)
    summary<-summary[,c(2:5,1)]
    summary$Estimate = ridge_coeff1se[,2]
    summary$dir_flag<-ifelse(summary$Estimate>=0,1,0)
    summary[which(summary$Variables=="(Intercept)"),"dir_flag"]<-NA
    
    if(i==1 & j==1){
      Model_LR_Summary<-rbind(Model_LR_Summary,summary)
    }else{
      Model_LR_Summary<-rbind.fill(Model_LR_Summary,summary) 
    }
    
    #### creating dataset with estimates direction summary ###############
    dir_summary<-summary
    dir_summary$mod_dir_flag<-dir_summary$dir_flag
    dir_summary$dir_flag<-NULL
    dir_summary<-merge.data.frame(dir_summary,short_listed_var,
                                  by.x=c("Variables"),by.y=c("variables"),all.x=T)
    dir_summary$cor_dir_flag<-ifelse(dir_summary$mod_dir_flag==dir_summary$dir_flag,1,0)
    dir_summary<-dir_summary[,c(2:5,1,6:ncol(dir_summary))]
    
    if(i==1 & j==1){
      Model_Dir_Summary <- rbind(Model_Dir_Summary,dir_summary)
    }else{
      Model_Dir_Summary <- rbind.fill(Model_Dir_Summary,dir_summary)
    }
    
  }
}

########### code to export fitted values data and model summary data ##########
###############################################################################

write.csv(Model_LR_fitted_actual,paste0(name," fitted values for ",country," run on ",date,".csv"))
write.csv(Model_LR_Summary,paste0(name," summary for ",country," run on ",date,".csv"))
write.csv(Model_Dir_Summary,paste0(name,"summary for est directions of ",country," run on ",date,".csv"))

####### code to summarize the model iterations and finding best iteration #####
###############################################################################

#Start with fitted values

##### subsetting for outsample data ###########################################
Model_LR_fitted_actual$year<-year(Model_LR_fitted_actual[,date_var])
Model_LR_fitted_actual$out.flag<-(Model_LR_fitted_actual$year)-(Model_LR_fitted_actual$Trained_upto_year)
outsample<-Model_LR_fitted_actual[which(Model_LR_fitted_actual$out.flag>=0),]
############### calculate annual error ######################################## 
by_vars <- group_by(outsample, Model_iter_num,Mod_eq, No_of_indep, Trained_upto_year, year)
outsample1<-outsample %>%
  group_by(Model_iter_num,Mod_eq, No_of_indep, Trained_upto_year, year) %>%
  summarize(
    Annual.Forecast=sum(Fitted),
    Annual.Actual=sum(Actual),
    Annual.PE=(Annual.Forecast-Annual.Actual)/Annual.Actual,
    MAPE=mean(Percent_Error)
  )
outsample1$Annual.Accuracy=(1-abs(outsample1$Annual.PE))
outsample1$Annual.Accuracy[which(outsample1$Annual.Accuracy<0)]<-0
############## calculating annual actuals lag #################################
lag<-unique(data.frame(year=outsample1$year,Annual.Actual=outsample1$Annual.Actual))                 
lag<-lag[order(lag$year),]
lag$Annual.Actual.Lag<-c(NA,lag$Annual.Actual[1:(nrow(lag)-1)])
lag$Annual.Actual<-NULL
## calculatiing actual/forecasted annual growth for each model iteration and for
## each validation period
outsample1<-merge.data.frame(outsample1, lag, by="year", all.x = TRUE)
outsample1<-outsample1[,c(2:5,1,6:ncol(outsample1))]
outsample1<-arrange(outsample1,Model_iter_num,Mod_eq, No_of_indep, Trained_upto_year, year)
outsample1<-outsample1[-which(outsample1$Trained_upto_year==outsample1$year),]
outsample1$Forecast.Growth<-(outsample1$Annual.Forecast-outsample1$Annual.Actual.Lag)/outsample1$Annual.Actual.Lag
outsample1$Actual.Growth<-(outsample1$Annual.Actual-outsample1$Annual.Actual.Lag)/outsample1$Annual.Actual.Lag
## direction-flag if the forecasted growth and actual growth have same direction
outsample1$Growth.Dir.Flag<-ifelse(
  (outsample1$Forecast.Growth>=0 & outsample1$Actual.Growth>=0)|
    (outsample1$Forecast.Growth<=0 & outsample1$Actual.Growth<=0)
  ,1,0)
##### export the datatset with predicted/ctual forecasted growths annual errors
write.csv(outsample1,paste0(name," fitted annual growths for ",country," run on ",date,".csv"))
#### find the model iterations with correct predicted growth direction in every 
#### cross validation in all cross validation periods
max_dir.flag<-0
for( i in 1: cross_val_prd){
  max_dir.flag<-max_dir.flag+i
}
outsample2<-outsample1 %>%
  group_by(Model_iter_num,Mod_eq, No_of_indep) %>%
  mutate(
    Sum.Growth.Dir.Flag=sum(Growth.Dir.Flag)
  )
outsample2<-unique(outsample2[which(outsample2$Sum.Growth.Dir.Flag==max_dir.flag),])
####### finding which model iterations have all est with correct directions#####
Est_Dir<-Model_Dir_Summary %>%
  group_by(Model_iter_num,Mod_eq) %>%
  summarize(
    Mean.cor_dir_flag=mean(cor_dir_flag,na.rm=T)
  )
### finding of all models which fcsted growth direction correctly have
### estimates with correct directions 
outsample2<-merge.data.frame(outsample2,Est_Dir,
                             by.x=c("Model_iter_num"),by.y=c("Model_iter_num")
                             ,all.x=T)
outsample2<-outsample2[which(outsample2$Mean.cor_dir_flag==1),]
#### export the best iteration annual numbers ###############################
write.csv(outsample2,paste0(name," with correct growth  & est. directions for ",country," run on ",date,".csv"))

##################### CODE ENDS HERE ############################################
#################################################################################