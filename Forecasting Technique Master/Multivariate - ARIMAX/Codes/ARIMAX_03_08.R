#####################################################################################################################################
######################################### A. INSTALL REQUIRED PACKAGES ##################################################

if(!require(tseries)){install.packages("tseries")};library(tseries)
if(!require(forecast)){install.packages("forecast")};library(forecast)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(fmsb)){install.packages("fmsb")};library(fmsb)
if(!require(fpp)){install.packages("fpp")};library(fpp)
if(!require(lmtest)){install.packages("lmtest")};library(lmtest)
if(!require(plyr)){install.packages("plyr")};library(plyr)
if(!require(ggplot2)){install.packages("ggplot2")};library(ggplot2)
if(!require(reshape)){install.packages("reshape")};library(reshape)
if(!require(stargazer)){install.packages("stargazer")};library(stargazer  )
if(!require(rmarkdown)){install.packages("rmarkdown")};library(rmarkdown)
if(!require(knitr)){install.packages("knitr")};library(knitr)
if(!require(tcltk)){install.packages("tcltk")};library(tcltk)
if(!require(stringi)){install.packages("stringi")};library(stringi)

#######################################################################################################################

######################################## B. USER MANUAL INPUTS BEGINS ########################################################
# 1: ENTER COUNTRY NAME:          
Country<-"Aus"        

# 2: ENTER START YEAR OF YOUR DATA
start_yr<-2009

# 3: ENTER THE START MONTH
start_month<-1

# 4: ENTER END YEAR: 
#NOTE: (It should be the last year with all 12 months of data available)
end_yr<-2016


# 5: ENTER THE MINIMUM COMBINATION OF REGRESSOR VARIABLES: 
num_min_var=3                   

# 6: ENTER THE MAXIMUM COMBINATION OF REGRESSOR VARIABLES:
num_max_var=6                             

# 7: ENTER THE REGRESSOR NAMES WHICH ARE MANDATE IN EVERY ITERATIONS
reg_names_for_iteration<-c("Cider.Perry.Volume","Currency.in.Circulation")

# 8: ENTER THE LATEST YEAR: 
#NOTE: (Eg. 2017 might have 2-3 months but not all the months)
Most_recent_yr<-2017

# 9: ENTER THE LATEST MONTH OF THE LATEST YEAR 
#NOTE:(Eg. Have Data only till Feb of 2017:
Most_recent_month<-2

# 10: NUMBER OF PERIODS TO FORECAST AHEAD
#NOTE: (Only to applied for Latest year)
Forecast_period<-12

## 11: ENTER THE DIRECTORY ADDRESS TO SAVE RESULTS:


######################################### USER MANUAL INPUT ENDS ###################################################

######################################## C. IMPORT RAW DATA ###########################################################

msg1<-tkmessageBox(title="Attention:",message="Please, upload the raw data!!!")
data_Arima<-read.csv(file.choose(),header=T)

###################################### IMPORT SHORTLISTED REGRESSOR NAMES ########################################

msg2<-tkmessageBox(title="Attention:",message="Please, upload the Regressor Names in a .CSV flat file")
short_listed_var<-read.csv(file.choose(),header=T)
comb_all<-as.vector(short_listed_var$Var)


####################################################################################################################

######################################### D. DATA PREPARATION BEGINS ###################################################

colnames(data_Arima)[1:2]<-c("Month","Beer_Volume")
data_Arima$Month<-as.Date(data_Arima[,1],"%m/%d/%Y")
data_Arima$year<-year(data_Arima$Month)
data_Arima$Beer_Volume<-as.numeric(data_Arima$Beer_Volume)
backup_data<-data_Arima
dep_var="Beer_Volume"  
date_var="Month"
end_month<-12
end_yr_train<-as.numeric(end_yr)-1
start_yr_test<-as.numeric(end_yr_train)+1
end_yr_test<-as.numeric(end_yr)
latest_date<-as.Date(paste(end_yr,end_month,1),"%Y%m%d")
earliest_date<-as.Date(paste(start_yr,start_month,1),"%Y%m%d")

#mandate_var<-read.csv(file.choose(),header=T)
mandate_var<-data.frame(var1=reg_names_for_iteration)
num_mandate_var<-length(mandate_var$var1)

## CREATE MODEL DATA 

data_Arima<-data_Arima[,which(colnames(data_Arima) %in% 
                                c(date_var,dep_var,as.vector(short_listed_var$Var)))]


# CONVERT Y-VARIABLE TO TIME-SERIES
y<-ts(data_Arima[,2],start = c(start_yr,start_month),end=c(end_yr,end_month),frequency = 12)


## CREATE DIFFERENT COMBINATIONS OF REGRESSORS FOR ITERATIONS ########################

Masterlist_combo <- data.frame(No_of_pred = double())
for (j in num_min_var:num_max_var){
  No_of_pred <- j
  da1 <- data.frame(t(data.frame(combn(comb_all, j))))
  
  da_c <- cbind.data.frame(No_of_pred,da1)
  
  Masterlist_combo <- rbind.fill(Masterlist_combo, da_c)
}
Masterlist_combo_1 <- Masterlist_combo[,-1]
combi_1<-data.frame(t((Masterlist_combo_1)))

colnames(combi_1)<-paste("Col_",seq(1:ncol(combi_1)))
rownames(c)<-NULL
Select_combination<-list()  

## CONFORMING MANDATE VARIABLES IN THE MIX OF REGRESSOR COMBINATIONS
num_true=0
tt1=1
for(tt in (1:ncol(combi_1))){
  test1<-data.frame(table(as.character(na.omit(combi_1[,tt])) %in% as.character(mandate_var[,1])))
  num_true<-test1[test1[,1]==TRUE,2]
  num_true<-ifelse(any(test1[,1]==TRUE),test1[test1[,1]==TRUE,2],0)
  
  if(num_true==num_mandate_var){
    Select_combination[[tt1]]<-combi_1[,tt]
    tt1=tt1+1
  }
}
Select_combination <- t(data.frame(stri_list2matrix(Select_combination,byrow = T)))
colnames(Select_combination)<-paste("Col_",seq(1:ncol(Select_combination)),sep="")

View(Select_combination)
########################################## END:DATA PREPARATION ###################################################
ncol(Select_combination)

####################################### E. BEGIN: ITERATION LOOP #############################################

iteration=1
counter_4<-0
counter_5<-0
counter_6<-0
reg_val_flag<-0

for(iteration in 1: ncol(Select_combination)){
  #  tryCatch({
  end_yr_train<-as.numeric(end_yr)-1   #Start all over
  start_yr_test<-as.numeric(end_yr_train)+1 #Start all over
  counter_3=0
  x_var_log_iter<-data.frame(NULL)
  
  x_var<-data.frame(Ind=as.character(Select_combination[!is.na(Select_combination[,iteration]),iteration]))
  
  for(sl_itr in 1: nrow(x_var)){
    v1<-as.character(x_var[sl_itr,])
    
    if(counter_3==0){
      x_var_log_iter<-ts(data_Arima[,v1])
      counter_3=counter_3+1
    }else {
      x_var_log_iter<-cbind(x_var_log_iter,ts(data_Arima[,v1]))
    }
  }
  
  #Nameing the columns of regressors
  colnames(x_var_log_iter)<-unlist(Select_combination[!is.na(Select_combination[,iteration]),iteration])
  
  
  
  #Creating dataframe of regression parameters (Iterations Basis)
  x<-ts.union(x_var_log_iter)
  
  #Getting the variable names
  reg_var_names<-data.frame(Select_combination[!is.na(Select_combination[,iteration]),iteration])
  reg_var_names<-paste(unlist(reg_var_names),collapse = "; ") 
  
  
  ### START LOOP FOR DIFFERENT TRAINING YEARS
  
  best_model_order<-0
  one_time_loop<-0
  t<-1
  control1<-0
  
  for(t in 1:3){
    
    #Creation of training and test dataset for Beer volume
    
    data_train<- window(y,start=c(start_yr,start_month),end=c(end_yr_train,12))
    data_test<- window(y,start=c(start_yr_test,1),end=c(end_yr_test,12))
    
    #Fetching num of rows for xreg
    
    xreg_train_row_end<-length(data_train)
    xreg_test_row_begin<-length(data_train)+1
    xreg_test_row_end<-length(y)
    
    #Creating train and test data for Regressors
    
    x_train<- x[1:xreg_train_row_end,]
    x_test<- x[xreg_test_row_begin:xreg_test_row_end,]
    tryCatch({
      x_forecast<-x[(xreg_test_row_end+1):(xreg_test_row_end+12),]
    },error=function(e){cat("ERROR :",conditionMessage(e),"Regressors doesnt have recent period data", "\n")})
    
    
    auto_out<-NULL
    if(one_time_loop==0){
      tryCatch({
        auto_arima_fit<-auto.arima(data_train,xreg = x_train)
        auto_arima_fst<-forecast(auto_arima_fit,h = length(data_test),xreg = x_test)$mean
        auto_arima_mape_out<-accuracy(auto_arima_fst,data_test)[5]
        auto_arima_mape_in<-sum(abs(auto_arima_fit$residuals)/data_train)/length(data_train)*100
        auto_arima_orders<-arimaorder(auto_arima_fit)
        auto_arima_annual_error<-abs((sum(data_test)-sum(auto_arima_fst))/sum(data_test))*100
        auto_out<-rbind(list=c(iteration,
                               ifelse(anyNA(arimaorder(auto_arima_fit)[1])==TRUE,0,arimaorder(auto_arima_fit)[1]),
                               ifelse(anyNA(arimaorder(auto_arima_fit)[2])==TRUE,0,arimaorder(auto_arima_fit)[2]),
                               ifelse(anyNA(arimaorder(auto_arima_fit)[3])==TRUE,0,arimaorder(auto_arima_fit)[3]),
                               ifelse(anyNA(arimaorder(auto_arima_fit)[4])==TRUE,0,arimaorder(auto_arima_fit)[4]),
                               ifelse(anyNA(arimaorder(auto_arima_fit)[5])==TRUE,0,arimaorder(auto_arima_fit)[5]),
                               ifelse(anyNA(arimaorder(auto_arima_fit)[6])==TRUE,0,arimaorder(auto_arima_fit)[6]),
                               ifelse(anyNA(arimaorder(auto_arima_fit)[7])==TRUE,12,arimaorder(auto_arima_fit)[7]),
                               auto_arima_mape_out,auto_arima_annual_error,auto_arima_fit$aic,auto_arima_mape_in,1))
        colnames(auto_out)<-c("Iteration_num","p","d","q","P","D","Q","Period","Outsample MAPE","Annual_Error","AIC","Insample MAPE","Serial_No")
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Best Model Selection", "\n")})
      
      ### LOOP TO CHECK VARIOUS COMBINATION OF ORDERS FOR ARIMAX
      
      j=0
      p <- 0
      dif <- 0
      q <- 0
      P <- 0
      Q <- 0
      Serial_No=2  
      
      for (p in 0:2){
        for(q in 0:2){
          for (P in 0:1){
            for (Q in 0:1){
              for(dif in 0:1){
                
                tryCatch({
                  
                  fit<-arima(data_train,order=c(p,dif,q),seasonal = list(order=c(P,1,Q),period=12)
                             ,xreg = x_train)
                  d <- sum(abs(fit$residuals)/(data_train))
                  MAPE_In <- (100 * d)/xreg_train_row_end
                  
                  pred<-predict(fit,n.ahead=length(data_test),newxreg = x_test)
                  accu<-accuracy(ts((pred$pred)),ts(data_test))
                  annual_error<- 100*abs((sum(data_test)-sum(pred$pred))/sum(data_test))
                  
                  out<-rbind(list=c(iteration,arimaorder(fit),accu[5],annual_error,fit$aic,MAPE_In,Serial_No))
                  
                  Serial_No=Serial_No+1
                  
                  if(j==0)
                  {
                    arimax_output<-data.frame(out)
                    j=j+1
                  }
                  else
                  {
                    arimax_output<-rbind(arimax_output,data.frame(out))
                    
                  }},error=function(e){cat("ERROR :",conditionMessage(e),"During Best Model selection Loop. At iteration-",iteration, "\n")})
              }
            }
          }
        }
      }
      
      ## arimax_output<-NULL
      
      colnames(arimax_output)<-c("Iteration_num","p","d","q","P","D","Q","Period","Outsample MAPE","Annual_Error","AIC","Insample MAPE","Serial_No")
      
      
      #Combining the auto arima result with the results of looped arima
      
      arimax_output<-rbind(arimax_output,auto_out)
      
      one_time_loop=one_time_loop+1
    }
    
    
    
    
    ## CHOOSING THE BEST MODEL
    
    if(best_model_order==0){
      #Order model outputs by lowest AIC value
      result_1<-arimax_output[order(arimax_output[,"AIC"]),]
      #Order model outputs by lowest out sample MAPE
      result_2<-arimax_output[order(arimax_output[,"Outsample MAPE"]),]
      #Order model outputs by lowest in sample MAPE
      result_3<-arimax_output[order(arimax_output[,"Insample MAPE"]),]
      #Order Model Outputs by Auto ARIMA result
      result_4<-arimax_output[order(arimax_output[,"Serial_No"]),]
      best_model_order=best_model_order+1
    }  
    
    
    ############################# F. RUN CHAMPION MODELS BASED ON ALL 4 ORDER SELECTION CRITERIA ###########################
    
    final_1<-NULL;final_2<-NULL;final_3<-NULL;final_4<-NULL
    
    #Error Handling::
    tryCatch({
      #Model 1
      final_1<-arima(data_train,order=c(result_1[1,"p"],result_1[1,"d"],result_1[1,"q"]),
                     seasonal = list(order=c(result_1[1,"P"],1,result_1[1,"Q"]),period=12),
                     xreg=x_train)
    },error=function(e){cat("ERROR :",conditionMessage(e),"During Champion Model fitting.Based on Lowest AIC. During iteration & Training year-",iteration,end_yr_train, "\n")})
    
    #Model 2
    tryCatch({
      final_2<-arima(data_train,order=c(result_2[1,"p"],result_2[1,"d"],result_2[1,"q"]),
                     seasonal = list(order=c(result_2[1,"P"],1,result_2[1,"Q"]),period=12),
                     xreg=x_train)
    },error=function(e){cat("ERROR :",conditionMessage(e),"During Champion Model fitting.Based on Lowest OutSample MAPE.During iteration & Training year-",iteration,end_yr_train, "\n")})
    
    #Model 3
    tryCatch({
      final_3<-arima(data_train,order=c(result_3[1,"p"],result_3[1,"d"],result_3[1,"q"]),
                     seasonal = list(order=c(result_3[1,"P"],1,result_3[1,"Q"]),period=12),
                     xreg=x_train)
    },error=function(e){cat("ERROR :",conditionMessage(e),"During Champion Model fitting.Based on Lowest INSample MAPE. During iteration & Training year-",iteration,end_yr_train, "\n")})
    
    tryCatch({
      #Model 4
      final_4<-arima(data_train,order=c(result_4[1,"p"],result_4[1,"d"],result_4[1,"q"]),
                     seasonal = list(order=c(result_4[1,"P"],result_4[1,"D"],result_4[1,"Q"]),period=12),
                     xreg=x_train)
    },error=function(e){cat("ERROR :",conditionMessage(e),"During Champion Model fitting.Based on AUto ARIMA. During iteration & Training year-",iteration,end_yr_train, "\n")})  
    
    ######################################## CHAMPION MODEL ENDS ###########################################################
    
    ###################################### G. ACCURACY CALCULATIONS FOR CHAMPION MODEL  #######################################
    
    results_f_1<-NULL;accu_1<-NULL;arimax_accu_1<-NULL;fit_accu_1<-NULL;actuals_fitted_1<-NULL;otp_1<-data.frame(NULL);date_fst_1<-NULL;coeff_1<-data.frame(NULL)
    results_f_2<-NULL;accu_2<-NULL;arimax_accu_2<-NULL;fit_accu_2<-NULL;actuals_fitted_2<-NULL;otp_2<-data.frame(NULL);date_fst_2<-NULL;coeff_2<-data.frame(NULL)
    results_f_3<-NULL;accu_3<-NULL;arimax_accu_3<-NULL;fit_accu_3<-NULL;actuals_fitted_3<-NULL;otp_3<-data.frame(NULL);date_fst_3<-NULL;coeff_3<-data.frame(NULL)
    results_f_4<-NULL;accu_4<-NULL;arimax_accu_4<-NULL;fit_accu_4<-NULL;actuals_fitted_4<-NULL;otp_4<-data.frame(NULL);date_fst_4<-NULL;coeff_4<-data.frame(NULL)
    
    ### Model 1::Begins Based on Lowest AIC value
    #Out sample monthly accuracy
    tryCatch({
      results_f_1<-predict(final_1,n.ahead=length(data_test),newxreg=x_test)
      accu_1<-1-(abs(as.numeric(data_test)-(as.numeric(results_f_1$pred)))/as.numeric(data_test))
      arimax_accu_1<-data.frame(as.numeric(data_test),as.numeric(results_f_1$pred),accu_1)
      colnames(arimax_accu_1)<-c("Actual_value","Predicted_value","Accuracy")
      #In Sample Monthly accuracy
      fit_accu_1<-1-abs(final_1$residuals/data_train)
      actuals_fitted_1<-cbind.data.frame(Actual_value=as.numeric(data_train),Predicted_value=as.numeric(fitted(final_1)),
                                         Accuracy=as.numeric(fit_accu_1))
      #combine in and out sample accuracies
      otp_1<-rbind(actuals_fitted_1,arimax_accu_1)
      date_fst_1=seq.Date(earliest_date,data_Arima[nrow(otp_1),1],by="1 month")
      otp_1<-cbind.data.frame(Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Lowest AIC",Year=year(date_fst_1),Month=date_fst_1,otp_1,Regressors=reg_var_names)
      
      #Coeffcients for Model 1::
      coeff_1<-coeftest(final_1)
      coeff_1<-cbind(Regressors=rownames(coeff_1),Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Lowest AIC",Estimate=coeff_1[,1],Std_Error=coeff_1[,2],z_value=coeff_1[,3],"Pr(>|z|)"=coeff_1[,4],Regressors=reg_var_names)
      
    },error=function(e){cat("ERROR :",conditionMessage(e),"At Champion Model-Accuracy calculation.LOwest AIC value.Iteration-",iteration,"Training Year-",end_yr_train, "\n")})
    
    #####Model 2::Begins Based on Lowest Outsample MAPE
    #Out sample monthly accuracy
    tryCatch({
      results_f_2<-predict(final_2,n.ahead=length(data_test),newxreg=x_test)
      accu_2<-1-(abs(as.numeric(data_test)-(as.numeric(results_f_2$pred)))/as.numeric(data_test))
      arimax_accu_2<-data.frame(as.numeric(data_test),as.numeric(results_f_2$pred),accu_2)
      colnames(arimax_accu_2)<-c("Actual_value","Predicted_value","Accuracy")
      #In Sample Monthly accuracy
      fit_accu_2<-1-abs(final_2$residuals/data_train)
      actuals_fitted_2<-cbind.data.frame(Actual_value=as.numeric(data_train),Predicted_value=as.numeric(fitted(final_2)),
                                         Accuracy=as.numeric(fit_accu_2))
      #combine in and out sample accuracies
      otp_2<-rbind(actuals_fitted_2,arimax_accu_2)
      date_fst_2=seq.Date(earliest_date,data_Arima[nrow(otp_2),1],by="1 month")
      otp_2<-cbind.data.frame(Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Lowest OutSample Mape",Year=year(date_fst_2),Month=date_fst_2,otp_2,Regressors=reg_var_names)
      
      #Coeffcients for Model 2::
      coeff_2<-coeftest(final_2)
      coeff_2<-cbind(Regressors=rownames(coeff_2),Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Lowest OutSample Mape",Estimate=coeff_2[,1],Std_Error=coeff_2[,2],z_value=coeff_2[,3],"Pr(>|z|)"=coeff_2[,4],Regressors=reg_var_names)
      
    },error=function(e){cat("ERROR :",conditionMessage(e),"At Champion Model-Accuracy calculation.LOwest OutSample Mape.Iteration-",iteration,"Training Year-",end_yr_train, "\n")})  
    
    ### Model 3::Begins Based on Lowest InSample MAPE
    #Out sample monthly accuracy
    tryCatch({
      results_f_3<-predict(final_3,n.ahead=length(data_test),newxreg=x_test)
      accu_3<-1-(abs(as.numeric(data_test)-(as.numeric(results_f_3$pred)))/as.numeric(data_test))
      arimax_accu_3<-data.frame(as.numeric(data_test),as.numeric(results_f_3$pred),accu_3)
      colnames(arimax_accu_3)<-c("Actual_value","Predicted_value","Accuracy")
      #In Sample Monthly accuracy
      fit_accu_3<-1-abs(final_3$residuals/data_train)
      actuals_fitted_3<-cbind.data.frame(Actual_value=as.numeric(data_train),Predicted_value=as.numeric(fitted(final_3)),
                                         Accuracy=as.numeric(fit_accu_3))
      #combine in and out sample accuracies
      otp_3<-rbind(actuals_fitted_3,arimax_accu_3)
      date_fst_3=seq.Date(earliest_date,data_Arima[nrow(otp_3),1],by="1 month")
      otp_3<-cbind.data.frame(Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Lowest INSample Mape",Year=year(date_fst_3),Month=date_fst_3,otp_3,Regressors=reg_var_names)
      
      #Coeffcients for Model 3::
      coeff_3<-coeftest(final_3)
      coeff_3<-cbind(Regressors=rownames(coeff_3),Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Lowest INSample Mape",Estimate=coeff_3[,1],Std_Error=coeff_3[,2],z_value=coeff_3[,3],"Pr(>|z|)"=coeff_3[,4],Regressors=reg_var_names)
      
    },error=function(e){cat("ERROR :",conditionMessage(e),"At Champion Model-Accuracy calculation.LOwest InSample MAPE.Iteration-",iteration,"Training Year-",end_yr_train, "\n")})  
    
    #####Model 4::Begins Based on Auto ARIMA
    #Out sample monthly accuracy
    tryCatch({
      results_f_4<-predict(final_4,n.ahead=length(data_test),newxreg=x_test)
      accu_4<-1-(abs(as.numeric(data_test)-(as.numeric(results_f_4$pred)))/as.numeric(data_test))
      arimax_accu_4<-data.frame(as.numeric(data_test),as.numeric(results_f_4$pred),accu_4)
      colnames(arimax_accu_4)<-c("Actual_value","Predicted_value","Accuracy")
      #In Sample Monthly accuracy
      fit_accu_4<-1-abs(final_4$residuals/data_train)
      actuals_fitted_4<-cbind.data.frame(Actual_value=as.numeric(data_train),Predicted_value=as.numeric(fitted(final_4)),
                                         Accuracy=as.numeric(fit_accu_4))
      #combine in and out sample accuracies
      otp_4<-rbind(actuals_fitted_4,arimax_accu_4)
      date_fst_4=seq.Date(earliest_date,data_Arima[nrow(otp_4),1],by="1 month")
      otp_4<-cbind.data.frame(Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Auto Arima",Year=year(date_fst_4),Month=date_fst_4,otp_4,Regressors=reg_var_names)
      
      #Coeffcients for Model 4::
      coeff_4<-coeftest(final_4)
      coeff_4<-cbind(Regressors=rownames(coeff_4),Iteration_num=iteration,Trained_Year=end_yr_train,Based_On="Auto Arima",Estimate=coeff_4[,1],Std_Error=coeff_4[,2],z_value=coeff_4[,3],"Pr(>|z|)"=coeff_4[,4],Regressors=reg_var_names)
      
    },error=function(e){cat("ERROR :",conditionMessage(e),"At Champion Model-Accuracy calculation.AUto ARIMA.Iteration-",iteration,"Training Year-",end_yr_train, "\n")})
    
    ### Combining All Models for All Iterations
    
    if(control1==0) {
      final_otp<-rbind(otp_1,otp_2,otp_3,otp_4)
      final_coeff<-rbind(coeff_1,coeff_2,coeff_3,coeff_4)
      control1=control1+1
    }else {
      final_otp<-rbind(final_otp,otp_1,otp_2,otp_3,otp_4)
      final_coeff<-rbind(final_coeff,coeff_1,coeff_2,coeff_3,coeff_4)
    }
    
    #Counter for training years
    end_yr_train<-end_yr_train-1
    start_yr_test<-start_yr_test-1
    
  } #t loop       
  
  ################################# ACCURACY CALCULATION OF CHAMPION MODELS ENDs  #########################################
  
  ################################### END: Running for different Training Years ########################################
  
  ###################################### H. YOY MARKET GROWTH CALCULATION #########################################
  tryCatch({
    yoy_results<-aggregate(final_otp[,c("Actual_value","Predicted_value")],final_otp[,c("Regressors","Based_On","Trained_Year","Year")],FUN = sum)
    yoy_results<-yoy_results[order(yoy_results$Based_On,yoy_results$Trained_Year),]
    yoy_results<-cbind.data.frame(yoy_results,Lag_actual=c(NA,yoy_results$Actual_value[1:nrow(yoy_results)-1]))
    yoy_results$Lag_actual[yoy_results$Year==start_yr]<-NA
    yoy_results<-cbind.data.frame(Iteration_num=iteration,yoy_results
                                  ,Accuracy=(1-abs((yoy_results$Predicted_value-yoy_results$Actual_value)/yoy_results$Actual_value)),
                                  PE=((yoy_results$Predicted_value-yoy_results$Actual_value)/yoy_results$Actual_value),
                                  Actual_mkt_growth=(yoy_results$Actual_value-yoy_results$Lag_actual)/yoy_results$Lag_actual,
                                  Predicted_mkt_growth=(yoy_results$Predicted_value-yoy_results$Lag_actual)/yoy_results$Lag_actual)
    remove_var<-which(colnames(yoy_results)=="Lag_actual")
    yoy_results<-yoy_results[,-remove_var]
  },error=function(e){cat("Error: ",conditionMessage(e),"During YoY calculation. Iteration-",iteration,"\n")})
  
  ################################################################################################################################
  
  
  ###########################################  MAKING ITERATION LOGs   ##################################################
  
  if(counter_4==0){
    final_otp_iteration<-final_otp
    final_coeff_iteration<-final_coeff
    yoy_results_iteration<-yoy_results
    arimax_output_iteration<-arimax_output
    counter_4=counter_4+1
  }else {
    final_otp_iteration<-rbind.data.frame(final_otp_iteration,final_otp)
    final_coeff_iteration<-rbind.data.frame(final_coeff_iteration,final_coeff)  
    yoy_results_iteration<-rbind.data.frame(yoy_results_iteration,yoy_results)
    arimax_output_iteration<-rbind.data.frame(arimax_output_iteration,arimax_output)
  }      
  
  
  ########################################################################################################################
  
  ##################################### I. Forecast for 2017 (2016 Trained model) #######################################
  if(any(unique(year(data_Arima$Month))==Most_recent_yr)){
    
    if(anyNA(colSums(data_Arima[year(data_Arima$Month)==Most_recent_yr,-c(1,2)]))==TRUE){
      #  msgBox <- tkmessageBox(title = "Attention:",message = paste("Can't Predict. As your most recent year-", Most_recent_yr,"-has missing Regressor values"))  
      reg_val_flag<-1
    }
    else {
      
      ##Model 1:
      data_train_m1<- window(y,start=c(start_yr,start_month),end=c(end_yr,end_month))
      x_train_m1<- x[1:xreg_test_row_end,]
      
      forecast_m1<-data.frame(NULL)
      forecast_m2<-data.frame(NULL)
      forecast_m3<-data.frame(NULL)
      forecast_m4<-data.frame(NULL)
      fit_final_m1<-NULL;r_f_m1<-NULL;fitted_values_m1<-NULL;fst_date_1<-NULL
      fit_final_m2<-NULL;r_f_m2<-NULL;fitted_values_m2<-NULL;fst_date_2<-NULL
      fit_final_m3<-NULL;r_f_m3<-NULL;fitted_values_m3<-NULL;fst_date_3<-NULL
      fit_final_m4<-NULL;r_f_m4<-NULL;fitted_values_m4<-NULL;fst_date_4<-NULL
      
      
      #Error handling
      tryCatch({
        fit_final_m1<-arima(data_train_m1,order=c(result_1[1,"p"],result_1[1,"d"],result_1[1,"q"]),
                            seasonal = list(order=c(result_1[1,"P"],1,result_1[1,"Q"]),period=12),xreg=x_train_m1)
        r_f_m1<-predict(fit_final_m1,n.ahead=nrow(x_forecast),newxreg=x_forecast)
        fitted_values_m1<-as.numeric(r_f_m1$pred)
        fst_date_1<-seq.Date(as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(1),as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(length(fitted_values_m1)),by="1 month")
        forecast_m1<-cbind.data.frame(Iteration_num=iteration,Based_On="Lowest AIC",Month=fst_date_1,Fitted=fitted_values_m1)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast of Latest Year-",Most_recent_yr,".Based on Lowest AIC value.Training year-",end_yr,".Iteration-",iteration, "\n")})
      
      ##Model 2:
      tryCatch({
        fit_final_m2<-arima(data_train_m1,order=c(result_2[1,"p"],result_2[1,"d"],result_2[1,"q"]),
                            seasonal = list(order=c(result_2[1,"P"],1,result_2[1,"Q"]),period=12),xreg=x_train_m1)
        r_f_m2<-predict(fit_final_m2,n.ahead=nrow(x_forecast),newxreg=x_forecast)
        fitted_values_m2<-as.numeric(r_f_m2$pred)
        fst_date_2<-seq.Date(as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(1),as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(nrow(fitted_values_m2)),by="1 month")
        forecast_m2<-cbind.data.frame(Iteration_num=iteration,Based_On="Lowest OutSample Mape",Month=fst_date_2,Fitted=fitted_values_m2)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast of Latest Year-",Most_recent_yr,".Based on Lowest OutSample MAPE.Training year-",end_yr,".Iteration-",iteration, "\n")})
      
      ##Model 3: 
      tryCatch({
        fit_final_m3<-arima(data_train_m1,order=c(result_3[1,"p"],result_3[1,"d"],result_3[1,"q"]),
                            seasonal = list(order=c(result_3[1,"P"],1,result_3[1,"Q"]),period=12),xreg=x_train_m1)
        r_f_m3<-predict(fit_final_m3,n.ahead=nrow(x_forecast),newxreg=x_forecast)
        fitted_values_m3<-as.numeric(r_f_m3$pred)
        fst_date_3<-seq.Date(as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(1),as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(nrow(fitted_values_m3)),by="1 month")
        forecast_m3<-cbind.data.frame(Iteration_num=iteration,Based_On="Lowest INSample Mape",Month=fst_date_3,Fitted=fitted_values_m3)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast of Latest Year-",Most_recent_yr,".Based on Lowest InSample MAPE.Training year-",end_yr,".Iteration-",iteration, "\n")})
      
      
      #Model 4:
      tryCatch({
        fit_final_m4<-arima(data_train_m1,order=c(result_4[1,"p"],result_4[1,"d"],result_4[1,"q"]),
                            seasonal = list(order=c(result_4[1,"P"],result_4[1,"D"],result_4[1,"Q"]),period=12),xreg=x_train_m1)
        r_f_m4<-predict(fit_final_m4,n.ahead=nrow(x_forecast),newxreg=x_forecast)
        fitted_values_m4<-as.numeric(r_f_m4$pred)
        fst_date_4<-seq.Date(as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(1),as.Date(paste(end_yr,end_month,1),format = "%Y%m%d")+months(nrow(fitted_values_m4)),by="1 month")
        forecast_m4<-cbind.data.frame(Iteration_num=iteration,Based_On="Auto Arima",Month=fst_date_4,Fitted=fitted_values_m4)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast of Latest Year-",Most_recent_yr,".Based on Auto ARIMA.Training year-",end_yr,".Iteration-",iteration, "\n")})
      
      #Collate forecast
      Forecast_Recent<-rbind.data.frame(forecast_m1,forecast_m2,forecast_m3,forecast_m4)
      
      #Collate Forecast for recent year (using Training till last year with all available data )
      if(counter_5==0) {
        Forecast_Recent_iteration<-Forecast_Recent
        counter_5=counter_5+1
      }
      else {
        Forecast_Recent_iteration<-rbind.data.frame(Forecast_Recent_iteration,Forecast_Recent)  
      }
      
    }
    
    ############################################## Forecast ends #######################################################
    
    ################################# J. FORECAST FOR LATEST YEAR (Till last last available data) ###################################
    
    if(anyNA(colSums(data_Arima[year(data_Arima$Month)==Most_recent_yr,-c(1,2)]))==TRUE){
      #  msgBox <- tkmessageBox(title = "Attention:",message = paste("Can't Predict. As your most recent year-", Most_recent_yr,"-has missing Regressor values"))  
      reg_val_flag<-1
    }
    else {
      
      ts_most_recent<-ts(data_Arima[,2],start = c(start_yr,1),end=c(Most_recent_yr,Most_recent_month),frequency = 12)
      data_train_most_recent<- window(ts_most_recent,start=c(start_yr,1),end=c(Most_recent_yr,Most_recent_month))
      x_train_most_recent<- x[1:(xreg_test_row_end+Most_recent_month),]
      x_forecast_most_recent<-x[(xreg_test_row_end+Most_recent_month+1):(xreg_test_row_end+12),]   #Needs change if Forecasting years are increased
      
      forecast_most_recent_m1<-data.frame(NULL)
      forecast_most_recent_m2<-data.frame(NULL)
      forecast_most_recent_m3<-data.frame(NULL)
      forecast_most_recent_m4<-data.frame(NULL)
      fit_final_most_recent_m1<-NULL;r_f_most_recent_m1<-NULL;fitted_values_most_recent_m1<-NULL;fst_date_most_recent_m1<-NULL
      fit_final_most_recent_m2<-NULL;r_f_most_recent_m2<-NULL;fitted_values_most_recent_m2<-NULL;fst_date_most_recent_m2<-NULL
      fit_final_most_recent_m3<-NULL;r_f_most_recent_m3<-NULL;fitted_values_most_recent_m3<-NULL;fst_date_most_recent_m3<-NULL
      fit_final_most_recent_m4<-NULL;r_f_most_recent_m4<-NULL;fitted_values_most_recent_m4<-NULL;fst_date_most_recent_m4<-NULL
      
      
      #Error Handling 
      tryCatch({
        #Model 1:
        fit_final_most_recent_m1<-arima(data_train_most_recent,order=c(result_1[1,"p"],result_1[1,"d"],result_1[1,"q"]),
                                        seasonal = list(order=c(result_1[1,"P"],1,result_1[1,"Q"]),period=12),xreg=x_train_most_recent)
        r_f_most_recent_m1<-predict(fit_final_most_recent_m1,n.ahead=nrow(x_forecast_most_recent),newxreg=x_forecast_most_recent)
        fitted_values_most_recent_m1<-as.numeric(r_f_most_recent_m1$pred)
        fst_date_most_recent_m1<-seq.Date(as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(1),as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(length(fitted_values_most_recent_m1)),by="1 month")
        forecast_most_recent_m1<-cbind.data.frame(Iteration_num=iteration,Based_On="Lowest AIC",Month=fst_date_most_recent_m1,Fitted=fitted_values_most_recent_m1)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast Module II of Latest Year-",Most_recent_yr,".Based on Lowest AIC Value.Training year-",Most_recent_yr,".Iteration-",iteration, "\n")})
      
      #Model 2:
      tryCatch({
        fit_final_most_recent_m2<-arima(data_train_most_recent,order=c(result_2[1,"p"],result_2[1,"d"],result_2[1,"q"]),
                                        seasonal = list(order=c(result_2[1,"P"],1,result_2[1,"Q"]),period=12),xreg=x_train_most_recent)
        r_f_most_recent_m2<-predict(fit_final_most_recent_m2,n.ahead=nrow(x_forecast_most_recent),newxreg=x_forecast_most_recent)
        fitted_values_most_recent_m2<-as.numeric(r_f_most_recent_m2$pred)
        fst_date_most_recent_m2<-seq.Date(as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(1),as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(length(fitted_values_most_recent_m2)),by="1 month")
        forecast_most_recent_m2<-cbind.data.frame(Iteration_num=iteration,Based_On="Lowest OutSample Mape",Month=fst_date_most_recent_m2,Fitted=fitted_values_most_recent_m2)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast Module II of Latest Year-",Most_recent_yr,".Based on Lowest OutSample MAPE.Training year-",Most_recent_yr,".Iteration-",iteration, "\n")})
      
      #Model 3:
      tryCatch({
        fit_final_most_recent_m3<-arima(data_train_most_recent,order=c(result_3[1,"p"],result_3[1,"d"],result_3[1,"q"]),
                                        seasonal = list(order=c(result_3[1,"P"],1,result_3[1,"Q"]),period=12),xreg=x_train_most_recent)
        r_f_most_recent_m3<-predict(fit_final_most_recent_m3,n.ahead=nrow(x_forecast_most_recent),newxreg=x_forecast_most_recent)
        fitted_values_most_recent_m3<-as.numeric(r_f_most_recent_m3$pred)
        fst_date_most_recent_m3<-seq.Date(as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(1),as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(length(fitted_values_most_recent_m3)),by="1 month")
        forecast_most_recent_m3<-cbind.data.frame(Iteration_num=iteration,Based_On="Lowest INSample Mape",Month=fst_date_most_recent_m3,Fitted=fitted_values_most_recent_m3)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast Module II of Latest Year-",Most_recent_yr,".Based on Lowest InSample MAPE.Training year-",Most_recent_yr,".Iteration-",iteration, "\n")})
      
      #Model 4:
      tryCatch({
        fit_final_most_recent_m4<-arima(data_train_most_recent,order=c(result_4[1,"p"],result_4[1,"d"],result_4[1,"q"]),
                                        seasonal = list(order=c(result_4[1,"P"],result_4[1,"D"],result_4[1,"Q"]),period=12),xreg=x_train_most_recent)
        r_f_most_recent_m4<-predict(fit_final_most_recent_m4,n.ahead=nrow(x_forecast_most_recent),newxreg=x_forecast_most_recent)
        fitted_values_most_recent_m4<-as.numeric(r_f_most_recent_m4$pred)
        fst_date_most_recent_m4<-seq.Date(as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(1),as.Date(paste(Most_recent_yr,Most_recent_month,1),format = "%Y%m%d")+months(length(fitted_values_most_recent_m4)),by="1 month")
        forecast_most_recent_m4<-cbind.data.frame(Iteration_num=iteration,Based_On="Auto Arima",Month=fst_date_most_recent_m4,Fitted=fitted_values_most_recent_m4)
        
      },error=function(e){cat("ERROR :",conditionMessage(e),"At Forecast Module II of Latest Year-",Most_recent_yr,".Based on AUTO ARIMA.Training year-",Most_recent_yr,".Iteration-",iteration, "\n")})
      
      #### COLLATE ALL FORECAST OF LEATEST YEAR:
      
      Forecast_Most_Recent<-rbind.data.frame(forecast_most_recent_m1,forecast_most_recent_m2,forecast_most_recent_m3,forecast_most_recent_m4)
      
      ### Forecast Iteration Log
      
      if(counter_6==0){
        Forecast_Most_Recent_iteration<-Forecast_Most_Recent
        counter_6=counter_6+1
      } 
      else 
      {
        Forecast_Most_Recent_iteration<-rbind.data.frame(Forecast_Most_Recent_iteration,Forecast_Most_Recent)
      }
    }
    
  }   #MOst recent yr filter    
  ########################################### Forecast with 2017 ends ###################################################
  
  
  print(paste("Iteration No -",iteration))
  
  
} #LOop Iteration



### MESSAGE FOR REGRESSOR VALUE MISSING:
if(reg_val_flag==1){
  msgBox <- tkmessageBox(title = "Attention:",message = paste("Can't Predict. As your most recent year-", Most_recent_yr,"-has missing Regressor values"))  
}

######################################   Iteration Loop ends   #####################################################
today<-Sys.time()
######################################### K. OUTPUT/SUMMARY ###########################################################################

# dir.create(paste(Saving_Folder_Address,"//ARIMAX_Iteration_Log(1-",iteration,")_on_",today,"_",Country,sep=""))
# setwd(paste(Saving_Folder_Address,"//ARIMAX_Iteration_Log(1-",iteration,")_on_",today,"_",Country,sep = ""))
write.csv(arimax_output_iteration, 'Arimax_PDQ_Order_Table.csv')
write.csv(final_otp_iteration, 'Final_Output_Table.csv')  
write.csv(final_coeff_iteration, 'Arimax_Coeff_Table.csv')
write.csv(yoy_results_iteration, 'Arimax_YOY_Results_Table.csv')
if(any(unique(year(data_Arima$Month))==Most_recent_yr)){
  if(reg_val_flag==0){
    write.csv(Forecast_Recent_iteration, 'Future_Predictions.csv')
  }
  
  if(reg_val_flag==0){
    write.csv(Forecast_Most_Recent_iteration, 'Future_Predictions_Most_Recent.csv')
  }
}

#####################################################################################################
final_otp<-NULL;final_otp_iteration<-NULL;arimax_output_iteration<-NULL;
final_coeff_iteration<-NULL;final_coeff<-NULL
