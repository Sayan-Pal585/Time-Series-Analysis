#################################################################################
################# code for Log linear regression model   ########################
################# code created on 20 june 2017        ###########################
#################################################################################


############################### used libraries ##################################
#################################################################################

require(car)
require(reshape2)
require(lubridate)
require(fmsb)
require(MASS)
require(QuantPsyc)
require(plyr)
require(dplyr)
require(tibble)
require(sqldf)
require(combinat)
require(dplyr)
require(tcltk) ##for message box

###############################################################################
############################   user inputs required  ##########################

country="Bolivia"                ## name of the country
dep_var="Beer.Consumption.Khl"   ## name of the dependent variable as in raw data
date_var="Date"                  ## name of the date variable as in raw data
num_min_var=4                    ## number of minimum variables in the model
num_max_var=7                    ## number of maximum variables in the model                              
global_start=2012                ## year from which data is available
global_end=2017                  ## year upto which data is available
cross_val_prd<-2  ## num of max years to go back from global end to train models

iteration<-4      ## number of the Log-LR model iteration , depending on new set of variables or data

###############################################################################
#################### importing raw data #######################################

##NOTE: date variable should be in format "%m/%d/%Y" eg: "01/31/2017"
##NOTE: raw data supposed to have all monthly, quarterly and annual variables

raw_data<-read.csv("raw data.csv",header=T)
raw_data[,date_var]<-as.Date(as.character(raw_data[,date_var]),format="%m/%d/%Y")
class(raw_data[,date_var])

################## creating automated names for files to be saved ############
##############################################################################

name<-paste0("Log-LR iter-",iteration)
date<-format(Sys.Date(),"%d%b%Y")

################### importing list of shortlisted variables ###################
################### including mandatory variables #############################

##    NOTE:header of the shortlisted variables 
##         file should be exactly like given format
## NOTE: dir_flag for (+)ly impacting var is 1, (-)ly impacting var is 0
##       and NA for variables which can take any direction like flags

short_listed_var<-read.csv("shortlisted variables.csv",header=T)
### saving the list of variables selected for this particular iteration#######
write.csv(short_listed_var,paste0("variables considered for ",name," ,country-","run on",date,".csv"))
##### creating a vector of variables for creating various combinations ######
short_listed_var$dir_flag<-as.numeric(short_listed_var$dir_flag)
comb_all<-as.vector(short_listed_var$variables)


################### importing list of mandatory variables  ####################
###############################################################################

##    NOTE:header of the mandatory variables 
##         file should be exactly like given format

####default values if  no mandatory variables are selected ####################
mandate_var<-NULL
num_mandate_var<-0

mandate_var<-read.csv("mandatory variables.csv",header=T)
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

if(min(model_data[,dep_var])<=0){
  print("zero/negative values present in dependent variable,treatment is needed !")}

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

Model_Log.LR_fitted_actual<- data.frame(Model_iter_num=character(),
                                        Mod_eq = character(),
                                        No_of_indep = integer(),
                                        Trained_upto_year=integer(),
                                        Fitted = double(),
                                        Fitted_Low.95=double(),
                                        Fitted_Upr.95=double(),
                                        Fitted_Low.90=double(),
                                        Fitted_Upr.90=double(),
                                        Actual = double(),
                                        Percent_Error = double()
)

Model_Log.LR_Summary <- data.frame(Model_iter_num=character(),
                                   Mod_eq = character(),
                                   No_of_indep = integer(),
                                   Trained_upto_year=integer(),
                                   Variables = character(),
                                   Estimate = double(),
                                   Std.Error = double(),
                                   tValue = double(),
                                   PValue = double(),
                                   RSqaure = double(),
                                   Adj.RSqaure = double(),
                                   dir_flag=integer(),
                                   VIF=double(),
                                   Std.Coeff=double(),
                                   Abs.Std.Coeff=double(),
                                   Relative.Imp=double
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
  ##outside loop selects a particular set of variables from selected combinations
  mod_dep_var<-paste0("log(",dep_var,")")
  mod_eq<-paste(mod_dep_var,selected_layer$equation[i],sep="~")
  no_of_ind<-(sum(charToRaw(mod_eq)==charToRaw("+"))+1)
  actual<-model_data[which(year(model_data[,date_var])<=global_end),dep_var] 
  for(j in 1:cross_val_prd){
    print(paste0("j=",j))
    ## inside loop fit models for  different training periods and
    ## generate summary, fitted and actual values datasets
    train<-model_data[which(year(model_data[,date_var])<=(global_end-j)),
                      -which(colnames(model_data) %in% c(date_var))]
    test<-model_data[which(year(model_data[,date_var])>(global_end-j) 
                           & year(model_data[,date_var])<=global_end),
                     -which(colnames(model_data) %in% c(date_var))]
    fit<-lm(mod_eq,data=train)
    ##creating fitted and actual dataset
    fit_val<-c(as.vector(exp(predict(fit,train))),as.vector(exp(predict(fit,test))))
    fit_val_low.95<-c(as.vector(exp(predict(fit,train, interval="predict")[,2])),
                      as.vector(exp(predict(fit,test, interval="predict")[,2])))
    fit_val_upr.95<-c(as.vector(exp(predict(fit,train, interval="predict")[,3])),
                      as.vector(exp(predict(fit,test, interval="predict")[,3])))
    
    fit_val_low.90<-c(as.vector(exp(predict(fit,train, interval="predict",level=0.90)[,2])),
                      as.vector(exp(predict(fit,test, interval="predict",level=0.90)[,2])))
    fit_val_upr.90<-c(as.vector(exp(predict(fit,train, interval="predict",level=0.90)[,3])),
                      as.vector(exp(predict(fit,test, interval="predict",level=0.90)[,3])))
    
    pe<-(fit_val-actual)/actual
    fit_fitted_actual<-data.frame(Model_iter_num=selected_layer$Model_iter_num[i],
                                  Mod_eq = mod_eq,
                                  No_of_indep = no_of_ind,
                                  Trained_upto_year=(global_end-j),
                                  Fitted = fit_val,
                                  Fitted_Low.95=fit_val_low.95,
                                  Fitted_Upr.95=fit_val_upr.95,
                                  Fitted_Low.90=fit_val_low.90,
                                  Fitted_Upr.90=fit_val_upr.90,
                                  Actual = actual,
                                  Percent_Error = pe
    )
    fit_fitted_actual[,date_var]<-model_data[which(year(model_data[,date_var])<=global_end),date_var]
    
    if(i==1 & j==1){
      Model_Log.LR_fitted_actual<-rbind(Model_Log.LR_fitted_actual,fit_fitted_actual)
    }else{
      Model_Log.LR_fitted_actual<-rbind.fill(Model_Log.LR_fitted_actual,fit_fitted_actual)
    }
    
    ## creating model summary dataset
    summary<-data.frame(Variables=row.names(summary(fit)$coefficients))
    summary$Model_iter_num<-selected_layer$Model_iter_num[i]
    summary$Mod_eq=mod_eq
    summary$No_of_indep<-no_of_ind
    summary$Trained_upto_year=(global_end-j)
    summary<-summary[,c(2:5,1)]
    summary$Estimate = summary(fit)$coefficients[,1]
    summary$Std.Error = summary(fit)$coefficients[,2]
    summary$tValue = summary(fit)$coefficients[,3]
    summary$PValue = summary(fit)$coefficients[,4]
    summary$RSqaure = summary(fit)$r.squared
    summary$Adj.RSqaure = summary(fit)$adj.r.squared
    summary$dir_flag<-ifelse(summary$Estimate>=0,1,0)
    summary[which(summary$Variables=="(Intercept)"),"dir_flag"]<-NA
    
    tryCatch({vif_forced_model <- data.frame(vif(fit))
    vif_forced_model$Variables <- rownames(vif_forced_model)
    rownames(vif_forced_model) <- NULL
    colnames(vif_forced_model)<-c("VIF","Variables")
    summary <- merge.data.frame(summary, vif_forced_model, by="Variables", all.x = TRUE)
    },error=function(e)cat("alias in",mod_eq,"corss validation",j))
    
    stdcoeff <- data.frame(lm.beta(fit))
    stdcoeff$abs <- abs(stdcoeff$lm.beta.fit.)
    stdcoeff$Rel_Imp <- abs(stdcoeff$abs/sum(stdcoeff$abs)) 
    stdcoeff <- data.frame(rownames_to_column(stdcoeff, "Variables"))
    colnames(stdcoeff)<-c("Variables","Std.Coeff","Abs.Std.Coeff","Relative.Imp")
    summary<- merge.data.frame(summary,stdcoeff, by= "Variables", all.x = TRUE)
    summary<-summary[,c(2:5,1,6:ncol(summary))]
    
    if(i==1 & j==1){
      Model_Log.LR_Summary<-rbind(Model_Log.LR_Summary,summary)
    }else{
      Model_Log.LR_Summary<-rbind.fill(Model_Log.LR_Summary,summary) 
    }
    
    #### creating dataset with estimates direction summary ###############
    dir_summary<-summary[,c(1:5,12)]
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

write.csv(Model_Log.LR_fitted_actual,paste0(name," fitted values for ",country," run on ",date,".csv"))
write.csv(Model_Log.LR_Summary,paste0(name," summary for ",country," run on ",date,".csv"))
write.csv(Model_Dir_Summary,paste0(name,"summary for est directions of ",country," run on ",date,".csv"))

####### code to summarize the model iterations and finding best iteration #####
###############################################################################

####### code to summarize the model iterations and finding best iteration #####
Model_Log.LR_fitted_actual$year<-year(Model_Log.LR_fitted_actual[,date_var])
Model_Log.LR_fitted_actual$out.flag<-(Model_Log.LR_fitted_actual$year)-(Model_Log.LR_fitted_actual$Trained_upto_year)
outsample<-Model_Log.LR_fitted_actual[which(Model_Log.LR_fitted_actual$out.flag>=0),]
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
## each trainig period
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
max_dir.flag<-cross_val_prd
outsample3<-outsample1[which((outsample1$year-outsample1$Trained_upto_year)==1),]
outsample3<-outsample3 %>%
  group_by(Model_iter_num,Mod_eq, No_of_indep) %>%
  mutate(
    Sum.Growth.Dir.Flag=sum(Growth.Dir.Flag)
  )
cor_growth_iter<-unique(outsample3[which(outsample3$Sum.Growth.Dir.Flag==max_dir.flag),"Model_iter_num"])
outsample3<-NULL
outsample2<-outsample1[which(outsample1$Model_iter_num %in% cor_growth_iter$Model_iter_num),]
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

