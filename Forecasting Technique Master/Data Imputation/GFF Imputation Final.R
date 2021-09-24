############################################################################
############################################################################
# CODE: INDEPENDENT FORECASTING
# AUTHOR: DIPANKAR DOLEY - GAC BANGALORE
# DATE: 16TH JUN 2017
# LAST UPDATE:4 August 2017
# ABOUT LAST UPDATE: ETS and UCM part added
# LAST UPDATE BY: Ashish Goel
############################################################################
#INSTRUCTIONS#
# SHOULD HAVE ATLEAST TWO COLUMNS TO BE IMPUTED
#Dummy flags CANNOT be imputed by this code

####   data must have a date couloumn with header-"Month" 
####   and format-"%m/%d/%Y" e.g.:01/31/1992
####   make sure no missing value in MOnth coloumn
####   make sure if th data have all month quarter or year coloumns at 
####   the start i.e. at the leftmost coloumn
########################## A. INSTALL REQUIRED PACKAGES ##################################


###################################################################################
###  Note: FOR UCM IF THE FOLLOWING ERROR COMES DATA NEEDS TO BE TRANSFORMED I.E.
### LOG SHOULD BE TAKEN OR DATA BE DEVIDED BY SOME LARGE NUMBER BEFORE IMPUTATION

#   Error in is.SSModel(do.call(updatefn, args = c(list(inits, model), update_args)),  : 
#         System matrices (excluding Z) contain NA or infinite values, 
#   covariance matrices contain values larger than 1e+07 
####################################################################################

if(!require(car)){install.packages("car")}
if(!require(reshape2)){install.packages("reshape2")}
if(!require(lubridate)){install.packages("lubridate")}
if(!require(fmsb)){install.packages("fmsb")}
if(!require(MASS)){install.packages("MASS")}
if(!require(forecast)){install.packages("forecast")}
if(!require(fpp)){install.packages("fpp")}
if(!require(rucm)){install.packages("rucm")}
if(!require(fpp2)){install.packages("fpp2")}
if(!require(smooth)){install.packages("smooth")}
if(!require(Mcomp)){install.packages("Mcomp")}
if(!require(DMwR)){install.packages("DMwR")}
if(!require(VIM)){install.packages("VIM")}
if(!require(plyr)){install.packages("plyr")}

#########################################################################################

setwd("C:\\Users\\C987706\\Desktop\\china")

meth1<-"kNN";meth2<-"Arima";meth3="ETS";meth4="UCM"

#########################################################################################

#setting data start and end point (based on imputed data to be prepared)
global.start<-2009            #based on dependent variable availability
global.end<-2016
dep_var<-"Beer.production"    #name of dependent variable if present in data
#dep_var<-""                   #if data does not have dependent variable


#enter the periodicity of the data to be imputed
#p=4  #quaterly data
#p=1  #annual data
p=12  #monthly data

## RESTRICTIONS ON MISSING VALUE (PERCENTAGE OF ALLOWABLE MISSING VALUES)
f<-0.3

#MD <- read.csv("year data.csv")
#MD<-read.csv("month data aus.csv" )
MD<-read.csv(file.choose(),header = T)
#rm(list=ls())

data_index<-4     ## enter the index from which the data start excluding date, year, quarter variables



MD$Month<-as.Date(MD$Month,"%m/%d/%Y")
class(MD$Month)
MD$year<-year(MD$Month)

MD = MD[1:sum(!is.na(MD$year)),]
#getting data from start to end point
dt<-MD[MD$year>=global.start & MD$year <=global.end,]


colnames(dt)[which(colnames(dt)==dep_var)]<-"Beer_Volume"

## IDENTIFYING DUMMY & FLAG VARIABLES AND CONVERT IT TO FACTOR FROM NUMERIC
## variables having only two levels i.e. "0" or "1" will be converted to factor
for(itr in 1: ncol(dt)){
  if(length(unique(dt[,itr]))==2){
    if(unique(unique(dt[,itr]) %in% c(1,0))==TRUE){
      dt[,itr]<-as.factor(dt[,itr])
    }
  }else{
    dt[,itr]<-dt[,itr]
  }
}


## FACTOR VARIABLE COLUMN NUMBERS
fac_vars<-names(Filter(is.factor,dt[1:2,-c(1:(data_index-1))]))
fac_vars<-which(colnames(dt) %in% fac_vars)


## CONVERTING OTHER VARIABLES AS NUMERIC
for(itr2 in data_index:ncol(dt)){
  if(!itr2 %in% fac_vars==TRUE){
    dt[,itr2]<-as.numeric(as.character(dt[,itr2]))
  }
}

## PATTERN IDENTIFICATION OF VARIABLES - (Based on data availability)

## FLAG # VARIABLES WITH NO OBSERVATION
mis_sum=list()
j<-1
for(itr3 in data_index: ncol(dt)){
  mis_val<-sum(is.na(dt[,itr3]))
  mis_sum[[j]]<-data.frame(var_names=names(dt[itr3]),mis_val)
  j<-j+1
}

mis_sum<-do.call(rbind,mis_sum)
var_no_obs<-as.vector(mis_sum[!is.na(match(mis_sum[,2],nrow(dt))),1])
var_no_obs_f<-cbind.data.frame(var=var_no_obs,flag=0)


## VARIABLES WITH COMPLETE OBS
dt2<-dt[,colnames(dt) %in% colnames(dt)[!colnames(dt) %in% var_no_obs]]
num_obs_witout_na<-list()
witout_na<-NULL
j<-1
for(itr4 in data_index:ncol(dt2)){
  witout_na<-sum(!is.na(dt2[,itr4]))
  num_obs_witout_na[[j]]<-data.frame(var_names=names(dt2)[itr4],witout_na)
  j<-j+1
}
num_obs_witout_na<-do.call(rbind,num_obs_witout_na)
var_with_complete<-as.vector(num_obs_witout_na[!is.na(match(num_obs_witout_na[,2],nrow(dt2))),1])
var_with_complete_f<-cbind.data.frame(var=var_with_complete,flag=9)


## FLAG VARIABLES WITH MISSING VALUES
dt3<-dt[,!colnames(dt) %in% c(var_no_obs,var_with_complete)]

## f=0.3 #### It can be Manual Input 

num_na<-NULL
rel_var<-NULL
num_na[1:(data_index-1)]<-nrow(dt3)
for (i in data_index: ncol(dt3)){
  num_na[i]<-sum(is.na(dt3[,i]))
}
rel_var<-colnames(dt3[,which(num_na<(nrow(dt3))*f & num_na >=0)])

## VARIABLES TO IMPUTE
dt4<-dt3[,rel_var]

## FLAG VARIABLES WHICH CAN'T BE IMPUTED (MORE THAN 70% DATA MISSING)
imputation_not_possible<-dt3[,!colnames(dt3) %in% rel_var]
var_imp_not_posible<-colnames(imputation_not_possible[,-c(1:(data_index-1))])
var_imp_not_posible_f<-cbind.data.frame(var=var_imp_not_posible,flag=8)

########################## IMPUTATION 1. kNN-ALL #############################

imputed_knn<-NULL
if(!rel_var[1] %in% NA){
  imputed_knn<-kNN(dt4,imp_var = F)
  colnames(imputed_knn)<-paste(colnames(imputed_knn),"_",meth1,sep="")
}

######################### IMPUTATION 1. COMPLETE ###########################

################### MISSING VALUE PATTERN IDENTIFICATION STARTS #####################

pattern_log<-list()
itr5<-NULL
if(!rel_var[1] %in% NA){
  for(itr5 in 1:ncol(dt4)){
    f_i<-NULL;l_i<-NULL
    f_i<-min(which(!is.na(dt4[,itr5])))
    l_i<-max(which(!is.na(dt4[,itr5])))
    
    if(f_i==1 & l_i==nrow(dt4)){
      if(sum(is.na(dt4[c(f_i:l_i),itr5]))>0){
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=3) #random/alternate
      }else{
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=9)  #have complte obs sudnt be in the mix
      }       
    }else if(f_i !=1 & l_i !=nrow(dt4)){
      if(sum(is.na(dt4[c(f_i:l_i),itr5]))>0){
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=3)  
      }else{
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=4)  #blank at start and end
      }
    }else if(f_i==1 & l_i != nrow(dt4)){
      if(sum(is.na(dt4[c(f_i:l_i),itr5]))>0){
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=3)
      }else{
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=2) #blank at last
      }
    }else if(f_i!=1 & l_i==nrow(dt4)){
      if(sum(is.na(dt4[c(f_i,l_i),itr5]))>0){
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=3)
      }else{
        pattern_log[[itr5]]<-data.frame(var=names(dt4)[itr5],flag=1)  #blank at begin
      }
    }##end of if else statement
  }##forloop
}##for if condition to check whether rel_var is null

## PATTERN LOG
if(!rel_var[1] %in% NA){
  pattern_log<-do.call(rbind,pattern_log)
  pattern_4_impute<-setnames(data.frame(t(pattern_log[,-1])),as.character(pattern_log[,1]))
}

#################### MISSING PATTERN IDENTIFICATION ENDS ############################

############################## IMPUTATION 2: #####################################

impute_final<-data.frame(NULL)
if(!rel_var[1] %in% NA){
  impute_var<-pattern_4_impute
  
  #creating time series of variables to be imputed
  t_impute<-ts(dt4,start=c(global.start,1),frequency=p)
  class(t_impute)
  
  imputed_data_cat1<-list()
  knn_table<-list()
  
  for (i in colnames(impute_var)){
    if(impute_var[1,i]==2){
      t_length<-min(which(is.na(t_impute[,i])))-1
      temp<-ts(t_impute[1:t_length,i],start=c(global.start,1),frequency=p)
      fit<-auto.arima(temp)
      predict<-forecast(fit,h=(((global.end-global.start+1)*p-t_length)))
      plot(predict)
      all<-c(as.vector(temp),as.vector(predict$mean))
      imputed_data_cat1[[i]]<-all
      
    }else if(impute_var[1,i]==1){
      vec<-rev(t_impute[,i])
      t_length<-min(which(is.na(vec)))-1
      t.series<-ts(vec[1:t_length],frequency=p)
      fit<-auto.arima(t.series)
      predict<-forecast(fit,h=(((global.end-global.start+1)*p-t_length)))
      
      all<-rev(c(as.vector(t.series),as.vector(predict$mean)))
      imputed_data_cat1[[i]]<-all
    }else if(impute_var[1,i]==3){    
      
      knn_table[[i]]<-as.data.frame(t_impute[,i])
      colnames(knn_table[[i]])<-i
      
    }else if(impute_var[1,i]==4){
      t_start<-min(which(!is.na(t_impute[,i])))
      t_end<-max(which(!is.na(t_impute[,i])))
      vec<-t_impute[t_start:t_end,i]
      temp<-ts(vec,frequency=p)
      fit<-auto.arima(temp)
      predict<-forecast(fit,h=(((global.end-global.start+1)*p-t_end)))
      plot(predict)
      all<-c(as.vector(temp),as.vector(predict$mean))
      temp<-ts(rev(all),frequency = p)
      fit<-auto.arima(temp)
      predict<-forecast(fit,h=(((global.end-global.start+1)*p-length(temp))))
      all<-c(as.vector(temp),as.vector(predict$mean))
      imputed_data_cat1[[i]]<-rev(all)
    }else{
      print (i)
      print ("no actian taken")
      imputed_data_cat1[,i]<-as.vector(t_impute[,i])
    }
    print (i)
  }
  
  ## COLLATION OF FINAL IMPUTED VARIABLE:
  
  imputed_data_cat1_f<-do.call(cbind,imputed_data_cat1)
  knn_table_f<-do.call(cbind,knn_table)
  if(is.null(nrow(imputed_data_cat1_f)) & is.null(nrow(knn_table_f))){
    knn_table_f2<-NULL
  }else if(is.null(nrow(knn_table_f))){
    knn_table_f2<-imputed_data_cat1_f
  }else if(is.null(nrow(imputed_data_cat1_f))){
    knn_table_f2<-knn_table_f
  }else{
    knn_table_f2<-cbind.data.frame(knn_table_f,imputed_data_cat1_f) 
  }
  #   
  #   knn_table_f2<-cbind.data.frame(knn_table_f,imputed_data_cat1_f)
  impute_final<-kNN(knn_table_f2,imp_var = FALSE)
  colnames(impute_final)<-paste(colnames(impute_final),"_",meth2,sep="") #might change when incorporating Loop
  
} #Loop rel_var

################################## METHOD 2 ENDS ###############################################

############################## IMPUTATION 3: #####################################

impute_final.1<-data.frame(NULL)
if(!rel_var[1] %in% NA){
  impute_var<-pattern_4_impute
  
  #creating time series of variables to be imputed
  t_impute<-ts(dt4,start=c(global.start,1),frequency=p)
  class(t_impute)
  
  imputed_data_cat1<-list()
  knn_table<-list()
  
  for (i in colnames(impute_var)){
    if(impute_var[1,i]==2){
      t_length<-min(which(is.na(t_impute[,i])))-1
      temp<-ts(t_impute[1:t_length,i],start=c(global.start,1),frequency=p)
      fit<-ets(temp)
      predict<-predict(fit,h=(((global.end-global.start+1)*p-t_length)))
      plot(predict)
      all<-c(as.vector(temp),as.vector(predict$mean))
      imputed_data_cat1[[i]]<-all
      
    }else if(impute_var[1,i]==1){
      vec<-rev(t_impute[,i])
      t_length<-min(which(is.na(vec)))-1
      t.series<-ts(vec[1:t_length],frequency=p)
      fit<-ets(t.series)
      predict<-predict(fit,h=(((global.end-global.start+1)*p-t_length)))
      
      all<-rev(c(as.vector(t.series),as.vector(predict$mean)))
      imputed_data_cat1[[i]]<-all
    }else if(impute_var[1,i]==3){    
      
      knn_table[[i]]<-as.data.frame(t_impute[,i])
      colnames(knn_table[[i]])<-i
      
    }else if(impute_var[1,i]==4){
      t_start<-min(which(!is.na(t_impute[,i])))
      t_end<-max(which(!is.na(t_impute[,i])))
      vec<-t_impute[t_start:t_end,i]
      temp<-ts(vec,frequency=p)
      fit<-ets(temp)
      predict<-predict(fit,h=(((global.end-global.start+1)*p-t_end)))
      plot(predict)
      all<-c(as.vector(temp),as.vector(predict$mean))
      temp<-ts(rev(all),frequency = p)
      fit<-ets(temp)
      predict<-predict(fit,h=(((global.end-global.start+1)*p-length(temp))))
      all<-c(as.vector(temp),as.vector(predict$mean))
      imputed_data_cat1[[i]]<-rev(all)
    }else{
      print (i)
      print ("no actian taken")
      imputed_data_cat1[,i]<-as.vector(t_impute[,i])
    }
    print (i)
  }
  
  ## COLLATION OF FINAL IMPUTED VARIABLE:
  
  imputed_data_cat1_f<-do.call(cbind,imputed_data_cat1)
  knn_table_f<-do.call(cbind,knn_table)
  if(is.null(nrow(imputed_data_cat1_f)) & is.null(nrow(knn_table_f))){
    knn_table_f2<-NULL
  }else if(is.null(nrow(knn_table_f))){
    knn_table_f2<-imputed_data_cat1_f
  }else if(is.null(nrow(imputed_data_cat1_f))){
    knn_table_f2<-knn_table_f
  }else{
    knn_table_f2<-cbind.data.frame(knn_table_f,imputed_data_cat1_f) 
  }
  #   
  #   knn_table_f2<-cbind.data.frame(knn_table_f,imputed_data_cat1_f)
  impute_final.1<-kNN(knn_table_f2,imp_var = FALSE)
  colnames(impute_final.1)<-paste(colnames(impute_final.1),"_",meth3,sep="") #might change when incorporating Loop
  
} #Loop rel_var

################################## METHOD 3 ENDS ###############################################

############################## IMPUTATION 4: #####################################

impute_final.2<-data.frame(NULL)
if(!rel_var[1] %in% NA){
  impute_var<-pattern_4_impute
  
  #creating time series of variables to be imputed
  t_impute<-ts(dt4,start=c(global.start,1),frequency=p)
  class(t_impute)
  
  imputed_data_cat1<-list()
  knn_table<-list()
  
  for (i in colnames(impute_var)){
    if(impute_var[1,i]==2){
      t_length<-min(which(is.na(t_impute[,i])))-1
      temp<-ts(t_impute[1:t_length,i],start=c(global.start,1),frequency=p)
      fit<-ucm(temp~0,temp,season = T,season.length = p,slope=T)
      predict<-predict(fit,n.ahead=((global.end-global.start+1)*p-t_length))
      #plot(predict)
      all<-c(as.vector(temp),as.vector(predict))
      imputed_data_cat1[[i]]<-all
      
    }else if(impute_var[1,i]==1){
      vec<-rev(t_impute[,i])
      t_length<-min(which(is.na(vec)))-1
      t.series<-ts(vec[1:t_length],frequency=p)
      fit<-ucm(t.series~0,t.series,season = T,season.length = p,slope=T)
      predict<-predict(fit,n.ahead=((global.end-global.start+1)*p-t_length))
      
      all<-rev(c(as.vector(t.series),as.vector(predict)))
      imputed_data_cat1[[i]]<-all
    }else if(impute_var[1,i]==3){    
      
      knn_table[[i]]<-as.data.frame(t_impute[,i])
      colnames(knn_table[[i]])<-i
      
    }else if(impute_var[1,i]==4){
      t_start<-min(which(!is.na(t_impute[,i])))
      t_end<-max(which(!is.na(t_impute[,i])))
      vec<-t_impute[t_start:t_end,i]
      temp<-ts(vec,frequency=p)
      fit<-ucm(temp~0,temp,season = T,season.length = p,slope=T)
      predict<-predict(fit,n.ahead=((global.end-global.start+1)*p-t_length))
      #plot(predict)
      all<-c(as.vector(temp),as.vector(predict))
      temp<-ts(rev(all),frequency = p)
      fit<-ucm(temp~0,temp,season = T,season.length = p,slope=T)
      predict<-predict(fit,n.ahead=(((global.end-global.start+1)*p-length(temp))))
      all<-c(as.vector(temp),as.vector(predict))
      imputed_data_cat1[[i]]<-rev(all)
    }else{
      print (i)
      print ("no actian taken")
      imputed_data_cat1[,i]<-as.vector(t_impute[,i])
    }
    print (i)
  }
  
  ## COLLATION OF FINAL IMPUTED VARIABLE:
  
  imputed_data_cat1_f<-do.call(cbind,imputed_data_cat1)
  knn_table_f<-do.call(cbind,knn_table)
  if(is.null(nrow(imputed_data_cat1_f)) & is.null(nrow(knn_table_f))){
    knn_table_f2<-NULL
  }else if(is.null(nrow(knn_table_f))){
    knn_table_f2<-imputed_data_cat1_f
  }else if(is.null(nrow(imputed_data_cat1_f))){
    knn_table_f2<-knn_table_f
  }else{
    knn_table_f2<-cbind.data.frame(knn_table_f,imputed_data_cat1_f) 
  }
  #   
  #   knn_table_f2<-cbind.data.frame(knn_table_f,imputed_data_cat1_f)
  impute_final.2<-kNN(knn_table_f2,imp_var = FALSE)
  
  colnames(impute_final.2)<-paste(colnames(impute_final.2),"_",meth4,sep="") #might change when incorporating Loop
  
} #Loop rel_var

################################## METHOD 4 ENDS ###############################################

#### saving the imputed data #####################################################
imputed_data_all<-cbind(imputed_knn,impute_final,impute_final.1,impute_final.2)
write.csv(imputed_data_all,"Imputed data by all methods.csv")


################################# REPORT PREPARATION:STARTS #################################

### GETTING BASIC INFORMATION ON THE RAW DATA

na_l<-list()
for(itr6 in data_index:ncol(dt)){
  Missing_Flag<-ifelse(sum(is.na(dt[,itr6]))>0,"Yes","No")
  n_mis<-sum(is.na(dt[,itr6]))
  min_dt<-dt[min(which(!is.na(dt[,itr6]))),"Month"]
  max_dt<-dt[max(which(!is.na(dt[,itr6]))),"Month"]
  if(n_mis>0){
    d_f<-data.frame(which(is.na(dt[,itr6])))
    index_mis<-paste(unlist(d_f),collapse = "; ")
  }else{
    index_mis<-NA
  }
  na_l[[itr6]]<-data.frame(Independent_variables=names(dt)[itr6],Missing_Flag,Num_mis_obs=n_mis,Data_Starting_Date=min_dt,Data_Ending_Date=max_dt,Row_Index_Mis=index_mis)
}
na_l_f<-do.call(rbind,na_l)

## REPORT MAKING
report=list()
if(!rel_var[1] %in% NA){
  report<-list(
    data.frame(Independent_variables=var_with_complete,Status="All Observations Present"),
    data.frame(Independent_variables=var_no_obs,Status="No Observations. Variable is Empty"),
    data.frame(Independent_variables=names(impute_final),Status="Variable is Imputed"),
    data.frame(Independent_variables=var_imp_not_posible,Status=paste("More than ",f*100," % values Missing. Variable cannot be Imputed"))  
  )
}else{
  report<-list(
    data.frame(Independent_variables=var_with_complete,Status="All Observations Present"),
    data.frame(Independent_variables=var_no_obs,Status="No Observations. Variable is Empty"),
    data.frame(Independent_variables=var_imp_not_posible,Status=paste("More than ",f*100," % values Missing. Variable cannot be Imputed"))  
  )}

report_f<-do.call(rbind,report)

write.csv(na_l_f,"Summary of varaibles before imputation.csv")
write.csv(report_f,"Action taken on varaibles.csv")
