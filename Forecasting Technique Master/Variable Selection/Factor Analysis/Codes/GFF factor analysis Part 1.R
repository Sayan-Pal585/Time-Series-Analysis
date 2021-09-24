##############################################################################
###    Factor analysis code
###    Written by : Ashish Goel
###    Last update: 4 August 2017
###    Last Update By: Ashish Goel
##############################################################################

#################### loading the libraries ###################################
require(car)
require(reshape2)
require(lubridate)
require(fmsb)
require(MASS)
library(forecast)
library(fpp)
library(svDialogs)
library(psych)
library(GPArotation)
##############################################################################

##################  setting the library ######################################
setwd("C:\\Users\\C987706\\Desktop\\china")
getwd()
##############################################################################

##################   INSTRUCTIONS FOR RUNNING THE CODE #######################
####      raw_data must have dependent variable, it is needed for multicollinearity check
####      raw_data must have only a date variable
####      no year, quarter or any other time variable should be there
##################   INSTRUCTIONS END HERE  ##################################

##################  USER INPUTS REQUIRED #####################################
input_date_format<-"%m/%d/%Y"   #### use  "/" or "-" as per the need as separator
### "%d-%b-%y" eg: "01-Jan-12" (1 juanuary 2012)
### "%d-%B-%y" eg: "01-January-12" (1 juanuary 2012)
### "%d-%m-%y" eg: "01-01-12" (1 juanuary 2012) and so on
date_var<-"Month"               #### name of the date variable in the raw data
dep_var<-"Beer.production"      #### name of the dependent varaible
gobal_start<-2009     #### year from which data needs to be considered/start
global_end<-2016      #### year upto whcih data is available or to be considered

################  import the data for Factor analysis ########################
raw_data<-read.csv("raw_data for conducting FA.csv")
raw_data[,date_var]<-as.Date(as.character(raw_data[,date_var]),input_date_format)
##############################################################################

######## separating numeric or integer variables for factor analysis #########
classes<-ifelse(sapply(raw_data,class) %in% c("numeric","integer") ,1,0)

data_factor<-raw_data[classes==0]
data<-cbind(raw_data[,date_var],raw_data[classes==1])
colnames(data)[1]<-date_var


############################## SELECT VARIABLES #############################
#### NOTE: select dependent variable also 
var_list <- colnames(data)
vars <- dlgList(var_list,multiple=T,title="Select DEP & INDEPENDENT variables")$res

write.csv(vars,"variables considered for Factor Analysis.csv")
#############################################################################

################ crreating data for factor analysis #########################
fa.data<-data[vars]

###### removing multicollinearity amongst the selected variables ############
Model_Summary_2 <- data.frame(Iteration = integer(),
                              Variables = character(),
                              Estimate = double(),
                              Std.Error = double(),
                              tValue = double(),
                              PValue = double(),
                              RSqaure = double(),
                              Adj.RSqaure = double())
lm_formula<-as.formula(paste0(dep_var,"~."))
M1 <- lm(lm_formula,data= fa.data)
summary_2 <- data.frame(summary(M1)$coefficients)
summary_2$variables <- row.names(summary_2)
summary_2$RSqaure <- summary(M1)$r.squared
summary_2$AdjRSqaure <- summary(M1)$adj.r.squared
row.names(summary_2) <- NULL

q<- c(summary_2$variables)
q<- q[-1]
final_data <- fa.data[q]

#########################  scaling the data for factor analysis ###############
data_scale <-as.data.frame(scale(final_data))

#########################  finding number of factors   ########################
parallel <- fa.parallel(data_scale, fm = 'minres', fa = 'fa')
num_f<-dlgInput("Enter number of factors based on parallel analysis", Sys.info()["user"])$res
num_f<-as.numeric(num_f)
rotation<-dlgInput("Enter roattion type for FA: 'oblimin' or 'Varimax'", Sys.info()["user"])$res
cut_off<-dlgInput("Enter cutoff for factor loadings", Sys.info()["user"])$res
cut_off<-as.numeric(cut_off)

fffactor <- fa(data_scale,nfactors =num_f,rotate = rotation,fm="minres")
print(fffactor)
print(fffactor$loadings,cutoff = cut_off)

########################  PART -2  #############################################
#### ONLY TO BE RUN AFTER FIXING PART-1
################################################################################
################################################################################
#### the part below is to be used when variables are selected based on 
#### different iteration of cutoff, rotation criteria or number of factors

#### THIS PART WILL SAVE FACTOR LOADINGS OF SELECTED VARIABLES AND
#### WILL CREATE FACTORS BASED ON SELECTED VARIABLES AND LOADING CUTOFF
################################################################################

loadings_fa<-as.data.frame(fffactor$loadings[,1:ncol(fffactor$loadings)])
loadings_fa$variables<-rownames(loadings_fa)
rownames(loadings_fa)<-NULL

write.csv(loadings_fa,file=paste0("Factor loadings without cutoff"
                                  ," & ",rotation,"-rotation type",".csv"))

factors_created<-loadings_fa
for (i in 1:(ncol(factors_created)-1)){
  factors_created[,i]<-ifelse(abs(factors_created[,i])<=cut_off,NA,factors_created[,i])
}

write.csv(factors_created,file=paste0("Factor loadings with loading cutoff-",
                                      as.character(cut_off)," & ",rotation,"-rotation type",".csv"))

factors_created<-loadings_fa
for (i in 1:(ncol(factors_created)-1)){
  factors_created[,i]<-ifelse(abs(factors_created[,i])<=cut_off,0,factors_created[,i])
}
v_series<-as.vector(factors_created$variables)
m<-data_scale[v_series]
colnames(m)
dim(m)
num<-ncol(m)

factors_created<-factors_created[,1:(ncol(factors_created)-1)]
var<-sapply(factors_created,var)
factors_created<-factors_created[var>0]
Factors<-data.frame(Factor_1=as.vector((as.matrix(m))%*%(as.matrix(factors_created[,1]))))
if(ncol(factors_created)>1){
  for(i in 2:ncol(factors_created)){
    Factors<-cbind(Factors,as.vector((as.matrix(m))%*%(as.matrix(factors_created[,i]))))
  }
}
colnames(Factors)<-c(paste0("Factor_",seq(1:ncol(Factors))))

write.csv(Factors,"factors created based on variables selected and loadings after applying cutoff.csv")

