require(car)
require(reshape2)
require(lubridate)
require(fmsb)
require(MASS)
library(forecast)
library(fpp)


setwd("C:\\Users\\C987706\\Desktop\\china")
getwd()
global_start<-2012  ## year from which data is available
country<-"Bolivia"
name<-paste0(country," run on ",Sys.Date())

################################################################
##final_data<-read.csv("raw data with all variables.csv",header=T)
#write.csv(final_data[1:10,],"format for raw data to be input to yoy data prep code.csv")
final_data<-raw_data
#library(stats)
class(final_data[,date_var])
dated<-final_data[,date_var]
final_data[,date_var]<-NULL


class<-NULL
for(i in 1:ncol(final_data)){
  class[i]<-ifelse((class(final_data[,i])=="numeric" | class(final_data[,i])=="integer"),1,0)
}

######  factor variables which should be used as it is
data_factor<-final_data[,which(class==0 )]

data<-final_data[,which(class==1 )]
colnames(data)

##### input file having types of variables based on transformation to be done ##
var_type<-read.csv("variable type for data prep for yoy model.csv",header=T,stringsAsFactors = F)
unique(var_type$type)

### optional step ######
###### transform variables whose neither yoy nor increment to last year 
###### makes sense like "Gross.National.Savings.GDP.Pct" i.e. gross saving 
###### in terms of % of gdp
data[,"Gross.National.Savings"]<-data[,"Gross.National.Savings.GDP.Pct"]*data[,"Nominal.GDP.Billion.USD"]
data[,"Gross.National.Savings.GDP.Pct"]<-NULL
data[,"Current.Account.Balance.Pct.GDP"]<-NULL
data[,"Gap.CPI.Pct.Change"]<-NULL
data[,"Ratio.CPI.Pct.Change"]<-NULL
#########################


###### variables to be used as it is like "Volume.Imports.GnS.Pct.Change" ###
var_as.it.is<-var_type[var_type$type =="as it is", "variables"]
# var_as.it.is<-c("Volume.Imports.GnS.Pct.Change" ,"Volume.Exports.GnS.Pct.Change",
#                 "Bol.CPI.Pct.Change" ,"Brz.CPI.Pct.Change","Number.Holidays")
data_as.it.is<-data[,which(colnames(data) %in% var_as.it.is)]

###### rate variables whose changefrom last year to be modeled
###### like gdp growth rate, inflation rate, to be interpreted as
###### unit change in rate causing % change in dependent variable

var_rate<-var_type[var_type$type =="lag.12.increment","variables"]
# var_rate<-c("GDP.Growth.Real.","Bol.Global.Competitiveness.Rank" ,
#             "Brz.Global.Competitiveness.Rank","Rank.Gap")
data_rate<-data[,which(colnames(data) %in% var_rate)]

###### var whose yoy to be used ####################
###### ratio variables like employment to population ratios, population ratios
###### or indices like CPI, PMI etc calculated on a base year or normal 
###### absolute variables like temp, population etc

var_yoy<-var_type[var_type$type =="yoy","variables"]
# var_yoy<-colnames(data)[-which(colnames(data) %in% c(var_rate,var_as.it.is))]
data_yoy<-data[,which(colnames(data) %in% var_yoy)]

############### yoy calculations #####################
yoy_func<-function(x){
  x<-as.vector(x)
  index<-length(x)
  y<-NULL
  y[1:12]<-NA
  for (i in 13:index){
    y[i]<-(x[i]-x[i-12])/x[i-12]
  }
  return (y)
}  


for (i in 1:ncol(data_yoy)){
  data_yoy[,i]<-yoy_func(data_yoy[,i])
}

colnames(data_yoy)<-paste(colnames(data_yoy),"yoy",sep="_")

############# calculating annual increment for rate variables ############
lag12_func<-function(x){
  x<-as.vector(x)
  index<-length(x)
  y<-NULL
  y[1:12]<-NA
  for (i in 13:index){
    y[i]<-(x[i]-x[i-12])
  }
  return (y)
} 

num<-ncol(data_rate)
for (i in 1:num){
  data_rate[,num+i]<-as.vector(lag12_func(data_rate[,i]))
  colnames(data_rate)[num+i]<-paste(colnames(data_rate)[i],"lag12.increment",sep="_")
}

ifelse(ncol(data_rate)==(2*num),print("correct execution"),print("Incorrect execution"))

######################## creating data to be used for yoy model ##########################
nrow(data_yoy)
nrow(data_rate)
nrow(data_factor)
nrow(data_as.it.is)
yoy.data<-cbind(data_factor,data_as.it.is,data_yoy,data_rate)
nrow(yoy.data)
ncol(yoy.data)

yoy.data[,date_var]<-dated

# dep_index<-which(colnames(yoy.data)==paste0(dep_var,"_yoy"))
# date_index<-which(colnames(yoy.data)==date_var)
cor_seq<-c(date_var,paste0(dep_var,"_yoy"),colnames(yoy.data)[-which(colnames(yoy.data) %in% c(paste0(dep_var,"_yoy"),date_var))])
yoy.data<-yoy.data[,cor_seq]
yoy.data<-yoy.data[which(year(yoy.data[,date_var])>global_start),]

write.csv(yoy.data,paste0("yoy data without multicollinearity check for ",name,".csv"))

######## eda plots for yoy data   ########

in_frame<-yoy.data

pdf(paste0("eda plots on yoy data for ",name,".pdf"), width = 16 , height = 10, title = "EDA Plots for yoy data")
par(mfrow=c(2,2))
for(i in 3:(ncol(in_frame))){
  if(class(in_frame[,i])=="numeric" | class(in_frame[,i])=="integer"){
    par(mar = c(5,4,4,5)+.1)
    plot(in_frame[,1], in_frame[,2], col = "black", type = "l", ylab = names(in_frame[2]), xlab = names(in_frame[1]))
    par(new = T)
    plot(in_frame[,1], in_frame[,i], col = "red", type = "l", xaxt = "n", yaxt = "n",
         xlab = "", ylab = "")
    axis(4)
    mtext(names(in_frame[i]), side = 4, line = 3)
    corr <- round(cor(in_frame[,2], in_frame[,i], method = "pearson", use = "pairwise.complete.obs"), digits = 2)
    color <- ifelse(abs(corr) > 0.3, "green", "gray")
    lngd <- paste0("Corr: ",corr)
    legend("topleft", legend = c(lngd), bty = "0", text.col = color)
  }
  
}

dev.off()
in_frame<-NULL

################# model fitting #####################

##    Multi-collinearity   ###
Model_Summary_2 <- data.frame(Iteration = integer(),
                              Variables = character(),
                              Estimate = double(),
                              Std.Error = double(),
                              tValue = double(),
                              PValue = double(),
                              RSqaure = double(),
                              Adj.RSqaure = double())


M1 <- lm(as.formula(paste0(dep_var,"_yoy~.")),data=yoy.data[,-which(colnames(yoy.data) %in% c(date_var))])
summary(M1)
summary_2 <- data.frame(summary(M1)$coefficients)
summary_2$variables <- row.names(summary_2)
summary_2$RSqaure <- summary(M1)$r.squared
summary_2$AdjRSqaure <- summary(M1)$adj.r.squared
row.names(summary_2) <- NULL
write.csv(summary_2,paste0("summary-multicollinearity on yoy data for ",name,".csv"))

k<- c(summary_2$variables)
k<- k[-1]
k<-as.vector(k)

data <- yoy.data[,which(colnames(yoy.data) %in% c(k,date_var,paste0(dep_var,"_yoy")))]

write.csv(data,paste0("yoy data after multicollinearity check for ",name,".csv"))

###############################################################################
##################    yoy data prep ends        ###############################
##################                              ###############################
###############################################################################
