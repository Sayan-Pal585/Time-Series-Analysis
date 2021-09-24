
if(!require(glmnet)){install.packages('glmnet')};library(glmnet)
if(!require(forecast)){install.packages('forecast')};library(forecast)
if(!require(lubridate)){install.packages('lubridate')};library(lubridate)

################################ INSTRUCTIONS #####################################

## COLUMN 1 AND 2 SHOULD BE DATES AND BEER VOLUME RESPECTIVELY
## ONLY KEEP DATA WITH COMPLETE YEARS (EXAMPLE: MULTIPLES OF 12 MONTHS)
## PLEASE CHECK FOR DATE FORMAT IN YOUR DATA BEFORE RUNNING

############################### RAW_DATA INPUT ################################

data <- read.csv(file.choose(),header=T)

############################### DATA PREPARATION #################################

colnames(data[,c(1:2)]) <- c('Month','Beer_Volume')
data[,'Month'] <- as.Date(data[,'Month'],'%m/%d/%Y')

## PREPARING A DATASET WITH COMPLETE DATA 

data <- subset(data,data$Month <= data[min(sapply(data, function(y) sum(!is.na(y)))),'Month'])
min_yr <- min(year(data[,'Month']))
max_yr <- max(year(data[,'Month']))
total_yrs <- (max_yr-min_yr+1)

## DIVIDING THE DATA INTO TRAINING AND TESTING USING 7:3 LOGIC
train <- round(total_yrs*0.7)
test <- round(total_yrs-train)

## DEPENDENT VARIABLE SUBSET
y <- data[,'Beer_Volume']

## INDEPENDENT VARIABLES SUBSET 
x <- data[,3:ncol(data)]
x[is.na(x)] <- 0

## Subsetting training data
y_train<-y[1:(12*train)]
x_train<-x[1:(12*train),]

## Subsetting test data
y_test<-y[((12*train)+1):(12*test)]
x_test<-x[((12*train)+1):(12*test),]

########################### DATA PREPARATION ENDS ###############################
j=0

for(a in seq(0.5,9,by=0.5)){
  fit<-cv.glmnet(as.matrix(x_train),y_train,alpha=(a/10),nfolds=4)
  #plot(fit)
  lambda<-fit$lambda.min
  final_fit<-glmnet(as.matrix(x_train),y_train,alpha=(a/10),lambda=lambda)
  results_test<-predict(final_fit,as.matrix(x_test))
  results_train<-predict(final_fit,as.matrix(x_train))
  alpha=a/10
  df<-data.frame(alpha,lambda,Outsample_Mape=accuracy(ts(results_test),ts(y_test))[5],Insample_Mape=accuracy(ts(results_train),ts(y_train))[5])
  
  if(j==0)
  {
    results<-df
    j=j+1
  }
  else
  {
    results<-data.frame(rbind(results,df))
  }
}

## Specifying column names
colnames(results)<-c("Alpha","Lambda","MAPE_Out","MAPE_In")

#Print the results
print(results)

#Sorting by out sample MAPE and printing the respective data with lowest Out sample MAPE
final_result<-results[order(results[,3]),]
print("Final alpha and Lambda value : ")
print(final_result[1,1:2])
cat("\n\n")
print("Final insample and out sample MAPE :")
print(final_result[1,3:4])

## FIT MODEL WITH THE LOWEST OUTSAMPLE 
fit_f<-glmnet(as.matrix(x_train),y_train,alpha=final_result[1,1],lambda=final_result[1,2])
tmp_coeffs <- coef(fit_f, s = "lambda.min")
temp_table <- data.frame(Var_names = tmp_coeffs@Dimnames[[1]][tmp_coeffs@i + 1], coefficient = tmp_coeffs@x)

## FINAL TABLE
list_variables <- temp_table[order(abs(temp_table[,2]),decreasing = T),]

############################ OUTPUT/SUMMARY ##################################

write.csv(list_variables,'Elastic_Net_results_4_variables.csv',row.names = F)