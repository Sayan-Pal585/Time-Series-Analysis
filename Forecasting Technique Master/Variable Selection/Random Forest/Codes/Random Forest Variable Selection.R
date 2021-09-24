#############################################################################################################

# RANDOM FOREST VARIABLE SELECTION 

############################################# NOTES: #################################################################

## PLEASE MAKE SURE THAT THE DATA HAS NO MISSING VALUES ANYWHERE
## RAW DATA SHOULD BE IN THE REQUIRED FORMAT::
## FLAT FILE SHOULD BE .CSV 
## COLUMN 1 SHOULD BE MONTH/DATE. FORMAT IN "mm/dd/yyyy"
## COLUMN 2 SHOULD BE BEER VOLUME/PRODUCTION/Y. FORMAT IN NUMERIC

#######################################################################################################################

##################################### A. Install Packages ########################################################
if(!require(svDialogs)){install.packages("svDialogs")};library(svDialogs)
if(!require(lubridate)){install.packages("lubridate")};library(lubridate)
if(!require(randomForest)){install.packages("randomForest")};library(randomForest)
if(!require(tcltk)){install.packages("tcltk")};library(tcltk)

#################################################################################################################

###################################### C. IMPORT RAW DATA #######################################################

setwd("C:\\Users\\C630972\\Documents\\Random_Forecast_Variable_selection\\Output")

data_raw<-read.csv(file.choose(),header=T)

data_raw$Month<-as.Date(data_raw[,1],"%m/%d/%Y")

############################################ D. DATA PREPARATION ############################################

# Import Data and Format Date/Month, column names of 1 & 2. Note: convert Beer volume class to Numeric (if integer)
colnames(data_raw)[1:2]<-c("Month","Beer_Volume")
data_raw$Beer_Volume<-as.numeric(data_raw[,"Beer_Volume"])

##################################### SELECT VARIABLES ###########################

var_list <- colnames(data_raw)
res1 <- dlgList(var_list,multiple=T,title="Select the variable you wish to Forecast")$res

############################### CONCERNED DATASET ###############################

data_raw <- data_raw[,c('Month','Beer_Volume',res1)]
## Preparing the dataset with last available data points
data_raw <- subset(data_raw,data_raw$Month <= data_raw[min(sapply(data_raw, function(y) sum(!is.na(y)))),'Month'])

#####################################################################################

if(any(res1 %in% c('Month','Beer_Volume'))){
  tkmessageBox(title="Attention:",message="You have selected Month & Dependent Variables. Please De-select and Run again.")
}else{
  
  rand_res <- randomForest(as.numeric(Beer_Volume)~.,data=data_raw[,c('Beer_Volume',res1)],importance=T)
  
  ############################## IMPORTANCE TABLE CREATION ##########################################
  
  imp_table <- data.frame(importance(rand_res,type=1))
  imp_table$Driver_names <- row.names(imp_table)
  row.names(imp_table) <- NULL
  imp_final_table <- imp_table[order(imp_table$X.IncMSE,decreasing = T),]
  colnames(imp_final_table) <- c('Importance_Score','Driver_names')
  imp_final_table <- imp_final_table[,c('Driver_names','Importance_Score')]
}  

####################################### OUTPUT SUMMARY ##########################################

write.csv(imp_final_table,'Random_Forecast_importance_score.csv',row.names = F)

########################################### END ######################################################
