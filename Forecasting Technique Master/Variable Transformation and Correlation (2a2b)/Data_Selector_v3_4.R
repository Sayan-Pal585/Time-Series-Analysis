############################################################################
############################################################################
# CODE: DATA SELECTOR (2A-2B)
# AUTHOR: VEDANT PRASAD - GAC BANGALORE
# DATE: 3RD AUG 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
##INSTRUCTIONS
##1. FIRST COL OF YOUR INPUT SHOULD BE MONTH IN '01/01/2001' FORMAT       
##2. SECOND COL OF YOUR INPUT SHOULD BE BEER VOLUME                       
############################################################################
#### A. INSTALL PACKAGES #### 

if(!require(lubridate)){install.packages("lubridate")}
if(!require(sqldf)){install.packages("sqldf")}

#### B. SPECIFY INPUT DATASET #### 

data = read.csv(file.choose(),header=T)

#### C. ENTER TIME PERIOD #### 

year_start = readline("Enter your training start year : ")
year_end = readline("Enter your training last year : ")
month_start = readline("Enter your training start month eg: 03 : ")
month_end = readline("Enter your training end month eg: 03 : ")


#### D. SET OUTPUT DIRECTORY  ####

setwd("C:/Users/C938474/Documents/GFF/DataSelector")


###RUN FROM HERE###
########################################################
##                  1.DATA CLEANING                   ## 
########################################################

colnames(data)[1:2]<-c("Month","Beer_Volume")
data = cbind(data, format_date = as.Date(as.character(data$Month),"%m/%d/%Y"))

### subsetting data based on beer volume availability 
#data <- data[1:sum(!is.na(data$Beer_Volume)),]
data<-subset(data,!is.na(data$Beer_Volume))

### data preparation ### 
data_backup = data
data = data[,-c(ncol(data))]

### subsetting data based on user input for month & year
#year_start = readline("Enter your training start year : ")
#year_end = readline("Enter your training last year : ")
#month_start = readline("Enter your training start month eg: 03 : ")
#month_end = readline("Enter your training end month eg: 03 : ")

start_date = paste0(month_start,"01",year_start)
start_date = as.Date(as.character(start_date), "%m%d%Y")

end_date = paste0(month_end,"01",year_end)
end_date = as.Date(as.character(end_date), "%m%d%Y")

data_backup = subset(data_backup, format_date >= start_date & format_date <= end_date)
##removing formatted_date column
data_backup = data_backup[,-c(ncol(data_backup))]


### calculating for missing values 
missing_values = sapply(data_backup, function(y) sum(length(which(is.na(y)))))
missing_values = data.frame(missing_values)

### calculating the minimum values 
min_value = apply(data_backup,2,min)
min_value = data.frame(colnames(data_backup),min_value)


### Data subsetting has been done #### 

### calculating the minimum values 
min_value_datasub = apply(data_backup[,-c(1,2)],2, min)
min_value_datasub = data.frame(variable = colnames(data_backup[,-c(1,2)]),value = min_value_datasub)

#### Flagging variables which have zero or negative values 
min_value_datasub = cbind(min_value_datasub, flag = ifelse(min_value_datasub$value <0 , 1, 0))

#### displaying warning box for zero & negative values 

#if (sum(min_value_datasub$value) > 0 ){
#    require(tcltk)
#    msgBox <- tkmessageBox(title = "WARNING",
#                           message = "Beware of transformations as you have negative values", icon = "info")
#    
#    View(subset(min_value_datasub,flag == 1))
#} 

#Invert the data
c = order(c(1:nrow(data_backup)), decreasing = T)
data_backup = data_backup[c,]

#Removing near-zero variance variables and variables with over 25% of NAs.
data_t3 = data_backup[,-c(1:2)] #It creates an auxiliary data frame to delete the variables with the criteria mentioned above.
teta = 1e-10 #It's the threshold for the variance. All variables whose variance is less than or equal to this value will be discarded.
n = length(which(diag(var(data_t3, use = "pairwise.complete.obs"))<=teta)) #It says how many variables will be discarded, if any.
if (!n==0){ #It enters if there's at least 1 variable to discard.
  data_t3 = data_t3[,-which(diag(var(data_t3, use = "pairwise.complete.obs"))<=teta)] #It measures the variance for all variables and deletes those whose variance is less than teta.  
}

nv = 0 #c saves the value of NAs for each variable. Starts in zero. 
for (i in 1:ncol(data_t3)){ #It goes through all variables that were left after removing near-zero variance variables
  nv[i] = length(which(is.na(data_t3[,i]))) #It counts how many NAs there are in the column for variable i 
}
data_t3 = data_t3[,which(nv/nrow(data_t3)<=0.25)] #It keeps the variables that have less or equal than 25% of NAs. 

nv = 0 #c saves the value of NAs for each variable. Starts in zero. 
for (i in 1:ncol(data_t3)){ #It goes through all variables that were left after removing near-zero variance variables
  nv[i] = length(which(is.na(data_t3[,i]))) #It counts how many NAs there are in the column for variable i 
}

##Check for Variance and Nulls##
length(which(nv/nrow(data_t3)>0.25)) #It tells how many variables are left with over 25% of NAs. This value is supposed to be zero.
length(which(diag(var(data_t3, use = "pairwise.complete.obs"))<=teta)) #It tells how many variables are left with near-zero variance (Var <= 1e-10). This value is supposed to be zero.
##Check for Variance and Nulls##


##Adding dependent back##
data_backup_new = cbind(data_backup[,c(1:2)],data_t3)


########################################################
##                2.DATA TRANSFORMATION               ## 
########################################################


#Variable transformation
data_new = data_backup_new[,-which(colnames(data_backup_new)=="Beer_Volume")]
data_new = data_new[,-which(colnames(data_new)=="Month")]
original_v = ncol(data_new)


for (i in 1:original_v){
  extra = original_v+ 6*(i-1)
  "SqRT, inv, log"
  col.name = colnames(data_new)
  data_new[,extra+1]=((data_new[,i])^0.5)
  data_new[,extra+2]=1/(data_new[,i])
  data_new[,extra+3]=log(data_new[,i])
  colnames(data_new) = c(col.name,paste(col.name[i],c("SqRT","inv","log"),sep = "_"))
  
  #lags
  col.name = colnames(data_new)
  for (j in 1:3){
    extra_lag = embed(data_new[,i],j+1)
    data_new[1:nrow(extra_lag),extra+j+3]=extra_lag[,1]
  }
  colnames(data_new) = c(col.name,paste(col.name[i],"lag",c(1:3),sep = "_"))
}


#### Summary table with mean and var
summary = data.frame(matrix(vector(), ncol(data_new), 8,dimnames=list(c(), c("Variable","Min.", "First Qu.", "Median", "Mean", "Third Qu.", "Max.", "Variance"))),stringsAsFactors=F)
for (i in 1:ncol(data_new)){
  summary[i,1]=colnames(data_new)[i]
  summary[i,2:7]=summary(data_new[,i])[1:6]
  summary[i,8]=var(data_new[,i],use="pairwise.complete.obs") 
}

write.csv(summary,"summary_raw_data_with_transformations.csv",row.names = F)


########################################################
##                3.VARIABLE SELECTION                ## 
########################################################


## Extracting variables with highest correlation from original & transformations ### 

#Correlation of lags with Beer_Volume
name_max_lag = 0
pos_max_lag = 0
id_max_lag = 0

name_max = 0
pos_max = 0
id_max = 0

col.name = colnames(data_new)
cor_matrix = cor(cbind(data_backup$Beer_Volume,data_new), use="pairwise.complete.obs")
cor_val = cor_matrix[,1]


for (i in 1:original_v){
  #position = grep(col.name[i],colnames(cor_matrix))
  position = c(i+1,i+original_v+((i-1)*6)-(i-1)+1:6)
  cor_extra = cor_val[position]
  
  #correlation of lags - finding the lag with highest correlation with Beer_Volume
  id_max_lag[i] = which.max(abs(cor_extra[5:7]))+4
  pos_max_lag[i] = position[id_max_lag[i]]
  name_max_lag[i] = colnames(cor_matrix)[pos_max_lag[i]]
  
  #correlation of other transformations - finding the transformation with highest correlation with Beer_Volume
  y = c(1:4,id_max_lag[i])
  x = abs(cor_extra[y])
  id_max[i] = which.max(x)
  pos_max[i] = position[y[id_max[i]]]
  name_max[i] = colnames(cor_matrix)[pos_max[i]]  
}


m<- c(name_max) #best of the variables and their transformations
final_data_1 <- data_new[,1:original_v]

final_data_2 <- data_new[m]
final_data_v1 <- cbind(final_data_1,final_data_2) #data with variables plus their best transformation
final_data_v1 <- cbind(data_backup$Beer_Volume,final_data_v1)
final_data_v1 <- cbind(data_backup$Month,final_data_v1)

final_data_v2 = cbind(data_backup$Beer_Volume,final_data_2) #data with best independents & the dependent variable
final_data_v2 <- cbind(data_backup$Month,final_data_v2)
final_data_v3 = cbind(data_backup$Beer_Volume,data_new) #data with best independents & the dependent variable
final_data_v3 <- cbind(data_backup$Month,final_data_v3)

names(final_data_v2)[names(final_data_v2) == 'data_backup$Beer_Volume'] <- 'Beer_Volume'
names(final_data_v1)[names(final_data_v1) == 'data_backup$Beer_Volume'] <- 'Beer_Volume'
names(final_data_v3)[names(final_data_v3) == 'data_backup$Beer_Volume'] <- 'Beer_Volume'
names(final_data_v2)[names(final_data_v2) == 'data_backup$Month'] <- 'Month'
names(final_data_v1)[names(final_data_v1) == 'data_backup$Month'] <- 'Month'
names(final_data_v3)[names(final_data_v3) == 'data_backup$Month'] <- 'Month'

#Invert the data
c = order(c(1:nrow(final_data_v1)), decreasing = T)
final_data_v1 = final_data_v1[c,]


c = order(c(1:nrow(final_data_v2)), decreasing = T)
final_data_v2 = final_data_v2[c,]

c = order(c(1:nrow(final_data_v3)), decreasing = T)
final_data_v3 = final_data_v3[c,]


write.csv(final_data_v1, "Original data with best transformations.csv",row.names = F)

write.csv(final_data_v2, "Final Data with best variables.csv",row.names = F)

write.csv(final_data_v3, "Original data with ALL transformations.csv",row.names = F)

final_cor = as.data.frame(cor(final_data_v2[,-1], use="pairwise.complete.obs"))
write.csv(final_cor, "Final transformed correlations.csv",row.names = F)

negative_vars<-subset(min_value_datasub,flag == 1)
write.csv(negative_vars, "Variables with negative values.csv",row.names = F)

########################################################
##                    END                             ## 
########################################################
