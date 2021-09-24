##### loading the libraries required ###################################################################
library(data.table)
library(tidyr)

##### cleaning the environment #########################################################################
rm(list=ls())

##### set directory as per your need ###################################################################
setwd("C:\\Users\\C987706\\Desktop\\Peru")

##### INSTRUCTIONS TO READ DATA #########################################################################
##### make sure the order of the variables in the raw data and the coefficients file is same ############
##### variables' names should also be same in both the files ############################################
##### NOTE: the raw data is the original data i.e. data before log transformation #######################
##### NOTE: take exponential of dummy flag if any going to model  so that log transformation gives 1 or 0
##### NOTE: no need to include seasonal dummy flags as there will be no annual variation ################

raw_data = read.csv("raw_data for impact driver analysis.csv")
coefficients = read.csv("coeff for impact calculations.csv")
coefficients = coefficients[1,] #It is assumed that the coefficients are found on the first row as in the example


##### Data creation #################################################################################
##### two datasets are created so that ratio with annual lags couls be taken ########################
former = raw_data[!raw_data$Year == max(raw_data$Year),] #It stores the value of variables at n-1
latest = raw_data[!raw_data$Year == min(raw_data$Year),] #It stores the value of variables at n

ratio = latest[,-c(1:3)]/former[,-c(1:3)] #Calculates the ratio of (n-1)/n
ratio = cbind(latest[,c(1:3)],ratio)
ratiop = ratio[,-c(1:3)] #Creates dataframe to store the value of ratio to power

for (i in 1:ncol(ratiop)){
  ratiop[,i] = ratio[,i+3]^as.numeric(coefficients[i]) #Calculates and stores the values of ratio to power
}
ratiop = cbind(ratio[,c(1:3)],ratiop) #Joins the state, year and month variables to the ratiop data frame

##### function to take geometric mean for each year ################################################
gmean <- function(x, na.rm = FALSE){ 
  if(na.rm) x <- x[!is.na(x)] 
  n <- length(x) 
  prod(x)^(1/n) 
} 

##### Impact calculations #################################################################################
years = unique(ratiop$Year)
variables = colnames(ratiop)
aux1 =  matrix(tapply(X = ratiop[,4], INDEX = list(ratio$Year),FUN = gmean), nrow = 1, byrow = F) #Initializes an auxiliary variable to calculate the first table
aux1 = as.data.frame(aux1) #Changes the format of the variable from matrix to data frame
colnames(aux1) = years #Gives names to the columns
aux1$Var = variables[4] 
for (i in 2:(ncol(ratiop)-3)){ #It starts at 2 because the first variable was used above.
  c = matrix(tapply(X = ratiop[,3+i], INDEX = list(ratio$Year),FUN = gmean), nrow = 1, byrow = F) #Calculates and stores the geometric mean of ratio to power per state and year
  c = as.data.frame(c) #Changes the format of the variable from matrix to data frame
  colnames(c) = years #Gives names to the columns
  c$Var = variables[3+i] 
  aux1 = rbind(c,aux1) #Merge the results for the variable being evaluated to the previous results
}
m = gather(aux1, year,ratiop,1:length(years)) #Changes the distribution of the data 
aux1 = spread(data = m, key = Var, value = ratiop) #Changes the distribution of the data to match the example
aux1$Fitted = apply(aux1[,-c(1)], 1, prod) #Calculates the fitted column as the product of all regressors
table1 = (aux1[,-c(1)] -1)*100 #Calculates table 1
Year = as.data.frame(years)
Impact_Table = cbind(Year,table1)


##### driver change analysis ###################################################################################
cities = unique(ratio$Country)
years = unique(ratio$Year)
variables = colnames(ratio)
aux2 =  matrix(tapply(X = ratio[,4], INDEX = list(ratio$Year),FUN = gmean), nrow = 1, byrow = F) #Initializes an auxiliary variable to calculate the first table
aux2 = as.data.frame(aux2) #Changes the format of the variable from matrix to data frame
colnames(aux2) = years #Gives names to the columns
aux2$Var = variables[4] 
for (i in 2:(ncol(ratio)-3)){ #It starts at 2 because the first variable was used above.
  c = matrix(tapply(X = ratio[,3+i], INDEX = list(ratio$Year),FUN = gmean), nrow = 1, byrow = F) #Calculates and stores the geometric mean of ratio to power per country and year
  c = as.data.frame(c) #Changes the format of the variable from matrix to data frame
  colnames(c) = years #Gives names to the columns
  c$Var = variables[3+i] 
  aux2 = rbind(c,aux2) #Merge the results for the variable being evaluated to the previous results
}

m1 = gather(aux2, year,ratio,1:length(years)) #Changes the distribution of the data 
aux2 = spread(data = m1, key = Var, value = ratio) #Changes the distribution of the data to match the example
table2 = (aux2[,-c(1)] -1)*100 #Driver Table
Driver_Table = cbind(Year,table2)


##### saving the impact and the driver analysis results ######################################################
Impact_Table_Transpose = setNames(data.frame(t(Impact_Table[,-1])), Impact_Table[,1])
Driver_Table_Transpose = setNames(data.frame(t(Driver_Table[,-1])), Driver_Table[,1])

write.csv(Impact_Table_Transpose, "Impact Table.csv")
write.csv(Driver_Table_Transpose, "Driver Table.csv")


