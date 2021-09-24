############################################################################
# CODE: VARIABLE SELECTION - RIDGE REGRESSION
# AUTHOR: DEEPESH SINGH - GAC BANGALORE
# DATE: 23RD FEB 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
#############################################################################
# Setting up environment

require(glmnet)
library(glmnet)
library(lubridate)
library(forecast)

############################################################################
#Set working directory

setwd("O:\\DATA\\Argentina\\Data")

############################################################################
#Read input file

in_frame <- read.csv(file.choose(), sep = ",", header = T, stringsAsFactors = F)

############################################################################
#User inputs

TrainingYear <- 2014
ValidationYear <- 2015


############################################################################
#Converting date to proper format

in_frame$Year <- year(as.Date(in_frame$Month, "%m/%d/%Y"))

# Removing rows where any column has NULL or 0
#in_frame <- in_frame[rowSums(is.na(in_frame)) == 0,]

in_frame[is.na(in_frame)] <- 0

#in_frame <- in_frame[rowSums(is.na(in_frame)) == NULL,]

# Cutting training data

training_data <- subset(in_frame, in_frame$Year <= TrainingYear)
validation_data <- subset(in_frame, in_frame$Year == ValidationYear)

# Removing month column
training_data <- training_data[, -c(1, ncol(training_data))]
validation_data <- validation_data[, -c(1, ncol(validation_data))]

# Making matrix for Ridge regression
# Removing depedent variable from matrix

# Construct design matrix
ridge_x <- model.matrix(Beer_Volume ~.-1, data = training_data)

ridge_y <- training_data$Beer_Volume


# Model selection
cv.ridge <- cv.glmnet(x = ridge_x, y = ridge_y, alpha = 0)
plot(cv.ridge)


cv.ridge$lambda.min
coef(cv.ridge, s = "lambda.min")

cv.ridge$lambda.1se
coef(cv.ridge, s = "lambda.1se")


# Applying ridge
lamb <- cv.ridge$lambda.1se

fit.ridge <- glmnet(x = ridge_x, y = ridge_y, alpha = 0, lambda = lamb)
plot(fit.ridge, xvar = "lambda", label = T)

# Variance plot shows sudden increase in R sqaure after 6-7
plot(fit.ridge,xvar="dev",label=TRUE)


# Getting coefficient properly sorted

# Validating the model against validation data

getcoeff <- function(method = "method"){
  ridge_coeff <- as.data.frame(as.matrix(coef(cv.ridge, s = method)))
  ridge_coeff$variables <- row.names(ridge_coeff)
  row.names(ridge_coeff) <- NULL
  ridge_coeff <- ridge_coeff[,c(2,1)]
  names(ridge_coeff) <- c("Variables", "Coefficeint")
  ridge_coeff$abscoeff <- abs(ridge_coeff$Coefficeint)
  ridge_coeff <- subset(ridge_coeff, ridge_coeff$abscoeff != 0)
  ridge_coeff <- ridge_coeff[order(-ridge_coeff$abscoeff),]
  return (ridge_coeff)
}

ridge_coeff1se <- getcoeff(method = "lambda.1se")
ridge_coeffMSE <- getcoeff(method = "lambda.min")


write.csv(ridge_coeffMSE,"ridge_coeffMSE.csv",row.names = F)
write.csv(ridge_coeff1se,"ridge_coeff1se.csv",row.names = F)

##################### CODE ENDS HERE ############################################
# REFERENCES
# 1. https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# 2. http://gerardnico.com/wiki/lang/r/ridge_lasso#model_selection
################################################################################