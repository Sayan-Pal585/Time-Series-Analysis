############################################################################
# CODE: VARIABLE SELECTION - LASSO REGRESSION
# AUTHOR: DEEPESH SINGH - GAC BANGALORE
# DATE: 23RD FEB 2017
# LAST UPDATE: N/A
# ABOUT LAST UPDATE: N/A
# LAST UPDATE BY: N/A
############################################################################
#Install required packages

rm(list=ls())

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

# Making matrix for lasso regression

# Removing depedent variable from matrix

# Construct design matrix
lasso_x <- model.matrix(Beer_Volume ~.-1, data = training_data)
lasso_y <- training_data$Beer_Volume


# Model selection
cv.lasso <- cv.glmnet(x = lasso_x, y = lasso_y, alpha = 1)
plot(cv.lasso)


cv.lasso$lambda.min

coef(cv.lasso, s = "lambda.min")

cv.lasso$lambda.1se
coef(cv.lasso, s = "lambda.1se")


# Applying lasso
lamb <- cv.lasso$lambda.1se

fit.lasso <- glmnet(x = lasso_x, y = lasso_y, alpha = 1, lambda = lamb)
plot(fit.lasso, xvar = "lambda", label = T)

# Variance plot shows sudden increase in R sqaure after 6-7
plot(fit.lasso,xvar="dev",label=TRUE)


# Getting coefficient properly sorted

# Validating the model against validation data

getcoeff <- function(method = "method"){
  lasso_coeff <- as.data.frame(as.matrix(coef(cv.lasso, s = method)))
  lasso_coeff$variables <- row.names(lasso_coeff)
  row.names(lasso_coeff) <- NULL
  lasso_coeff <- lasso_coeff[,c(2,1)]
  names(lasso_coeff) <- c("Variables", "Coefficeint")
  lasso_coeff$abscoeff <- abs(lasso_coeff$Coefficeint)
  lasso_coeff <- subset(lasso_coeff, lasso_coeff$abscoeff != 0)
  lasso_coeff <- lasso_coeff[order(-lasso_coeff$abscoeff),]
  return (lasso_coeff)
}

lasso_coeff1se <- getcoeff(method = "lambda.1se")
lasso_coeffMSE <- getcoeff(method = "lambda.min")


write.csv(lasso_coeffMSE,"lasso_coeffMSE.csv",row.names = F)
write.csv(lasso_coeff1se,"lasso_coeff1se.csv",row.names = F)

# Prediction using glmnet function
# Construct predict matrix

lasso_x_pred <- model.matrix(Beer_Volume ~.-1, data = validation_data)

lasso_y_pred <- validation_data$Beer_Volume

# Prediction and accuracy check

pred_results_insample <- predict(fit.lasso, as.matrix(lasso_x))
pred_results <- predict(fit.lasso, as.matrix(lasso_x_pred ))


insample_accuracy <- data.frame(accuracy(ts(pred_results_insample),ts(lasso_y)))  #In Sample accuracy
outsample_accuracy <- data.frame(accuracy(ts(pred_results), ts(lasso_y_pred)))   # Outsample accuracy


write.csv(insample_accuracy,"LASSO_insample.csv",row.names = F)
write.csv(outsample_accuracy,"LASSO_outsample.csv",row.names = F)


##################### CODE ENDS HERE ############################################
# REFERENCES
# 1. https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html
# 2. http://gerardnico.com/wiki/lang/r/ridge_lasso#model_selection
################################################################################
