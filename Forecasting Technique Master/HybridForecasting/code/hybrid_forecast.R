# To create sample and summary and other manipulations
if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)}

# To convert catagorical into dummy columns
if(!require(dummies)){
  install.packages("dummies")
  library(dummies)} 
# To Create modelling functions
if(!require(car)){
  install.packages("car")
  library(car)}

# To use pipe operator
if(!require(magrittr)){
  install.packages("magrittr")
  library(magrittr)
}

# To use random forest algorithm
if(!require(randomForest)){
install.packages("randomForest")
library(randomForest)
}

# to convert different outputs into datasets
if(!require(broom)){
  install.packages("broom")
  library(broom)
}



source("Regression Functions.R")

#*******************Creating dataframe and setting dependent variable**********

basedata<-read.csv("Renewal_Propensity-ADS_v7.csv",header=T,na.strings = c(""," "),stringsAsFactors = T)

str(basedata)



#Removing columns that are not required for analysis
basedataSub<-basedata[ , -which(names(basedata) %in% c ("Opportunity_18_ID","Created_Date","Expiration_Date","Inception_Renewal_Date","Premium_In_Force_converted_","Primary_Carrier","incept_create_diff","Gen_Channel","Renewaltag"))]

str(basedataSub)
# Converting categorical varaibles into columns
basedata_f <- dummy.data.frame(basedataSub, sep = "_")
basedata_f$Dep<-as.factor(basedata_f$Lapsetag)



#********************************Out Of Time Validation*****************

#1. Creating out of time based train test datasets

InTime<- basedata_f %>% filter(Train.Test== 2)
OutTimeTest<- basedata_f %>% filter(Train.Test== 3)



InTime<- basedata_f %>% filter(train_test== 1)
OutTimeTest<- basedata_f %>% filter(train_test== 2)
write.csv(InTime,"intime.csv")
write.csv(OutTimeTest,"outtime.csv")

#****************************Out Of Sample Validation****************

# Creating Train Test dataframes

sample<- floor(0.70 * nrow(InTime))
set.seed(123)
trainIndex <- sample(seq_len(nrow(InTime)), size = sample)
train <- InTime[trainIndex, ]
test <- InTime[-trainIndex, ]



# Creating Linear model to find vif
LinFit <- lm(model_eqn,data=train)
colnames(train)
# Checking and removing alias
alias(LinFit)

# Adding different measures of parameters into summary
summary<-data.frame(Variables=row.names(summary(LinFit)$coefficients))
summary$Estimate = summary(LinFit)$coefficients[,1]
summary$Std.Error = summary(LinFit)$coefficients[,2]
summary$tValue = summary(LinFit)$coefficients[,3]
summary$PValue = summary(LinFit)$coefficients[,4]
summary$RSq = summary(LinFit)$r.squared
summary$Adj.RSq = summary(LinFit)$adj.r.squared

# Adding vif to the the summary to check multicolinearity

vif_forced_model <- data.frame(vif(LinFit))
vif_forced_model$Variables <- rownames(vif_forced_model)
rownames(vif_forced_model) <- NULL
colnames(vif_forced_model)<-c("VIF","Variables")
summary <- merge.data.frame(summary, vif_forced_model, by="Variables", all.x = TRUE)
View(summary)

# Iterative model equation with removing of columns using vif and p value (Using backward elimination)

# model_eqn<-Dep~Line_of_Business_Aviation	+
#   Line_of_Business_Energy	+
#   Line_of_Business_Financial_Lines	+
#   #Line_of_Business_Liability	+
#   Line_of_Business_Marine	+
#   #Line_of_Business_Motor +
#   #Line_of_Business_Package_Product	+
#   Line_of_Business_Property +
#   Primary_Excess_Missing	+
#   Primary_Excess_Excess	+
#   #Business_Unit_Specialty_Inland_Marine+
#   #Business_Unit_Healthcare+
#   #Business_Unit_Burnett_Co_QBE_Insurance_Group+
#   QBE_Premium_converted_	+
#   gwp_lost_2yrs	+
#   num_won_2yrs	+
#   gwp_won_2yrs	+
#   num_lostnew_2yrs	+
#   num_wonnew_2yrs	+
#   gwp_decline_2yrs	+
#   num_existing	+
#   num_lostrenew_2yrs	+
#   broker_contact_num_won_2yrs	+
#   incept_create_bin_incept_within_half_year	+
#   incept_create_bin_incept_within_the_year	+
#   MTP_Channel_Agency	+
#   MTP_Channel_AJ_Gallagher



model_eqn<-Dep~Line_of_Business_Aviation	+
  Line_of_Business_Financial_Lines	+
  #Line_of_Business_Construction +
  Line_of_Business_Liability	+
  #Line_of_Business_Motor +
  QBE_Premium_converted_	+
  gwp_lost_2yrs	+
  gwp_won_2yrs	+
  gwp_wonnew_2yrs +
  broker_GWP_lost_2yrs +
  broker_contact_num_won_2yrs	+
  broker_contact_GWP_lost_2yrs +
  broker_contact_num_won_2yrs +
  broker_months_since_last_won +
  MTP_Channel_Agency +
  MTP_Channel_AJ_Gallagher








colnames(train)


# Fitting model
rf <- randomForest(model_eqn, data = InTime, importance = TRUE,ntree=575, mtry=4)
# % incresed mse and increase node purity
write.csv(importance(rf,type = 1),"MeanDecreaseAccuracy.csv")
write.csv(importance(rf,type = 2),"MeanDecreaseGini.csv")
View(importance(rf))
View(importance(rf,type = 2))
summary(rf)
rf
# Calculating probabilities and adding it to the dataframe
predicted_prob_tr<-predict(rf,newdata = train,type=c("prob"))
train$pred_prob<-(predicted_prob_tr)[,2]
write.csv(train,"train_set.csv")

predicted_prob_te<-predict(rf,newdata = test,type=c("prob"))
test$pred_prob<-predicted_prob_te[,2]
write.csv(test,"test_set.csv")

# Creating Gain Table
trainGain <- getGainsTableforTrain(train$Dep , train$pred_prob)
View(trainGain)

testGain <- getGainsTableforTest(test$Dep , test$pred_prob)

View(testGain)

# Ploting Lift Chart
plotLiftChartforTrain(trainGain)

plotLiftChartforTest(testGain)

# Calculating KS value
KSvalue<-KS(train$pred_prob,train$Dep)
KSvalue

# Ploting ROC Curve and Calculating area under the curve of the polt
optErrorProb<-plotROC(train$Dep, train$pred_prob)
optErrorProb
auc(train$Dep, train$pred_prob)

# Calculating Acurracy and Precision
createConfusionMatforTrain(train$Dep, train$pred_prob,optErrorProb)
createConfusionMatforTest(test$Dep, test$pred_prob,optErrorProb)

# summary(LogFit)

# confusionMatrix(test$Dep,test$pred_prob)


#***************Out of Time Validation****************

predicted_prob_ot<-predict(rf,newdata = OutTimeTest,type=c("prob"))
OutTimeTest$pred_prob<-predicted_prob_ot[,2]
write.csv(OutTimeTest,"outtimeprob.csv")
predicted_prob_ot

# Confusion matrix fro out of time validation
createConfusionMatforTest(OutTimeTest$Dep, OutTimeTest$pred_prob, 0.1)


# Writing confusion matrix to csv
cm<-tidy(createConfusionMatforTest(OutTimeTest$Dep, OutTimeTest$pred_prob, optErrorProb))
write.csv(cm,"ConfusionMatrixOutofTimeTestDataset.csv")
class(OutTimeTest$Dep)
class(OutTimeTest$pred_prob)
# Gain Table
OutTestGain <- getGainsTableforTest(OutTimeTest$Dep , OutTimeTest$pred_prob)
write.csv(OutTestGain,"OutofTimeTestGain.csv")
View(OutTestGain)

# Calculating KS value
KSvalue<-KS(OutTimeTest$pred_prob,OutTimeTest$Dep)
KSvalue

# Lift Chart
plotLiftChartforTest(OutTestGain)

auc(OutTimeTest$Dep, OutTimeTest$pred_prob)
