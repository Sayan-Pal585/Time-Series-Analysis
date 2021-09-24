##=========================================================================##

##------------- Regression based Forecast(Steel) --------------------------##

##=========================================================================##

library(tidyverse)
library(broom)
library(car)
library(gbm)
setwd('C:\\Users\\TrinaNaskar\\Documents\\Daimler\\Notebooks\\Lagged')

## ------------ Importing raw data
raw_data <- read_csv('SteelMultivariate_Lag_ADS_Regress.csv')
raw_data$Month <- as.factor(raw_data$Month)
raw_data <- raw_data%>%select(-contains('Ironore'))

##------------ Split train and test

# Sampling split
smp_size <- floor(0.904 * nrow(raw_data))
set.seed(123)
train_ind <- sample(seq_len(nrow(raw_data)), size = smp_size)

train <- raw_data[train_ind, ]
test <- raw_data[-train_ind, ]

# train <- raw_data[1:119,]
# test <- raw_data[120:125,]


##------------ lm


#lm_all <- lm(Steel ~ Coal_Lag4 + Al_Lag1 + Coal_Lag2 + Nickel_Lag2 ,data = train) 

lm_all <- lm(Steel ~ Coal_Lag4 + Al_Lag1 + Coal_Lag2  + Nickel_Lag2  ,data = train) 
lmod <- tidy(lm_all)
summary(lm_all)
vif(lm_all)

pred <- predict(lm_all, test)
test_pred <- test%>%
  mutate(pred_val = pred,
         APE = abs(Steel - pred_val)/Steel)
# MAPE
MAPE <- mean(test_pred$APE)
MAPE

test_pred_df <- test_pred%>%select(Month, Steel, pred_val)
write_csv(test_pred_df, 'test_pred_df_lm_3mo.csv')

##-------------gbm 

raw_data2 <- raw_data
raw_data <- raw_data%>%keep(names(raw_data) %in% var | names(raw_data) %in% c('Steel'))

## 75% of the sample size
smp_size <- floor(0.904 * nrow(raw_data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(raw_data)), size = smp_size)

train <- raw_data[train_ind, ]
test <- raw_data[-train_ind, ]


# train GBM model
gbm.fit <- gbm(
  formula = Steel ~ .,
  distribution = "gaussian",
  data = train,
  n.trees = 500,
  interaction.depth = 1,
  shrinkage = 0.001,
  cv.folds = 5,
  n.cores = NULL, # will use all cores by default
  verbose = FALSE
) 

gbm_varimp <- (summary(gbm.fit))

pred <- predict(gbm.fit, n.trees = 500, test)
test_pred <- test%>%
  mutate(pred_val = pred,
         APE = abs(Steel - pred_val)/Steel)

# MAPE
MAPE <- mean(test_pred$APE)
MAPE


##--------------- Contribution (total)
data_relevant <- raw_data%>%select(Coal_Lag4, Coal_Lag2, Al_Lag1, Nickel_Lag2, Steel)
data_org <- data_relevant
for(i in c(1:ncol(data_relevant)))
{
  if(names(data_relevant)[i]!='Steel')
  {
    data_relevant[,i] <- data_relevant[,i] * as.numeric(lmod[lmod$term == names(data_relevant)[i],"estimate"])
  }
}

data_contribution <- 100*(data_relevant/data_relevant$Steel)

# Effect of month on price 
kruskal.test(data_relevant$Steel ~ raw_data$Month)

# Plot Steel vs Month
library("ggpubr")
ggboxplot(raw_data, x = "Month", y = "Steel", 
          color = "Month", 
          order = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
          ylab = "Weight", xlab = "Treatment")

# Multiple pairwise comparisons 
pairwise_Kruskal_Wallis <- pairwise.wilcox.test(raw_data$Steel, raw_data$Month,
                     p.adjust.method = "none")
#pairwise_Kruskal_Wallis <- as.data.frame(pairwise_Kruskal_Wallis)


##--------------------------- Loops for accuracy sheets 

train_main <- raw_data[1:101,]
test_main <- raw_data[102:125,]
pred_store <- data.frame(pred = c(0),
                         val = c(0))

for(i in c(24:1))
{

lm_all <- lm(Steel ~ Coal_Lag4 + Al_Lag1 + Coal_Lag2  + Nickel_Lag2  ,data = train_main[1:(nrow(train_main)-i),]) 
test <- test_main[(nrow(test_main)-i+1):nrow(test_main),]
pred <- predict(lm_all, test)
pred <- as.data.frame(pred)
pred$val <- i
pred_store <-  rbind(pred_store, pred)

}

