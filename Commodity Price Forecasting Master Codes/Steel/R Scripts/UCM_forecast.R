##=========================================================================##

##-------------------- UCM based Forecast(Steel) --------------------------##

##=========================================================================##

##------------------ Example implementation 

library(rucm)
library(datasets)
library(tidyverse)
setwd('C:\\Users\\TrinaNaskar\\Documents\\Daimler\\Notebooks\\Lagged')

##--------------- Example Implementation
# modelNile <- ucm(formula = Nile~0, data = Nile, level = TRUE)
# modelNile
# 
# plot(Nile, ylab = "Flow of Nile")
# lines(modelNile$s.level, col = "blue")
# legend("topright", legend = c("Observed flow","S_level"), col = c("black","blue"), lty = 1)
# 
# 
# modelNile <- ucm(formula = Nile~0, data = Nile, level = TRUE, slope = TRUE)
# predict(modelNile$model, n.ahead = 12) # Forecasting

##------------------ Steel implementation 

## ------------ Importing raw data
raw_data <- read_csv('SteelDataset_2009.csv')
#raw_data$Month <- as.factor(raw_data$Month)
#raw_data <- raw_data%>%select(-contains('Ironore'))


modelPrice <- ucm(formula = Price~0, data = raw_data, level = TRUE)
modelPrice

plot(raw_data$Price, ylab = "Price of Price")
lines(modelPrice$s.level, col = "blue")
legend("topright", legend = c("Observed flow","S_level"), col = c("black","blue"), lty = 1)

modelPrice <- ucm(formula = Price~0, data = raw_data, level = TRUE, slope = TRUE)
pred <- predict(modelPrice$model, n.ahead = 24) # Forecasting
pred_df <- as.data.frame(pred)

pred_df <- raw_data[109:132,'Price']
pred_df$pred_val <- pred
pred_df <- pred_df%>%
  mutate(ape = abs(Price - pred_val)/Price,
         date = raw_data[109:132,]$Date)

MAPE <- mean(pred_df$ape)
MAPE 

write_csv(pred_df,"pred_UCM_DecUpdate.csv")

pred_store2 <- data.frame(iteration = 1,
                          fit = 0.00000001)

### Accuracy Sheet Loop

for(i in c(0:25))
  {
    modelPrice <- ucm(formula = Price~0, data = raw_data[1:(107+i),], level = TRUE, slope = TRUE)
    pred <- predict(modelPrice$model, n.ahead = 24)
    pred_store1 <- as.data.frame(pred)
    pred_store1$iteration <- i
    pred_store2 <- rbind(pred_store2, pred_store1)

  }