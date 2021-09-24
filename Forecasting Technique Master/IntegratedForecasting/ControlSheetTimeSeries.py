# -*- coding: utf-8 -*-
"""
Created on Fri Jan 25 15:35:58 2019

@author: TheMathCompany
"""

################################ Math Market ##################################

###############################################################################
#Importing python packages
###############################################################################
import pandas as pd
from datetime import datetime
import os
import warnings
from plotly.offline import download_plotlyjs as plot


###############################################################################
#Importing Modules
###############################################################################
import TimeSeriesFunctions as tsf


###############################################################################
#Reading the dataframe(ADS)
###############################################################################
df =pd.read_csv("dataset/unemp_rate.csv",parse_dates=['date'],index_col='date')


###############################################################################
# FUNCTION: Plots Auto correlation(ACF) and partial autocorrelation (PACF)
# INPUT: df - Data Frame(ADS), dep_var - Dependent variable(String variable 
#        eg. "CARAVAN">)
# OUTPUT: Plot of ACF & PACF
###############################################################################
fig = tsf.plotAcfPacf(df,'value')
#saving plot 
fig.savefig("saved files/plots/acfPacf")


###############################################################################
# FUNCTION: Splits the data and plots the graph for Trend/Seasonality 
#           and randomness
# INPUT: df - Data Frame(ADS), dep_var - Dependent variable(String variable 
#        eg. "CARAVAN">)
# OUTPUT: Plots of Trend/seasonality and randomness
###############################################################################
trend,seasonality,irregularity = tsf.stl(df, 'value')
#saving plots
plot(trend,config=tsf.config,
     filename='saved files/plots/trend plot')
plot(seasonality,config=tsf.config,
     filename='saved files/plots/seasonality plot')
plot(irregularity,config=tsf.config,
     filename='saved files/plots/irregularity plot')


###############################################################################
# Splitting the dataset into train and test
###############################################################################
train = pd.DataFrame(df[:int(len(df)*7/10)])
test = pd.DataFrame(df[int(len(df)*7/10):])


###############################################################################
# FUNCTION: Performs explonential smoothing on dataframe
# INPUT: train - Data Frame(ADS), dep_var - Dependent variable(String variable 
#        eg. "CARAVAN">,optimized = True/False)
# OUTPUT: Predicted values, MAPE
###############################################################################
model_fit=tsf.expSmoothing(train,depVar='value', optimized=True)
warnings.simplefilter('ignore')
dfWithTrainPred=tsf.holtsWinterTrainPred(train,model_fit,depVar='value')
dfWithTestPred=tsf.holtsWinterTestPred(test,model_fit,depVar='value')


###############################################################################
# FUNCTION: Performs Forecasting Technique Holt's Winter
# INPUT: train - Data Frame(ADS), dates, seasonal_periods,
#        seasonal(Additive/Multiplicative)
# OUTPUT: Predicted values and Model Summary
###############################################################################
model_fit = tsf.holtsWinter(train,'value',dates=df.index, seasonal_periods=15,
                            seasonal='add')
mape = tsf.modelSummary(train,model_fit,'value')
dfWithTrainPred=tsf.holtsWinterTrainPred(train,model_fit,depVar='value')
dfWithTestPred=tsf.holtsWinterTestPred(test,model_fit,depVar='value')
dfWithTrainPred.head()


###############################################################################
# FUNCTION: Performs Forecasting Technique Arima
# INPUT: train - Data Frame(ADS), dep_var - Dependent variable(String variable 
#        eg. "CARAVAN">, order (p,d,q)
#        seasonal(Additive/Multiplicative)
# OUTPUT: Predicted values and Model Summary
###############################################################################
build_model =tsf.arima(train,order=(4,0,3),depVar='value')
model_fit = build_model[0]
parameters = build_model[1]
mape = tsf.modelSummary(train,model_fit,depVar='value')
dfWithTrainPred=tsf.TrainPred(train,model_fit,depVar='value')
dfWithTestPred=tsf.TestPred(test,model_fit,depVar='value')



###############################################################################
# FUNCTION: Performs Forecasting Technique Arima
# INPUT: train - Data Frame(ADS), dep_var - Dependent variable(String variable 
#        eg. "CARAVAN">, seasonal - integer, autoregressive - integer,
#        irregular (True/False)
#        seasonal(Additive/Multiplicative)
# OUTPUT: Predicted values and Model Summary
###############################################################################
build_model =tsf.ucm(train,depVar='value',seasonal=12,autoregressive=17,
                     irregular=True)
model_fit = build_model[0]
parameters = build_model[1]
mape = tsf.modelSummary(df,model_fit,depVar='value')
dfWithTrainPred=tsf.TrainPred(train,model_fit,depVar='value')
dfWithTestPred=tsf.TestPred(test,model_fit,depVar='value')

###############################################################################
# FUNCTION: Performs Forecasting Technique Arima
# INPUT: train - Data Frame(ADS), dep_var - Dependent variable(String variable 
#        eg. "CARAVAN">, order - (p,d,q), seasonal_order - (P,D,Q,m)
#        irregular (True/False)
#        seasonal(Additive/Multiplicative)
# OUTPUT: Predicted values and Model Summary
###############################################################################
build_model =tsf.sarima(train,depVar='value', order = (4,1,1),
                        seasonal_order = (6,0,5,12))
model_fit = build_model[0]
parameters = build_model[1]
mape = tsf.modelSummary(train,model_fit,depVar='value')
dfWithTrainPred=tsf.TrainPred(train,model_fit,depVar='value')
dfWithTestPred=tsf.TestPred(test,model_fit,depVar='value')


###############################################################################
# Performing Grid Search on MovingAverages, Arima and Sarima to get optimal
# Tuning Parameters
###############################################################################
tsf.gridSearchSarima(df,'value',max_p=3,max_d=3,max_q=3,silent=False)
tsf.gridSearchArima(df,'value',max_p=3,max_d=3,max_q=3,silent=False)
tsf.gridSearchMovingAverage(df,'value',bestWindow=7)


###############################################################################
# Saving iteration to csv
###############################################################################
timestamp = datetime.datetime.now()
tsf.saveIterations(parameters,mape,timestamp)


###############################################################################
# Executing Model comparison
###############################################################################
os.startfile("saved files\\iterations\\Model_comparison.csv")