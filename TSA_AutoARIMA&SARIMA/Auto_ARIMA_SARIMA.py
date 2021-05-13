# -*- coding: utf-8 -*-
"""
Created on Thu May 13 11:13:28 2021

@author: sayanpal
"""

#%%Loading Important Libraries
import pandas as pd
import numpy as np
from datetime import timedelta
from datetime import datetime
import configparser
import calendar
import datetime as dt
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.statespace.sarimax import SARIMAX
import itertools
import statsmodels.api as sm
import matplotlib.pyplot as plt
plt.style.use('fivethirtyeight')
import itertools
## import all required libraries
import matplotlib.pyplot as plt 
import seaborn as sns 
import warnings 
warnings.filterwarnings("ignore")
#%%Set Dir
import os
os.chdir('C:\\Users\\sayanpal\\Downloads\\New folder\\')

#%% Reading Data
data = pd.read_csv('training_data.csv')

#%%Preparing data
data.set_index('WEEK_START_DATE', inplace=True)
data.index = pd.to_datetime(data.index)
data1=data.groupby('SKU').resample('1M').sum()

data1.reset_index(drop=False, inplace=True)
data1 = data1.drop(columns =['FISCAL_MONTH'])

data1['YEAR'] = data1['WEEK_START_DATE'].dt.year
data1['MONTH'] = data1['WEEK_START_DATE'].dt.month

#Merging two columns
data1['FISCAL_MONTH']=data1['YEAR'].astype(str) + data1['MONTH'].astype(str).str.zfill(2)

#converting columns type
data1['DATE'] = pd.to_datetime(data1['FISCAL_MONTH'], format='%Y%m')

#droping columns
data1 = data1.drop(columns =['FISCAL_MONTH','YEAR','MONTH','WEEK_START_DATE'])

#rearanging the columns
cols = data1.columns.tolist()
cols = cols[-1:] + cols[:-1]

data1 = data1[cols]
   
#%%Spliting Data & Modeling for SKU's
sku = np.array(data1['SKU'])
sku = np.unique(sku) 
finalResult = pd.DataFrame()

for i in sku:

     buf = data1[(data1.SKU==i)].copy()
     train=buf[0:52]
     test=buf[52:56]

     y = pd.Series(data=train['SHIPPED_QTY'].values, index=train['DATE'])
     y1= pd.Series(data=test['SHIPPED_QTY'].values, index=test['DATE'])
     #y.plot(figsize=(15, 6))

     warnings.filterwarnings("ignore")
     ##Model Run
     def arima_grid_search(dataframe, s):
         p = range(2)
         d = range(2)
         q = range(2)
         param_combinations = list(itertools.product(p, d, q))
         lowest_aic, pdq, pdqs = None, None, None
         total_iterations = 0
         for order in param_combinations:    
            for (p, q, d) in param_combinations:
                seasonal_order = (p, q, d, s)
                total_iterations += 1
                try:
                    model = SARIMAX(y, order=order, 
                        seasonal_order=seasonal_order,trend='ct',
                        enforce_stationarity=False,
                        enforce_invertibility=False,
                        disp=False
                    )
                    model_result = model.fit(maxiter=200, disp=False)

                    if not lowest_aic or model_result.aic < lowest_aic:
                        lowest_aic = model_result.aic
                        pdq, pdqs = order, seasonal_order

                except Exception as ex:
                    continue

            return lowest_aic, pdq, pdqs 
    
     lowest_aic, order, seasonal_order = arima_grid_search(y, 12)   
    
     #print('ARIMA{}x{}'.format(order, seasonal_order))
     #print('Lowest AIC: %.3f'%lowest_aic)
     #modeling with best parameters for every sku
     model = SARIMAX(
        y,
        order=order,
        seasonal_order=seasonal_order,
        enforce_stationarity=False,
        enforce_invertibility=False,
        disp=False
    )

     model_results = model.fit(maxiter=200, disp=False)
    
     pred_uc = model_results.get_forecast(steps=8)
     #forecast values for evey sku
     forecast= pred_uc.predicted_mean   
    
     f1=forecast.to_frame()
     f1.reset_index(drop=False, inplace=True)
     ff=f1.rename(columns={"index":"DATE"})
    
     final = ff[4:8]
     final['SKU']=[i,i,i,i]
     result = final.copy()
     #Storing final result
     finalResult = finalResult.append(result, ignore_index = True)
     
#%%Wmape calculatio for test data--not reqd for forcasting score 
#WMAPE Calculation
def wmape(actual, forecast):
    # Take a series (actual) and a dataframe (forecast) and calculate wmape
    # for each forecast. Output shape is (1, num_forecasts)

    # Convert to numpy arrays for broadasting
    forecast = np.array(forecast.values)
    actual=np.array(actual.values).reshape((-1, 1))

    # Make an array of mape (same shape as forecast)
    se_mape = abs(actual-forecast)/actual

    # Calculate sum of actual values
    ft_actual_sum = actual.sum(axis=0)

    # Multiply the actual values by the mape
    se_actual_prod_mape = actual * se_mape

    # Take the sum of the product of actual values and mape
    # Make sure to sum down the rows (1 for each column)
    ft_actual_prod_mape_sum = se_actual_prod_mape.sum(axis=0)

    # Calculate the wmape for each forecast and return as a dictionary
    ft_wmape_forecast = ft_actual_prod_mape_sum / ft_actual_sum
    return {f'Forecast_{i+1}_wmape': wmape for i, wmape in enumerate(ft_wmape_forecast)}

actuals=pd.Series(data=test['SHIPPED_QTY'].values)
wmape(actuals,forecast[0:4])
#%%Collating final score 
finalResult['YEAR'] = finalResult['DATE'].dt.year
finalResult['MONTH'] = finalResult['DATE'].dt.month

#Merging two columns
finalResult['FISCAL_MONTH']=finalResult['YEAR'].astype(str) + finalResult['MONTH'].astype(str).str.zfill(2)

#droping columns
finalResult = finalResult.drop(columns =['YEAR','MONTH'])

finalResult = finalResult[['SKU','FISCAL_MONTH','predicted_mean']]
finalResult.columns = ['SKU','FISCAL_MONTH','SHIPPED_QTY']
finalResult.reset_index(drop=True, inplace=True)
finalResult.SHIPPED_QTY = finalResult.SHIPPED_QTY.round()

#writing output file
finalResult.to_csv("Final_Score_Sayan Pal.csv")
#%%
