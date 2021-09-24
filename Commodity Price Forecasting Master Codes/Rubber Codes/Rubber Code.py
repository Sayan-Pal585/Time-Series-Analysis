from ucm import ucm
import pandas as pd
from statsmodels.tsa.seasonal import seasonal_decompose
import statsmodels.api as sm
from statsmodels.api import OLS
import datetime
from dateutil.relativedelta import relativedelta
import time
from dateutil import parser
import numpy as np
from datetime import datetime as dt
from datetime import timedelta
from matplotlib import pyplot
from sklearn.metrics import mean_squared_error
from math import sqrt
from statsmodels.tsa.api import ExponentialSmoothing, SimpleExpSmoothing, Holt
from pmdarima import auto_arima
from statsmodels.tsa.stattools import adfuller
from numpy import log
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
import pmdarima as pm
import warnings
warnings.filterwarnings("ignore")

################FILES PATH ######

input_file = r'D:\Daimler\Rubber\Output Files\Rubber.xlsx'
forecast_output_Rubber_Holt_Winter = r'D:\Daimler\Rubber\Output Files\Forecast_Output_Rubber_Holt_Winter.csv'
forecast_output_Rubber_UCM_Univariate = r'D:\Daimler\Rubber\Output Files\Forecast_Output_Rubber_UCM_Univariate.csv'

##################READ THE FILE##############

df = pd.read_excel(input_file, sheet_name = 'Univariate Input File')
df['Date'] = pd.to_datetime(df['Date'], format = '%d-%m-%Y')
df.set_index('Date', inplace=True)

##################SLICING THE DATA##############

train_1YR = df[0:120] 
test_1YR = df[120:]

train_2YR = df[0:108] 
test_2YR = df[108:]

###################READING ACCURACY FILES##################

accuracy_input = pd.read_excel(input_file, sheet_name = 'Accuracy Input Rubber')
accuracy_input['Snap_Month'] = pd.to_datetime(accuracy_input['Snap_Month'],format = '%d-%m-%Y')
accuracy_input.set_index('Snap_Month', inplace=True) 

######################HOLT LINEAR TREND#######################
########1 YEAR#######

holt_linear_trend_1YR = test_1YR.copy()
fit_holt_linear_trend_1YR = Holt(np.asarray(train_1YR['Rubber Price'])).fit(smoothing_level = 0.3,smoothing_slope = 0.1)
holt_linear_trend_1YR['Holt_linear'] = fit_holt_linear_trend_1YR.forecast(len(test_1YR))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(holt_linear_trend_1YR['Rubber Price'], holt_linear_trend_1YR['Holt_linear']))

########2 YEAR#######

holt_linear_trend_2YR = test_2YR.copy()
fit_holt_linear_trend_2YR = Holt(np.asarray(train_2YR['Rubber Price'])).fit(smoothing_level = 0.3,smoothing_slope = 0.1)
holt_linear_trend_2YR['Holt_linear'] = fit_holt_linear_trend_2YR.forecast(len(test_2YR))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(holt_linear_trend_2YR['Rubber Price'], holt_linear_trend_2YR['Holt_linear']))

########2 YEARS#######

for i in range(108,132):
    dfTrain=df[:i]
    dfTest=df[i:]
    fit_holt_linear_trend = Holt(np.asarray(dfTrain['Rubber Price'])).fit(smoothing_level = 0.3,smoothing_slope = 0.1)
    holt_linear_trend = fit_holt_linear_trend.forecast(24)	
    nischay = pd.Series(holt_linear_trend).to_frame('Forecasted Price')
    nischay.set_index(accuracy_input.index[i-107:i-107+24],inplace=True)
    accuracy_input=pd.merge(accuracy_input,nischay,how='left',left_index=True,right_index=True)

#####################HOLT WINTER####################
########1 YEAR#######

holt_winter_1YR = test_1YR.copy()
fit1 = ExponentialSmoothing(np.asarray(train_1YR['Rubber Price']) ,seasonal_periods=10 ,trend='add', seasonal='add',).fit()
holt_winter_1YR['Holt_Winter'] = fit1.forecast(len(test_1YR))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(holt_winter_1YR['Rubber Price'], holt_winter_1YR['Holt_Winter']))

########2 YEAR#######

holt_winter_2YR = test_2YR.copy()
fit1 = ExponentialSmoothing(np.asarray(train_2YR['Rubber Price']) ,seasonal_periods=10 ,trend='add', seasonal='add',).fit()
holt_winter_2YR['Holt_Winter'] = fit1.forecast(len(test_2YR))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(holt_winter_2YR['Rubber Price'], holt_winter_2YR['Holt_Winter']))

########2 YEARS#######

for i in range(108,132):
    dfTrain=df[:i]
    dfTest=df[i:]
    fit_holt_winter = ExponentialSmoothing(np.asarray(dfTrain['Rubber Price']) ,seasonal_periods=10 ,trend='add', seasonal='add',).fit()
    holt_winter = fit_holt_winter.forecast(24)	
    nischay = pd.Series(holt_winter).to_frame('Forecasted Price')
    nischay.set_index(accuracy_input.index[i-107:i-107+24],inplace=True)
    accuracy_input=pd.merge(accuracy_input,nischay,how='left',left_index=True,right_index=True)
    accuracy_input.to_csv(r'D:\Daimler\Rubber\Output Files\Checking.csv')
    
########2 YEARS FORWARD LOOKING FORECAST#######

df_to_forecast = pd.read_excel(input_file, sheet_name = 'Dates to forecast')
holt_winter_univariate_2YR_forecast = df_to_forecast.copy()
fit1 = ExponentialSmoothing(np.asarray(df['Rubber Price']) ,seasonal_periods = 10 ,trend='add', seasonal='add').fit()
holt_winter_univariate_2YR_forecast['Holt_Winter'] = fit1.forecast(len(df_to_forecast))
holt_winter_univariate_2YR_forecast.to_csv(forecast_output_Rubber_Holt_Winter, index = False)

#####################UCM UNIVARIATE####################
########1 YEAR#######

ucm_univariate_1YR = test_1YR.copy()
ucmModel = train_1YR.ucm.build('Rubber Price',level='local level',trend=True,autoregressive = 1,stochastic_seasonal=False, irregular=True, maxiter=10)
ucm_univariate_1YR['UCM'] = fit1.forecast(len(test_1YR))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(ucm_univariate_1YR['Rubber Price'], ucm_univariate_1YR['UCM']))

########2 YEAR#######

ucm_univariate_2YR = test_2YR.copy()
ucmModel = train_2YR.ucm.build('Rubber Price',level='local level',trend=True,autoregressive = 1,stochastic_seasonal=False, irregular=True, maxiter=10)
ucm_univariate_2YR['UCM'] = fit1.forecast(len(test_2YR))

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(ucm_univariate_2YR['Rubber Price'], ucm_univariate_2YR['UCM']))

########2 YEARS#######

for i in range(108,132):
    df_univariate_Train = df[:i]
    df_univariate_Test = df[i:]
    
    ucmModel = df_univariate_Train.ucm.build('Rubber Price',level='local level',trend=True,autoregressive = 1,stochastic_seasonal=False, irregular=True, maxiter=10)
    ucmSummary = df_univariate_Train.ucm.getSummary(ucmModel)
    forecast1=pd.DataFrame(ucmModel.forecast(24))
    
    forecast1.set_index(accuracy_input.index[i-107:i-107+24],inplace=True)
    accuracy_input=pd.merge(accuracy_input,forecast1,how='outer',left_index=True,right_index=True)

########2 YEARS FORWARD LOOKING FORECAST#######

df_to_forecast = pd.read_excel(input_file, sheet_name = 'Dates to forecast')
ucm_univariate_2YR_forecast = df_to_forecast.copy()
ucmModel = df.ucm.build('Rubber Price',level='local level',trend=True,autoregressive = 1,stochastic_seasonal=False, irregular=True, maxiter=10)
ucm_univariate_2YR_forecast['UCM'] = fit1.forecast(len(df_to_forecast))
ucm_univariate_2YR_forecast.to_csv(forecast_output_Rubber_UCM_Univariate, index = False)

#################EXPONENTIAL MOVING AVERAGE############

########1 YEAR#######

ema = test_1YR.copy()
ema_1YR = ema['Rubber Price'].ewm(span=24, adjust=False).mean()
ema_1YR = pd.Series(ema_1YR).to_frame('Forecasted Price')
ema_1YR = pd.merge(ema,ema_1YR,how='left',left_index=True,right_index=True)

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(ema_1YR['Rubber Price'], ema_1YR['Forecasted Price']))

########2 YEAR#######

ema = test_2YR.copy()
ema_2YR = ema['Rubber Price'].ewm(span=24, adjust=False).mean()
ema_2YR = pd.Series(ema_2YR).to_frame('Forecasted Price')
ema_2YR = pd.merge(ema,ema_2YR,how='left',left_index=True,right_index=True)

def mean_absolute_percentage_error(y_true, y_pred): 
    y_true, y_pred = np.array(y_true), np.array(y_pred)
    return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
print(mean_absolute_percentage_error(ema_2YR['Rubber Price'], ema_2YR['Forecasted Price']))
