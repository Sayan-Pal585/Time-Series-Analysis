from mathmodels.forecast import ucm
import pandas as pd
from statsmodels.tsa.seasonal import seasonal_decompose
import statsmodels.api as sm
from statsmodels.api import OLS
import datetime
from dateutil.relativedelta import relativedelta
import time
import pandas as pd
import warnings
import plotly
import matplotlib
import numpy as np
import matplotlib.pyplot as plt
from mathmodels.forecast import ucm

################ FILES PATH ######

input_file = r'D:\Daimler\Final Cobalt Before Meeting\Cobalt.xlsx'
r1 = r'D:\Daimler\Final Cobalt Before Meeting\R1.csv'
forecast_output_Cobalt_UCM_Multivariate = r'D:\Daimler\Final Cobalt Before Meeting\Anti Log Forecast_Output_Cobalt_UCM_Multivariate.csv'

###################READING ACCURACY FILES##################

accuracy_input = pd.read_excel(input_file, sheet_name = 'Accuracy_Input_Cobalt')

#####################UCM MULTIVARIATE####################
########1 YEAR#######
def train_testsplit_UCM_multivariate(value,exog_list):
    df_new = pd.read_excel(input_file, sheet_name = 'Log Input File')
    df_new['Date'] = pd.to_datetime(df_new['Date'],format='%d-%m-%Y')
    df_new.set_index('Date',inplace=True)
    train_1YR = df_new[0:104] 
    test_1YR = df_new[104:]

    ucmModel = train_1YR.ucm.build(value,exog= train_1YR[exog_list],level='dconstant', trend=True, seasonal = 12, stochastic_trend=False, cycle= False, irregular=True, maxiter=10)
    ucmSummary = train_1YR.ucm.getSummary(ucmModel)

    test = pd.DataFrame(ucmModel.forecast(len(test_1YR),exog=test_1YR[exog_list]),columns=['Predict'])
    test = 10**(test)
    test_1YR = test_1YR.merge(test['Predict'],  how='outer',left_index=True, right_index=True)
    test_1YR['Cobalt Price'] = 10**(test_1YR['Cobalt Price'])
    def mean_absolute_percentage_error(y_true, y_pred): 
        y_true, y_pred = np.array(y_true), np.array(y_pred)
        return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
    print(mean_absolute_percentage_error(test_1YR['Cobalt Price'], test_1YR['Predict']))
train_testsplit_UCM_multivariate('Cobalt Price', ['Flag'])

########2 YEAR#######

def train_testsplit_UCM_multivariate(value,exog_list):
    df_new = pd.read_excel(input_file, sheet_name = 'Log Input File')
    df_new['Date'] = pd.to_datetime(df_new['Date'],format='%d-%m-%Y')
    df_new.set_index('Date',inplace=True)
    train_2YR = df_new[0:92] 
    test_2YR = df_new[92:]

    ucmModel = train_2YR.ucm.build(value,exog= train_2YR[exog_list],level='dconstant', trend=True, seasonal = 12, stochastic_trend=False, cycle= False, irregular=True, maxiter=10)
    ucmSummary = train_2YR.ucm.getSummary(ucmModel)

    test = pd.DataFrame(ucmModel.forecast(len(test_2YR),exog=test_2YR[exog_list]),columns=['Predict'])
    test = 10**(test)
    test_2YR = test_2YR.merge(test['Predict'],  how='outer',left_index=True, right_index=True)
    test_2YR['Cobalt Price'] = 10**(test_2YR['Cobalt Price'])

    def mean_absolute_percentage_error(y_true, y_pred): 
        y_true, y_pred = np.array(y_true), np.array(y_pred)
        return np.mean(np.abs((y_true - y_pred) / y_true)) * 100
    print(mean_absolute_percentage_error(test_2YR['Cobalt Price'], test_2YR['Predict']))
train_testsplit_UCM_multivariate('Cobalt Price', ['Flag'])

########2 YEARS#######

df = pd.read_excel(input_file, sheet_name = 'Log Input File for Forecast')

df['Date'] = pd.to_datetime(df['Date'],format='%d-%m-%Y')
df.set_index('Date',inplace=True)
accuracy_input['Snap_Month'] = pd.to_datetime(accuracy_input['Snap_Month'],format='%d-%m-%Y')
accuracy_input.set_index('Snap_Month', inplace=True)

for i in range(91,116):
    dftrain=df[:i]
    dftest=df[i:]
    dftest=dftest.head(24)
    
    ucmModel = dftrain.ucm.build('Cobalt Price',exog= dftrain['Flag'],level='dconstant', trend=True, seasonal = 12, stochastic_trend=False, cycle= False, irregular=True, maxiter=10)
    ucmSummary = dftrain.ucm.getSummary(ucmModel)

    test = pd.DataFrame(ucmModel.forecast(24,exog=dftest[['Flag']]),columns=['Predict'])
    test = 10**(test)
    
    accuracy_input=pd.merge(accuracy_input,test,how='outer',left_index=True,right_index=True)
    
accuracy_input.columns = ['A','F1','F2','F3','F4','F5','F6','F7','F8','F9','F10','F11','F12','F13','F14','F15','F16','F17','F18','F19','F20','F21','F22','F23','F24','F25']
accuracy_input.to_csv(r1)    

########2 YEARS FORWARD LOOKING FORECAST#######

forward_looking_forecast = accuracy_input['F25']
forward_looking_forecast.to_csv(forecast_output_Cobalt_UCM_Multivariate)