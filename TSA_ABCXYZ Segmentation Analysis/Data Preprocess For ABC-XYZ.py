# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 14:41:53 2021

@author: User
"""

#%%Loading Important Libraries
import pandas as pd
import numpy as np

import warnings 
warnings.filterwarnings("ignore")

#%%Set Work Dir
import os
os.chdir('C:\\Users\\sayanpal\\OneDrive - Deloitte (O365D)\\Documents\\SABIC\\SABIC Data\\PA2 Sales Price Prediction & Derivation\\PA2 Python Scripts')
#%%Reading Data
data = pd.read_csv('1315 LDPE KSA Grade Data.csv')

#%%Data preprocess
data = data.drop(columns =['Sales Type','Profit Center','Company Code','SBU','BU','Region','Sale Quantity TO'])

#Period Column treatment
data['year']=data['Period'].astype(str).str[:4]data['month']=data['Period'].astype(str).str[-2:]

#Merging two columns
data['date'] = data['year'].astype(str) + data['month'].astype(str).str.zfill(2)

#converting columns type
data['TimePeriod'] = pd.to_datetime(data['date'], format='%Y%m')

#droping columns
data = data.drop(columns =['year','month','date','Period'])

#rearanging the columns
cols = data.columns.tolist()
cols = cols[-1:] + cols[:-1]

data = data[cols]

#Merging two columns
data['Plant_Material'] = data['Plant'].astype(str) + data['Material'].astype(str).str.zfill(2)

#rearranging the columns
data = data[['TimePeriod','Plant','Material','Plant_Material','MaterialDescription','Total Revenue','SP / Ton']]

#%%
df1 = data[['Plant_Material', 'MaterialDescription']]
df2 = df1.drop_duplicates()
df2.reset_index(level=0, inplace=True)
gradefinal_list = df2[['Plant_Material', 'MaterialDescription']]
md1 = np.array(gradefinal_list['Plant_Material'])

final = pd.DataFrame()

for i in md1:    
    buf = data[(data.Plant_Material==i)].copy()    
    groupby_day = buf.groupby(pd.PeriodIndex(data=buf.TimePeriod, freq='D'))    
    results = groupby_day.sum()    
    idx = pd.period_range(min(data.TimePeriod), max(data.TimePeriod))    
    results=results.reindex(idx, fill_value=0)    
    #results['TimePeriod'] = results.index    
    results.reset_index(level=0, inplace=True)    
    results.rename(columns = {'index':'TimePeriod'}, inplace = True)    
    results['year_month']=results['TimePeriod'].astype(str).str[:7]    
    results['year']=results['year_month'].astype(str).str[:4]    
    results['month']=results['year_month'].astype(str).str[-2:]    
    results['date'] = results['year'].astype(str) + results['month'].astype(str).str.zfill(2)    
    results['TimePeriod'] = pd.to_datetime(results['date'], format='%Y%m')    
    results = results.drop(columns =['year','month','date','year_month'])    
    results.set_index('TimePeriod', inplace=True)    
    results = results.resample('MS').sum()    
    results.reset_index(level=0, inplace=True)    
    results.rename(columns = {'SP / Ton':'Price'}, inplace = True)    
    
    result = results.copy()    
    final = final.append(result, ignore_index = True)
    
#%%Preparing final data
n = len(pd.unique(final['TimePeriod']))
grades_list_df = gradefinal_list.loc[gradefinal_list.index.repeat(n)].reset_index(drop=True)
frames = [final, grades_list_df]
FinalResult = pd.concat(frames,axis=1, join='inner')

#Demerging Column
FinalResult['Plant']=FinalResult['Plant_Material'].astype(str).str[:4]
FinalResult['MaterialCode']=FinalResult['Plant_Material'].astype(str).str[-8:]

#Dropping Columns 
FinalResult = FinalResult.drop(columns =['Plant_Material','Material'])

#rearranging the columns
FinalResult = FinalResult[['TimePeriod','Plant','MaterialCode','MaterialDescription','Total Revenue','Price']]

#%%Writng final Data
#writing output file
FinalResult.to_csv("PA2_Final_Data(ABC_XYZ).csv")
#%%