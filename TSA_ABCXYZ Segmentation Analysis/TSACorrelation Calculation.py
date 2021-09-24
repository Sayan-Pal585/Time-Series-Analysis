# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 14:33:37 2021

@author: User
"""

#%%Loading Important Libraries
import pandas as pd
import numpy as np
from scipy.stats import pearsonr
import seaborn as sns

import warnings 
warnings.filterwarnings("ignore")

#%%Set Work Dir
import os
os.chdir('C:\\Users\\sayanpal\\OneDrive - Deloitte (O365D)\\Documents\\SABIC\\SABIC Data\\PA2 Sales Price Prediction & Derivation\\PA2 Python Scripts')
#%%Reading Data
mi_data = pd.read_csv('MI REF LDPE KSA.csv')
grade_data = pd.read_csv('PA2_Final_Data.csv').

#%%Data preprocess
mi_data_list = mi_data['MI REF LDPE KSA'].tolist()
#Merging two columns
grade_data['Plant_Material'] = grade_data['Plant'].astype(str) + grade_data['MaterialCode'].astype(str).str.zfill(2)
#rearranging the columns
grade_data = grade_data[['TimePeriod','Plant','MaterialCode','Plant_Material','MaterialDescription','Price']]

#%%Correlation Calculation
df1 = grade_data[['Plant_Material', 'MaterialDescription']]
df2 = df1.drop_duplicates()
df2.reset_index(level=0, inplace=True)
gradefinal_list = df2[['Plant_Material', 'MaterialDescription']]
md = np.array(gradefinal_list['Plant_Material'])

final = pd.DataFrame()
finalResult = pd.DataFrame()

for i in md:    
    buf = grade_data[(grade_data.Plant_Material==i)].copy()    
    buf_list = buf['Price'].tolist()        
    correlation, p_value = pearsonr(mi_data_list, buf_list)       
    
    final['Plant_Material'] = [i]    
    final['corr'] = [correlation]    
    result = final.copy()    
    finalResult = finalResult.append(result, ignore_index = True)    
    
#%%Preparing final Data
frames = [finalResult, gradefinal_list.MaterialDescription]
FinalResult = pd.concat(frames,axis=1, join='inner')

#Demerging Column
FinalResult['Plant']=FinalResult['Plant_Material'].astype(str).str[:4]
FinalResult['MaterialCode']=FinalResult['Plant_Material'].astype(str).str[-8:]

#Dropping Columns 
FinalResult = FinalResult.drop(columns =['Plant_Material'])

#rearranging the columns
FinalResult = FinalResult[['Plant','MaterialCode','MaterialDescription','corr']]

#%%Writing Final Data
#writing output 
fileFinalResult.to_csv("PA2_Final_Corr.csv")
#%%Histogram Plot
#Merging two columns
FinalResult['Plant_Material'] = FinalResult['Plant'].astype(str) + FinalResult['MaterialCode'].astype(str).str.zfill(2)
df = FinalResult.groupby(['Plant_Material'])['corr'].sum().reset_index()
df['corr'].plot(kind='hist')

#%%