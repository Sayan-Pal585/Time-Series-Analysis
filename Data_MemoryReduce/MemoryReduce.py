# -*- coding: utf-8 -*-
"""
Created on Sun Oct  4 10:38:15 2020

@author: Sayan Pal
"""
#%% Function to reduce the pandas DataFrame

import numpy as np
import pandas as pd
#%% Import DataSet

data = pd.read_csv("C:/Users/User/Downloads/Iowa_Liquor_Sales.csv")
#%% Reducing DataFrame Size

def reduce_mem_usage(props):
    start_mem_usg = props.memory_usage().sum() / 1024**2 
    print("Memory usage of properties dataframe is :",start_mem_usg," MB")
    for col in props.columns:
        if (props[col].dtype != object) & (props[col].isnull().values.any()==False):  # Exclude strings        
            # make variables for Int, max and min
            mx = props[col].max()
            mn = props[col].min()

            # Make Integer/unsigned Integer datatypes
            if mn >= 0:
                if mx < 255:
                    props[col] = props[col].astype(np.uint8)
                elif mx < 65535:
                    props[col] = props[col].astype(np.uint16)
                elif mx < 4294967295:
                    props[col] = props[col].astype(np.uint32)
                else:
                    props[col] = props[col].astype(np.uint64)
            else:
                if mn > np.iinfo(np.int8).min and mx < np.iinfo(np.int8).max:
                    props[col] = props[col].astype(np.int8)
                elif mn > np.iinfo(np.int16).min and mx < np.iinfo(np.int16).max:
                    props[col] = props[col].astype(np.int16)
                elif mn > np.iinfo(np.int32).min and mx < np.iinfo(np.int32).max:
                    props[col] = props[col].astype(np.int32)
                elif mn > np.iinfo(np.int64).min and mx < np.iinfo(np.int64).max:
                    props[col] = props[col].astype(np.int64)    
        
        # Make float datatypes 32 bit
        else:
            pass            
    
    # Print final result
    print("___MEMORY USAGE AFTER COMPLETION:___")
    mem_usg = props.memory_usage().sum() / 1024**2 
    print("Memory usage is: ",mem_usg," MB")
    print("This is ",100*mem_usg/start_mem_usg,"% of the initial size")
    return props

#%% Call the Function

myData = reduce_mem_usage(props = data)
#%%
