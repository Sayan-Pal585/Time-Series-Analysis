# -*- coding: utf-8 -*-
"""
Created on Fri Sep 24 14:47:41 2021

@author: User
"""

#%%Loading Important Libraries
import pandas as pd
import numpy as np
import statistics
from decimal import Decimal, getcontext
import seaborn as sns
import matplotlib.pyplot as plt

import warnings 
warnings.filterwarnings("ignore")

#%%Set Work Dir
import os
os.chdir('C:\\Users\\sayanpal\\OneDrive - Deloitte (O365D)\\Documents\\SABIC\\SABIC Data\\PA2 Sales Price Prediction & Derivation\\PA2 Python Scripts')

#%%Reading Data
data = pd.read_csv('PA2_Final_Data(ABC_XYZ).csv')

#%%Data Preprocess

#Merging two columns
data['Plant_Material'] = data['Plant'].astype(str) + data['MaterialCode'].astype(str).str.zfill(2)

#rearranging the columns
data = data[['TimePeriod','Plant_Material','MaterialDescription','Total Revenue','Price']]

#%%
df1 = data[['Plant_Material', 'MaterialDescription']]
df2 = df1.drop_duplicates()
df2.reset_index(level=0, inplace=True)
gradefinal_list = df2[['Plant_Material', 'MaterialDescription']]
md1 = np.array(gradefinal_list['Plant_Material'])

final = pd.DataFrame()
finalResult = pd.DataFrame()

for i in md1:
    buf = data[(data.Plant_Material==i)].copy()
    no_of_nonzero = (buf['Price'] != 0).sum()
    max_month = len(pd.unique(buf['TimePeriod']))
    #no_of_zero = (buf['Price'] == 0).sum()
    no_of_zero = max_month - no_of_nonzero
    Total_Revenue = buf['Total Revenue'].sum()
    last_yr = buf.tail(12).reset_index()
    last_yr = last_yr[['TimePeriod','Plant_Material','MaterialDescription','Total Revenue','Price']]
    last_yr_sale_status = ['Sales' if ((last_yr['Price'] !=0).sum()) else 'No_Sales']
    
    #getcontext().prec = 2*n
    #Standard deviation Calculation
    #std_dev = statistics.stdev(buf.Price)
    std_dev1 = np.std(buf.Price)
    mean = np.mean(buf.Price)
    #define function to calculate cv
    coeff_var = [std_dev1/mean if mean != 0 else 0]
    #cv = lambda x: np.std(x, ddof=1) / np.mean(x) * 100
    #coeff_var = cv(buf.Price)
        
    final['Plant_Material'] = [i]
    final['no_of_nonzero'] = [no_of_nonzero]
    final['no_of_zero'] = [no_of_zero]
    final['max_month'] = [max_month]
    final['coeff_var'] = [coeff_var]
    final['Total_Revenue'] = [Total_Revenue]
    final['last_yr_sale_status'] = [last_yr_sale_status]
    
    result = final.copy()
    result['coeff_var'] = result['coeff_var'].str[0]
    result['last_yr_sale_status'] = result['last_yr_sale_status'].str[0]
    
    finalResult = finalResult.append(result, ignore_index = True)
    
#%%ABC-XYZ Segmentation Analysis

finalResult['coeff_var'].min()
finalResult['coeff_var'].mean()
finalResult['coeff_var'].max()

f, ax = plt.subplots(figsize=(15, 6))
ax = sns.distplot(finalResult['coeff_var']).set_title("Coefficient of Variation",fontsize=15)

#XYZ Segmentation Analysis
def xyz_classify_product(cov):
    """Apply an XYZ classification to each product based on 
    its coefficient of variation in order quantity.

    :param cov: Coefficient of variation in order quantity for Grades/Derivatives
    :return: XYZ classification class
    """

    if cov <= 0.5:
        return 'X'
    elif cov > 0.5 and cov <= 1.0:
        return 'Y'
    else:
        return 'Z'

finalResult['xyz_class'] = finalResult['coeff_var'].apply(xyz_classify_product)
finalResult.xyz_class.value_counts()

#XYZ Summary Table
xyz_df = finalResult.groupby('xyz_class').agg(
    total_materials=('Plant_Material', 'nunique'),
    #total_revenue=('Total_Revenue', sum),
).reset_index()

finalResult['revenue_cumsum'] = finalResult['Total_Revenue'].cumsum()
finalResult['revenue_total'] = finalResult['Total_Revenue'].sum()
finalResult['revenue_running_percentage'] = (finalResult['revenue_cumsum'] / finalResult['revenue_total']) * 100

#ABC Segmentation Analysis
def abc_classify_product(percentage):
    """Apply an ABC classification to each product based on 
    its ranked percentage revenue contribution. Any split 
    can be used to suit your data. 

    :param percentage: Running percentage of revenue contributed
    :return: ABC inventory classification
    """

    if percentage > 0 and percentage <= 80:
        return 'A'
    elif percentage > 80 and percentage <= 90:
        return 'B'
    else:
        return 'C'

finalResult['abc_class'] = finalResult['revenue_running_percentage'].apply(abc_classify_product)
finalResult['abc_rank'] = finalResult['revenue_running_percentage'].rank().astype(int)

#ABC Summary Table
abc_df = finalResult.groupby('abc_class').agg(
    total_materials=('Plant_Material', 'nunique'),
    total_revenue=('Total_Revenue', sum),
).reset_index()

#ABC-XYZ Analysis
finalResult['abc_xyz_class'] = finalResult['abc_class'].astype(str) + finalResult['xyz_class'].astype(str)

#ABC-XYZ Summary Table
df_abc_xyz_summary = finalResult.groupby('abc_xyz_class').agg(
    total_materials=('Plant_Material', 'nunique'),  
    total_revenue=('Total_Revenue', sum)  
).reset_index()

#%%Writing output file
#writing output file
finalResult.to_csv("PA2_DataSegmentation_ABC_XYZ.csv")

#%%
