
# coding: utf-8

# #### Importing the necessary packages

# In[2]:


try:
    import pandas as pd
except ImportError:
    print("Pandas module not found")
try:
    import numpy as np
except ImportError:
    print("Numpy module not found")


# #### Function to calculate the percentile distribution of a continuous column 

# In[4]:


################################################################################################################################
## FUNCTION: Calculating percentile distribution for provided column
## INPUT: dataframe, column name and desired percentile increment in the range - {(0,10),(90,100)
## OUTPUT: histogram for the selected column. Boxplot for selected column if continuous
################################################################################################################################


def percentileDistribution(df2,columnName,percentileIncrement=0.1):
    
    
    """ This function finds the percentile distribution of the specified column name from the dataset, the default values 
    displayed are with an increment of 1 percent from 0 to 10, quartiles(0,25,50,100), one-third of the data(33,67), increment
    of 1 percent from 90 to 99 and and increment of 0.1 from 99 to 100. The function can also take in a percentileIncrement 
    from the user, this increment is displayed from 0-10 and 90-100
    Parameters
    ----------
    df                  : csv file (or file path) passed as string or pandas dataframe
    
    column name         : name of the column from the dataset (continous column)
    
    percentileIncrement : numeric value (example 0.1)
    
                  
    Examples
    --------
   
    percentileDistribution(df="csv_filename.csv",'tenure',percentileIncrement=0.1)
   
    """
    percentiles={}
    currentPercentile=0
    df = df2.copy()
    df.dropna(inplace=True)
    
    try:
        if percentileIncrement>0:
            while currentPercentile<=100:
                percentiles[round(currentPercentile,2)]=round(np.percentile(df[columnName],currentPercentile),2)
                if currentPercentile<=9.99 or currentPercentile>=89:
                    currentPercentile+=percentileIncrement
                else:
                    currentPercentile+=10
        else:
            listPercentile = [10,20,25,30,33,40,50,60,67,70,75,80,90]
            while currentPercentile<=100:
                if currentPercentile<10 or (currentPercentile>=90 and currentPercentile<99):
                    percentiles[round(currentPercentile,2)]=round(np.percentile(df[columnName],currentPercentile),2)
                    currentPercentile+=1
                elif currentPercentile>=10 and currentPercentile<=90:
                    for i in listPercentile:
                        percentiles[round(currentPercentile,2)]=round(np.percentile(df[columnName],currentPercentile),2)
                        currentPercentile=i
                else:
                    percentiles[round(currentPercentile,2)]=round(np.percentile(df[columnName],currentPercentile),2)
                    currentPercentile+=0.1
    except:
        print("The datatype of input column is not int/float type")
        percentiles[0]=0
        percentiles[1]=0
        percentiles[100]=0
                
    return percentiles

