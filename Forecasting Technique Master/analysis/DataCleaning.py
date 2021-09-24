# coding: utf-8

# #### Importing necessary packages



try:
    import pandas as pd
except ImportError:
    print("Pandas module not found")
try:
    import numpy as np
except ImportError:
    print("Numpy module not found")
try:
    from scipy import stats
except ImportError:
    print("Stats module not found")
import warnings


# #### Function to find continuous columns



########################################################################################################################################################
## FUNCTION: Continuous columns whose datatype is object type; This function will remove all strings from those columns and convert it to int/float type
## INPUT: df - dataframe with continous columns having datatype as object
## OUTPUT: df - dataframe with continous columns having datatype as int/float
########################################################################################################################################################

numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64','float','int']

def findContinuousColumns(mdf, threshold = 100):
    
    
    
    """ This function identifies the continous columns which might have garbage values present in them, the function tries to 
    convert the first 100/threshold values into float type, if more than 95% are convertable then the column is converted to float and hence will be
    identified as numeric column
    
    
    Parameters
    ----------
    mdf          : pandas dataframe.
    threshold    : Number of rows to be check to identify continuous
                  
    Examples
    --------
   
    findContinuousColumns(df)
   
    """
    
    df = mdf.copy()
    listObjects=[]
    for i in df:
        if (df[i].dtype) not in numerics:
            listObjects.append(i)
    if len(listObjects) > 0:
        for j in listObjects:
            counter=0
            for i in range(threshold):
                value=df[j][i]
                try:
                    float(value)
                    counter+=1
                except:
                    continue
            if counter >= threshold*(0.95):
                df[j]=pd.to_numeric(df[j],errors='coerce')
    return df


# #### Function to impute data


################################################################################################################################
## FUNCTION: Replacing all garbage and missing values with nan
## INPUT: df - dataframe with garbage and missing values
## OUTPUT: df - dataframe without garbage and missing values
################################################################################################################################


def imputeData(mdf, imputeWith='nan', imputeCategorical=False, missingLimit=20):
    
    
    """ This function replaces all the missing values in the dataframe with np.NaN
    Parameters
    ----------
    mdf                      : pandas dataframe.
    imputeWith               : default value is 'nan', 
                               Optional, accept any one of the following : 'mean', 'median', 'mode', 'zero','nan'
    imputeCategorical        : Optional, boolean. If True, object type columns will be imputed with mode of the column 
    missingLimit             : Percentage of allowed limit of missing values in a column, if missing value is above
                               this limit column will be dropped        
    Examples
    --------
   
    imputedata(data)
   
    """
    droppedColumnList=[]
    missingValues=['Nan','na','NA','none','None','  ','','^ ',np.nan,'NAN','nan']
    df = mdf.copy()
    
    warnings.simplefilter('ignore')
    if imputeWith=='mean':
        for i in df:
            if df[i].dtype in numerics:
                df[i].replace(missingValues,round(np.mean(df[i]),2),regex=True, inplace=True)
                
    elif imputeWith=='median':
        for i in df:
            if df[i].dtype in numerics:
                df[i].replace(missingValues,round(np.median(df[i]),2),regex=True, inplace=True)
    
    elif imputeWith=='mode':
        for i in df:
            if df[i].dtype in numerics:
                df[i].replace(missingValues,round(np.mode(df[i]),2),regex=True, inplace=True)
    
    elif imputeWith=='zero':
        for i in df:
            if df[i].dtype in numerics:
                df[i].replace(missingValues,0,regex=True, inplace=True)
    
    else:
        try:
            df.replace(missingValues,np.nan,regex=True, inplace=True)
            for i in df.columns:
                if df[i].isnull().sum() > (len(df)*missingLimit)/100:
                    df.drop(i,axis=1,inplace=True)
                    droppedColumnList.append(i)
                    
        except Exception as e:
            print("Could not replace the missing values specified")
            print(e)
    if imputeCategorical==True:
        try:
            for i in df:
                if df[i].dtype not in numerics:
                    df[i].replace(missingValues,stats.mode(df[i])[0][0],regex=True, inplace=True)
            for i in df.columns:
                if df[i].isnull().sum() > (len(df)*missingLimit)/100:
                    df.drop(i,axis=1,inplace=True)
                    droppedColumnList.append(i)
        except Exception as e:
            print("Unable to perform imputation for categorical column")
            print(e)
    return(df,droppedColumnList)


# #### Function to remove IDs 



################################################################################################################################
## FUNCTION: Removing categorical columns having more than 300 unique values
## INPUT: df - dataframe from which column will be dropped
## OUTPUT: df - dataframe without categorical columns with more than 300 unique values
################################################################################################################################


def removeIds(mdf, threshold=300):
    
    
    """ This function removes all ID columns from the dataset, the factor specified for the number of unique values is 300, if 
    the number of unique values exceeds 300 it is identified as an ID column
    Parameters
    ----------
    mdf          : pandas dataframe.
    threshold    : maximum number of unique values in any column
                  
    Examples
    --------
   
    removeIds(df)
    
    
    Return the dataframe after dropping columns that have more than 50% unique values
   
    """
# Code to identify and remove ids and name columns
    df = mdf.copy()
    for i in df:
        try:
            if (len(df[i].unique())) > threshold and (df[i].dtype == 'object' or df[i].dtype == 'str') :
                df.drop(i,axis=1,inplace=True)
        except Exception as e:
            print("Could not drop ID columns")
            print(e)
    return df 


# #### Function to remove date columns from the dataframe



################################################################################################################################
## FUNCTION: Removing date columns from dataframe
## INPUT: df - dataframe with date columns
## OUTPUT: df - dataframe without date columns
################################################################################################################################


def removeDates(mdf):  
    
    """ This function removes all date columns from the dataset, the date columns are identified from the dataframe, and these 
    columns are dropped
    
    
    Parameters
    ----------
    mdf : pandas dataframe.
                  
    Examples
    --------
   
    removeDates(df)
    
   
    """
    #code to identify and remove the date columns from dataset
    df = mdf.copy()
    for i in df:
        if df[i].dtype == 'object':
            df[i]=pd.to_datetime(df[i],errors='ignore',infer_datetime_format=True,exact=False)
        if df[i].dtype == 'datetime64[ns]':
            try:
                df.drop(i,axis=1,inplace=True)
            except Exception as e:
                print("Could not drop date column")
                print(e)
    return df




def removeOutlier(dfIn, colNames = 'all', lowerLimit = None, upperLimit = None, IQRCoeff = 1.5):
    """ This function removes outliers from the dataset based on input parameter
    or default parameters.
    
    
    Parameters
    ----------
    dfIn        : pandas dataframe.
    colNames    : List of column names to be treated
    lowerLimit  : Limit below which all values will be removed
    upperLimit  : Limit above which all values will be removed
    IQRCoeff    : This value will be multiplied with IQR before subtracting from Q1
                  or adding to Q3 to calculate lowerLimit and upperLimit
                  
    Examples
    --------
   
    removeOutliers(df)
    
   
    """
    dfOut = dfIn.copy()
    if colNames=='all':
        colNames = dfIn.columns
    for i in colNames:
        if dfIn[i].dtype in numerics:
            q1 = dfIn[i].quantile(0.25)
            q3 = dfIn[i].quantile(0.75)
            iqr = q3-q1 #Interquartile range
            if lowerLimit == None:
                lowerLimit = q1-IQRCoeff*iqr
            if upperLimit==None:
                upperLimit = q3+IQRCoeff*iqr
            dfOut[i] = dfIn[i].where((dfIn[i] > (lowerLimit - 1))
                                   & (dfIn[i] < (upperLimit + 1)))
    dfOut = dfOut[~dfOut.isin(['NaN']).any(axis=1)]           
    
    return dfOut