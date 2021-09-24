
# coding: utf-8

# #### Importing necessary libraries


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
try:
    from scipy.stats import kurtosis, skew
except ImportError:
    print("Kurtosis, Skew not found")


# #### Function to find the descriptive statistics of a continuous column


################################################################################################################################
## FUNCTION: Calculating Descriptive Statistics for provided column
## INPUT: dataframe, column name
## OUTPUT: Dictionary containing descriptive statistics of input column
################################################################################################################################


def calculateDescriptiveStatistics(df2,columnName):
    
    """ This function finds all the descriptive statistics of the continuous columns from the dataset
    Parameters
    ----------
    univariateDf : csv file (or file path) passed as string or pandas dataframe
    
    columnName   : name of the continuous column from the dataset
                  
    Examples
    --------
   
   calculateDescriptiveStatistics(df="csv_filename.csv",'Churn')
   
    """
    statistics = {}
    df = df2.copy()
    df.dropna(inplace=True)
    try:
        statistics["Mean"] = round(np.mean(df[columnName]),2)
    except Exception as e:
        statistics["Mean"] = 0
        print(e)
    try:
        statistics["Median"] = round(np.median(df[columnName]),2)
    except Exception as e:
        statistics["Median"] = 0
        print(e)
    try:
        statistics["Mode"] = round(stats.mode(df[columnName])[0][0],2)
    except Exception as e:
        statistics["Mode"] = 0
        print(e)
    try:
        statistics["Min"] = round(min(df[columnName]),2)
    except Exception as e:
        statistics["Min"] = 0
        print(e)
    try:
        statistics["Max"] = round(max(df[columnName]),2)
    except Exception as e:
        statistics["Max"] = 0
        print(e)
    try:
        statistics["Range"] = round(max(df[columnName])-min(df[columnName]),2)
    except Exception as e:
        statistics["Range"] = 0
        print(e)
    try:
        statistics["Variance"] = round(np.var(df[columnName]),2)
    except Exception as e:
        statistics["Variance"] = 0
        print(e)
    try:
        statistics["Standard deviation"] = round(np.std(df[columnName]),2)
    except Exception as e:
        statistics["Standard deviation"] = 0
        print(e)
    try:
        statistics["Skewness"] = round(skew(df[columnName]),2)
    except Exception as e:
        statistics["Skewness"] = 0
        print(e)
    try:
        statistics["Kurtosis"] = round(kurtosis(df[columnName]),2)
    except Exception as e:
        statistics["Kurtosis"] = 0
        print(e)
    return statistics

