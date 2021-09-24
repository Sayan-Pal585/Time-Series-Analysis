
# coding: utf-8

# #### Importing packages and notebooks



try:
    import pandas as pd
except ImportError:
    print("Pandas module not found")
try:
    import numpy as np
except ImportError:
    print("Numpy module not found")
try:
    import import_ipynb
except ImportError:
    print("Import_ipynb module not found")
#importing percentile distribution notebook
try:
    from analysis import PercentileDistribution
except:
    print("Percentile distribution notebook not found")
#importing stats notebook
try:
    from analysis import Stats
except:
    print("Stats notebook not found")
import os


# #### Function to create univariate analysis of categorical/continuous variables and the correlation matrix in csv files



################################################################################################################################
## FUNCTION: Create csv files containing univariate analysis of continuous/categorical variables and correlation matrix
## INPUT: dataframe
## OUTPUT: univariateContinuous.csv, univariateCategorical.csv and correlationMatrix.csv
################################################################################################################################



def createUnivariatecsv(df2, percentileIncrement=-1):
    df = df2.copy()
    df.dropna(inplace=True)
    if not os.path.exists('analysis report'):
            os.makedirs('analysis report')
    numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64','float','int']
    listContinuous=[]
    listCategorical=[]
    listCategoriesCount=[]
    
    # Separating continuous and categorical column names
    for columnName in df:
        if df[columnName].dtype in numerics:
            listContinuous.append(columnName)
        else:
            listCategorical.append(columnName)
    
    
    # Creating list of names of continuous and categorical columns
    try:
        univariateContinuous = pd.DataFrame(listContinuous,columns=['Column Name'])
        univariateContinuous['Missingcount'] =0
    except Exception as e:
        print("Error occured while creating a list for continuous columns")
        print(e)
    try:
        univariateCategorical = pd.DataFrame(columns=['Category','count'])
    except Exception as e:
        print("Error occured while creating a list for categorical columns")
        print(e)
    
    
    
    
    
################################# Creating univariate analysis csv file for Continuous variables ###############################
    for i in univariateContinuous.index:
        
        # Calculating Descriptive Statistics
        try:
            descriptiveStatistics = Stats.calculateDescriptiveStatistics(df,univariateContinuous['Column Name'].iloc[i])
        except Exception as e:
            print("Error occured while calculating descriptive statistics for column: "+ univariateContinuous['Column Name'].iloc[i])
            print(e)
        
        univariateContinuous.at[i,'Missingcount']=df[univariateContinuous.at[i,'Column Name']].isna().sum()
        # Appending all descriptive statistics to dataframe
        for statistic,value in descriptiveStatistics.items():
            try:
                univariateContinuous.loc[i,statistic]=value
            except Exception as e:
                print("Error occured while appending descriptive statistics to dataframe")
                print(e)
        

        # Calculating Percentile Distribution
        try:
            percentDist = PercentileDistribution.percentileDistribution(df,univariateContinuous['Column Name'].iloc[i],percentileIncrement=-1)
        except Exception as e:
            print("Error occured while calculating percentile distribution for column: "+ univariateContinuous['Column Name'].iloc[i])
            print(e)
        
        
        # Appending percentile distribution to dataframe
        for percentile,distribution in percentDist.items():
            try:
                univariateContinuous.loc[i,str(percentile)]=distribution
            except Exception as e:
                print("Error occured while appending descriptive statistics to dataframe")
                print(e)
        
    try:
        univariateContinuous.to_csv("analysis report/univariateContinuous.csv")
    except Exception as e:
        print("Error occured while saving csv")
        print(e)
    
    
    
    
    
################################# Creating univariate analysis csv file for Categorical variables ###############################

    for column in listCategorical:
            if len(df[column].unique())<50:
                
                # Creating a list of columns with count in each category
                try:
                    listCategoriesCount= listCategoriesCount+([(column,m,n) for m,n in dict(df[column].value_counts()).items()])
                except Exception as e:
                    print("Error occured while calculating count of categorical variables")
                    print(e)
                try:
                    pd.DataFrame(listCategoriesCount,columns=['Column Name','Category','Count']).to_csv("analysis report/univariateCategorical.csv")
                except Exception as e:
                    print("Error occured while saving csv file")
                    print(e)
                
    
    
    
    
    
    
    
################################# Creating correlation matrix #####################################################################

    try:
        df.corr().to_csv("analysis report/correlationMatrix.csv")
    except Exception as e:
        print("Error occured while creating correlation matrix")
        print(e)

