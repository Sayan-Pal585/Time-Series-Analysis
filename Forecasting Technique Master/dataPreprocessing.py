# -*- coding: utf-8 -*-
"""
Created on Fri Jan 25 15:35:58 2019

@author: TheMathCompany
"""

################################ Math Market ################################

#Importing python script files to call functions
import analysis.DataCleaning as DataCleaning
import analysis.CreateUnivariateCSV as CreateUnivariateCSV
import analysis.plots.Uniplot as Uniplot
import analysis.plots.Biplot as Biplot

#The below code imports packages to be used by control sheet
#Data manipulation packages
import pandas as pd

#Below given code is used to select dataframe
df = pd.read_csv("IntegratedForecastingTechniques\dataset\unemp_rate.csv")

#Univariate Analysis
######################################################################################################################################################################################
##FUNCTION: create percentile distribution and descriptive statistics for 
#           continuous columns, create count of different categories for
#           categorical column, create correlation matrix -- to drop highly
#           corelated columns
##INPUT: df - Data Frame, percentileIncrement - set the increment in the range
#        of 0-10 and 90-100 
##OUTPUT: Three files in the currentdirectory/analysisreport containing
#         description about variable of the data frame
######################################################################################################################################################################################
df.head()

CreateUnivariateCSV.createUnivariatecsv(df, percentileIncrement=-1)

# Function be default replaces blanks and garbage values with nan
######################################################################################################################################################################################
##FUNCTION: This function will replaces blanks and garbage values with 
#           nan(by default) or defined parameter. If missing is above 
#           defined limit, This function will drop that column
##INPUT: df - Data Frame, imputeWith - Metric to impute with, 
#             imputeCategorical - boolean if True Categorical variables will
#             be imputed with mode of respective column
##OUTPUT: Return imputed dataframe with list of dropped columns (if any)
######################################################################################################################################################################################
dfCleaned,droppedColumnList = DataCleaning.imputeData(df,imputeWith='mean',
                                                      imputeCategorical=True,
                                                      missingLimit=20)
dfCleaned

######################################################################################################################################################################################
##FUNCTION: This function will try to evaluate columns that are read as object
#           type due to garbage value and convert them to float/int type
##INPUT: df - Data Frame, Threshold - Limit upto which function will check for
#        values to be continuous 
##OUTPUT: Dataframe with converted columns
######################################################################################################################################################################################
listOfIdentifiedColumns = DataCleaning.findContinuousColumns(df,
                                                             threshold = 100)
listOfIdentifiedColumns



######################################################################################################################################################################################
##FUNCTION: This function remove all the columns that have complete/partial unique 
#           values
##INPUT: df - Data Frame(ADS), threshold - Number of permissible unique values
#             in dataframe
##OUTPUT: Retrun dataframe with columns that have less then permissible unique
#         values
######################################################################################################################################################################################
dfWithoutIDs = DataCleaning.removeIds(df, threshold=300)
dfWithoutIDs


######################################################################################################################################################################################
##FUNCTION: Histogram Plot with the given varaibles
##INPUT: df - Data Frame(ADS), columnName - Name of the column in dataframe
##OUTPUT: Histogram Plot of the input column
######################################################################################################################################################################################
Uniplot.createHistogram(df,'value')


######################################################################################################################################################################################
##FUNCTION: Box Plot with the given variables
##INPUT: df - Data Frame(ADS), columnName - Name of the column in dataframe
##OUTPUT: Box Plot of the input column
######################################################################################################################################################################################
Uniplot.createBoxplot(df,'value')


######################################################################################################################################################################################
##FUNCTION: This function remove outlier with the default/defined limits for
#            the column
##INPUT: df - Data Frame(ADS), colNames - Name of the columns in dataframe
#             lowerLimit - Column Limit below which rows will be deleted
#             upperLimit - Column Limit above which rows will be deleted
##OUTPUT: Retrun dataframe with treated outliers
######################################################################################################################################################################################
dfWithoutOutliers = DataCleaning.removeOutlier(df, colNames = ['age','bmi'],
                                               lowerLimit = None,
                                               upperLimit = None,
                                               IQRCoeff = 0.3)
dfWithoutOutliers

#Enter dependent variable column name within the quotes
df.head()
dep_var = "insuranceclaim"


# BIVARIATE
# How dependent variable is varying with all different columns
# Barline plot
######################################################################################################################################################################################
##FUNCTION: This function display Bar-Line Combo Plot, where Bar represent
#            values in column X and line represent event rate in column Y
##INPUT: df - Data Frame(ADS), colX - Name of the columns for X Axis
#             colY - Name of column for Y-Axis
#             dependent - Column name of dependent varaible
#             event - Value of event in dependent Variable
##OUTPUT: Retrun Bar-Line Combo Plot
######################################################################################################################################################################################
Biplot.createGraphYContinuous(df,colX='dates',colY='value')

