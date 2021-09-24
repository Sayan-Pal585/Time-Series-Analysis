
# coding: utf-8

# #### Importing the necessary packages

# In[1]:


"""
UnivariateModule - a univariate analysis library for exploratory data analysis in Python
=======================================================================================

**UnivariateModule** is a Python package by MathMarket,
providing fast and expressive data analysis designed to work with csv files or pandas dataframe.
It aims to provide faster exploring and analysing data via vizualization while
doing **real world** data analysis in Python.

The module is built using numpy, pandas, import_ipynb, plotly and ipywidget.
Therefore, these libraries are need to be installed in order to use the module.

Other neccessary modules include Uniplot, DataCleaning, CreateUnivariateCSV and UnivariateInterface,
these need to be present in same directory as this module.

The module consist of one function:
`univariate(data)`


Main Features
-------------
Here are just a few of the things that UnivariateModule does well:

  - A brief information about the dataset which is the number of observations and the number of variables
  - If the column choosen is categorical, then a histogram is generated
  - If the column choosen is continuous then 
    1) A histogram is generated
    2) A table of descriptive statistics is generated which consists of
    Measures of central tendency
    a)Mean
    b)Median
    c)Mode
    Measures of dispersion
    d)Min
    e)Max
    f)Range
    g)Variance
    h)Standard deviation
    Measure of asymmtery
    i)Skewness
    Measure of flatness
    j)Kurtosis
    3)A boxplot is generated which represents the outliers
    4)A table containing percentile distribution is generated, here the user can enter the increment for 
    the percentile distribution.For example if the user enters 0.1, the table generated will have an increment of 0.1 from
    10 to 20 and 90 to 100.
    5)A graph of percentile distribution is also generated
    6)A csv of the categorical variable consists of its frequency distribution
    7)A csv of the continuous variable consists of descriptive statistics and its percentile distribution
  - Further, these graphs can be downloaded and saved as images.
"""


# Importing required modules
try:
    import pandas as pd
except ImportError:
    print("Pandas module not found")
try:
    import numpy as np
except ImportError:
    print("Numpy module not found")
try:
    import plotly.graph_objs as go
except ImportError:
    print("Go module not found")
try:
    from plotly.offline import download_plotlyjs as pltjs, init_notebook_mode, iplot
except ImportError:
    print("Plotly offline not found")
init_notebook_mode(connected=True)

# Importing required classes from ipywidgets
try:
    from ipywidgets import widgets,Layout
except ImportError:
    print("ipywidgets module not found")
try:
    from IPython.display import display, clear_output
except ImportError:
    print("Module not found ")

# Importing univariate modules
try:
    from analysis import DataCleaning
except ImportError:
    print("Datacleaning notebook not found")
try:
    import import_ipynb
except ImportError:
    print("Import_ipynb notebook not found")
try:
    from analysis import UnivariateInterface as interface
except ImportError:
    print("UnivariateInterface notebook not found")
import tkinter as Ttk
from tkinter import ttk, filedialog
from tkinter.ttk import *
from tkinter import *


# #### Function to create a popup dialog box to select the csv file

# In[5]:


################################################################################################################################
##FUNCTION: Gives a popup dialog box to select csv file
##INPUT: None
##OUTPUT: Dataframe selected by user
################################################################################################################################

def readCsv():
    """
    readCsv function - A popup using tkinter package to select csv
    =======================================================================================
    **readCsv** is a Python function by MathMarket (Team Tesla) of TheMathCompany,
    providing a fast and dynamic way to select a csv file and convert it to a pandas dataframe.

    This functions return dataframe at index 0 and dataframe name at index 1
    
    The function is built using  filedialogue from tkinter package
    Therefore, these libraries are needed to be installed in order to use the module.

    The module consists of one function:
    `readCsv()` 
    """
    try:
        root = Ttk.Tk()
    except:
        print("An error occured")
    try:
        root.df =  filedialog.askopenfilename(initialdir = "/",title = "Select file",filetypes = (("Csv files","*.csv"),("all files","*.*")))
    except FileNotFoundError:
        print("File not found.")
    except EOFError:
        print("End of file condition")
    except OSError:
        print("An error has occured")
    except NameError:
        print("Name error occured")
    try:
        df = root.df
    except OSError:
         print("An error has occured")
    try:
        root.withdraw()
    except:
         print("Unbable to close tkinter popup")
    try:
        df = pd.read_csv(df)
    except OSError:
         print("An error has occured")
    except NameError:
        print("Name error occured")
    try:
        df =pd.DataFrame(df)
        df.head()
    except OSError:
         print("An error has occured")
    except NameError:
        print("Name error occured")
    try:
        return(df)
    except IOError:
        print("IO error occured")


# #### Function to create the univariate interface with dataset information table, histogram chart, descriptive statistics table, boxplot chart, percentile distribution table and percentile distribution graph

# In[6]:


def univariate(data):
    
    """2 dimensional plotly graphs provided with dropdowns containing
    columns of the input dataframe or csv file
    The output is reactive, therefore you can have different plots according
    to the choice of columns from dropdown

    Parameters
    ----------
    data : csv file (or file path) passed as string
           or pandas dataframe

    Examples
    --------
    univariate_module.univariate(data=pandas_dataframe)

    """

################################################### Reading csv file into dataframe##############################################
#try:
    try:
        if type(data)==type(pd.DataFrame()):
            univariateDf = data
        else:
            univariateDf = pd.read_csv(data)
    except Exception as e:
        print("Could not identify dataset")
        print(e)




    # checking size of dataframe
    if univariateDf.count()[0]<2 and len(list(univariateDf.columns))<2:
        print("This file is empty")

    # creating a list of continuous variables datatypes
    numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64','float','int']

    for i in univariateDf:
        if len(univariateDf[i].unique())<2:
            univariateDf.drop(i, axis=1,inplace=True)

    
################################################### Imputing dataframe##########################################################
    
    # Imputing missing values
    try:
        univariateDf,droppedColumnList = DataCleaning.imputeData(univariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to impute data")
        print(e)
    # If a continuous column is read as object datatype because of garbage strings, these columns are identified as numeric datatype
    # by converting them to float type
    try:
        univariateDf = DataCleaning.findContinuousColumns(univariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to find continuous columns")
        print(e)
    # Removing columns that have more than 300 unique values and are object type
    try:
        univariateDf = DataCleaning.removeIds(univariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to remove Ids")
        print(e)
    # Removing datetime columns
    try:
        univariateDf = DataCleaning.removeDates(univariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to remove date columns")
        print(e)
    
    
    
    
    # Making style to display complete content
    style = {'description_width': 'initial','margin':'top'}


################################################### Creating dropdown menu for column selection#################################
    try:
        columnNamesDropdown = widgets.Dropdown(
            options=list(univariateDf.columns),
            value=univariateDf.columns[0]
        )
    except Exception as e:
        print("Error while creating dropdown menu for column selection")
        print(e)




################################################### Creating text input box for percentile increment############################
    try:
        percentileIncrementText = widgets.FloatText(
            value=1,
            description='Enter the increment for distribution between 0 to 10 and 90 to 100',
            disabled=False,
            style = {'description_width': 'initial'},
            layout = {'width': '500px'}

        )
    except Exception as e:
        print("Error while creating text input box for percentile distribution")
        print(e)
#    HBox([Label('A too long description'), percentileIncrementText()])

    
    # Displaying plots with first column
    if univariateDf[univariateDf.columns[0]].dtype in numerics:
        try:
            interface.displayContinuousVariables(univariateDf,columnNamesDropdown,percentileIncrementText,univariateDf.columns[0])
            if len(droppedColumnList) > 0:
                print("Following Columns has been dropped : %s"%droppedColumnList)
        except Exception as e:
            print("Error while displaying the first column given it is a continuos column")
            print(e)
    else:
        try:
            interface.displayCategoricalVariables(univariateDf,columnNamesDropdown,univariateDf.columns[0])
            if len(droppedColumnList) > 0:
                print("Following Columns has been dropped : %s"%droppedColumnList)
        except Exception as e:
            print("Error while displaying the first column given it is a categorical column")
            print(e)

            
################################################################################################################################
## FUNCTION: responding to user interaction
## INPUT: df - Column selected by user
## OUTPUT: histogram for the selected column. Boxplot for selected column if continuous
################################################################################################################################

    def response(change):
        # Checking if user changes column name or percentile increment
        if change['new'] in univariateDf.columns:
            if univariateDf[change['new']].dtype in numerics:
                try:
                    interface.displayContinuousVariables(univariateDf,columnNamesDropdown,percentileIncrementText,change['new'])
                    if len(droppedColumnList) > 0:
                        print("Following Columns has been dropped : %s"%droppedColumnList)
                except Exception as e:
                    print("Error occured while responding to user interaction for continuous columns")
                    print(e)
            else:
                try:
                    interface.displayCategoricalVariables(univariateDf,columnNamesDropdown,change['new'])
                    if len(droppedColumnList) > 0:
                        print("Following Columns has been dropped : %s"%droppedColumnList)
                except Exception as e:
                    print("Error occured while responding to user interaction for categorical columns")
                    print(e)
        else:
            # Displaying percentile distribution with changed percentile Increment value
            try:
                interface.displayContinuousVariables(univariateDf,columnNamesDropdown,percentileIncrementText,columnNamesDropdown.value,change['new'])
                if len(droppedColumnList) > 0:
                        print("Following Columns has been dropped : %s"%droppedColumnList)
            except Exception as e:
                print("Error occured while entering a percentile increment by the user")
                print(e)


    # Listener waiting for user interaction
    try:
        columnNamesDropdown.observe(response,names='value')
    except Exception as e:
        print("Error occured while user has changed column name")
        print(e)
    try:
        percentileIncrementText.observe(response,names='value')
    except Exception as e:
        print("Error occured while user has entered a percentile increment")
        print(e)

