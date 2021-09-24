
# coding: utf-8

# #### Importing necessary packages

# In[1]:


"""
bivariate_module - a bivariate analysis library for exploratory data analysis in Python
=======================================================================================

**bivariate_module** is a Python package by MathMarket (Team Tesla) of TheMathCompany,
providing fast and expressive data analysis designed to work with csv files or pandas dataframe.
It aims to provide faster exploring and analysing data via vizualization while
doing **real world** data analysis in Python.

The module is built using numpy, pandas, import_ipynb, plotly and ipywidget.
Therefore, these libraries are need to be installed in order to use the module.

Other neccessary modules include biplot, removeIdDate, imputing and findContinuous,
that are need to be present in same directory as this module.

The module consist of one function:
`bivariate(data,dependent=None,event=None)`


Main Features
-------------
Here are just a few of the things that bivariate_module does well:

  - Provide graphical repersentation of 2 to 5 variables.
  - Bar-line plot: If one of the column (x-axis or y-axis)
    is continuous. Percentage of continuous column in each category
    or decile (in case of continuous variable) will be shown.
  - Bubble plot: With choice to select x-axis, y-axis,
    bubble size (continuous variable),bubble color (categorical variable)
    and bubble group (compulsory; to change the level of data)
  - Total number of bubbles will be equal to the number of categories
    in bubble group if bubble color is not selected, otherwise will be equal
    to the number of categories in bubble color.
  - Scatter plot: if size of the bubble is not defined. You can also set
    bubble color to the scatter plot.
  - Stacked-Bar plot: If both x-axis and y-axis are categorical columns.
  - Bar Line plot (with dependent variable): Event rate of dependent varaible
    via bar-line plot showing percentage of event in 
    each decile (in case of continuous column) or category.
  - Further, these graphs can be downloaded and saved as png images.
"""


# Checking if the required libraries are installed
lib1 = get_ipython().getoutput('pip install pandas')
lib2 = get_ipython().getoutput('pip install numpy')
lib3 = get_ipython().getoutput('pip install plotly')
lib4 = get_ipython().getoutput('pip install ipywidgets')

# importing required modules
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
    from bubbly.bubbly import bubbleplot
except ImportError:
    print("Bubbly module not found")
try:
    from plotly.offline import download_plotlyjs as pltjs, init_notebook_mode, plot, iplot
except ImportError:
    print("Plotly offline module not found")
init_notebook_mode(connected=True)

# importing required classes from ipywidgets
try:
    from ipywidgets import widgets
except ImportError:
    print("ipywidgets module not found")
try:
    from IPython.display import display, clear_output
except ImportError:
    print("Module not found")

# importing neccessary modules
try:
    import import_ipynb
except ImportError:
    print("import_ipynb module not found")
try:
    from analysis import Biplot
except ImportError:
    print("Biplot notebook not found")
try:
    from analysis import DataCleaning
except ImportError:
    print("DataCleaning notebook not found")

# importing MISC
import tkinter as Ttk
from tkinter import ttk, filedialog
from tkinter.ttk import *
from tkinter import *


# #### Fucntion to create a popup dialog box to select the csv file

# In[1]:


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


# #### This function creates an interface containing dropdown widgets from ipywidget and graphs from plotly



def bivariate(data,dependent=None,event=None):
    
    
    """2 to 5 dimensional plotly graphs provided with dropdowns containing
    columns of the input dataframe or csv file and list of graphs.
    The output is reactive, therefore you can have diffrent plots according
    to the choice of columns from dropdown.

    Parameters
    ----------
    data      : csv file (or file path) passed as string
                or pandas dataframe

    dependent : Column name of categorical variable
                passed as string. Can be used only
                when event is specified.
                
    event     : Category in the dependent column passed
                as string.

    Examples
    --------
    bivariate_module.bivariate(data="csv_filename.csv")

    bivariate_module.bivariate(data="path/csv_filename.csv")

    bivariate_module.bivariate(data=pandas_dataframe)

    bivariate_module.bivariate(data="example.csv",dependent="categorical_variable", event="category_in_categorical_variable" )
    """
    
################################# Reading csv file into dataframe###############################################################
    if type(data)==type(pd.DataFrame()):
        try:
            bivariateDf = data
        except Exception as e:
            print("The given dataset is not a dataframe")
            print(e)
    else:
        try:
            bivariateDf = pd.read_csv(data)
        except Exception as e:
            print("Enter only csv files")
            print(e)




    #checking size of dataframe
    if bivariateDf.count()[0]<2 and len(list(bivariateDf.columns))<2:
        print("This file is empty")
        
    if dependent!=None and event==None:
        print("Please provide event of your dependent varaiable")
        
        
    yearColumn=None
    if yearColumn==None or bivariateDf[yearColumn].dtype!='int64':
        dateTime=None
        
    drop=[]
    for i in bivariateDf:
        if len(bivariateDf[i].unique())<2:
            drop.append(i)
            bivariateDf.drop(i, axis=1,inplace=True)
        
        
    # creating a list of continuous variables datatypes
    numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64','float','int']
    
    # Configurations for plotly graph
    config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}
    
    
#################################################### Imputing dataframe#########################################################
    
    # imputing missing values
    try:
        bivariateDf,droppedColumnList = DataCleaning.imputeData(bivariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to impute data")
        print(e)
    #If continuous column read as object datatype because of garbage strings; Converting those columns to int/float
    try:
        bivariateDf = DataCleaning.findContinuousColumns(bivariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to find continuous columns")
        print(e)
    # removing columns that have more than 90% unique values and are categorical type
    try:
        bivariateDf = DataCleaning.removeIds(bivariateDf)
    except Exception as e:
        print("Error while calling datacleaning notebook to remove date columns")
        print(e)

        
#################################################### Creating dropdown menu for column selection################################
    try:
        xDropdown = widgets.Dropdown(
            options=list(bivariateDf.columns),
            value=bivariateDf.columns[0],
            description='X-Axis'
        )
    except Exception as e:
        print("Error occured while creating dropdown for X axis")
        print(e)
        
    try:
        yDropdown = widgets.Dropdown(
            options=list(bivariateDf.columns),
            value=bivariateDf.columns[1],
            description='Y-Axis'
        )
    except Exception as e:
        print("Error occured while creating dropdown for Y axis")
        print(e)
        
    try:
        graphDropdown = widgets.Dropdown(
            options = ['Bar-Line-Combo','Stacked-Bar-Graph'],
            value= None,
            description = 'Graph Type'
        )
    except Exception as e:
        print("Error occured while creating dropdown for graph selection")
        print(e)
    
    
    #List of column name that are continuous
    listContinuous=[None]
    listCategorical=[None]
    listGroupColumn = []
    for i in bivariateDf.columns:
        if len(bivariateDf[i].unique()) < 100:
            listCategorical.append(i)
            listGroupColumn.append(i)   
        else:
            listContinuous.append(i)
    
    try:
        sizeDropdown = widgets.Dropdown(
            options = listContinuous,
            value= None,
            description = 'Bubble Size'
        )
    except Exception as e:
        print("Error occured while creating size dropdown for bubble plot")
        print(e)
        
    try:
        colorDropdown = widgets.Dropdown(
            options = listCategorical,
            value = None,
            description = 'Scatter Color'
        )
    except Exception as e:
        print("Error occured while creating dropdown for selecting bubble colour")
        print(e)
        
    try:
        if len(listGroupColumn) > 0:
            bubbleGroupDropdown = widgets.Dropdown(
                options = listGroupColumn,
                value = None,
                description = 'Bubble Group'
            )
        else:
            bubbleGroupDropdown = widgets.Dropdown(
                options = listGroupColumn,
                value = None,
                description = 'Bubble Group'
            )
    except Exception as e:
        print("Error occured while creating bubble group dropdown")
        print(e)


    if bivariateDf[bivariateDf.columns[0]].dtype in numerics or bivariateDf[bivariateDf.columns[1]].dtype in numerics:
        try:
            bivariateBarLine= Biplot.createBarLine(bivariateDf,bivariateDf.columns[0],bivariateDf.columns[1])
        except Exception as e:
            print("Error occured while calling biplot notebook to generate barline graph")
            print(e)
        try:
            container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
        except Exception as e:
            print("Error occured while generating container consisting of x axis, y axis and graph type")
            print(e)
        display(container)
        try:
            iplot(bivariateBarLine,config=config)
        except Exception as e:
            print("Error occured while displaying the barline graph")
            print(e)
        if len(drop) > 0:
            print("The following singular matrix columns are dropped : %s"%drop)
        if len(droppedColumnList) > 0:
            print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
        
    else:
        try:
            stackedBarGraph= Biplot.createStackedBarGraph(bivariateDf,bivariateDf.columns[0],bivariateDf.columns[1])
        except Exception as e:
            print("Error occured while calling biplot notebook to generate stack bar graph")
            print(e)
        try:
            container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
        except Exception as e:
            print("Error occured while generating container consisting of x axis, y axis and graph type")
            print(e)
        display(container)
        try:
            iplot(stackedBarGraph,config=config)
        except Exception as e:
            print("Error occured while displaying stack bar graph")
            print(e)
        if len(drop) > 0:
            print("The following singular matrix columns are dropped : %s"%drop)
        if len(droppedColumnList) > 0:
            print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)

################################################################################################################################
## FUNCTION: responding to user interaction
## INPUT: df - Column selected by user
## OUTPUT: histogram for the selected column. Boxplot for selected column if continuous
################################################################################################################################

    def response(change):
        
        
        # User select dependent variable for X or Y Axis
        
        if (change['new']==dependent or yDropdown.value==dependent or xDropdown.value==dependent) and graphDropdown.value=='Bar-Line-Combo':
            
            if change['owner'].description=='X-Axis' and event!=None:
                try:
                    bivariateBarLine= Biplot.createGraphDependent(bivariateDf,change['new'],yDropdown.value,dependent,event)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])                
                clear_output(wait=True)
                display(container)
                iplot(bivariateBarLine,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
            
            elif change['owner'].description=='Y-Axis' and event!=None:
                try:
                    bivariateBarLine= Biplot.createGraphDependent(bivariateDf, xDropdown.value, change['new'],dependent,event)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])                
                clear_output(wait=True)
                display(container)
                iplot(bivariateBarLine,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
            elif event!=None:
                try:
                    bivariateBarLine= Biplot.createGraphDependent(bivariateDf, xDropdown.value, yDropdown.value,dependent,event)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])                
                clear_output(wait=True)
                display(container)
                iplot(bivariateBarLine,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
            else:
                print("Please provide event for dependent variable")
                
                
                
                
                
                
        # User select different column for X-Axis
        
        elif change['owner'].description=='X-Axis':
            
            if graphDropdown.value=='Scatter-Graph':
                # Plot bar-line combo graph
                try:
                    bivariateBubble=Biplot.createGraphBubble(bivariateDf,change['new'],yDropdown.value, bubbleGroup=bubbleGroupDropdown.value,bubbleSize=sizeDropdown.value, bubbleColor=colorDropdown.value, timeColumn = yearColumn)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown]),widgets.HBox(children=[colorDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(bivariateBubble,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
            elif bivariateDf[change['new']].dtype in numerics or bivariateDf[yDropdown.value].dtype in numerics:
                # Plot bar-line combo graph
                graphDropdown.value='Bar-Line-Combo'
                try:
                    bivariateBarLine= Biplot.createBarLine(bivariateDf,change['new'],yDropdown.value)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(bivariateBarLine,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
                
            else:
                # Plot stacked bar graph
                graphDropdown.value='Stacked-Bar-Graph'
                try:
                    stackedBarGraph= Biplot.createStackedBarGraph(bivariateDf,change['new'],yDropdown.value)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(stackedBarGraph,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
                
        
        # User select different column for Y-Axis
            
        elif change['owner'].description=='Y-Axis':
            if graphDropdown.value=='Bubble-Graph':
                # Plot bar-line combo graph
                try:
                    bivariateBubble=Biplot.createGraphBubble(bivariateDf,xDropdown.value,change['new'], bubbleGroup=bubbleGroupDropdown.value,bubbleSize=sizeDropdown.value,bubbleColor=colorDropdown.value, timeColumn = yearColumn)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown]),widgets.HBox(children=[colorDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(bivariateBubble,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
            elif bivariateDf[change['new']].dtype in numerics or bivariateDf[xDropdown.value].dtype in numerics:
                try:
                    # Plot bar-line combo graph
                    bivariateBarLine= Biplot.createBarLine(bivariateDf,xDropdown.value,change['new'])
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(bivariateBarLine,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
            else:
                try:
                    # Plot stacked bar graph
                    stackedBarGraph= Biplot.createStackedBarGraph(bivariateDf,xDropdown.value,change['new'])
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(stackedBarGraph,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
                
                
                
        # User selected different Graph
            
        elif change['owner'].description=='Graph Type':
            if change['new']=='Stacked-Bar-Graph':
                try:
                    # Plot Stacked bar graph
                    stackedBarGraph= Biplot.createStackedBarGraph(bivariateDf,xDropdown.value,yDropdown.value)
                except Exception as e:
                    print(" Graph cannot be plotted for selected columns")
                    print(e)
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(stackedBarGraph,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                    
                    
                    
                    
            elif change['new']=='Bubble-Graph':
                try:
                    # Plot Scatter graph
                    bivariateBubble=Biplot.createGraphBubble(bivariateDf,xDropdown.value,yDropdown.value, bubbleGroup=bubbleGroupDropdown.value, bubbleSize=sizeDropdown.value,bubbleColor=colorDropdown.value, timeColumn = yearColumn)
                except Exception as e:
                    print("Graph cannot be plotted for selected columns")
                    print(e)
                
                container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown]),widgets.HBox(children=[colorDropdown])])
                clear_output(wait=True)
                display(container)
                iplot(bivariateBubble,config=config)
                if len(drop) > 0:
                    print("The following singular matrix columns are dropped : %s"%drop)
                if len(droppedColumnList) > 0:
                    print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                
                
                
                
            else:
                if bivariateDf[yDropdown.value].dtype in numerics or bivariateDf[xDropdown.value].dtype in numerics:
                    try:
                        # Plot Bar-line combo graph
                        bivariateBarLine= Biplot.createBarLine(bivariateDf,yDropdown.value,xDropdown.value)
                    except Exception as e:
                        print(" Graph cannot be plotted for selected columns")
                        print(e)
                    container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown])])
                    clear_output(wait=True)
                    display(container)
                    iplot(bivariateBarLine,config=config)
                    if len(drop) > 0:
                        print("The following singular matrix columns are dropped : %s"%drop)
                    if len(droppedColumnList) > 0:
                        print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
                    
                    
                    
                    
                else:
                    print("Cannot Plot Bar-Line Combo Graph for both Categorical column")
                    
                    
                    
                    
                    
        # Bubble plot specific Dropdowns
        
        
        # user selected different column for bubble size
        elif change['owner'].description=='Bubble Size':
            try:
                bivariateBubble=Biplot.createGraphBubble(bivariateDf,xDropdown.value,yDropdown.value, bubbleGroup=bubbleGroupDropdown.value,bubbleSize=change['new'],bubbleColor=colorDropdown.value, timeColumn = yearColumn)
            except Exception as e:
                print(" Graph cannot be plotted for selected columns")
                print(e)
            container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown]),widgets.HBox(children=[colorDropdown])])
            clear_output(wait=True)
            display(container)
            iplot(bivariateBubble,config=config)
            if len(drop) > 0:
                print("The following singular matrix columns are dropped : %s"%drop)
            if len(droppedColumnList) > 0:
                print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
            
        
        
        # user selected different column for bubble color    
        elif change['owner'].description=='Bubble Color':
            try:
                bivariateBubble=Biplot.createGraphBubble(bivariateDf,xDropdown.value,yDropdown.value,bubbleSize=sizeDropdown.value,bubbleColor=change['new'], bubbleGroup=bubbleGroupDropdown.value, timeColumn = yearColumn)
            except Exception as e:
                print(" Graph cannot be plotted for selected columns")
                print(e)
            container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown]),widgets.HBox(children=[colorDropdown])])
            clear_output(wait=True)
            display(container)
            iplot(bivariateBubble,config=config)
            if len(drop) > 0:
                print("The following singular matrix columns are dropped : %s"%drop)
            if len(droppedColumnList) > 0:
                print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
            
        
        
        # user selected different column for bubble group    
        elif change['owner'].description=='Bubble Group':
            
            try:
                bivariateBubble=Biplot.createGraphBubble(bivariateDf,xDropdown.value,yDropdown.value,bubbleSize=sizeDropdown.value,bubbleColor=colorDropdown.value,bubbleGroup=change['new'], timeColumn = yearColumn)
            except Exception as e:
                print(" Graph cannot be plotted for selected columns")
                print(e)
            container = widgets.VBox([widgets.HBox(children=[xDropdown,yDropdown,graphDropdown]),widgets.HBox(children=[colorDropdown])])
            clear_output(wait=True)
            display(container)
            iplot(bivariateBubble,config=config)
            if len(drop) > 0:
                print("The following singular matrix columns are dropped : %s"%drop)
            if len(droppedColumnList) > 0:
                print("The following columns which have more than 80% missing values have been dropped : %s"%droppedColumnList)
            
            
            
            

    # Listener waiting for user interaction
    xDropdown.observe(response,names='value')
    yDropdown.observe(response,names='value')
    graphDropdown.observe(response,names='value')
    sizeDropdown.observe(response,names='value')
    colorDropdown.observe(response,names='value')
    bubbleGroupDropdown.observe(response,names='value')

