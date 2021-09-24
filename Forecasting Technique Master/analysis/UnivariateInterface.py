
# coding: utf-8

# #### Importing the necessary packages

# In[1]:


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
    print("ipywidgets not found")
try:
    from IPython.display import display, clear_output
except ImportError:
    print("Module not found")

# Importing required classes from ipywidgets
try:
    import import_ipynb
except ImportError:
    print("Import_ipynb not found")
try:
    from analysis import Uniplot
except ImportError:
    print("Uniplot notebook not found")
try:
    from analysis import Stats
except ImportError:
    print("Stats notebook not found")
try:
    from analysis import PercentileDistribution
except ImportError:
    print("Percentile distribution notebook not found")


# #### Function to call all the univariate objects on the interface for a continuous variable

# In[2]:


################################################################################################################################
## FUNCTION: Creating and Displaying interface for continuous variables
## INPUT: df, Column selected by user, Column Dropdown widget, percentile increment for distribution, percentile textbox widget
## OUTPUT: Display interface containing histogram, boxplot, descriptive statistics, percentile distribution and graph of percentile distribution
################################################################################################################################

def displayContinuousVariables(univariateDf,columnNamesDropdown,percentileIncrementText,columnName,percentileIncrement=-1):
    
    """ An interface is created to display univariate objects, ipython widgets are used to create the dropdown for the column
    names and textbox for percentile increment, a table is created for representing descriptive statistics using plotly,
    a percentile distribution table and graph is generated .Histogram, boxplot, percentile distribution curve and 
    dataset information functions are called from uniplot notebook
    
    Parameters
    ----------
    univariateDf             : csv file (or file path) passed as string or pandas dataframe
    
    columnNamesDropdown      :column name dropdowns widget. (ipython dropdown widget)
    
    percentile increment text: widget textbox
    
    column name              : column name from dataset
    
    percentileIncrement      :  numeric value (default = -1)
        

           
           
    Examples
    --------
   displayContinuousVariables(pandas_dataframe,ipython_dropdown_widget,ipython_textbox,'ColumnNameChurn',percentileIncrement=1)
   
   
   
   
    """
    
    # Plotting Histogram
    try:
        univariateHistogram = Uniplot.create_histogram(univariateDf,columnName)
    except Exception as e:
        print("Error while calling histogram")
        print(e)
    # Plotting Boxplot
    try:
        univariateBoxplot = Uniplot.create_boxplot(univariateDf,columnName)
    except Exception as e:
        print("Error while calling boxplot")
        print(e)
    # Calculating Descriptive Statistics (mean,median,mode etc)
    try:
        descriptiveStatistics=Stats.calculateDescriptiveStatistics(univariateDf,columnName)
    except Exception as e:
        print("Error while calling descriptive statistics notebook")
        print(e)

    
    # Configurations for plotly graph
    config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}
    
    
    
    # Creating table of Descriptive Statistics
    try:
        statistics = list(descriptiveStatistics.keys())
    except Exception as e:
        print("Error while calling descriptive table keys")
        print(e)
    try:
        description = list(descriptiveStatistics.values())
    except Exception as e:
        print("Error while calling descriptive table values")
        print(e)
    try:
        traceDescStats = go.Table(header=dict(values = statistics,
                                     line = dict(color='#7D7F80'),
                                     fill = dict(color='rgb(62,64,62)'),
                                     align=['center'],
                                     font = dict(family='opensans',color = 'white', size = 16),
                                     height=30
                                    ),
                         cells = dict(values = description,
                                      line = dict(color='#7D7F80'),
                                      fill = dict(color = 'rgb(138,138,138)'),
                                      align=['center'],
                                      font = dict(family='opensans',color = 'white', size=14),
                                      height=30
                                     )
                        )
    except Exception as e:
        print("Error while creating table for descriptive statistics")
        print(e)
    try:
        dataDescStats=[traceDescStats]
        statsTable = go.FigureWidget(dataDescStats, layout=go.Layout(height=300,width=950,title="Descriptive Statistics",titlefont=dict(family='opensans',size=20,color='black')))
    except Exception as e:
        print("Error while creating figure widget for descriptive statistics table")
        print(e)
    
    # Calculating and creating Layout of Percentile Distribution
    try:
        percentileDistributionTitle = widgets.HTML(value='<div align="center"><font size="4">Percentile Distribution</font></div>')
    except Exception as e:
        print("Error while creating title for percentile distribution layout")
        print(e)
    # Calculating Percentile Distribution
    try:
        perDist = PercentileDistribution.percentileDistribution(univariateDf,columnName,percentileIncrement)
    except Exception as e:
        print("Error while calling percentile distribution notebook ")
        print(e)
    # Creating table of Descriptive Statistics
    try:
        percentile = list(perDist.keys())
    except Exception as e:
        print("Error while calling percentile distribution keys")
        print(e)
    try:
        distribution = list(perDist.values())
    except Exception as e:
        print("Error while calling percentile distribution values")
        print(e)
        
        
    
    
    try:
        tracePerDist = go.Table(header=dict(values = percentile,
                                            line = dict(color='#7D7F80'),
                                            fill = dict(color='rgb(62,64,62)'),
                                            align=['center'],
                                            font = dict(family='opensans',color = 'white', size = 16),
                                            height=30),
                                cells = dict(values = distribution,
                                             line = dict(color='#7D7F80'),
                                             fill = dict(color = 'rgb(138,138,138)'),
                                             align=['center'],
                                             font = dict(family='opensans',color = 'white', size=14),
                                             height=30)
                    )
    except Exception as e:
        print("Error while creating table for percentile distribution")
        print(e)
    try:
        dataPerDist=[tracePerDist]
        percentDist = go.FigureWidget(dataPerDist, layout=go.Layout(height=250,width=70*len(percentile)))
    except Exception as e:
        print("Error while creating figure widget for percentile distribution table")
        print(e)
    
    # Creating Distribution plot for percentile distribution
    try:
        distributionPlot = Uniplot.percentile_distribution_curve(perDist)
    except Exception as e:
        print("Error while calling percentile distribution plot")
        print(e)
    
    
    
    
    
    # Wrapping all items into container
    try:
        container_top = widgets.HBox([widgets.Label('Column Name'),columnNamesDropdown])
    except Exception as e:
        print("Error while displaying container for column names dropdown")
        print(e)
    try:
        container_bottom = widgets.VBox([percentileDistributionTitle,
                                         percentileIncrementText])
    except Exception as e:
        print("Error while displaying container for percentile increment text box")
        print(e)

    
    # Removing all previous objects from display
    try:
        clear_output(wait=True)
    except Exception as e:
        print("Error while clearing the interface to display the updated interface")
        print(e)
    # Displaying container and Graphs
    try:
        iplot(Uniplot.datasetInfo(univariateDf), config=config)
    except Exception as e:
        print("Error occured while displaying datasetinfo -- continuous column function")
        print(e)
    try:
        display(container_top)
    except Exception as e:
        print("Error occured while displaying container for column names dropdown -- continous function")
        print(e)
    try:
        iplot(univariateHistogram,filename=columnName+"Histogram",config=config)
    except Exception as e:
        print("Error occured while displaying histogram for continuous column")
        print(e)
    try:
        iplot(statsTable,filename=columnName+"DescriptiveStatistics",config=config)
    except Exception as e:
        print("Error occured while displaying statistics table for continuous coluumn")
        print(e)
    try:
        iplot(univariateBoxplot,filename=columnName+"Boxplot",config=config)
    except Exception as e:
        print("Error occured while displaying boxplot for continous columns")
        print(e)
    try:
        display(container_bottom)
    except Exception as e:
        print("Error occured while displaying container for percentile distrubution increment")
        print(e)
    try:
        iplot(percentDist,config=config)
    except Exception as e:
        print("Error occured while displaying percentile distribution table for continuous column")
        print(e)
    try:
        iplot(distributionPlot,config=config)
    except Exception as e:
        print("Error occured while displaying percentile distribution plot for continuous column")
        print(e)


# #### Function to call all the univariate objects on the interface for a categorical variable

# In[3]:


################################################################################################################################
## FUNCTION: Creating and Displaying interface for categorical variables
## INPUT: df, Column selected by user, Column Dropdown widget
## OUTPUT: Display interface containing histogram
################################################################################################################################

def displayCategoricalVariables(univariateDf,columnNamesDropdown,columnName):
    
    
    """ If a categorical variable is choosen a histogram is generated, this function creates a interface for the categorical 
    variables choosen.The histogram generated is called from uniplot notebook
    
    Parameters
    ----------
    univariateDf        : csv file (or file path) passed as string or pandas dataframe
    
    columnNamesDropdown :column name dropdowns widget (ipython widget)
    
    column name         : column name from dataset
           
    Examples
    --------
   
   
   displayCategoricalVariables('Churn.csv',columnNamesDropdown,'Churn')
   
   
    """
    
    
    # Configurations for plotly graph
    try:
        config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}
    except Exception as e:
        print("Error occured while configuring settings for plotly")
        print(e)
    
    # Plotting Histogram
    try:
        univariateHistogram = Uniplot.create_histogram(univariateDf,columnName)
    except Exception as e:
        print("Error while creating histogram for categorical variable")
        print(e)
    
    # Wrapping all items into container
    try:
        container = widgets.HBox([widgets.Label('Column Name'),columnNamesDropdown])
    except Exception as e:
        print("Error while creating container for categorical variable- dropdown)")
        print(e)
    # Removing all previous objects from display
    clear_output(wait=True)
    
    # Displaying container and Graphs
    try:
        iplot(Uniplot.datasetInfo(univariateDf), config=config)
    except Exception as e:
        print("Error occured while displaying datasetinfo container -- categorical column function")
        print(e)
    try:
        display(container)
    except Exception as e:
        print("Error occured while displaying container for column names dropdown -- categorical function")
        print(e)
    try:
        iplot(univariateHistogram,filename=columnName+"Histogram",config=config)
    except Exception as e:
        print("Error occured while displaying histogram for categorical column")
        print(e)

