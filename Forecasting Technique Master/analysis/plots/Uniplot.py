
# coding: utf-8

# #### Importing the necessary packages

try:
    import plotly.graph_objs as go
except ImportError:
    print("Go module not found")
    
from analysis import Stats
from analysis import PercentileDistribution
from plotly.offline import download_plotlyjs as pltjs, init_notebook_mode, plot



# Creating graph configuration to mantain data privacy
config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}



# #### Function to create histogram for the selected column 


################################################################################################################################
## FUNCTION: Create histogram object
## INPUT: df - dataframe from which column will be selected, columnName - name of the column for which histogram is needed
## OUTPUT: Histogram as graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createHistogram(df,columnName):
    
    
    """ 
    a histogram is generated for both categorical and continuous variables which gives the distribution of data.
    Parameters
    ----------
    df : csv file (or file path) passed as string
           or pandas dataframe and the column name
           
    Examples
    --------
    create_histogram(data="csv_filename.csv",'lat')

    create_histogram(data="path/csv_filename.csv",'lat')

    create_histogram(data=pandas_dataframe,'lat')
    
    uniplot.create_histogram(data="csv_filename.csv",'lat')

    """
	
    # providing data for histogram
    try:
        data = [go.Histogram(x=df[columnName],autobinx=False,nbinsx=10,visible=True,name=str(columnName),marker=dict(color='rgb(0,31,95)'))]
    except Exception as e:
        print("Histogram could not be generated for the given column")
        print(e)
    # setting chart title and axis title for the graph
    try:
        layout = go.Layout(
            title=("Histogram of "+columnName).title(),
            titlefont=dict(
                family='opensans',
                size=20,
                color='black'
            ),
            xaxis=dict(
                linewidth = 2,
                title=(columnName+'_levels').lower(),
                zeroline=True,
                showline=True,
                titlefont=dict(
                    family='opensans',
                    size=16,
                    color='black'
                )
            ),
            yaxis=dict(
                linewidth = 2,
                title=('Population_in_'+columnName+'_Levels').lower(),
                zeroline=True,
                showline=True,
                titlefont=dict(
                    family='opensans',
                    size=16,
                    color='black'
                )
            )
        )
    except Exception as e:
        print("Layout for histogram could not be generated")
        print(e)
    return plot(go.FigureWidget(data=data, layout=layout),config=config)


# #### Function to create a boxplot

################################################################################################################################
## FUNCTION: Create boxplot object
## INPUT: df - dataframe from which column will be selected, columnName - name of the column for which histogram is needed
## OUTPUT: Boxplot as graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createBoxplot(df,columnName):
    
    
    """ a boxplot is generated for continuous variables from which outliers can be seen.
    Parameters
    ----------
    data : csv file (or file path) passed as string
           or pandas dataframe and the column name
           
    Examples
    --------
    create_boxplot(df="csv_filename.csv",'lat')

    create_boxplot(df="path/csv_filename.csv",'lat')

    create_boxplot(df=pandas_dataframe,'lat')

    """
    
     
    
    # providing data to boxplot
    try:
        data = [go.Box(x=df[columnName],
                       boxpoints='suspectedoutliers',
                       visible=True, name=(columnName).lower(),
                       marker=dict(color='rgb(0,31,95)'))]
    except Exception as e:
        print("Boxplot could not be generated")
        print(e)
    try:
        layout = go.Layout(
        title=("Boxplot of "+columnName).title(),
        titlefont=dict(
            family='opensans',
            size=20,
            color='black'
            ),
        xaxis = dict(linewidth = 2, zeroline=False, showline=True),
        yaxis = dict(linewidth = 2, zeroline=False, showline=True),
        )
    except Exception as e:
        print("Layout for boxplot could not be generated")
        print(e)
    return plot(go.FigureWidget(data=data,layout=layout),config=config)


# #### Function to create a percentile distribution graph

# In[13]:


################################################################################################################################
## FUNCTION: Create percentile distribution graph object
## INPUT: df - dataframe from which column will be selected, columnName - name of the column for which histogram is needed
## OUTPUT: line chart as graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################



def percentileDistributionCurve(perDist):
    
    
    """ a percentile distribution is generated for continuous variables
    Parameters
    ----------
    data : csv file (or file path) passed as string
           or pandas dataframe and the column name
           
    Examples
    --------
    percentile_distribution_curve(perDist)
    

    """
    
    # providing data to line plot
    try:
        data= [go.Scatter(x = list('`'+str(i) for i in perDist.keys()),
                          y = list(perDist.values()),
                          marker=dict(color='rgb(0,31,95)'),
                          mode = 'lines',
                          name = 'lines')]
    except Exception as e:
        print("Percentile distribution graph could not be generated")
        print(e)
    try:
        layout = go.Layout(
            xaxis=dict(
                linewidth = 2,
                title=("Percentiles").lower(),
                titlefont=dict(
                    family='opensans',
                    size=16,
                    color='black'
                )
            ),
            yaxis=dict(
                linewidth = 2,
                title=('Distribution').lower(),
                titlefont=dict(
                    family='opensans',
                    size=16,
                    color='black'
                    )
                )
        )
    except Exception as e:
        print("Layout for a percentile distribution graph could not be generated")
        print(e)
    return plot(go.FigureWidget(data=data,layout=layout),config=config)


# #### Function to give a brief description about the dataset


################################################################################################################################
## FUNCTION: Creates table object with information about the dataset
## INPUT: df - Dataframe 
## OUTPUT: Plots a table with number of variables and observations
## REQUIRED: plotly.graph_objs as go
################################################################################################################################

def datasetInfo(df):
    
    """ a table is generated which shows the number of variables and observations of the dataset
    Parameters
    ----------
    data : csv file (or file path) passed as string
           or pandas dataframe
    Examples
    --------
    datasetInfo(df="csv_filename.csv")
           
    """
    
    try:
        config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}
        trace = go.Table(
        header=dict(values=['<b>Dataset information<b>', '<b>Values<b>'],
                    line = dict(color='#7D7F80'),
                    fill = dict(color='#3E403E'),
                    font = dict(family='opensans',color = 'white', size = 12),
                    align = ['left'] * 5),
        cells=dict(values=[['No of variables','No of observations'],
                           [df.shape[1],df.shape[0]]],
                   line = dict(color='rgb(62,64,62)'),
                   fill = dict(color='#8A8A8A'),
                   font = dict(family='opensans',color = 'white', size = 12),
                   align = ['left'] * 5))
    except Exception as e:
        print("Could not create trace for datasetinfo")
        print(e)
    try:
        layout = dict(width=500, height=260,title ='Dataset Information',titlefont=dict(
                    family='opensans',
                    size=20,
                    color='black'
                    ))
    except Exception as e:
        print("Could not create layout for datasetinfo")
        print(e)
    try:
        data = [trace]
        fig = dict(data=data, layout=layout)
    except Exception as e:
        print("Error while creating figure for datasetinfo")
        print(e)
    return plot(fig,config=config)


def descriptiveStatisticsTable(df, columnName):
    # Calculating Descriptive Statistics (mean,median,mode etc)
    try:
        descriptiveStatistics=Stats.calculateDescriptiveStatistics(df,'tenure')
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
    return statsTable



def percentileDistributionTable(df,columnName,percentileIncrement=-1):
    # Calculating Percentile Distribution
    try:
        perDist = PercentileDistribution.percentileDistribution(df,columnName,percentileIncrement)
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
    return percentDist

