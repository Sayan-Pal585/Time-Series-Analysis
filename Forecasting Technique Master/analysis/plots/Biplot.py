
# coding: utf-8

# #### Importing the necessary packages


"""biplot - a set of commonly used bivariate plots designed for data analysis
=====================================================================

**biplot** is a Python package providing fast, convinient, and expressive plots
that are commonly used for bivariate analysis designed to work with pandas dataframe.

Main Features
-------------
List of functions available in biplot:

  - createGraphDependent   : Create plot showing event rate in each category
                              or decile of independent.
                              
  - createGraphXYContinuous: Create plot showing percentage of Y-axis in each 
                             decile of X-axis.
                             
  - createGraphYContinuous : Create plot showing percentage of Y-axis in each 
                             category of X-axis.
                             
  - createGraphBubble      : Create bubble plot.
  
  - createStackedBarGraph  : Create plot between two categorical variables
                             represented as stacked bar with number of occurance
                             of each category from X-axis column with each category
                             of Y-axis column.
"""

# importing graph object library from plotly
try:
    import plotly.graph_objs as go
except ImportError:
    print("Go module not found")
try:
    from bubbly.bubbly import bubbleplot
except ImportError:
    print("Bubbly module not found")
try:
    import pandas as pd
    import numpy as np
except ImportError:
    print("Pandas module not found")
try:
    from plotly.offline import download_plotlyjs as pltjs, plot
except ImportError:
    print("Plotly offline not found")



numerics = ['int16', 'int32', 'int64', 'float16', 'float32', 'float64','float','int']


# Creating graph configuration to mantain data privacy
config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}

# #### Function to select the appropriate graph with choosen X and Y variables


################################################################################################################################
## FUNCTION: Create Bar-Line graph object
## INPUT: df - dataframe from which column will be selected, columnName for x and y axis
## OUTPUT: Bar-Line Graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createBarLine(df,colX,colY,dependent=None,event=None):
    if (colY==dependent or colX==dependent) and event!=None:
        try:
            return createGraphDependent(df,colX,colY,dependent,event)
        except Exception as e:
            print("Unable to create graph with the given dependent and event")
            print(e)
    elif len(df[colX].unique()) > 20 and df[colX].dtype in numerics and len(df[colY].unique()) > 20 and df[colY].dtype in numerics:
        try:
            return createGraphXYContinuous(df,colX,colY)
        except Exception as e:
            print("Error occur while trying to plot two continuous variables")
            print(e)
    elif len(df[colY].unique()) > 20 and df[colY].dtype in numerics:
        try:
            return createGraphYContinuous(df,colX,colY)
        except Exception as e:
            print("Error occur while trying to plot X axis as categorical")
            print(e)

    elif len(df[colX].unique()) > 20 and df[colX].dtype in numerics:
        try:
            return createGraphXContinuous(df,colX,colY)
        except Exception as e:
            print("Error occur while trying to plot Y axis as categorical")
            print(e)

    else:
        try:
            return createStackedBarGraph(df,colX,colY)
        except Exception as e:
            print("Error occur while trying to plot both axis as categorical")
            print(e)


# #### Function to generate bar line graph if dependent variable and event rate is specified


################################################################################################################################
## FUNCTION: Create Bar-Line graph object
## INPUT: df - dataframe from which column will be selected, columnName for x and y axis
## OUTPUT: Bar-Line Graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createGraphDependent(df,colX,colY,event,dependent = None):
    
    
    """A bar-line plotly graph with independent variable on X-Axis
    and percentage of event of dependent variable in each category/decile.
    
    Parameters
    ----------
    df        : pandas dataframe containing columns colX and colY
    
    colX      : Name of column in dataframe df passed as string.
    
    colY      : Column name of dependent variable passed as string.
    
    dependent : Column name of categorical variable
                passed as string. Can be used only
                when event is specified.
                
    event     : Category in the dependent column passed
                as string.
                
    Examples
    --------
    
    createGraphDependent(df,"exColumnName1","exColumnName2","exColumnName2","categoryInexColumn2")
    
    or
    
    createGraphDependent(df,"exColumnName1","exColumnName2","exColumnName1","categoryInexColumn1")
    
    """
    
    data=[]
    if dependent==None:
        dependent=colY
    # Layout
    try:
        layout = go.Layout(
            title=("Percentage of "+colY+" in "+colX).title(),
            xaxis = dict(zeroline=False,
                showline=True
                    ),
            yaxis=dict(
                title=("Population in "+colX).lower(),
                zeroline=False,
                showline=True
            ),
            yaxis2=dict(
                title= ("% of event").lower(),
                titlefont=dict(
                    color='rgb(0, 0, 0)'
                ),
                tickfont=dict(
                    color='rgb(0, 0, 0)'
                ),
                overlaying='y',
                side='right',
                zeroline=False,
                showline=True
            )
        )
    except Exception as e:
        print("Error occured while creating layout if dependent and event is specified")
        print(e)
    if event!=None and (df[colX].dtype in numerics or df[colY].dtype in numerics) and event in df[dependent].unique():
        try:
            if colX==dependent:
                colX=colY
                colY=dependent
            try:
                bar = df.groupby(pd.qcut(df[colX],10,duplicates='drop',precision=0))[colX].count().reset_index(name='count')
            except Exception as e:
                print("Error occcured while grouping colX ")
                print(e)
            mxbar = bar[colX].astype(str)
            print(bar[colX])
            print(bar[colX].dtype)
            mybar = bar['count']
            try:
                line=df.groupby(pd.qcut(df[colX],10,duplicates='drop',precision=0))[colY].value_counts().reset_index(name='count')
            except Exception as e:
                print("Error occured while grouping value counts of colY by colX")
                print(e)
            try:
                myline = line.groupby([colX])['count'].sum().reset_index()
            except Exception as e:
                print("Error occured while calculating sum of value counts of colY")
                print(e)
            try:
                line=pd.merge(line,myline,how='left',on=colX)
            except Exception as e:
                print("Error occured while merging lists")
                print(e)
            try:
                line['percent']=line['count_x']/line['count_y']*100
            except Exception as e:
                print("Error occured while calculating percentage of value counts of colY in each decile of colX")
                print(e)
            line['percent'][line[colY]!=event]=0
            try:
                myline = line.groupby([colX])['percent'].sum().reset_index()
            except Exception as e:
                print("Error occued while grouping by colX with the event rate")
                print(e)
        # Creating bar and line graphs
            try:
                data = [go.Bar(x=mxbar,y=mybar, name=(colX).lower(), marker=dict(color='rgb(62,64,62)')),go.Scatter(x=mxbar,y =myline['percent'], marker=dict(color='rgb(237,112,56)'),name =(colY).lower(), text=[round(i,2) for i in myline['percent']], textposition='bottom center',textfont=dict(family='opensans',color='rgb(245,168,65)'),mode='lines+text',yaxis='y2')]
            except Exception as e:
                print("Error oocured while creating bar and line graph")
                print(e)
            except Exception as e:
                print("Error occured while creating layout for bar and line graph")
                print(e)
        except Exception as e:
            print("Unable to show barline graph with the selected variable")
            print(e)
        return plot(go.FigureWidget(data=data, layout=layout), config=config)
    elif event!=None and event in df[dependent].unique():
        try:
            if colX==dependent:
                colX=colY
                colY=dependent
            try:
                bar = pd.crosstab(df[colX],df[colY]).reset_index()
            except Exception as e:
                print("Error while creating crosstab for colX and colY selected")
                print(e)
            try:
                bar['sum'] = bar.sum(axis=1)
            except Exception as e:
                print("Error occured while calculating sum of the categories")
                print(e)
            try:
                bar['percent']=(bar[event]/bar['sum'])*100
            except Exception as e:
                print("Error while calculating percentage of event")
                print(e)
            try:
                data = [go.Bar(x=bar[colX],y=bar['sum'], name=(colX).lower(), marker=dict(color='rgb(62,64,62)')),go.Scatter(x=bar[colX],y =bar['percent'], name=(colY).lower(), marker=dict(color='rgb(237,112,56)'), text=[round(i,2) for i in bar['percent']], textposition='bottom center',textfont=dict(family='opensans',color='rgb(245,168,65)'),mode='lines+text',yaxis='y2')]
            except Exception as e:
                print("Error while creating bar line graph with a categorical column chosen")
                print(e)
            return plot(go.FigureWidget(data=data, layout=layout), config=config)
        except Exception as e:
            print("Unable to show barline graph with the selected variable")
            print(e)
        except Exception as e:
            print("Error while creating layout for bar line graph")
            print(e)
        
        return plot(go.FigureWidget(data=data, layout=layout), config = config)
    
    else:
        print(" Provided event is not in dependent variable")
        return plot(go.FigureWidget(data=data, layout=layout), config=config)


            
            
            
        


# #### Function to generate bar line graph if X and Y are both continuous


################################################################################################################################
## FUNCTION: Create Bar-Line graph object
## INPUT: df - dataframe from which column will be selected, columnName for x and y axis
## OUTPUT: Bar-Line Graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################



def createGraphXYContinuous(df,colX,colY):
    
    """A bar-line plotly graph with continuous variable on X-Axis
    and Y-Axis.
    
    Parameters
    ----------
    df        : pandas dataframe containing columns colX and colY
    
    colX      : Column Name of continuous variable in dataframe.
    
    colY      : Column Name of another continuous variable in dataframe.
    
    Examples
    --------
    
    createGraphXYContinuous(dfname,"continuousVariable1","continuousVariable2")
    
    """
    
    
    data=[]
    try:
        # creating deciles from column x and getting count in each decile
        try:
            bar = df.groupby(pd.qcut(df[colX],10,duplicates='drop',precision=0))[colX].count().reset_index(name='count')
        except Exception as e:
            print("Error occured while grouping colX")
            print(e)
        mxbar = bar[colX].astype(str)
        mybar = bar['count']

        # calculating percentage of column y in decile of column x
        try:
            line = df.groupby(pd.qcut(df[colX],10,duplicates='drop',precision=0))[colY].sum().reset_index(name = 'sum')
        except Exception as e:
            print("Error occured while grouping colX with colY")
            print(e)
        try:
            line['percent'] = (line['sum']/line['sum'].sum())*100
        except Exception as e:
            print("Error occured while calculating percentage of colY in colX")
            print(e)

        # Creating bar and line graphs
        try:
            data = [go.Bar(x=mxbar,y=mybar, name=colX, marker=dict(color='rgb(62,64,62)')),go.Scatter(x=mxbar,y =line['percent'], name=("percent of "+colY).lower(), marker=dict(color='rgb(237,112,56)'), text=[round(i,2) for i in line['percent']],     textposition='bottom center',textfont=dict(family='opensans',color='rgb(245,168,65)'),mode='lines+text',yaxis='y2')]
        except Exception as e:
            print("Error occured while plotting bar-line graph if X and Y choosen is continuous")
            print(e)
    except Exception as e:
        print("Bar line cannot be plotted with selected columns")
        print(e)

    # Creating layout for the graph
    try:
        layout = go.Layout(
            title=("Percentage of "+colY+" in "+colX).title(),
            xaxis = dict(zeroline=False,
                    showline=True
                    ),
            yaxis=dict(
                title=("Population in "+colX).lower(),
                zeroline=False,
                showline=True
            ),
            yaxis2=dict(
                title= ("% of "+colY).lower(),
                titlefont=dict(
                    color='rgb(0,0,0)'
                ),
                tickfont=dict(
                    color='rgb(0,0,0)'
                ),
                overlaying='y',
                side='right',
                zeroline=False,
                showline=True
            )
        )
    except Exception as e:
        print("Error occured while creating layout if X and Y columns choosen are continuous")
        print(e)
    return plot(go.FigureWidget(data=data, layout=layout), config=config)


# #### Function to generate bar line graph if Y is continuous


################################################################################################################################
## FUNCTION: Create Bar-Line graph object
## INPUT: df - dataframe from which column will be selected, columnName for x and y axis
## OUTPUT: Bar-Line Graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createGraphYContinuous(df,colX,colY):
    
    """A bar-line plotly graph with continuous variable on Y-Axis
    and categorical variable on X-Axis.
    
    Parameters
    ----------
    df        : pandas dataframe containing columns colX and colY
    
    colX      : Column Name of categorical variable in dataframe.
    
    colY      : Column Name of continuous variable in dataframe.
    
    Examples
    --------
    
    createGraphYContinuous(dfname,"categoricalVariable","continuousVariable")
    
    """
    
    
    
    data=[]
    
    try:
        try:
            bar = df.groupby([colX])[colX].count().reset_index(name='count')
        except Exception as e:
            print("Error occured while grouping colX")
            print(e)
        mxbar=bar[colX]
        mybar=bar['count']

        # calculating percentage of column x in each category of y
        try:
            line = df.groupby([colX])[colY].sum().reset_index(name='sum')
        except Exception as e:
            print("Error occured while grouping colX with colY")
            print(e)
        try:
            line['percent'] = line['sum']/line['sum'].sum()*100
        except Exception as e:
            print("Error occured while calculating percentage of colY in colX")
            print(e)

        # Creating bar and line graphs
        try:
            data = [go.Bar(x=mxbar,y=mybar,name=(colX).lower(), marker=dict(color='rgb(62,64,62)')),go.Scatter(x=mxbar,y =line['percent'],name=("percent of "+colY).lower(), marker=dict(color='rgb(237,112,56)'), text=[round(i,2) for i in line['percent']], textposition='bottom center',textfont=dict(family='opensans',color='rgb(245,168,65)'),mode='lines+text',yaxis='y2')]
        except Exception as e:
            print("Error occured while plotting bar-line graph Y is continuous")
            print(e)
    except Exception as e:
        print("Bar line cannot be plotted with selected columns")
        print(e)


    # Creating layout for the graph
    try:
        layout = go.Layout(
            title=("Percentage of "+colY+" in "+colX).title(),
            xaxis = dict(zeroline=False,
                showline=True
                    ),
            yaxis=dict(
                title=("Population in "+colX).lower(),
                showline=True,
                zeroline = False
            ),
            yaxis2=dict(
                title= ("% of "+colY).lower(),
                titlefont=dict(
                    color='rgb(0, 0, 0)'
                ),
                tickfont=dict(
                    color='rgb(0, 0, 0)'
                ),
                overlaying='y',
                side='right',
                zeroline=False,
                showline=True
            )
        )
    except Exception as e:
        print("Error occured while creating layout if Y column is continuous")
        print(e)
    return plot(go.FigureWidget(data=data, layout=layout), config=config)


# #### Function to generate bar line graph if X is continuous


################################################################################################################################
## FUNCTION: Create Bar-Line graph object
## INPUT: df - dataframe from which column will be selected, columnName for x and y axis
## OUTPUT: Bar-Line Graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createGraphXContinuous(df,colX,colY):
    
    """A bar-line plotly graph with continuous variable on X-Axis
    and categorical variable on Y-Axis.
    
    Parameters
    ----------
    df        : pandas dataframe containing columns colX and colY
    
    colX      : Column Name of continuous variable in dataframe.
    
    colY      : Column Name of categorical variable in dataframe.
    
    Examples
    --------
    
    createGraphXContinuous(dfname,"continuousVariable","categoricalVariable")
    
    """
    
    temp=colY
    colY=colX
    colX=temp
    try:
        bar = df.groupby([colX])[colX].count().reset_index(name='count')
    except Exception as e:
        print("Error occured while grouping colX")
        print(e)
    mxbar=bar[colX]
    mybar=bar['count']
    data=[]
    try:
        # calculating percentage of column x in each category of y
        try:
            line = df.groupby([colX])[colY].sum().reset_index(name='sum')
        except Exception as e:
            print("Error occured while grouping colX with colY")
            print(e)
        try:
            line['percent'] = line['sum']/line['sum'].sum()*100
        except Exception as e:
            print("Error occured while calculating percentage of colY in colX")
            print(e)

        # Creating bar and line graphs
        try:
            data = [go.Bar(x=mxbar,y=mybar, name=(colX).lower(), marker=dict(color='rgb(62,64,62)')), go.Scatter(x=mxbar,y =line['percent'],name =("Pecent of "+colY).lower(), marker=dict(color='rgb(237,112,56)'), text=[round(i,2) for i in line['percent']], textposition='bottom center',textfont=dict(family='opensans',color='rgb(245,168,65)'),mode='lines+text',yaxis='y2')]
        except Exception as e:
            print("Error occured while plotting bar-line graph if X is continuous")
            print(e)
    except Exception as e:
        print("Bar Line cannot be plotted with selected columns")
        print(e)


    # Creating layout for the graph
    try:
        layout = go.Layout(
            title=("Percentage of "+colY+" in "+colX).title(),
            xaxis = dict(zeroline=False,
                showline=True
                    ),
            yaxis=dict(
                title=("Population in "+colX).lower(),
                zeroline = False,
                showline = True
                
            ),
            yaxis2=dict(
                title= "% of "+colY,
                titlefont=dict(
                    color='rgb(0,0,0)'
                ),
                tickfont=dict(
                    color='rgb(0,0,0)'
                ),
                overlaying='y',
                side='right',
                zeroline=False,
                showline=True
            )
        )
    except Exception as e:
        print("Error occured while creating layout if X column is continuous")
        print(e)
    return plot(go.FigureWidget(data=data, layout=layout), config=config)


# #### Function to generate bubble graph


##########################################################################################################################################
## FUNCTION: Create Bubble graph object
## INPUT: df - dataframe from which column will be selected, columnName for x,y,bubbleSize (Continuous) and bubbleColor (Categorical) axis
## OUTPUT: Bubble Graph object
## REQUIRED: plotly.graph_objs as go
##########################################################################################################################################


def createGraphScatter(df2,colX,colY,scatterColor=None):
    
    """A bubble graph from plotly with continuous or categorical variable
    column for X-Axis and Y-Axis. A Categorical variables for bubble color
    and bubble group. A continuous column for bubble size.
    
    Parameters
    ----------
    df          : pandas dataframe containing columns colX and colY
    
    colX        : Column Name of another continuous variable in dataframe.
    
    colY        : Column Name of categorical variable in dataframe.
    
    bubbleGroup : Column Name of categorical variable; used to group the
                  data before plotting.
                  
    bubbleColor : Column Name of categorical variable; used to set color of
                  the bubble before plotting.
                  
    bubbleSize  : Column Name of a continuous variable; used to set the size
                  of bubble for each variable
                  
    timeColumn  : Column Name of date or time column. The datatype of this 
                  column has to be int type.
                  
    Examples
    --------
    
    createGraphBubble(dfname,"continuous/categoricalVariable1","continuous/categoricalVariable2", "categoricalVariable3", "continuousVariable4", "categoricalVariable5", "timeColumn6")
    
    """
    bubbleSize = None
    bubbleColor = None
    bubbleColor = scatterColor
    
    df = df2.copy()
    df.dropna(inplace=True)
    
    if bubbleSize !=None and bubbleColor !=None:
        title = colX+' - '+colY+' - '+bubbleSize+" - "+bubbleColor
    elif bubbleColor !=None:
        title = colX+' - '+colY+" - "+bubbleColor
    else:
        title = colX+' - '+colY
        
        
    layout = go.Layout(
        title = title.title(),
        xaxis=dict(
            title=(colX).lower(),
            showline=True,
            zeroline = False
        ),
        yaxis=dict(
            title=(colY).lower(),
            showline=True,
            zeroline = False
        )
    )
    
    


#    bubbleScale = 0.5
#    if bubbleSize is not None and min(df[bubbleSize])<0:
#
#        k=0-min(df[bubbleSize])
#        df[bubbleSize]+=k
#        bubbleScale=4.*max(df[bubbleSize])/(40.**2)	
#
#
#
#
#        figure = bubbleplot(dataset=df,x_column=colX,y_column=colY,bubble_column=bubbleGroup, size_column=bubbleSize,
#					color_column=bubbleColor,time_column=timeColumn, x_title=colX, y_title=colY,
#					title=colX+" vs "+colY, x_logscale=True, scale_bubble=bubbleScale)
#
#        return plot(figure, config=config)
#    
#    
#    
#    
#    
#    
#    # for x or y are categorical columns
#    else:
#        if bubbleSize is not None and min(df[bubbleSize])<0:
#
#            k=0-min(df[bubbleSize])
#            df[bubbleSize]+=k
#
#
#        trace=[]
#        data = []

#        if bubbleSize is not None:
#            try:
#                if bubbleColor is not None:
#                    #df=df.groupby([bubbleColor])[df.columns].mean().reset_index()
#                    for i in df[bubbleColor]:
#                        if bubbleSize is not None:
#                            trace.append(go.Scatter(
#                                x=df[colX][df[bubbleColor] == i],
#                                y=df[colY][df[bubbleColor] == i],
#                                mode='markers',
#                                name=str(i),
#                                marker=dict(
#                                    symbol='circle',
#                                    sizemode='area',
#                                    sizeref=2.*max(df[bubbleSize][df[bubbleColor] == i])/(40.**2),
#                                    sizemin=4,
#                                    size=(df[bubbleSize]/df[bubbleSize])*100,
#                                    line=dict(
#                                        width=2
#                                    ),
#                                )
#                            ))
#                    data=trace
##                    for i in range(len(df[bubbleColor].unique())):
##                        data.append(trace[i])
#                if bubbleColor is None:
#                    data = [go.Scatter(
#                        x=df[colX],
#                        y=df[colY],
#                        mode='markers',
#                        marker=dict(
#                            color='rgb(0,31,95)',
#                            size=df[bubbleSize],
#                            sizeref=2.*max(df[bubbleSize])/(40.**2),
#                            sizemin=4,
#                            sizemode='area'
#
#                        )
#                    )]
#            except ValueError:
#                print(" Cannot plot bubble plot as size column contains nan values")
#        else:
    trace = []
    try:
        if bubbleColor is not None:
            for i in df[bubbleColor].unique():
                trace.append(go.Scatter(
                    x=df[colX][df[bubbleColor] == i],
                    y=df[colY][df[bubbleColor] == i],
                    mode='markers',
                    name=str(i).lower(),
                    marker=dict(
                        symbol='circle',
                    )
                ))
            data = trace
#                    for i in range(len(df[bubbleColor].unique())):
#                        data.append(trace[i])
        else:
            data = [go.Scatter(
                x=df[colX],
                y=df[colY],
                mode='markers',
                marker=dict(color='rgb(0,31,95)')
            )]
    except:
        print("Scatter plot cannot be plotted with selected variables")
        
    return plot(go.FigureWidget(data = data, layout=layout), config=config)
            


# #### Function to generate stacked bar graph for categorical variables



################################################################################################################################
## FUNCTION: Create Stacked Bar graph object
## INPUT: df - dataframe from which column will be selected, categorical columnName for x and y axis
## OUTPUT: Stacked Bar Graph object
## REQUIRED: plotly.graph_objs as go
################################################################################################################################


def createStackedBarGraph(df,colX,colY):
    
    """A stacked-bar plotly graph with categorical variable on both
    X and Y axis.
    
    Parameters
    ----------
    df        : pandas dataframe containing columns colX and colY
    
    colX      : Column Name of categorical variable in dataframe.
    
    colY      : Column Name of categorical variable in dataframe.
    
    Examples
    --------
    
    createStackedBarGraph(dfname,"categoricalVariable1","categoricalVariable2")
    
    """
    
    try:
        layout = go.Layout(
            title = (colY+' - '+colX).title(),
            xaxis=dict(
                title=(colX).lower(),
                showline=True
            ),
            yaxis=dict(
                title=(colY).lower(),
                showline=True
            ),
            barmode='stack'
        )
    except Exception as e:
        print("Layout could not be generated for a stack bar graph")
        print(e)
    
    if colX!=colY:
        
        try:
            try:
                mybar=df.groupby([colX,colY])[colY].count().reset_index(name='count')
            except Exception as e:
                print("Error generated will grouping colY with colX")
                print(e)

            trace=[]

            for i in df[colY].unique():
                try:
                    trace.append(go.Bar(
                        x=mybar[colX][mybar[colY]==i],
                        y=mybar['count'][mybar[colY]==i],
                        text=[round(i,2) for i in mybar['count'][mybar[colY]==i]], textposition='auto',textfont=dict(family='opensans',size=10),
                        name=str(i).lower()))
                except Exception as e:
                    print("Error occured while appending the stacks in bar stack graph")
                    print(e)
        except Exception as e:
            print("Unable to plot stacked bar graph with selected columns")
            print(e)
        
    else:
        try:
            try:
                mybar=df.groupby([colX])[colY].count().reset_index(name='count')
            except Exception as e:
                print("Error occured while grouping colY with colX")
                print(e)

            trace=[]

            for i in df[colY].unique():
                try:
                    trace.append(go.Bar(

                    x=mybar[colX],

                    y=mybar['count'],

                    text=[round(i,2) for i in mybar['count'][mybar[colY]==i]], textposition='auto',textfont=dict(family='opensans',size=10),

                    name=str(i).lower()))
                except Exception as e:
                    print("Error occured while appending the stacks in bar stack graph ")
                    print(e)
        except Exception as e:
            print("Unable to plot stacked bar graph with selected columns")
            print(e)
    return plot(go.FigureWidget(data=trace,layout=layout), config=config)

