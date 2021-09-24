# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:11:31 2019

@author: Akhil Saraswat
"""

def stl(df, depVar, model='additive', filt=None, freq=None, two_sided=True, extrapolate_trend=0):
    
    
    # decomposing data into trend, sesonal and irregular component
    try:
        stlDecompose = sm.tsa.seasonal_decompose(df['value'], model=model, filt=filt, freq=freq, two_sided=two_sided, extrapolate_trend=extrapolate_trend)
    except Exception as e:
        print("Unable to run Seasonal Decompose with the given input")
        print(e)
    
    # setting diffrent component of data into figure
    try:
        data = [go.Scatter(x=stlDecompose.trend.index,y=stlDecompose.trend)]
        layout = go.Layout(
                    title="Trend",
                    yaxis=dict(
                        title="Trend"
                    ),
                    xaxis=dict(
                        title="Timeline"
                    )
                )
        trendPlot = go.Figure(data=data,layout=layout)
    except Exception as e:
        print("Cannot plot trend of the given data")
        print(e)
    try:
        data = [go.Scatter(x=stlDecompose.seasonal.index,y=stlDecompose.seasonal)]
        layout = go.Layout(
                    title="Seasonality",
                    yaxis=dict(
                        title="Seasonality"
                    ),
                    xaxis=dict(
                        title="Timeline"
                    )
                )
        seasonalPlot = go.Figure(data=data,layout=layout)
    except Exception as e:
        print("Cannot plot seasonality of the given data")
        print(e)
    try:
        data = [go.Scatter(x=stlDecompose.resid.index,y=stlDecompose.resid)]
        layout = go.Layout(
                    title="Irregularity",
                    yaxis=dict(
                        title="Irregularity"
                    ),
                    xaxis=dict(
                        title="Timeline"
                    )
                )
        irregularPlot = go.Figure(data=data,layout=layout)
    except Exception as e:
        print("Cannot plot irregularity of the given data")
        print(e)
    return trendPlot,seasonalPlot,irregularPlot