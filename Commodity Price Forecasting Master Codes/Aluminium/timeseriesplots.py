# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
import plotly.graph_objs as go
import statsmodels.api as sm
from statsmodels.tsa.stattools import acf, pacf
import pandas as pd
# from plotly.offline import plot, iplot, init_notebook_mode
# init_notebook_mode(connected=True)


@pd.api.extensions.register_dataframe_accessor("timeseriesplots")
class timeseriesplots:
    
    def __init__(self, pandas_obj):
        self._obj = pandas_obj
    
###############################################################################
##FUNCTION: Gives a popup dialog box to select csv file
##INPUT: None
##OUTPUT: Dataframe selected by user
###############################################################################
    def plotAcfPacf(self,dep_var,n_lags=1):
        """
        This function calculate autocorrelation and partial autocorrelation 
        for the given time series and plot them against number of lags. These 
        graphs can be used to get values for autoregression(p) by partial 
        autocorrelation plot and moving average(q) by autocorrelation plot. 
        By counting the number of lags upto which correlation value is greater 
        than standard error range. The differencing (diff) can help in 
        understanding heteroscedasticity(d)

        Parameters
        ----------
        dep_var : 'str', required
            column name of the series.
        n_lags : 'int', optional (Default=1)
            maximum number of desired lags.

        Usage
        ------
        >>> acfPacf = df.timeseriesplots.plotAcfPacf('value', n_lags = 40)
        >>> df['valueOneLevel'] = df['value'].diff()
        >>> acfPacfdiff = df.timeseriesplots.plotAcfPacf('valueOneLevel', n_lags = 40)
        >>>

        """
        df = self._obj
        acf_values = acf(df[dep_var], n_lags=n_lags)
        trace_acf = go.Bar(x=np.array(range(n_lags+1)), y=acf_values)
        data = [trace_acf]
        layout = {
            'title': "ACF Plot"
        }
        acf_fig = {'data': data, 'layout': layout}

        pacf_values = pacf(df[dep_var], n_lags=n_lags)
        trace_pacf = go.Bar(x=np.array(range(n_lags+1)), y=pacf_values)
        data = [trace_pacf]
        layout = {
            'title': "PACF Plot"
        }
        pacf_fig = {'data': data, 'layout': layout}

        return acf_fig, pacf_fig

        """
        Return
        ------
        Returns the ACF and PACF plots of the dependent variable
        """
            
    
    
    def stl(self, dep_var, model='additive', filt=None, freq=None, two_sided=True, extrapolate_trend=0):
        """This function decompose and plot series into trend, seasonality and
        irregularity.

        Parameters:
        -----------
        dep_var : 'str', required
            The column name of the dependent variable
        model : 'str' {"additive", "multiplicative"}, optional (Default=additive)
            Type of seasonal component. Abbreviations are accepted.
        filt : 'array', optional (Default=None)
            The filter coefficients for filtering out the seasonal component.
            The concrete moving average method used in filtering is determined by two_sided.
        freq : 'int', optional (Default=None)
            Frequency of the series. Must be used if x is not a pandas object.
            Overrides default periodicity of x if x is a pandas
            object with a timeseries index.
        two_sided : 'bool', optional (Default=True)
            The moving average method used in filtering.
            If True (default), a centered moving average is computed using the filt.
            If False, the filter coefficients are for past values only.
        extrapolate_trend : 'int' or 'freq', optional (Default=0)
            If set to > 0, the trend resulting from the convolution is
            linear least-squares extrapolated on both ends (or the single one
            if two_sided is False) considering this many (+1) closest points.
            If set to 'freq', use `freq` closest points. Setting this parameter
            results in no NaN values in trend or resid components.

        Usage
        ------
        >>> trend,seasonality,irregularity = df.timeseriesplots.stl(['value'],model='additive',filt=None,freq=None,two_sided=True, extrapolate_trend=0)
        >>> 
        """
        df = self._obj
        # decomposing data into trend, sesonal and irregular component
        stlDecompose = sm.tsa.seasonal_decompose(df[dep_var], model=model, filt=filt, freq=freq, two_sided=two_sided, extrapolate_trend=extrapolate_trend)
        return stlDecompose.trend, stlDecompose.seasonal, stlDecompose.resid

        """
        Return
        ------
        Returns the decomposition series for the trend, seasonality and residue component
        """

    
    
    
    def stlFigure(self, trend, seasonal, resid):
        """
        This function plot trend, seasonality and irregularity

        Parameters:
        -----------
        trend : pandas series, required
            trend series.
        seasonal : pandas series, required
            seasonal series.
        resid : pandas series, required
            Residual/ noise/ Irregularity series.
        
        Usage
        ------
        >>> trendFigure, seasonalityFigure, irregularityFigure = df.timeseriesplots.stlFigure(trend,seasonality,irregularity)
        >>> 
        
        """
        # setting diffrent component of data into figure
        data = [go.Scatter(x=trend.index,y=trend)]
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
        data = [go.Scatter(x=seasonal.index,y=seasonal)]
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
        data = [go.Scatter(x=resid.index,y=resid)]
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
        return trendPlot,seasonalPlot,irregularPlot

        """
        Return
        ------
        Returns the decomposed visualization figures for the trend, seasonal and residual component
        """

    # def plot(self, figure, filename = False, isNotebook = False):
    #     """
    #     This function will plot, plotly figures
    #     Paramenter:
    #     -----------
    #     self      : A pandas DataFrame
    #     figure    : Plotly figure object (figure_objs)
    #     filename  : str object need to be provided to save graphs in current directory
    #     isNotebook: boolean, optional. plot graph inside notebook if true
    #     """
    #     config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}
    #     if isNotebook:
    #         if filename!=False:
    #             iplot(figure, filename=filename, config=config)
    #         else:
    #             iplot(figure, config=config)
    #     else:
    #         if filename!=False:
    #             plot(figure, filename=filename, config=config)
    #         else:
    #             plot(figure, config=config)
