# -*- coding: utf-8 -*-
import pandas as pd
import numpy as np
from statsmodels.tsa.arima_model import ARIMA

@pd.api.extensions.register_dataframe_accessor("arima")
class arima(object):
    """ARIMA stands for AR - Auto reggression (p), I - Integration(d) 
    and MA - Moving Average(q). The model take (p,d,q) as input to built model
    """
    def __init__(self, pandas_obj):
        self._obj = pandas_obj

    def build(self, dep_var, order=(0,0,0), exog=None, dates=None, freq=None, missing='none', file_name=False):
        """
        Build and fit arima model on the given time series.

        Parameters
        ---------

        dep_var : 'str', required
            The column name of the dependent variable
        exog : 'arr', optional (default=None)
            Array of exogenous regressors, shaped nobs x k.
            Exogenous variables.
            Series or dataframe of independent variables :math `x`
        order : 'arr' (default=(0,0,0)), iterable
            The (p,d,q) order of the model for the number of AR parameters,
            differences, and MA parameters to use.
        dates : array-like of datetime, optional (default=None)
            An array-like object of datetime objects. If a pandas object is 
            given for endog or exog, it is assumed to have a DateIndex.
        freq : 'str', optional (default=None)
            The frequency of the time-series. A Pandas offset or 'B', 'D', 'W',
            'M', 'A', or 'Q'. This is optional if dates are given.
            'B' - business day, ie., Mon. - Fri.
            'D' - daily
            'W' - weekly
            'M' - monthly
            'A' - annual
            'Q' - quarterly
        file_name : 'str' (default=None)
            Name of the pkl file
        """
        df = self._obj
        import warnings
        warnings.simplefilter("ignore")
        model = ARIMA(df[dep_var], order=order, exog=exog, dates=dates, freq=freq, missing=missing)
        model = model.fit(disp=False)
        if not file_name:
            file_name = 'buildModelArima.pkl'
        model.save(file_name)
        return(model)

        """
         Usage
        -----
        >>> arima_model = dfTrain.arima.build('value', order = (6,0,6))
        >>>

        Returns
        -------
        Model fit
        """

    def getSummary(self,model):
        """
        This function summarizes the model
        
        Parameters
        ---------
        model : model_instance, required
            Fitted ARIMA model object
        
        Usage
        -----
        >>> arimaSummary = dfTrain.arima.getSummary(arima_model)
        >>>
        """
        return model.summary()

        """
        Returns
        -------
        Dataframe comprising of the model summary
        """
    
    def scoreTrain(self,model,dep_var):
        """This function returns the predicted values for train set.

        Parameters
        ----------
        model : model_instance, required
            Fitted ARIMA model object
        dep_var : 'str', required
            The column name of the dependent variable

        Usage
        -----
        >>> arimaDfTrain = dfTrain.arima.scoreTrain(arima_model, 'value')
        >>>
        """
        
        df=self._obj
        # Calculating predicted values using predict function
        df['predicted'] =df[dep_var] - model.resid
        return df

        """
        Return
        ------
        Modified train dataframe with the forecasted values appended as a column 'predicted'
        """
    
    def scoreTest(self,model,dep_var):
        """
        This function return the forecasted values for test set.

        Parameters
        ----------

        model : model_instance, required
            Fitted ARIMA model object
        dep_var : 'str', required
            The column name of the dependent variable
        
        Usage
        -----
        >>> arimaDfTest = dfTest.arima.scoreTest(arima_model, 'value') 
        >>>
        """
        df=self._obj
        df['predicted']=model.forecast(len(df))[0]
        return df

        """
        Return
        ------
        Modified test dataframe with the forecasted values appended as a column 'predicted'
        """
