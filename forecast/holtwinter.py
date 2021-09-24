# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
from statsmodels.tsa.holtwinters import ExponentialSmoothing

@pd.api.extensions.register_dataframe_accessor("holtwinter")
class holtwinter(object):
    """Holt's winter is a statistical that uses triple exponential smoothing.
    This model is good at handling parabolic trends.
    """
    def __init__(self, pandas_obj):
        self._obj = pandas_obj
    
    def build(self,dep_var,trend=None,damped=False,seasonal=None,seasonal_periods=None,dates=None,freq=None,missing='none',file_name=False, use_boxcox=True):
        """
        Build and fit and holt's winter model on given time series.

        Parameters
        ----------
        dep_var : 'str', required
            The column name of the dependent variable.
        trend : 'str', optional (Default=None)
            {"add", "mul", "additive", "multiplicative", None}
            Type of trend component.
        damped : 'bool', optional (Default=False)
            Should the trend component be damped.
        seasonal : 'str',optional (Default=None)
        {"add", "mul", "additive", "multiplicative", None}, optional
            Type of seasonal component.
        seasonal_periods : int, optional (Default=None)
            The number of seasons to consider for the holt winters.
        freq : str, optional (Default=None)
        The frequency of the time-series. A Pandas offset or 'B', 'D', 'W',
        'M', 'A', or 'Q'. This is optional if dates are given.
        'B' - business day, ie., Mon. - Fri.
        'D' - daily
        'W' - weekly
        'M' - monthly
        'A' - annual
        'Q' - quarterly
        use_boxcox : boolean, optional (Default=True)
            Weather to use boxcox while fitting model.
        file_name: 'str', optional (Default=True)
        Name of the .pkl file (Default=False)

        Notes
        -----
        This is a full implementation of the holtwinter exponential smoothing.
        This includes all the unstable methods as well as the stable methods.
        The implementation of the library covers the functionality of the R 
        library as much as possible whilst still being pythonic.
        
        Usage
        -----
        >>> holtWinterModel = dfTrain.holtwinter.build('value', trend='add', seasonal='add',seasonal_periods=4)
        >>> 
        """
        df = self._obj
        import warnings
        warnings.simplefilter("ignore")
        model =ExponentialSmoothing(df[dep_var], trend=trend, damped=damped, seasonal=seasonal, seasonal_periods=seasonal_periods, dates=dates, freq=freq, missing=missing)
        model = model.fit(use_boxcox=use_boxcox)
        if not file_name:
            file_name = 'buildModelHoltWinter.pkl'
        model.save(file_name)
        return model

        """
        Returns
        -------
        Model fit
        """
    
    def scoreTrain(self,model,dep_var):
        """
        This function return the predicted values for train set and print
        MAPE for the Train set.

        Parameters
        ----------
        model : model_instance, required
            Fitted Holt-Winter model object
        dep_var : 'str', required
            The column name of the dependent variable

        Usage
        -----
        >>> holtWinterDfTrain = dfTrain.holtwinter.scoreTrain(holtWinterModel, 'value')
        >>> 
        """
        
        df = self._obj
        df['predicted']=model.predict(start=0,end=len(df))
        return df

        """
        Return
        ------
        Modified train dataframe with the forecasted values appended as a column 'predicted'
        """
    
    def scoreTest(self,model,dep_var):
        """
        This function return the forecasted values for test set and print
        MAPE for the test set.

        Parameters
        ----------
        model : model_instance, required
            Fitted Holt-Winter model object
        dep_var : 'str', required
            The column name of the dependent variable

        Usage
        -----
        >>> holtWinterDfTest = dfTest.holtwinter.scoreTest(holtWinterModel, 'value')
        >>> 
        """
        df = self._obj
        df['predicted']=np.array(model.forecast(len(df)))
        return df

        """
        Return
        ------
        Modified test dataframe with the forecasted values appended as a column 'predicted'
        """