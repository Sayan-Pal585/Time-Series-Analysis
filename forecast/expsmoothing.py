# -*- coding: utf-8 -*-
"""
Created on Thu Mar  7 13:44:49 2019

@author: TheMathCompany
"""

import pandas as pd
import numpy as np
from statsmodels.tsa.api import SimpleExpSmoothing
import warnings
warnings.simplefilter("ignore")

@pd.api.extensions.register_dataframe_accessor("expsmoothing")
class expsmoothing(object):

    def __init__(self, pandas_obj):
        self._obj = pandas_obj

    def build(self,dep_var, smoothing_level=None, optimized=True, filename=False):
        """
        Parameters
        ----------
        df    : pandas dataframe
        dep_var: dependent variable name
            The name of the variable for prediction
        smoothing_level : float, optional
            The smoothing_level value of the simple exponential smoothing, if the value is
            set then this value will be used as the value.
        optimized : bool
            Should the values that have not been set above be optimized automatically?
        """
        df = self._obj

        model = SimpleExpSmoothing(df[dep_var])
        model_fit = model.fit(smoothing_level=smoothing_level,optimized=optimized)
        # save the model to disk
        if not filename:
            filename = 'buildModelExpSmoothing.pkl'
        model_fit.save(filename)
        return model_fit

    
    def scoreTrain(self,model_fit,dep_var):
        """
        This function return the predicted values for train set and print
        MAPE for the Train set.
        
        pandas_obj       : A pandas DataFrame
        
        model_fit        : fitted model of exponential smoothing
        
        dep_var           : column name of the dependent variable as string
        """
        
        df = self._obj
        df['predicted']=model_fit.predict(start=0,end=len(df))
        return df
    
    
    def scoreTest(self,model_fit,dep_var):
        """
        This function return the forecasted values for test set and print
        MAPE for the test set.
        
        pandas_obj       : A pandas DataFrame
        
        model_fit        : fitted model of exponential Smoothing
        
        dep_var           : column name of the dependent variable as string
        """
        
        df = self._obj
        df['predicted']=np.array(model_fit.forecast(len(df)))
        return df