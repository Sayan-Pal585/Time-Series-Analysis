# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:01:40 2019

@author: AKhil Saraswat
"""


def holtsWinter(df,depVar, trend=None, damped=False, seasonal=None, seasonal_periods=None, dates=None, freq=None, missing='none'):
    """
    Exponential Smoothing

    Parameters
    ----------
    endog : array-like
        Time series
    trend : {"add", "mul", "additive", "multiplicative", None}, optional
        Type of trend component.
    damped : bool, optional
        Should the trend component be damped.
    seasonal : {"add", "mul", "additive", "multiplicative", None}, optional
        Type of seasonal component.
    seasonal_periods : int, optional
        The number of seasons to consider for the holt winters.
    freq : str, optional
    The frequency of the time-series. A Pandas offset or 'B', 'D', 'W',
    'M', 'A', or 'Q'. This is optional if dates are given.
    'B' - business day, ie., Mon. - Fri.
    'D' - daily
    'W' - weekly
    'M' - monthly
    'A' - annual
    'Q' - quarterly
        
    Notes
    -----
    This is a full implementation of the holt winters exponential smoothing.
    This includes all the unstable methods as well as the stable methods.
    The implementation of the library covers the functionality of the R 
    library as much as possible whilst still being pythonic.
    
    """
    try:
        model =ExponentialSmoothing(df[depVar], trend=trend, damped=damped, seasonal=seasonal, seasonal_periods=seasonal_periods, dates=dates, freq=freq, missing=missing)
    except Exception as e:
        print("Unable to create holt's winter model with the given data")
        print(e)
    try:
        model_fit = model.fit(use_boxcox=True)
    except Exception as e:
        print("Unable to fit holt's winter model to the dataset")
        print(e)
    return model_fit