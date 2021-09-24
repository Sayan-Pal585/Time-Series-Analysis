# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:02:21 2019

@author: Akhil Saraswat
"""

def arima(df,depVar=None, order=(0,0,1), exog=None, dates=None, freq=None, missing='none'):
    """
    Parameters
    ----------
    df : pandas dataframe or array-like
        The endogenous variable.
    depVar : dependent variable
        The column name of the dependent variable
    exog   : array_like or None, optional
        Array of exogenous regressors, shaped nobs x k.
        Exogenous variables.
        Series or dataframe of independent variables :math `x`
    order : iterable
        The (p,d,q) order of the model for the number of AR parameters,
        differences, and MA parameters to use.
    dates : array-like of datetime, optional
        An array-like object of datetime objects. If a pandas object is given
        for endog or exog, it is assumed to have a DateIndex.
    freq : str, optional
        The frequency of the time-series. A Pandas offset or 'B', 'D', 'W',
        'M', 'A', or 'Q'. This is optional if dates are given.
        'B' - business day, ie., Mon. - Fri.
        'D' - daily
        'W' - weekly
        'M' - monthly
        'A' - annual
        'Q' - quarterly
    """
    if depVar is not None:
        try:
            model = ARIMA(df[depVar], order=order, exog=exog, dates=dates, freq=freq, missing=missing)
            print(model)
        except Exception as e:
            print("Unable to plot arima with the given depVar")
            print(e)
            
    else:
        try:
            model = ARIMA(df[0], order=order, dates=dates, freq=freq, missing=missing)
        except Exception as e:
            print("Unable to create Arima model, please provide depVar")
            print(e)
    try:
        model_fit = model.fit(disp=0)
    except Exception as e:
        print("Unable to fit the model")
        print(e)
    model1 = "ARIMA"
    return(model_fit,{"order":order,"model":model1})