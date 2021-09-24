# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:06:45 2019

@author: Akhil Saraswat
"""
def expSmoothing(df,depVar=None, smoothing_level=None, optimized=True):
    """
    Parameters
    ----------
    df    : pandas dataframe
    depVar: dependent variable name
        The name of the variable for prediction
    smoothing_level : float, optional
        The smoothing_level value of the simple exponential smoothing, if the value is
        set then this value will be used as the value.
    optimized : bool
        Should the values that have not been set above be optimized automatically?
    """
    if depVar is not None:
        try:
            model = SimpleExpSmoothing(df[depVar])
        except Exception as e:
            print("Unable to run exponentialSmoothing with the given data")
            print(e)
        try:
            model_fit = model.fit(smoothing_level=smoothing_level,optimized=optimized)
        except Exception as e:
            print("Unable to fit SimpleExpSmoothing to the dataset")
            print(e)
    else:
        try:
            model = SimpleExpSmoothing(df[0])
        except Exception as e:
            print("Unable to run exponentialSmoothing to the dataset, please provide depVar")
            print(e)
        try:
            model_fit = model.fit(smoothing_level=smoothing_level,optimized=optimized)
        except Exception as e:
            print("Unable to fit SimpleExpSmoothing to the dataset")
            print(e)
    return model_fit
