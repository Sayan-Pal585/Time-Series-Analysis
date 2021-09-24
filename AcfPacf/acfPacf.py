# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:05:17 2019

@author: Akhil Saraswat
"""

def plotAcfPacf(df,depVar,nlags=1):
    
    
    # Calculating acf and pacf
    nlags=100
    try:
        autoCorr =acf(df[depVar],nlags=nlags)
    except Exception as e:
        print("Unable to calculate acf for given series")
        print(e)
    try:
        pAutoCorr = pacf(df[depVar],nlags=nlags)
    except Exception as e:
        print("Unable to calculate pacf for given series")
        print(e)
        
    
    
    # ploting acf and pacf without any differencing
    print("Autocorrelation and Partial Autocorrelation")
    print("-------------------------------------------")
    try:
        fig = pyplot.figure(figsize=(12,8))
    except Exception as e:
        print("Unable to create pyplot object")
        print(e)
    try:
        ax1 = fig.add_subplot(211)
    except Exception as e:
        print("Unable to add subplot axes 211 to the figure")
        print(e)
    try:
        fig = sm.graphics.tsa.plot_acf(df[depVar].dropna(), lags=nlags, ax=ax1)
    except:
        print("Cannot add acf plot from statsmodels library to the figure object")
        print(e)
    try:
        ax2 = fig.add_subplot(212)
    except Exception as e:
        print("Unable to add subplot axes 212 to the figure")
        print(e)
    try:
        fig = sm.graphics.tsa.plot_pacf(df[depVar].dropna(), lags=nlags, ax=ax2)
    except:
        print("Cannot add pacf plot from statsmodels library to the figure object")
        print(e)
    try:
        pyplot.show()
    except:
        print("Unable to display pyplot")
        print(e)
    
    
    
    
    
    
    
    # ploting acf and pacf after differencing once
    print("Autocorrelation and Partial Autocorrelation After Differencing Once")
    print("-----------------------")
    try:
        fig = pyplot.figure(figsize=(12,8))
    except Exception as e:
        print("Unable to create pyplot object")
        print(e)
    try:
        ax1 = fig.add_subplot(211)
    except Exception as e:
        print("Unable to add subplot axes 211 to the figure")
        print(e)
    try:
        # calculating acf after diffrencing once, diff(12) will give results with the seasonal period as 12
        fig = sm.graphics.tsa.plot_acf(df[depVar].diff().dropna(), lags=nlags, ax=ax1)
    except:
        print("Cannot add acf plot from statsmodels library to the figure object")
        print(e)
    try:
        ax2 = fig.add_subplot(212)
    except Exception as e:
        print("Unable to add subplot axes 212 to the figure")
        print(e)
    try:
        # calculating pacf after diffrencing once, diff(12) will give results with the seasonal period as 12
        fig = sm.graphics.tsa.plot_pacf(df[depVar].diff().dropna(), lags=nlags, ax=ax2)
    except:
        print("Cannot add pacf plot from statsmodels library to the figure object")
        print(e)
    try:
        pyplot.show()
    except:
        print("Unable to display pyplot")
        print(e)
        
    
    
    
    
    
    
    # ploting acf and pacf after differencing once
    print("Autocorrelation and Partial Autocorrelation After Differencing Twice")
    print("------------------------")
    try:
        fig = pyplot.figure(figsize=(12,8))
    except Exception as e:
        print("Unable to create pyplot object")
        print(e)
    try:
        ax1 = fig.add_subplot(211)
    except Exception as e:
        print("Unable to add subplot axes 211 to the figure")
        print(e)
    try:
        # calculating acf after diffrencing twice, diff(12) will give results with the seasonal period as 12
        fig = sm.graphics.tsa.plot_acf(df[depVar].diff().diff().dropna(), lags=nlags, ax=ax1)
    except:
        print("Cannot add acf plot from statsmodels library to the figure object")
        print(e)
    try:
        ax2 = fig.add_subplot(212)
    except Exception as e:
        print("Unable to add subplot axes 212 to the figure")
        print(e)
    try:
        # calculating pacf after diffrencing twice, diff(12) will give results with the seasonal period as 12
        fig = sm.graphics.tsa.plot_pacf(df[depVar].diff().diff().dropna(), lags=nlags, ax=ax2)
    except:
        print("Cannot add pacf plot from statsmodels library to the figure object")
        print(e)
    try:
        pyplot.show()
    except:
        print("Unable to display pyplot")
        print(e)