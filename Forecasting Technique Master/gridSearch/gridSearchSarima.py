# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:10:49 2019

@author: Akhil Saraswat
"""

def gridSearchSarima(df,depVar,max_p=3,max_d=3,max_q=3,silent=False):
    
    try:
        itemObj = df[depVar]
    except Exception as e:
        print("depVar is not present in the given dataframe df")
        print(e)
    try:
        p=range(0,max_p)
    except Exception as e:
        print("max_p has to be an integer greater than 0")
        print(e)
    try:
        d=range(0,max_d)
    except Exception as e:
        print("max_d has to be an integer greater than 0")
        print(e)
    try:
        q=range(0,max_q)
    except Exception as e:
        print("max_q has to be an integer greater than 0")
        print(e)
    # Generate all different combinations of p, q and q triplets
    try:
        pdq = list(itertools.product(p, d, q))
    except Exception as e:
        print("Unable to create list of all combination of pdq from the given value")
        print(e)

    # Generate all different combinations of seasonal p, q and q triplets
    try:
        seasonalPDQ = [(x[0], x[1], x[2], 12) for x in list(itertools.product(p, d, q))]
    except Exception as e:
        print("Unable to create list of all combination of seasonalPDQ from the given value")
        print(e)

    
    try:
        bestAIC = np.inf
    except Exception as e:
        print("Numpy is not installed/imported in time series function module")
        print(e)
    bestParam = None
    bestSParam = None
    
    print('Running GridSearch')
    
    #use gridsearch to look for optimial arima parameters
    try:
        for param in pdq:
            
            try:
                for paramSeasonal in seasonalPDQ:
                    try:
                        results = sarima(df,depVar,
                                     order=param,
                                     seasonal_order=paramSeasonal,
                                     enforce_stationarity=False,
                                     enforce_invertibility=False)


                        #if current run of AIC is better than the best one so far, overwrite it
                        if results.aic<bestAIC:
                            bestAIC = results.aic
                            bestParam = param
                            bestSParam = paramSeasonal
                            print("Iteration result AIC :")
                            print(results.aic)
                            print(param)
                            print(paramSeasonal)

                    except:
                        continue
            except Exception as e:
                print("unable to iterate through seasonalPDQ")
                print(e)
    except Exception as e:
        print("unable to iterate through pdq")
        print(e)
                
    print('the best ones are:',bestAIC,bestParam,bestSParam)