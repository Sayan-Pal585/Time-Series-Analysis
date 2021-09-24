# -*- coding: utf-8 -*-
"""
Created on Thu Feb  7 18:09:36 2019

@author: Akhil Saraswat
"""

def gridSearchArima(df,depVar,max_p=3,max_d=3,max_q=3,silent=False):
    
    try:
        itemObj = df[depVar]
    except Exception as e:
        print("depVar is not present in the given dataframe df")
        print(e)
    try:
        p=range(1,max_p)
    except Exception as e:
        print("max_p has to be an integer greater than 0")
        print(e)
    try:
        d=range(1,max_d)
    except Exception as e:
        print("max_d has to be an integer greater than 0")
        print(e)
    try:
        q=range(1,max_q)
    except Exception as e:
        print("max_q has to be an integer greater than 0")
        print(e)
    # Generate all different combinations of p, q and q triplets
    try:
        pdq = list(itertools.product(p, d, q))
    except Exception as e:
        print("Unable to create list of all combination of pdq from the given value")
        print(e)

    
    try:
        bestmape = np.inf
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
                results,pdqValues = arima(df,depVar,
                             order=param)


                #if current run of AIC is better than the best one so far, overwrite it
                currenterror = np.mean(np.abs((results.resid)/df[depVar])) * 100
                if currenterror<bestmape:
                    bestmape = currenterror
                    bestParam = param
                    print("Iteration result AIC :")
                    print(bestmape)
                    print(param)

            except:
                continue
    except Exception as e:
        print("unable to iterate through pdq")
        print(e)
                
    return bestmape,param