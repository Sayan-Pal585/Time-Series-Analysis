# Time-Series-Analysis
Time Series Analysis & Forecasting
## Understanding the properties of time series data
### Stationarity of data 

 * A stationary time series is one whose statistical properties such as mean, variance, autocorrelation, etc. are all constant over time.
 * A time series with cyclic behavior (but not trend or seasonality) is stationary. That is because the cycles are not of fixed length
 * A stationarized series is relatively easy to predict: you simply predict that its statistical properties will be the same in the future as they have been in the past.
 * The predictions for the stationarized series can then be "untransformed," by reversing whatever mathematical transformations were previously used, to obtain predictions for the original series. Thus, finding the sequence of transformations needed to stationarize a time series often provides important clues in the search for an appropriate forecasting model.


 * Determine its properties and stationarity: 

* Before we can fit an ARIMA model, our time series has to be stationary. Time series is stationary if its mean level and variance stay steady over time

* Transformations such as logarithms can help to stabilize the variance of a time series. Differencing can help stabilize the mean of a time series by removing changes in the level of a time series, and so eliminating trend and seasonality

```{r stationarity test,warning = FALSE, echo=TRUE, message=FALSE}
# The Augmented Dickey–Fuller (ADF) t-statistic test: small p-values suggest the data is stationary and doesn’t need to be differenced stationarity
tseries::adf.test(data_ts,alternative = "stationary")

# The Kwiatkowski-Phillips-Schmidt-Shin (KPSS) test; here accepting the null hypothesis means that the series is stationarity, and small p-values suggest that the series is not stationary and a differencing is required
tseries::kpss.test(data_ts)

# The Ljung-Box test examines whether there is significant evidence for non-zero correlations at lags 1-20. Small p-values (i.e., less than 0.05) suggest that the series is stationary.
Box.test(data_ts,type="Ljung-Box")
```

* Note that the P-value for above test is 0.01 & series is stationary. In case series not stationary, we will try differencing and logging the time series:

* Let's check how the data behaves after differencing and taking log
```{r differencing data,warning = FALSE, echo=TRUE, message=FALSE}
# Differenced Time Series
# Choose the Data Treatment if the timeseries is not stationary and convert the timeseries accordingly
# Logged Time Series
logged_ts <- log10(data_ts)
plot(logged_ts,ylab="Log (forecast varaible)",type="l")
tseries::adf.test(logged_ts,alternative = "stationary")

# Difference Series and enter the number of differences required
differenced_ts <- diff(data_ts, differences = 1)
plot(differenced_ts,ylab="Differenced (forecast varaible)",type="l")
tseries::adf.test(differenced_ts,alternative = "stationary")
```

* **Over differencing:** A series that has been differenced one too many times will show very strong negative autocorrelation and a strong MA signature, probably with a unit root in MA coefficients. We will evaluate this when we check the ACF and PACF plots

###Considerations while deciding on the training data and test data

* There is a usual thumb rule of splitting the data into 70% training and 30% test. Although, a fewforecasting models like STL, Hybrid, Opera require minimum two cycles (2 years)

* We shall use the trasformed data (differeced or log) for the forecasting models ahead

### Quick Recap

* In our data treatment exercise, we observed that the existing series was not stationary and was stationized by double differencing. Now, we will go ahead with the forecasting exercise

##ARIMA models
* ARIMA models is one of the most widely-used approaches to time series forecasting, and provide complementary approaches to the problem. ARIMA models aim to describe the autocorrelations in the data.

* A nonseasonal ARIMA model is classified as an "ARIMA(p,d,q)" model, where:
    + p is the number of autoregressive terms
    + d is the number of nonseasonal differences needed for stationarity, and
    + q is the number of lagged forecast errors in the prediction equation

* In effect, an ARIMA model will take an order-differencing input to make the series stationary. As we saw previously, this time series can be stationarized by double differencing. Hence, the order of differencing can be inputted when manually fitting an ARIMA model.

* Let's take a look at the series at hand to determine its other properties.

### Estimating AR & MA orders through ACF and PACF inspection

* After a time series has been stationarized by differencing, the next step in fitting an ARIMA model is to determine whether AR or MA terms are needed to correct any autocorrelation that remains in the differenced series
* The **Autocorrelation Function plot** is merely a bar chart of the coefficients of correlation between a time series and lags of itself
* A **partial autocorrelation function plot** depicts the amount of correlation between a variable and a lag of itself that is not explained by correlations at all lower-order-lags. The autocorrelation of a time series Y at lag 1 is the coefficient of correlation between Yt and Yt-1, which is presumably also the correlation between Yt-1 and Yt-2. But if Yt is correlated with Yt-1, and Yt-1 is equally correlated with Yt-2, then we should also expect to find correlation between Yt and Yt-2. In fact, the amount of correlation we should expect at lag 2 is precisely the square of the lag-1 correlation. Thus, the correlation at lag 1 "propagates" to lag 2 and presumably to higher-order lags. The partial autocorrelation at lag 2 is therefore the difference between the actual correlation at lag 2 and the expected correlation due to the propagation of correlation at lag 1

```{r acfpacf,echo=TRUE,warning = FALSE}
# ACF Plot
# Note that we are converting the time series back to a data frame for the ACF function to interpret it as observations
acf(as.data.frame(data_ts), lag.max = 33)

# PACF Plot
# Note that we are converting the time series back to a data frame for the PACF function to interpret it as observations
pacf(as.data.frame(data_ts), lag.max = 33)
```

* **How to understand if the AR & MA orders are significant from the plots?**

    + The data may follow an ARIMA(p,d,0p,d,0) model if the ACF and PACF plots of the differenced data show the following patterns:
        - the ACF is exponentially decaying or sinusoidal;
        - there is a significant spike at lag pp in PACF, but none beyond lag pp.
    + The data may follow an ARIMA(0,d,q0,d,q) model if the ACF and PACF plots of the differenced data show the following patterns:
        - the PACF is exponentially decaying or sinusoidal;
        - there is a significant spike at lag qq in ACF, but none beyond lag qq

* **How do we determine the correct p & q orders from the plots?**
    + Some rough rules of thumb:
        - If the stationarized series has positive autocorrelation at lag 1, AR terms often work best. If it has negative autocorrelation at lag 1, MA terms often work best
        - AR signature – ACF that dies out gradually and PACF that cuts off sharply after a few lags. An AR series is usually positively autocorrelated at lag 1 (or even borderline nonstationary) 
        - MA signature – ACF that cuts off sharply after a few lags and PACF that dies out more gradually. An MA series is usually negatively autcorrelated at lag 1 (or even mildly overdifferenced)

* Once the AR and the MA component is fixed. You may have to iterate a bit on the order of AR and MA to get the best (lowest) AIC & PIC value


### Diagnosing the Models
* While diagnosing the models, we want to check the following conditions:
    + The model has one of the lowest AIc/AICc among all models
    + The residuals are not significantly autocorrelated
    + The distribution of residuals do not follow a pattern (normal distribution)


* As mentioned above, we want to check the following conditions:
    + The ACF of the residuals shows no significant autocorrelations - a good result
    + The Q-Q plot is a normal probability plot.  It doesn't look too bad, so the assumption of normally distributed residuals looks okay
    + The bottom left plot gives p-values for the White Noise Probability (Ljung-Box-Pierce statistics) for each lag up to 20.  These statistics consider the accumulated residual autocorrelation from lag 1 up to and including the lag on the horizontal axis.  The dashed red line is at .05.  All p-values are above it.  That's a good result.  We want non-significant values for this statistic when looking at residuals

* Visual inspection to identify any surges or troughs that the model is unable to pick up

### Summary of diagnostics and accuracy checks

* A good model should
    + Have low Insample MAPE (Be able to capture monthly variations and have a fit that is close to actual monthly volumes)
    + Have low Outsample MAPE (Directionally correct predictions that are not far off from the actual growth/decline observed)
    + Have the lowest possible AICc (when comparing multiple models having different p,d,q values)
    + Should follow the trend and seasonality based on the visual inspection

For Reference :-
1. https://otexts.com/fpp2/index.html
