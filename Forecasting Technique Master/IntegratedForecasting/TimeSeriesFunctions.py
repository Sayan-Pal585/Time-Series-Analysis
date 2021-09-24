
# coding: utf-8



import pandas as pd
import numpy as np


from pandas.plotting import autocorrelation_plot
from statsmodels.graphics.tsaplots import plot_acf


from plotly.offline import download_plotlyjs as pltjs, init_notebook_mode, iplot,plot
init_notebook_mode(connected=True)
import plotly.graph_objs as go


from datetime import datetime

import itertools

from matplotlib import pyplot
config = {'showLink': False, 'displaylogo':False, 'modeBarButtonsToRemove':['sendDataToCloud']}


import statsmodels.api as sm
from statsmodels.tsa.api import SimpleExpSmoothing
from statsmodels.tsa.holtwinters import ExponentialSmoothing
from statsmodels.tsa.stattools import acf,pacf
from statsmodels.tsa.arima_model import ARIMA
from statsmodels.tsa.statespace.sarimax import SARIMAX
from statsmodels.tsa.statespace.structural import UnobservedComponents,UnobservedComponentsResults






import tkinter as Ttk
from tkinter import ttk, filedialog
from tkinter.ttk import *
from tkinter import *


from sklearn.model_selection import GridSearchCV


# In[67]:


################################################################################################################################
##FUNCTION: Gives a popup dialog box to select csv file
##INPUT: None
##OUTPUT: Dataframe selected by user
################################################################################################################################

def readCsv():
    """
    readCsv function - A popup using tkinter package to select csv
    =======================================================================================
    **readCsv** is a Python function by MathMarket (Team Tesla) of TheMathCompany,
    providing a fast and dynamic way to select a csv file and convert it to a pandas dataframe.

    This functions return dataframe at index 0 and dataframe name at index 1
    
    The function is built using  filedialogue from tkinter package
    Therefore, these libraries are needed to be installed in order to use the module.

    The module consists of one function:
    `readCsv()` 
    """
 
    root = Ttk.Tk()
    root.df =  filedialog.askopenfilename(initialdir = "/",title = "Select file",filetypes = (("Csv files","*.csv"),("all files","*.*")))
    df = root.df
    root.withdraw()
    df = pd.read_csv(df)
    df =pd.DataFrame(df)
    df.head()
    return(df)


# In[68]:


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
        return fig
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


# In[69]:


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
    order = str(order)
    return(model_fit,{"order":order,"model":model1})


# In[70]:


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


# In[71]:


def ucm(df, depVar=None, level=False, trend=False, seasonal=None, exog=None, freq_seasonal=None, cycle=False, autoregressive=None, irregular=False, stochastic_level=False, stochastic_trend=False, stochastic_seasonal=True, stochastic_freq_seasonal=None, stochastic_cycle=False, damped_cycle=False, cycle_period_bounds=None, mle_regression=True, start_params=None, transformed=True, cov_type='opg', cov_kwds=None, method='lbfgs', maxiter=50, full_output=1, disp=5, callback=None, return_params=False, optim_score=None, optim_complex_step=None, optim_hessian=None, flags=None):
    """
    Univariate unobserved components time series model

    These are also known as structural time series models, and decompose a
    (univariate) time series into trend, seasonal, cyclical, and irregular
    components.

    Parameters
    ----------

    For Model Build
    ---------------
    
    df    : pandas dataframe
    depVar: dependent variable name
        The name of the variable for prediction
    exog   : array_like or None, optional
        Array of exogenous regressors, shaped nobs x k.
        Exogenous variables.
        Series or dataframe of independent variables
    level : bool or string, optional
        Whether or not to include a level component. Default is False. Can also
        be a string specification of the level / trend component; see Notes
        for available model specification strings.
    trend : bool, optional
        Whether or not to include a trend component. Default is False. If True,
        `level` must also be True.
    seasonal : int or None, optional
        The period of the seasonal component, if any. Default is None.
    freq_seasonal: list of dicts or None, optional.
        Whether (and how) to model seasonal component(s) with trig. functions.
        If specified, there is one dictionary for each frequency-domain
        seasonal component.  Each dictionary must have the key, value pair for
        'period' -- integer and may have a key, value pair for
        'harmonics' -- integer. If 'harmonics' is not specified in any of the
        dictionaries, it defaults to the floor of period/2.
    cycle : bool, optional
        Whether or not to include a cycle component. Default is False.
    ar : int or None, optional
        The order of the autoregressive component. Default is None.
    exog : array_like or None, optional
        Array of exogenous regressors, shaped nobs x k.
        Series or dataframe of independent variables
        If defined model will run ARIMAX
    irregular : bool, optional
        Whether or not to include an irregular component. Default is False.
    stochastic_level : bool, optional
        Whether or not any level component is stochastic. Default is False.
    stochastic_trend : bool, optional
        Whether or not any trend component is stochastic. Default is False.
    stochastic_seasonal : bool, optional
        Whether or not any seasonal component is stochastic. Default is True.
    stochastic_freq_seasonal: list of bools, optional
        Whether or not each seasonal component(s) is (are) stochastic.  Default
        is True for each component.  The list should be of the same length as
        freq_seasonal.
    stochastic_cycle : bool, optional
        Whether or not any cycle component is stochastic. Default is False.
    damped_cycle : bool, optional
        Whether or not the cycle component is damped. Default is False.
    cycle_period_bounds : tuple, optional
        A tuple with lower and upper allowed bounds for the period of the
        cycle. If not provided, the following default bounds are used:
        (1) if no date / time information is provided, the frequency is
        constrained to be between zero and :math:`\pi`, so the period is
        constrained to be in [0.5, infinity].
        (2) If the date / time information is provided, the default bounds
        allow the cyclical component to be between 1.5 and 12 years; depending
        on the frequency of the endogenous variable, this will imply different
        specific bounds.
        
    For Model Fit
    -------------
    
    Fits the model by maximum likelihood via Kalman filter.

    Parameters
    ----------
    start_params : array_like, optional
        Initial guess of the solution for the loglikelihood maximization.
        If None, the default is given by Model.start_params.
    transformed : boolean, optional
        Whether or not `start_params` is already transformed. Default is
        True.
    cov_type : str, optional
        The `cov_type` keyword governs the method for calculating the
        covariance matrix of parameter estimates. Can be one of:

        - 'opg' for the outer product of gradient estimator
        - 'oim' for the observed information matrix estimator, calculated
          using the method of Harvey (1989)
        - 'approx' for the observed information matrix estimator,
          calculated using a numerical approximation of the Hessian matrix.
        - 'robust' for an approximate (quasi-maximum likelihood) covariance
          matrix that may be valid even in the presense of some
          misspecifications. Intermediate calculations use the 'oim'
          method.
        - 'robust_approx' is the same as 'robust' except that the
          intermediate calculations use the 'approx' method.
        - 'none' for no covariance matrix calculation.
    cov_kwds : dict or None, optional
        A dictionary of arguments affecting covariance matrix computation.

        **opg, oim, approx, robust, robust_approx**

        - 'approx_complex_step' : boolean, optional - If True, numerical
          approximations are computed using complex-step methods. If False,
          numerical approximations are computed using finite difference
          methods. Default is True.
        - 'approx_centered' : boolean, optional - If True, numerical
          approximations computed using finite difference methods use a
          centered approximation. Default is False.
    method : str, optional
        The `method` determines which solver from `scipy.optimize`
        is used, and it can be chosen from among the following strings:

        - 'newton' for Newton-Raphson, 'nm' for Nelder-Mead
        - 'bfgs' for Broyden-Fletcher-Goldfarb-Shanno (BFGS)
        - 'lbfgs' for limited-memory BFGS with optional box constraints
        - 'powell' for modified Powell's method
        - 'cg' for conjugate gradient
        - 'ncg' for Newton-conjugate gradient
        - 'basinhopping' for global basin-hopping solver

        The explicit arguments in `fit` are passed to the solver,
        with the exception of the basin-hopping solver. Each
        solver has several optional arguments that are not the same across
        solvers. See the notes section below (or scipy.optimize) for the
        available arguments and for the list of explicit arguments that the
        basin-hopping solver supports.
    maxiter : int, optional
        The maximum number of iterations to perform.
    full_output : boolean, optional
        Set to True to have all available output in the Results object's
        mle_retvals attribute. The output is dependent on the solver.
        See LikelihoodModelResults notes section for more information.
    disp : boolean, optional
        Set to True to print convergence messages.
    callback : callable callback(xk), optional
        Called after each iteration, as callback(xk), where xk is the
        current parameter vector.
    return_params : boolean, optional
        Whether or not to return only the array of maximizing parameters.
        Default is False.
    optim_score : {'harvey', 'approx'} or None, optional
        The method by which the score vector is calculated. 'harvey' uses
        the method from Harvey (1989), 'approx' uses either finite
        difference or complex step differentiation depending upon the
        value of `optim_complex_step`, and None uses the built-in gradient
        approximation of the optimizer. Default is None. This keyword is
        only relevant if the optimization method uses the score.
    optim_complex_step : bool, optional
        Whether or not to use complex step differentiation when
        approximating the score; if False, finite difference approximation
        is used. Default is True. This keyword is only relevant if
        `optim_score` is set to 'harvey' or 'approx'.
    optim_hessian : {'opg','oim','approx'}, optional
        The method by which the Hessian is numerically approximated. 'opg'
        uses outer product of gradients, 'oim' uses the information
        matrix formula from Harvey (1989), and 'approx' uses numerical
        approximation. This keyword is only relevant if the
        optimization method uses the Hessian matrix.
    
    
    Notes
    -----

    These models take the general form

    .. math::

        y_t = \mu_t + \gamma_t + c_t + \varepsilon_t

    where :math:`y_t` refers to the observation vector at time :math:`t`,
    :math:`\mu_t` refers to the trend component, :math:`\gamma_t` refers to the
    seasonal component, :math:`c_t` refers to the cycle, and
    :math:`\varepsilon_t` is the irregular. The modeling details of these
    components are given below.

    **Trend**

    The trend component is a dynamic extension of a regression model that
    includes an intercept and linear time-trend. It can be written:

    .. math::

        \mu_t = \mu_{t-1} + \beta_{t-1} + \eta_{t-1} \\
        \beta_t = \beta_{t-1} + \zeta_{t-1}

    where the level is a generalization of the intercept term that can
    dynamically vary across time, and the trend is a generalization of the
    time-trend such that the slope can dynamically vary across time.

    Here :math:`\eta_t \sim N(0, \sigma_\eta^2)` and
    :math:`\zeta_t \sim N(0, \sigma_\zeta^2)`.

    For both elements (level and trend), we can consider models in which:

    - The element is included vs excluded (if the trend is included, there must
      also be a level included).
    - The element is deterministic vs stochastic (i.e. whether or not the
      variance on the error term is confined to be zero or not)

    The only additional parameters to be estimated via MLE are the variances of
    any included stochastic components.

    The level/trend components can be specified using the boolean keyword
    arguments `level`, `stochastic_level`, `trend`, etc., or all at once as a
    string argument to `level`. The following table shows the available
    model specifications:

    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Model name                       | Full string syntax                   | Abbreviated syntax | Model                                            |
    +==================================+======================================+====================+==================================================+
    | No trend                         | `'irregular'`                        | `'ntrend'`         | .. math:: y_t &= \varepsilon_t                   |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Fixed intercept                  | `'fixed intercept'`                  |                    | .. math:: y_t &= \mu                             |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Deterministic constant           | `'deterministic constant'`           | `'dconstant'`      | .. math:: y_t &= \mu + \varepsilon_t             |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Local level                      | `'local level'`                      | `'llevel'`         | .. math:: y_t &= \mu_t + \varepsilon_t \\        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \eta_t                  |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Random walk                      | `'random walk'`                      | `'rwalk'`          | .. math:: y_t &= \mu_t \\                        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \eta_t                  |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Fixed slope                      | `'fixed slope'`                      |                    | .. math:: y_t &= \mu_t \\                        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta                   |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Deterministic trend              | `'deterministic trend'`              | `'dtrend'`         | .. math:: y_t &= \mu_t + \varepsilon_t \\        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta                   |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Local linear deterministic trend | `'local linear deterministic trend'` | `'lldtrend'`       | .. math:: y_t &= \mu_t + \varepsilon_t \\        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta + \eta_t          |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Random walk with drift           | `'random walk with drift'`           | `'rwdrift'`        | .. math:: y_t &= \mu_t \\                        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta + \eta_t          |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Local linear trend               | `'local linear trend'`               | `'lltrend'`        | .. math:: y_t &= \mu_t + \varepsilon_t \\        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta_{t-1} + \eta_t \\ |
    |                                  |                                      |                    |     \beta_t &= \beta_{t-1} + \zeta_t             |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Smooth trend                     | `'smooth trend'`                     | `'strend'`         | .. math:: y_t &= \mu_t + \varepsilon_t \\        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta_{t-1} \\          |
    |                                  |                                      |                    |     \beta_t &= \beta_{t-1} + \zeta_t             |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+
    | Random trend                     | `'random trend'`                     | `'rtrend'`         | .. math:: y_t &= \mu_t \\                        |
    |                                  |                                      |                    |     \mu_t &= \mu_{t-1} + \beta_{t-1} \\          |
    |                                  |                                      |                    |     \beta_t &= \beta_{t-1} + \zeta_t             |
    +----------------------------------+--------------------------------------+--------------------+--------------------------------------------------+

    Following the fitting of the model, the unobserved level and trend
    component time series are available in the results class in the
    `level` and `trend` attributes, respectively.

    **Seasonal (Time-domain)**

    The seasonal component is modeled as:

    .. math::

        \gamma_t = - \sum_{j=1}^{s-1} \gamma_{t+1-j} + \omega_t \\
        \omega_t \sim N(0, \sigma_\omega^2)

    The periodicity (number of seasons) is s, and the defining character is
    that (without the error term), the seasonal components sum to zero across
    one complete cycle. The inclusion of an error term allows the seasonal
    effects to vary over time (if this is not desired, :math:`\sigma_\omega^2`
    can be set to zero using the `stochastic_seasonal=False` keyword argument).

    This component results in one parameter to be selected via maximum
    likelihood: :math:`\sigma_\omega^2`, and one parameter to be chosen, the
    number of seasons `s`.

    Following the fitting of the model, the unobserved seasonal component
    time series is available in the results class in the `seasonal`
    attribute.

    ** Frequency-domain Seasonal**

    Each frequency-domain seasonal component is modeled as:

    .. math::

        \gamma_t & =  \sum_{j=1}^h \gamma_{j, t} \\
        \gamma_{j, t+1} & = \gamma_{j, t}\cos(\lambda_j)
                        + \gamma^{*}_{j, t}\sin(\lambda_j) + \omega_{j,t} \\
        \gamma^{*}_{j, t+1} & = -\gamma^{(1)}_{j, t}\sin(\lambda_j)
                            + \gamma^{*}_{j, t}\cos(\lambda_j)
                            + \omega^{*}_{j, t}, \\
        \omega^{*}_{j, t}, \omega_{j, t} & \sim N(0, \sigma_{\omega^2}) \\
        \lambda_j & = \frac{2 \pi j}{s}

    where j ranges from 1 to h.

    The periodicity (number of "seasons" in a "year") is s and the number of
    harmonics is h.  Note that h is configurable to be less than s/2, but
    s/2 harmonics is sufficient to fully model all seasonal variations of
    periodicity s.  Like the time domain seasonal term (cf. Seasonal section,
    above), the inclusion of the error terms allows for the seasonal effects to
    vary over time.  The argument stochastic_freq_seasonal can be used to set
    one or more of the seasonal components of this type to be non-random,
    meaning they will not vary over time.

    This component results in one parameter to be fitted using maximum
    likelihood: :math:`\sigma_{\omega^2}`, and up to two parameters to be
    chosen, the number of seasons s and optionally the number of harmonics
    h, with :math:`1 \leq h \leq \floor(s/2)`.

    After fitting the model, each unobserved seasonal component modeled in the
    frequency domain is available in the results class in the `freq_seasonal`
    attribute.

    **Cycle**

    The cyclical component is intended to capture cyclical effects at time
    frames much longer than captured by the seasonal component. For example,
    in economics the cyclical term is often intended to capture the business
    cycle, and is then expected to have a period between "1.5 and 12 years"
    (see Durbin and Koopman).

    .. math::

        c_{t+1} & = \rho_c (\tilde c_t \cos \lambda_c t
                + \tilde c_t^* \sin \lambda_c) +
                \tilde \omega_t \\
        c_{t+1}^* & = \rho_c (- \tilde c_t \sin \lambda_c  t +
                \tilde c_t^* \cos \lambda_c) +
                \tilde \omega_t^* \\

    where :math:`\omega_t, \tilde \omega_t iid N(0, \sigma_{\tilde \omega}^2)`

    The parameter :math:`\lambda_c` (the frequency of the cycle) is an
    additional parameter to be estimated by MLE.

    If the cyclical effect is stochastic (`stochastic_cycle=True`), then there
    is another parameter to estimate (the variance of the error term - note
    that both of the error terms here share the same variance, but are assumed
    to have independent draws).

    If the cycle is damped (`damped_cycle=True`), then there is a third
    parameter to estimate, :math:`\rho_c`.

    In order to achieve cycles with the appropriate frequencies, bounds are
    imposed on the parameter :math:`\lambda_c` in estimation. These can be
    controlled via the keyword argument `cycle_period_bounds`, which, if
    specified, must be a tuple of bounds on the **period** `(lower, upper)`.
    The bounds on the frequency are then calculated from those bounds.

    The default bounds, if none are provided, are selected in the following
    way:

    1. If no date / time information is provided, the frequency is
       constrained to be between zero and :math:`\pi`, so the period is
       constrained to be in :math:`[0.5, \infty]`.
    2. If the date / time information is provided, the default bounds
       allow the cyclical component to be between 1.5 and 12 years; depending
       on the frequency of the endogenous variable, this will imply different
       specific bounds.

    Following the fitting of the model, the unobserved cyclical component
    time series is available in the results class in the `cycle`
    attribute.

    **Irregular**

    The irregular components are independent and identically distributed (iid):

    .. math::

        \varepsilon_t \sim N(0, \sigma_\varepsilon^2)

    **Autoregressive Irregular**

    An autoregressive component (often used as a replacement for the white
    noise irregular term) can be specified as:

    .. math::

        \varepsilon_t = \rho(L) \varepsilon_{t-1} + \epsilon_t \\
        \epsilon_t \sim N(0, \sigma_\epsilon^2)

    In this case, the AR order is specified via the `autoregressive` keyword,
    and the autoregressive coefficients are estimated.

    Following the fitting of the model, the unobserved autoregressive component
    time series is available in the results class in the `autoregressive`
    attribute.

    **Regression effects**

    Exogenous regressors can be pass to the `exog` argument. The regression
    coefficients will be estimated by maximum likelihood unless
    `mle_regression=False`, in which case the regression coefficients will be
    included in the state vector where they are essentially estimated via
    recursive OLS.

    If the regression_coefficients are included in the state vector, the
    recursive estimates are available in the results class in the
    `regression_coefficients` attribute.

    """
    
    
    if depVar is not None:
        try:
            model = UnobservedComponents(df[depVar], level=level, trend=trend, exog=exog, seasonal=seasonal, freq_seasonal=freq_seasonal, cycle=cycle, autoregressive=autoregressive, irregular=irregular, stochastic_level=stochastic_level, stochastic_trend=stochastic_trend, stochastic_seasonal=stochastic_seasonal, stochastic_freq_seasonal=stochastic_freq_seasonal, stochastic_cycle=stochastic_cycle, damped_cycle=damped_cycle, cycle_period_bounds=cycle_period_bounds, mle_regression=mle_regression)
        except Exception as e:
            print("Unable to create UCM with the given data")
            print(e)
    else:
        try:
            model = UnobservedComponents(df[0], trend=trend, seasonal=seasonal, freq_seasonal=freq_seasonal, cycle=cycle, autoregressive=autoregressive, irregular=irregular, stochastic_level=stochastic_level, stochastic_trend=stochastic_trend, stochastic_seasonal=stochastic_seasonal, stochastic_freq_seasonal=stochastic_freq_seasonal, stochastic_cycle=stochastic_cycle, damped_cycle=damped_cycle, cycle_period_bounds=cycle_period_bounds, mle_regression=mle_regression)
        except Exception as e:
            print("Unable to create UCM with the given data, please provide depVar")
            print(e)
    try:
        model_fit = model.fit(method='powell', disp=False)
    except Exception as e:
        print("Unable to fit ucm to the dataset")
        print(e)
    model1 = "UCM"
    return(model_fit,{"model":model1,"level":level, "trend":trend, "exog":exog, "seasonal":seasonal, "freq_seasonal":freq_seasonal, "cycle":cycle, "autoregressive":autoregressive, "irregular":irregular, "stochastic_level":stochastic_level, "stochastic_trend":stochastic_trend, "stochastic_seasonal":stochastic_seasonal, "stochastic_freq_seasonal":stochastic_freq_seasonal, "stochastic_cycle":stochastic_cycle, "damped_cycle":damped_cycle, "cycle_period_bounds":cycle_period_bounds, "mle_regression":mle_regression})


# In[72]:


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


# In[73]:


def sarima(df, depVar, exog=None, order=(1, 0, 0), seasonal_order=(0, 0, 0, 0), trend=None, measurement_error=False, time_varying_regression=False, mle_regression=True, simple_differencing=False, enforce_stationarity=True, enforce_invertibility=True, hamilton_representation=False, start_params=None, transformed=True, cov_type='opg', cov_kwds=None, method='lbfgs', maxiter=50, full_output=1, disp=5, callback=None, return_params=False, optim_score=None, optim_complex_step=None, optim_hessian=None, flags=None):
    """
    Seasonal AutoRegressive Integrated Moving Average with eXogenous regressors
    model

    Parameters
    ----------
    
    For Model Built
    ---------------
    endog : array_like
        The observed time-series process :math:`y`
    exog : array_like, optional
        Array of exogenous regressors, shaped nobs x k.
        Exogenous variables.
        Series or dataframe of independent variables
    order : iterable or iterable of iterables, optional
        The (p,d,q) order of the model for the number of AR parameters,
        differences, and MA parameters. `d` must be an integer
        indicating the integration order of the process, while
        `p` and `q` may either be an integers indicating the AR and MA
        orders (so that all lags up to those orders are included) or else
        iterables giving specific AR and / or MA lags to include. Default is
        an AR(1) model: (1,0,0).
    seasonal_order : iterable, optional
        The (P,D,Q,s) order of the seasonal component of the model for the
        AR parameters, differences, MA parameters, and periodicity.
        `d` must be an integer indicating the integration order of the process,
        while `p` and `q` may either be an integers indicating the AR and MA
        orders (so that all lags up to those orders are included) or else
        iterables giving specific AR and / or MA lags to include. `s` is an
        integer giving the periodicity (number of periods in season), often it
        is 4 for quarterly data or 12 for monthly data. Default is no seasonal
        effect.
    trend : str{'n','c','t','ct'} or iterable, optional
        Parameter controlling the deterministic trend polynomial :math:`A(t)`.
        Can be specified as a string where 'c' indicates a constant (i.e. a
        degree zero component of the trend polynomial), 't' indicates a
        linear trend with time, and 'ct' is both. Can also be specified as an
        iterable defining the polynomial as in `numpy.poly1d`, where
        `[1,1,0,1]` would denote :math:`a + bt + ct^3`. Default is to not
        include a trend component.
    measurement_error : boolean, optional
        Whether or not to assume the endogenous observations `endog` were
        measured with error. Default is False.
    time_varying_regression : boolean, optional
        Used when an explanatory variables, `exog`, are provided provided
        to select whether or not coefficients on the exogenous regressors are
        allowed to vary over time. Default is False.
    mle_regression : boolean, optional
        Whether or not to use estimate the regression coefficients for the
        exogenous variables as part of maximum likelihood estimation or through
        the Kalman filter (i.e. recursive least squares). If
        `time_varying_regression` is True, this must be set to False. Default
        is True.
    simple_differencing : boolean, optional
        Whether or not to use partially conditional maximum likelihood
        estimation. If True, differencing is performed prior to estimation,
        which discards the first :math:`s D + d` initial rows but results in a
        smaller state-space formulation. If False, the full SARIMAX model is
        put in state-space form so that all datapoints can be used in
        estimation. Default is False.
    enforce_stationarity : boolean, optional
        Whether or not to transform the AR parameters to enforce stationarity
        in the autoregressive component of the model. Default is True.
    enforce_invertibility : boolean, optional
        Whether or not to transform the MA parameters to enforce invertibility
        in the moving average component of the model. Default is True.
    hamilton_representation : boolean, optional
        Whether or not to use the Hamilton representation of an ARMA process
        (if True) or the Harvey representation (if False). Default is False.
        
    For Model Fit
    -------------
    Fits the model by maximum likelihood via Kalman filter.

    Parameters
    ----------
    start_params : array_like, optional
        Initial guess of the solution for the loglikelihood maximization.
        If None, the default is given by Model.start_params.
    transformed : boolean, optional
        Whether or not `start_params` is already transformed. Default is
        True.
    cov_type : str, optional
        The `cov_type` keyword governs the method for calculating the
        covariance matrix of parameter estimates. Can be one of:

        - 'opg' for the outer product of gradient estimator
        - 'oim' for the observed information matrix estimator, calculated
          using the method of Harvey (1989)
        - 'approx' for the observed information matrix estimator,
          calculated using a numerical approximation of the Hessian matrix.
        - 'robust' for an approximate (quasi-maximum likelihood) covariance
          matrix that may be valid even in the presense of some
          misspecifications. Intermediate calculations use the 'oim'
          method.
        - 'robust_approx' is the same as 'robust' except that the
          intermediate calculations use the 'approx' method.
        - 'none' for no covariance matrix calculation.
    cov_kwds : dict or None, optional
        A dictionary of arguments affecting covariance matrix computation.

        **opg, oim, approx, robust, robust_approx**

        - 'approx_complex_step' : boolean, optional - If True, numerical
          approximations are computed using complex-step methods. If False,
          numerical approximations are computed using finite difference
          methods. Default is True.
        - 'approx_centered' : boolean, optional - If True, numerical
          approximations computed using finite difference methods use a
          centered approximation. Default is False.
    method : str, optional
        The `method` determines which solver from `scipy.optimize`
        is used, and it can be chosen from among the following strings:

        - 'newton' for Newton-Raphson, 'nm' for Nelder-Mead
        - 'bfgs' for Broyden-Fletcher-Goldfarb-Shanno (BFGS)
        - 'lbfgs' for limited-memory BFGS with optional box constraints
        - 'powell' for modified Powell's method
        - 'cg' for conjugate gradient
        - 'ncg' for Newton-conjugate gradient
        - 'basinhopping' for global basin-hopping solver

        The explicit arguments in `fit` are passed to the solver,
        with the exception of the basin-hopping solver. Each
        solver has several optional arguments that are not the same across
        solvers. See the notes section below (or scipy.optimize) for the
        available arguments and for the list of explicit arguments that the
        basin-hopping solver supports.
    maxiter : int, optional
        The maximum number of iterations to perform.
    full_output : boolean, optional
        Set to True to have all available output in the Results object's
        mle_retvals attribute. The output is dependent on the solver.
        See LikelihoodModelResults notes section for more information.
    disp : boolean, optional
        Set to True to print convergence messages.
    callback : callable callback(xk), optional
        Called after each iteration, as callback(xk), where xk is the
        current parameter vector.
    return_params : boolean, optional
        Whether or not to return only the array of maximizing parameters.
        Default is False.
    optim_score : {'harvey', 'approx'} or None, optional
        The method by which the score vector is calculated. 'harvey' uses
        the method from Harvey (1989), 'approx' uses either finite
        difference or complex step differentiation depending upon the
        value of `optim_complex_step`, and None uses the built-in gradient
        approximation of the optimizer. Default is None. This keyword is
        only relevant if the optimization method uses the score.
    optim_complex_step : bool, optional
        Whether or not to use complex step differentiation when
        approximating the score; if False, finite difference approximation
        is used. Default is True. This keyword is only relevant if
        `optim_score` is set to 'harvey' or 'approx'.
    optim_hessian : {'opg','oim','approx'}, optional
        The method by which the Hessian is numerically approximated. 'opg'
        uses outer product of gradients, 'oim' uses the information
        matrix formula from Harvey (1989), and 'approx' uses numerical
        approximation. This keyword is only relevant if the
        optimization method uses the Hessian matrix.

    
    """
    try:
        model = SARIMAX(df[depVar], exog=exog, order=order, seasonal_order=seasonal_order, trend=trend, measurement_error=measurement_error, time_varying_regression=time_varying_regression, mle_regression=mle_regression, simple_differencing=simple_differencing, enforce_stationarity=enforce_stationarity, enforce_invertibility=enforce_invertibility, hamilton_representation=hamilton_representation)
    except Exception as e:
        print("Unable to create Sarima with the given data")
        print(e)
    try:
        model_fit =model.fit()
    except Exception as e:
        print("Unable to fit sarima on the given data")
        print(e)
        
    order = str(order)
    return (model_fit,{"order":order,"model":"SARIMA"})


# In[74]:


def modelSummary(df,model_fit,depVar):
    try:
        # residual errors in the train set
        residuals = model_fit.resid
    except Exception as e:
        print("model_fit don't have residual function")
        print(e)
    try:
        mape = np.mean(np.abs((residuals)/df[depVar])) * 100
    except Exception as e:
        print("Error occured while calculating mape")
        print(e)
    try:
        print("Train MAPE : %f"%mape)
    except Exception as e:
        print("Mape value does not exist")
        print(e)
    try:
        print(model_fit.summary())
    except Exception as e:
        print("model_fit don't have summary function")
        print(e)
    
    print("Residuals description")
    print("---------------------")
    try:
        print(residuals.describe())
    except Exception as e:
        print("residual description does not exist")
        print(e)
    aic = model_fit.aic
    return({"MAPE":mape,"AIC":aic})


# In[75]:


def TrainPred(df,model_fit,depVar):
    try:
        residuals = model_fit.resid
    except Exception as e:
        print("model_fit don't have residual function")
        print(e)
    
    if depVar is not None:
        
        
        try:
            # Calculating predicted values using predict function
            df['predicted'] =model_fit.predict(start=0,end=len(df))
        except Exception as e:
            print("unable to run predict function the dataframe")
            print(e)
            
            
        try:
            mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[depVar])) * 100
        except Exception as e:
            print("Error occured while calculating mape")
            print(e)
            
            
        print("Train MAPE : %f"%mape)
    else:
        
        
        try:
            mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[0])) * 100
        except Exception as e:
            print("Unable to calculate mape, please provide depVar")
            print(e)
        
        
        try:
            # Calculating predicted values by subtracting residual errors in train set
            df['predicted'] =df-residuals
        except Exception as e:
            print("residuals values cannot be found")
            print(e)
        try:
            print("Train MAPE : %f"%mape)
        except Exception as e:
            print("Mape value does not exist")
            print(e)
    return df


# In[76]:


def TestPred(df,model_fit,depVar):
    if depVar is not None:
        try:
            df['predicted']=model_fit.forecast(len(df))[0]
        except Exception as e:
            try:
                df['predicted']=np.array(model_fit.forecast(len(df)))
            except Exception as e:
                print("Unable to forecast with the model")
                print(e)
        
        
        try:
            mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[depVar]))* 100
        except Exception as e:
            print("Error occured while calculating mape")
            print(e)
        
        
        try:
            print("Test MAPE : %f"%mape)
        except Exception as e:
            print("Mape value does not exist")
            print(e)
    else:
        
        try:
            print("Test MAPE : %f"%mape)
        except Exception as e:
            print("Mape value does not exist")
            print(e)
    return df


# In[77]:


def holtsWinterTrainPred(df,model_fit,depVar):
    try:
        df['predicted']=model_fit.predict(start=0,end=len(df))
    except Exception as e:
        print("unable to run predict function the dataframe")
        print(e)
        
    try:
        mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[depVar])) * 100
    except Exception as e:
        print("Error occured while calculating mape")
        print(e)
    try:
        print("Train MAPE : %f"%mape)
    except Exception as e:
        print("Mape value does not exist")
        print(e)
    return df


# In[78]:


def holtsWinterTestPred(df,model_fit,depVar):
    try:
        df['predicted']=np.array(model_fit.forecast(len(df)))
    except Exception as e:
        print("Unable to forecast for the given length of dataframe")
        print(e)
    try:
        mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[depVar])) * 100
    except Exception as e:
        print("Error occured while calculating mape")
        print(e)
    try:
        print("Test MAPE : %f"%mape)
    except Exception as e:
        print("Mape value does not exist")
        print(e)
    return df


# In[79]:


def ucmSummary(df,model_fit,depVar="value"):
    try:
        print(model_fit.summary())
    except Exception as e:
        print("model_fit don't have summary function")
        print(e)
    try:
        model_fit.plot_components(legend_loc='lower right', figsize=(15, 9))
    except Exception as e:
        print("model_fit do not have plot_components function")
        print(e)
    try:
        # residual errors in the train set
        residuals = model_fit.resid
    except Exception as e:
        print("model_fit don't have residual function")
        print(e)
    try:
        mape = np.mean(np.abs((residuals)/df[depVar])) * 100
    except Exception as e:
        print("Error occured while calculating mape")
        print(e)
    aic = model_fit.aic
    return({"MAPE":mape,"AIC":aic})
    try:
        pyplot.show()
    except Exception as e:
        print("unable to show pyplot")
        print(e)
    return model_fit.plot_components(legend_loc='lower right', figsize=(15, 9))


# In[80]:


def ucmTrainPred(df,depVar,model_fit):
    try:
        df['predicted']=model_fit.predict(start=0)
    except Exception as e:
        print("model_fit don't have predict function")
        print(e)
    try:
        mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[depVar])) * 100
    except Exception as e:
        print("Error occured while calculating mape")
        print(e)
    try:
        print("MAPE : %f"%mape)
    except Exception as e:
        print("Mape value does not exist")
        print(e)
    return df


# In[81]:


def ucmTestPred(df,depVar,model_fit):
    try:
        df['predicted']=np.array(model_fit.forecast(len(df)))
    except Exception as e:
        print("Unable to forecast for the given length of dataframe")
        print(e)
    try:
        mape = np.mean(np.abs((df[depVar]-df['predicted']) / df[depVar])) * 100
    except Exception as e:
        print("Error occured while calculating mape")
        print(e)
    try:
        print("MAPE : %f"%mape)
    except Exception as e:
        print("Mape value does not exist")
        print(e)
    return df


# In[82]:


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




def stl(df, depVar, model='additive', filt=None, freq=None, two_sided=True, extrapolate_trend=0):
    
    
    # decomposing data into trend, sesonal and irregular component
    try:
        stlDecompose = sm.tsa.seasonal_decompose(df['value'], model=model, filt=filt, freq=freq, two_sided=two_sided, extrapolate_trend=extrapolate_trend)
    except Exception as e:
        print("Unable to run Seasonal Decompose with the given input")
        print(e)
    
    # setting diffrent component of data into figure
    try:
        data = [go.Scatter(x=stlDecompose.trend.index,y=stlDecompose.trend)]
        layout = go.Layout(
                    title="Trend",
                    yaxis=dict(
                        title="Trend"
                    ),
                    xaxis=dict(
                        title="Timeline"
                    )
                )
        trendPlot = go.Figure(data=data,layout=layout)
    except Exception as e:
        print("Cannot plot trend of the given data")
        print(e)
    try:
        data = [go.Scatter(x=stlDecompose.seasonal.index,y=stlDecompose.seasonal)]
        layout = go.Layout(
                    title="Seasonality",
                    yaxis=dict(
                        title="Seasonality"
                    ),
                    xaxis=dict(
                        title="Timeline"
                    )
                )
        seasonalPlot = go.Figure(data=data,layout=layout)
    except Exception as e:
        print("Cannot plot seasonality of the given data")
        print(e)
    try:
        data = [go.Scatter(x=stlDecompose.resid.index,y=stlDecompose.resid)]
        layout = go.Layout(
                    title="Irregularity",
                    yaxis=dict(
                        title="Irregularity"
                    ),
                    xaxis=dict(
                        title="Timeline"
                    )
                )
        irregularPlot = go.Figure(data=data,layout=layout)
    except Exception as e:
        print("Cannot plot irregularity of the given data")
        print(e)
    return trendPlot,seasonalPlot,irregularPlot


# In[85]:


##################################################################################################################################################################################
##FUNCTION: Saves model output to csv in new row, saves plots with iteration number+timestamp
##INPUT: concordance - returned output form concordance values,confuse -output returned from confusion matrix function, roc - output returned from generateRoc function, ks - output returned from ks function
##OUTPUT: Appends model output to next row in csv 
##################################################################################################################################################################################
import os
import csv
def saveIterations(parameters,mape_arima,timestamp):
    """
saveIteration - A function to create folder for saving plots,csv and text iterations,model comparison and description in Python
================================================================================================================================

**saveIterations** is a Python package by MathMarket (Team Tesla) of TheMathCompany,
It aims to create folder in working directory with "saved files" name.
Iterations for table and text files are saved in "saved files/iterations"
Itetrations for plots are saved in "saved files/plots"

Iterable csv file named as Model comparison in created in "saved files/iterations" to compare iterations made, 
*clickable link* is given in iteration_number column of Model comparison csv file

*Clickable link* leads to HTML file created by the function named Model description for the clicked row.

This function returns naming convention i.e. *user input* +timestamp

The module is built using matplotlib,pandas.
Therefore, these libraries are need to be installed in order to use the module.

The module consist of one function:
`saveIterations(concordence,confuse,roc,ks,iteration_values,gains_table,timestamp,gains_plot,train_confuse,test_confuse,dataset_name)`
"""    
    try:
        user_input = input("Enter Iteration Number: ")
    except Exception as e:
        print(e)
        print("Could not get user input for iteration number, please give appropriate input.")
    try:
        timestamp = str(timestamp)
    except Exception as e:
        print(e)
    try:
        timestamp = filter(str.isalnum, timestamp)
    except Exception as e:
        print(e)
    try:
        timestamp = list(timestamp)
    except Exception as e:
        print(e)
    try:
        str1 = "".join(timestamp)
    except Exception as e:
        print(e)
    try:
        user_input = user_input+"  - "+str1
    except Exception as e:
        print(e)
        print("Could not append timestamp with user input.")
    try:
        wd = os.getcwd()
    except Exception as e:
        print(e)
    try:
        url = wd+"/saved files/{}"
    except Exception as e:
        print(e)
    try:
        hyperlink = '=HYPERLINK("%s.html", "%s")' % (url.format(user_input), user_input)
    except Exception as e:
        print(e)
        print("Could not generate hyperlink.")
    try:
        saved_files = r'saved files'
    except Exception as e:
        print(e)
    try:
        if not os.path.exists(saved_files):
            os.makedirs(saved_files)
    except Exception as e:
        print(e)
    try:
        newpath = r'saved files/plots' 
    except Exception as e:
        print(e)
    try:
        if not os.path.exists(newpath):
            os.makedirs(newpath)
    except Exception as e:
        print(e)
    try:
        metrices = r'saved files/iterations' 
    except Exception as e:
        print(e)
    try:
        if not os.path.exists(metrices):
            os.makedirs(metrices)
    except Exception as e:
        print(e)
        print("Could not generate folders for saving iterations")
    try:
        Model_comparison = pd.DataFrame(dict(model=[],iteration_number = [],order=[], MAPE = [], AIC = [],level=[],trend=[],exog=[],seasonal=[],freq_seasonal=[],cycle=[], autoregressive=[],irregular=[],stochastic_level=[],stochastic_trend=[],stochastic_seasonal=[],stochastic_freq_seasonal=[],stochastic_cycle=[],damped_cycle=[],cycle_period_bounds=[],mle_regression=[])) 
    except Exception as e:
        print(e)
    try:
        if not os.path.exists(r"saved files/iterations/"+"Model_comparison.csv"):
            Model_comparison.to_csv(r"saved files/iterations/"+"Model_comparison.csv",index = False)
    except Exception as e:
        print(e)
        print("Could not create and save Model comparison.")
    try:
        d1 = parameters
    except Exception as e:
        print(e)
    try:
        d2 = mape_arima
    except Exception as e:
        print(e)
    try:
        d3 = {"iteration_number":hyperlink}
    except Exception as e:
        print(e)
    try:
        d1.update(d2)
    except Exception as e:
        print(e)
    try:
        d1.update(d3)
    except Exception as e:
        print(e)
    try:
        with open(r'saved files/iterations/'+r'Model_comparison.csv', 'a', newline='') as csvfile:
            
            fieldnames = ['model','iteration_number','order','MAPE','AIC','level','trend','exog','seasonal','freq_seasonal','cycle','autoregressive','irregular','stochastic_level','stochastic_trend','stochastic_seasonal','stochastic_freq_seasonal','stochastic_cycle','damped_cycle','cycle_period_bounds','mle_regression']
            writer = csv.DictWriter(csvfile, fieldnames=fieldnames)
            writer.writerow(d1)
    except Exception as e:
        print(e)
        print("Could not append iteration to model comparison.")
    try:
        return(user_input)
    except Exception as e:
        print(e)


# In[86]:


def gridSearchMovingAverage(df,depVar, bestWindow=2):
    
    max_lag = len(df)
    
    leasterror = np.inf
    
    print('Running GridSearch for Moving Average')
    
    #use gridsearch to look for optimial Moving Average parameters
    for param in range(2,max_lag):
        try:
            results =df[depVar].rolling(window=param).mean().shift()

            #if current run of AIC is better than the best one so far, overwrite it
            currenterror = np.mean(np.abs((df[depVar]-results)/df[depVar])) * 100

            if currenterror<leasterror:
                leasterror = currenterror
                bestWindow = param
        except:
            continue
                
    print('the best MAPE and Window for Moving Average :',leasterror,bestWindow)
    return leasterror,bestWindow


# In[87]:


def gridSearchHoltWinter(df,depVar,trend=['add','mul',None],seasonal=['add','mul',None],seasonal_periods=[1,3,4,6,12]):
    
    itemObj = df[depVar]
    parameter = list(itertools.product(trend,seasonal,seasonal_periods))
    
    
    leasterror = np.inf
    
    print('Running GridSearch for Moving Average')
    
    #use gridsearch to look for optimial Moving Average parameters
    for param in parameter:
        try:
            results =df[depVar].rolling(window=param).mean().shift()

            #if current run of AIC is better than the best one so far, overwrite it
            currenterror = np.mean(np.abs((df[depVar]-results)/df[depVar])) * 100

            if currenterror<leasterror:
                leasterror = currenterror
                bestParam = param
        except:
            continue
                
    print('the best MAPE and Window for Moving Average :',leasterror,bestParam)
    return bestParam