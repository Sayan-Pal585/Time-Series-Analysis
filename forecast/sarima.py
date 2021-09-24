# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
from statsmodels.tsa.statespace.sarimax import SARIMAX

@pd.api.extensions.register_dataframe_accessor("sarima")
class sarima(object):
    """
    Although arima can handle data with a trend, it does not support
    time series with a seasonal component. An extension to ARIMA that
    supports the direct modeling of the seasonal component of the series is
    called SARIMA. ARIMA expects data that is either not seasonal or has
    the seasonal component removed, e.g. seasonally adjusted via methods
    such as seasonal differencing. Seasonal Autoregressive Integrated
    Moving Average, SARIMA or Seasonal ARIMA, is an extension of ARIMA that
    explicitly supports univariate time series data with a seasonal
    component. It adds three new hyperparameters to specify the
    autoregression (P), differencing (D) and moving average (Q) for the
    seasonal component of the series, as well as an additional parameter
    for the period of the seasonalitym.

    Trend Elements: p: Trend autoregression order,
    d: Trend difference order, q: Trend moving average order.
    Seasonal Elements: P: Seasonal autoregressive order,
    D: Seasonal difference order, Q: Seasonal moving average order,
    m: The number of time steps for a single seasonal period.
    """
    def __init__(self, pandas_obj):
        self._obj = pandas_obj

    def build(self, dep_var, exog=None, order=(0, 0, 1), seasonal_order=(0, 0, 0, 0), trend=None, measurement_error=False, time_varying_regression=False, mle_regression=True, simple_differencing=False, enforce_stationarity=True, enforce_invertibility=True, hamilton_representation=False, start_params=None, transformed=True, cov_type='opg', cov_kwds=None, method='lbfgs', maxiter=50, full_output=1, disp=False, callback=None, return_params=False, optim_score=None, optim_complex_step=None, optim_hessian=None, flags=None, file_name=False):
        """
        Parameters
        ----------
        For Model Built
        ---------------
        dep_var : 'str', required
            The column name of the dependent variable
        exog : 'arr', optional (Default=None)
            Array of exogenous regressors, shaped nobs x k.
            Exogenous variables. Series or dataframe of independent variables
        order : 'arr', required, (Default=(0,0,1))
            Iterable or iterable of iterables.
            The (p,d,q) order of the model for the number of AR parameters,
            differences, and MA parameters. `d` must be an integer
            indicating the integration order of the process, while
            `p` and `q` may either be an integers indicating the AR and MA
            orders (so that all lags up to those orders are included) or else
            iterables giving specific AR and / or MA lags to include. Default is
            an AR(1) model: (1,0,0).
        seasonal_order : 'arr',iterable, required (Default=(0,0,0,0))
            The (P,D,Q,s) order of the seasonal component of the model for the
            AR parameters, differences, MA parameters, and periodicity.
            `d` must be an integer indicating the integration order of the process,
            while `p` and `q` may either be an integers indicating the AR and MA
            orders (so that all lags up to those orders are included) or else
            iterables giving specific AR and / or MA lags to include. `s` is an
            integer giving the periodicity (number of periods in season), often it
            is 4 for quarterly data or 12 for monthly data. Default is no seasonal
            effect.
        endog : array_like
            The observed time-series process :math:`y`.
        exog : array_like, optional
            Array of exogenous regressors, shaped nobs x k.
            Exogenous variables.
            Series or dataframe of independent variables
        trend : 'str' {'n','c','t','ct'} or iterable, optional (Default=None)
            Parameter controlling the deterministic trend polynomial :math:`A(t)`.
            Can be specified as a string where 'c' indicates a constant (i.e. a
            degree zero component of the trend polynomial), 't' indicates a
            linear trend with time, and 'ct' is both. Can also be specified as an
            iterable defining the polynomial as in `numpy.poly1d`, where
            `[1,1,0,1]` would denote :math:`a + bt + ct^3`. Default is to not
            include a trend component.
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
            `[1,1,0,1]` would denote :math:`a + bt + ct^3`. 
        measurement_error : 'boolean', optional (Default=False)
            Whether or not to assume the endogenous observations `endog` were
            measured with error.
        time_varying_regression : 'boolean', optional (Default=False)
            Used when an explanatory variables, `exog`, are provided provided
            to select whether or not coefficients on the exogenous regressors are
            allowed to vary over time.
        mle_regression : 'boolean', optional (Default=True)
            Whether or not to use estimate the regression coefficients for the
            exogenous variables as part of maximum likelihood estimation or through
            the Kalman filter (i.e. recursive least squares). If
            `time_varying_regression` is True, this must be set to False. 
        simple_differencing : 'boolean', optional (Default=False)
            Whether or not to use partially conditional maximum likelihood
            estimation. If True, differencing is performed prior to estimation,
            which discards the first :math:`s D + d` initial rows but results in a
            smaller state-space formulation. If False, the full SARIMAX model is
            put in state-space form so that all datapoints can be used in
            estimation. 
        enforce_stationarity : 'boolean', optional (Default=True)
            Whether or not to transform the AR parameters to enforce stationarity
            in the autoregressive component of the model. 
        enforce_invertibility : 'boolean', optional (Default=True)
            Whether or not to transform the MA parameters to enforce invertibility
            in the moving average component of the model.
        hamilton_representation : 'boolean', optional (Default=False)
            Whether or not to use the Hamilton representation of an ARMA process
            (if True) or the Harvey representation (if False). 

        For Model Fit
        -------------
        Fits the model by maximum likelihood via Kalman filter.

        Parameters
        ----------
        start_params : 'arr', optional, (Default=None)
            Initial guess of the solution for the loglikelihood maximization.
            If None, the default is given by.
        transformed : 'boolean', optional (Default=True)
            Whether or not `start_params` is already transformed. 

        cov_type : 'str', optional (Default='opg')
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
        cov_kwds : 'dict' or None, optional (Default='None')
            A dictionary of arguments affecting covariance matrix computation.

            **opg, oim, approx, robust, robust_approx**

            - 'approx_complex_step' : boolean, optional - If True, numerical
              approximations are computed using complex-step methods. If False,
              numerical approximations are computed using finite difference
              methods. Default is True.
            - 'approx_centered' : boolean, optional - If True, numerical
              approximations computed using finite difference methods use a
              centered approximation. Default is False.
        method : 'str', optional (Default='lbfgs')
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
        maxiter : 'int', optional (Default='None')
            The maximum number of iterations to perform.
        full_output : 'boolean', optional (Default=1)
            Set to True to have all available output in the Results object's
            mle_retvals attribute. The output is dependent on the solver.
            See LikelihoodModelResults notes section for more information.
        disp : 'boolean', optional (Default=False)
            Set to True to print convergence messages.
        callback : callable callback(xk), optional (Default=None)
            Called after each iteration, as callback(xk), where xk is the
            current parameter vector.
        return_params : 'boolean', optional (Default=False)
            Whether or not to return only the array of maximizing parameters.
            Default is False.
        optim_score : {'harvey', 'approx'} or None, optional (Default='None)
            The method by which the score vector is calculated. 'harvey' uses
            the method from Harvey (1989), 'approx' uses either finite
            difference or complex step differentiation depending upon the
            value of `optim_complex_step`, and None uses the built-in gradient
            approximation of the optimizer. This keyword is only relevant if the optimization method uses the score.
        optim_complex_step : 'bool', optional (Default='None')
            Whether or not to use complex step differentiation when
            approximating the score; if False, finite difference approximation
            is used. Default is True. This keyword is only relevant if
            `optim_score` is set to 'harvey' or 'approx'.
        optim_hessian : {'opg','oim','approx'}, optional (Default='None')
            The method by which the Hessian is numerically approximated. 'opg'
            uses outer product of gradients, 'oim' uses the information
            matrix formula from Harvey (1989), and 'approx' uses numerical
            approximation. This keyword is only relevant if the
            optimization method uses the Hessian matrix.

        Usage
        -----
        >>> sarimaModel = dfTrain.sarima.build('value', order = (0,0,2), seasonal_order = (2,1,2,4), enforce_invertibility=False)
        >>>
        """
        df = self._obj
        import warnings
        warnings.simplefilter("ignore")
        model = SARIMAX(df[dep_var], exog=exog, order=order, seasonal_order=seasonal_order, trend=trend, measurement_error=measurement_error, time_varying_regression=time_varying_regression, mle_regression=mle_regression, simple_differencing=simple_differencing, enforce_stationarity=enforce_stationarity, enforce_invertibility=False, hamilton_representation=hamilton_representation)
        model =model.fit(disp=disp)
        if not file_name:
            file_name = 'buildModelSarima.pkl'
        model.save(file_name)
        return(model)
        """
        Returns
        -------
        Model fit
        """
    
    def getSummary(self,model):
        """
        This function summarizes the model
        
        Parameters
        ---------
        model : model_instance, required
            Fitted SARIMA model object

        Usage
        -----
        >>> sarimaSummary = dfTrain.sarima.getSummary(sarimaModel)
        >>>
        """
        return model.summary()

        """
        Returns
        -------
        Dataframe comprising of the model summary
        """
    
    def scoreTrain(self,model,dep_var):
        """This function returns the predicted values for train set.

        Parameters
        ----------
        model : model_instance, required
            Fitted SARIMA model object
        dep_var : 'str', required
            The column name of the dependent variable

        Usage
        -----
        >>> sarimaDfTrain = dfTrain.sarima.scoreTrain(sarimaModel, 'value')
        >>>
        """
        df=self._obj
        # Calculating predicted values using predict function
        df['predicted'] =model.predict(start=0,end=len(df))
        return df

        """
        Return
        ------
        Modified train dataframe with the forecasted values appended as a column 'predicted'
        """
    
    def scoreTest(self,model,dep_var):
        """This function returns the predicted values for train set.

        Parameters
        ----------
        model : model_instance, required
            Fitted SARIMA model object
        dep_var : 'str', required
            The column name of the dependent variable

        Usage
        -----
        >>> sarimaDfTest = dfTest.sarima.scoreTest(sarimaModel, 'value')
        >>>
        """
        
        df=self._obj
        df['predicted']=model.forecast(len(df))
        return df
        """
        Return
        ------
        Modified test dataframe with the forecasted values appended as a column 'predicted'
        """