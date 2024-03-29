# -*- coding: utf-8 -*-

import pandas as pd
import numpy as np
from matplotlib import pyplot
from statsmodels.tsa.statespace.structural import UnobservedComponents

@pd.api.extensions.register_dataframe_accessor("ucm")
class ucm(object):
    
    def __init__(self, pandas_obj):
        self._obj = pandas_obj
        
    def build(self, dep_var, level=False, trend=False, seasonal=None, exog=None, freq_seasonal=None, cycle=False, autoregressive=None, irregular=False, stochastic_level=False, stochastic_trend=False, stochastic_seasonal=True, stochastic_freq_seasonal=None, stochastic_cycle=False, damped_cycle=False, cycle_period_bounds=None, mle_regression=True, start_params=None, transformed=True, cov_type='opg', cov_kwds=None, method='lbfgs', maxiter=50, full_output=1, disp=5, callback=None, return_params=False, optim_score=None, optim_complex_step=None, optim_hessian=None, flags=None, file_name=False):
        
        """
        Univariate unobserved components time series model
    
        These are also known as structural time series models, and decompose a
        (univariate) time series into trend, seasonal, cyclical, and irregular
        components.
    
        Parameters
        ----------
    
        For Model Build
        ---------------

        dep_var: dependent variable name
            The name of the variable for prediction.
        exog : array_like or None, optional
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
        freq_seasonal : list of dicts or None, optional
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
    
            yt = µt + γt + ψt + εt, εt ∼ NID(0, σ2ε), t = 1, . . . , n,
    
        where :math:`y_t` refers to the observation vector at time :math:`t`,
        :math:`\mu_t` refers to the trend component, :math:`\gamma_t` refers to the
        seasonal component, :math:`c_t` refers to the cycle, and
        :math:`\varepsilon_t` is the irregular. The modeling details of these
        components are given below.
    
        **Trend**
    
        The trend component is a dynamic extension of a regression model that
        includes an intercept and linear time-trend. It can be written:
    
        .. math::
    
            µt+1 = µt + ηt, ηt ∼ NID(0, σ2η)
    
        where NID(0, σ2) refers to a normally independently distributed series 
        with mean zero and variance σ2. The disturbance series ηt is therefore 
        serially independent and mutually independent of all other disturbance 
        series related to y
    
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
        model specifications :
    
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
    
        It is usually more appropriate to allow the seasonal pattern to change 
        (slowly) over time. For this purpose we can relax the summing-to-zero 
        constraint by replacing it with the stochastic equation given by The 
        seasonal component is modeled as:

        γt+1 = −γt − . . . − γt−S+2 + ωt, γt+j = γt+j−S, ωt ∼ NID(0, σ2ω)

        where the disturbance series ωt is serially independent and mutually 
        independent of all other disturbance series, for t = S − 1, . . . , n 
        and j = 2, . . . , S − 1. The initial variables γS−1, . . . , γ1 are 
        treated as unknown coefficients. When the disturbance variance 
        (σ^2)ω = 0, we return to the case of fixed seasonal effects. When the 
        variance is large, the seasonal pattern will vary quickly over time.
    
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

        Usage
        -----
        >>> ucmModel = dfTrain.ucm.build('value', level='rwalk', trend=False, seasonal = 4, stochastic_seasonal=False, cycle= False,
        >>>                     autoregressive = 0, irregular=True, maxiter=1)
        >>>
        """
        df=self._obj
        import warnings
        warnings.simplefilter('ignore')
        model = UnobservedComponents(df[dep_var], level=level, trend=trend, exog=exog, seasonal=seasonal, freq_seasonal=freq_seasonal, cycle=cycle, autoregressive=autoregressive, irregular=irregular, stochastic_level=stochastic_level, stochastic_trend=stochastic_trend, stochastic_seasonal=stochastic_seasonal, stochastic_freq_seasonal=stochastic_freq_seasonal, stochastic_cycle=stochastic_cycle, damped_cycle=damped_cycle, cycle_period_bounds=cycle_period_bounds, mle_regression=mle_regression)
        model = model.fit(method='powell', disp=False)
        if not file_name:
            file_name = 'buildModelUCM.pkl'
        model.save(file_name)
        return(model)
        
        
    def getSummary(self,model, plotSize = (15,9)):
        """
        This Function retruns the summary of UCM function containing
        plot of  series component etc.

        Parameters
        ----------
        model : model_instance, required
            Fitted UCM model object
        dep_var : 'str', required
            The column name of the dependent variable

        
        Usage
        -----
        >>> ucmSummary = dfTrain.ucm.getSummary(ucmModel)
        >>>
        """
        df=self._obj
        # residual errors in the train set
        return(model.summary(), model.plot_components(legend_loc='lower right', figsize=plotSize))

        """
        Return
        ------
        Modified train dataframe with the forecasted values appended as a column 'predicted'
        """
            
    def scoreTrain(self,model,dep_var):
        """
        This function return the predicted values for train set and print
        MAPE for the Train set.

        Parameters
        ----------
        model : model_instance, required
            Fitted UCM model object
        dep_var : 'str', required
            The column name of the dependent variable

        
        Usage
        -----
        >>> ucmDfTrain = dfTrain.ucm.scoreTrain(ucmModel, 'value')
        >>>
        """
        
        df=self._obj
        df['predicted'] =model.predict(start=0,end=len(df))
        return df

        """
        Return
        ------
        Modified train dataframe with the forecasted values appended as a column 'predicted'
        """
    
    def scoreTest(self,model,dep_var):
        """
        This function returns the forecasted values for test set.

        Parameters
        ----------

        model : model_instance, required
            Fitted UCM model object
        dep_var : 'str', required
            The column name of the dependent variable
        Usage
        -----
        >>> ucmDfTest = dfTest.ucm.scoreTest(ucmModel, 'value')
        >>>
        """
        
        df=self._obj
        df['predicted']=np.array(model.forecast(len(df)))
        return df

        """
        Return
        ------
        Modified test dataframe with the forecasted values appended as a column 'predicted'
        """
